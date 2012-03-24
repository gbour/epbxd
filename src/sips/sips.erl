
-module(sips).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

-export([start_link/3, init/1, acceptor/2, processor/2, code_change/3, terminate/2, handle_call/3, handle_info/2,
	handle_cast/2]).
-ifdef(debug).
	-export([sipdecoder/1, response/2, msgencode/2]).
-endif.


-include("sips.hrl").
-include("utils.hrl").

-define(TCP_OPTIONS,[list, {packet,0},{active,false},{reuseaddr,true}]).


start_link(Mod, Args, Opts) ->
	%%TODO: manage port as configuration parameter
	Server = #server{port=7779},
	gen_server:start_link({local, sips}, ?MODULE, [Server, {sips, sipdecoder}], []).


%% we extract Port variable from Server record
init([Server = #server{port=Port}, Callback]) ->
	%% create mnesia tables
	mnesia:create_table(registrations, 
		[{attributes,record_info(fields,registration)},{record_name,registration}]),
	mnesia:create_table(endpoints, 
		[{attributes,record_info(fields,endpoint)},{record_name,endpoint}]),
	lists:foreach(fun(EP) ->
				mnesia:dirty_write(endpoints, #endpoint{name=proplists:get_value(name,EP)})
		end, config:get(endpoints)
	),

	%% /END

	%%TODO: manage TCP/UDP switch
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, Socket} -> 
			%% make a copy of Server record, setting socket variable
			_State = Server#server{socket=Socket},
			%% spawn accept process
			proc_lib:spawn_link(?MODULE, acceptor, [Socket, Callback]),
			{ok, _State};

		{error, Reason} ->
			?ERROR("sips::gen_tcp:listen= ~p", [Reason]),
			{stop, Reason}
	end.

%% accept loop
acceptor(LSocket, Callback) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket}   -> 
			%% should use a supervisor to manage a pool of processors
			P = proc_lib:spawn_link(?MODULE, processor, [Socket, Callback]),
			%%gen_tcp:controlling_process(Socket, P),

			acceptor(LSocket, Callback);
			
		{error, Reason} ->
			?ERROR("sips::gen_tcp:accept= ~p", [Reason]),
			{error, Reason}
	end.

%% process incoming connection
processor(Socket, {M, C}) ->
	random:seed(now()),

	%% invoke message decoder
	{Status, Messages} = M:C(Socket),

	%%TODO: handle messages
	?DEBUG("processor:end= ~p", [Status]),
	case Status of
		ok ->
			lists:foreach(fun(M) -> handle(M, Socket) end, Messages)
	end.

%% decode SIP message
sipdecoder(Socket) ->
	%% read socket
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} -> 
			?DEBUG("~p", [Data]),
			msgdecode([], Data);

		{error, Reason} ->
			?ERROR("sips:gen_tcp::recv= ~p", [Reason]),
			{error, Reason}
	end.

send(Sock, Message) ->
	?DEBUG("send message: ~s", [msgencode(Message)]),
	gen_tcp:send(Sock, msgencode(Message)).

%%
%% Decode SIP message
%%
msgdecode(Mms, []) ->
	{ok, Mms};
msgdecode(Mms, Stream)  ->
	case string:str(Stream, "\r\n\r\n") of
		0 ->
			{invalid, Mms};
		P ->
			{Status, Mn, Rest} = headers_decode(
				start,
				#message{},
				string:tokens(string:substr(Stream,1,P-1), "\r\n"), 
				string:substr(Stream,P+4)
			),
			msgdecode(Mms++[Mn], Rest)
	end.


headers_decode(start, Message, [Token|Next], Rest) ->
	case string:tokens(Token, " ") of
		["SIP/"++Version, Status, Reason] ->
			headers_decode(
				header,
				Message#message{type=response,version=Version,status=list_to_integer(Status),reason=Reason},
				Next,
				Rest
			);

		[Method,URI,"SIP/"++Version] ->
			headers_decode(
				header,
				Message#message{type=request,version=Version,method=list_to_atom(Method),uri=URI},
				Next,
				Rest
			);

		_Else ->
			?ERROR("sips:msgdecode= invalid SIP message start-line= ~p", [_Else]),
			{error, invalid_message}
	end;

%msgdecode(header, Message, [""|Next]) ->
%	Message#message{content=string:join(Next, "\r\n")};

headers_decode(header, Message, [Token|Next], Rest) ->
	[Key, Value] = re:split(Token, ": ", [{return,list},{parts,2}]),
	M = Message#message{headers=dict:append(
			utils:title(Key),
			header:decode(utils:title(Key), Value),
			Message#message.headers
		)},
	headers_decode(header, M, Next, Rest);
% end of headers -> reading content
% TODO: Content-Length is expressed in octet, not characters
headers_decode(header, Message, [], Rest) ->
	case lists:nth(1,dict:fetch("Content-length", Message#message.headers)) of
		0 ->
			{ok, Message, Rest};
		L ->
			{C, R} = lists:split(L,Rest),
			{ok, Message#message{content=C},R}
	end.
				


%%
%% Encode SIP message
%%
msgencode(Message) ->
	msgencode(start, Message).
msgencode(start, Message = #message{version=V,type=request,method=M,uri=U,headers=H}) ->
	string:join(
		lists:append([
			[atom_to_list(M)," ",uri:encode(U)," SIP/",V,"\r\n"],
			lists:map(fun({X,Y}) -> header:encode(X,Y)++"\r\n" end, H),
			["\r\n"]
		]),
	"");
msgencode(start, Message = #message{version=V, type=response, status=S, reason=R, headers=H}) ->
	string:join(
		lists:append([
			["SIP/",V," ",integer_to_list(S)," ",R,"\r\n"],
			lists:map(fun({X,Y}) -> header:encode(X,Y)++"\r\n" end, H),
			["\r\n"]
		]),
	"");
msgencode(start, M) ->
	fail.
%%#message(type=response,type=response,status=200,reason=Trying,

%% response code
response(trying, Msg)    ->
	Msg#message{type=response, status=100, reason="Trying"};
response(ok    , Msg)    ->
	Msg#message{type=response, status=200, reason="OK"}.
% custom reason
response(Type, Msg, Reason) ->
	Response = response(Type, Msg),
	Response#message{reason=Response#message.reason++" "++Reason}.

%%
%% Handle SIP REQUESTS
%%

%% REGISTER
handle(M=#message{type=request,method=REGISTER, headers=Headers}, Sock) ->
	?DEBUG("handle register", []),

	% send TRYING message
	%gen_tcp:send(Sock, msgencode(response(M, 100))),

	% lookup endpoints database
	To    = lists:nth(1,dict:fetch("To", Headers)),
	User  = To#address.uri#uri.user,
	?DEBUG("SIP:REGISTER= loopkup ~s endpoint", [User]),

	% default registration expiry (in seconds)
	Expires = 3600,

	case
		mnesia:dirty_read(endpoints,User)
	of
		% 200 OK
		[Endpt] -> 
				?DEBUG("Found endpoint: ~p", [Endpt]),

				Contact = lists:nth(1,dict:fetch("Contact",Headers)),
				Ua      = lists:nth(1,dict:fetch("User-agent", Headers)),

				mnesia:dirty_write(registrations,#registration{
					name   = User,
					uri    = Contact#address.uri
				}),

				% TODO: handle Via header
				To =  lists:nth(1,dict:fetch("To",Headers)),
				_To =	To#address{params=lists:append(To#address.params,[{"tag",header:tag()}])},

				Via = lists:nth(1,dict:fetch("Via",Headers)),

				send(Sock, response(ok, #message{
					headers=[
						{"Via"           , Via},
						{"From"          , lists:nth(1,dict:fetch("From",Headers))},
						{"To"            ,_To},
						{"Call-id"       , lists:nth(1,dict:fetch("Call-id",Headers))},
						{"Cseq"          , lists:nth(1,dict:fetch("Cseq",Headers))},
						{"Expires"       , 3600},
						{"User-agent"    ,"epbxd"},
						{"Allow"         ,"foobar"},
						{"Content-length",0}
					]
				}, "\\o/")),

				ok;

		% 404 NOT FOUND
		[]      -> ?DEBUG("Endpoint not found. Returning 404",[])
	end,

	ok;
handle(#message{type=T,method=M,status=S,reason=R}, _) ->
	?DEBUG("unknown message ~w ~w/~b ~s",[T,M,S,R]),
	fail.

%% unused

code_change(_OldVersion, Library, _Extra) ->
	{ok, Library}.
terminate(_Reason, _Library) ->
	ok.
handle_call(_Msg, _Caller, State) ->
	{noreply, State}.
handle_info(_Msg, Library) ->
	{noreply, Library}.
handle_cast({free, Ch}, State) ->
	{noreply, State}.


-module(sips).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

-export([start_link/3, init/1, acceptor/2, processor/2, code_change/3, terminate/2, handle_call/3, handle_info/2,
	handle_cast/2]).
-ifdef(debug).
	-export([sip_decoder/2,response/2, msgencode/1, msgencode/2]).
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
	%% invoke message decoder
	{Status, Message} = M:C(Socket, #message{}),

	%%TODO: handle messages
	?DEBUG("processor:end= ~p", [Status, Message]),
	handle(Message, Socket).

%% decode SIP message
sipdecoder(Socket, Message) ->
	%% read socket
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} -> 
			?DEBUG("~p", [Data]),
			{ok, msgdecode(start, Message, re:split(Data, "\r\n", [{return,list}]))};

		{error, Reason} ->
			?ERROR("sips:gen_tcp::recv= ~p", [Reason]),
			{error, Reason}
	end.

%% decode SIP headers
msgdecode(start, Message, [Token|Next]) ->
	M = case re:split(Token," ",[{return,list},{parts,3}]) of
		["SIP/"++Version, Status, Reason] ->
			Message#message{type=response,version=Version,status=Status,reason=Reason};
		[Method,URI,"SIP/"++Version] ->
			Message#message{type=request,version=Version,method=list_to_atom(Method),uri=URI};

		_Else ->
			?ERROR("sips:msgdecode= invalid SIP message start-line= ~p", [_Else]),
			""
	end,

	msgdecode(header, M, Next);

msgdecode(header, Message, [""|Next]) ->
	Message#message{content=string:join(Next, "\r\n")};

msgdecode(header, Message, [Token|Next]) ->
	[Key, Value] = re:split(Token, ": ", [{return,list},{parts,2}]),
	M = Message#message{headers=dict:append(utils:title(Key),Value,Message#message.headers)},
	msgdecode(header, M, Next).


%% encode message
msgencode(Message) ->
	msgencode(start, Message).
msgencode(start, Message = #message{version=V, type=request, method=M, uri=U}) ->
	M++" "++U++" SIP/"++V++"\r\n";
msgencode(start, Message = #message{version=V, type=response, status=S, reason=R, headers=H}) ->
	EH = lists:map(fun({X,Y}) -> io_lib:format("~s: ~s\r\n", [utils:title(X),Y]) end,	
		dict:to_list(H)),
	lists:append([["SIP/",V," ",integer_to_list(S)," ",R,"\r\n"], EH, ["\r\n"]]);
msgencode(start, M) ->
	fail.
%%#message(type=response,type=response,status=200,reason=Trying,

%% response code
response(Request, 100) ->
	Request#message{type=response, status=100, reason="Trying"};
response(Request, 200) ->
	Request#message{type=response, status=200, reason="OK"}.


handle(M=#message{type=request,method=REGISTER, headers=Headers}, Sock) ->
	?DEBUG("handle register", []),

	gen_tcp:send(Sock, msgencode(response(M, 100))),

	?DEBUG("~p", dict:fetch("User-agent", Headers)),
	%%mnesia:write(#registration{ip=,port=,transport=,ua=,timeout=,ping=}).
	gen_tcp:send(Sock, msgencode(response(M, 200))),
	ok;
handle(_, _) ->
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

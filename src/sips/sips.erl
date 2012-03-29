
-module(sips).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

-export([start_link/3, init/1, acceptor/2, processor/2, code_change/3, terminate/2,
		handle_call/3, handle_info/2, handle_cast/2, handle/2, app/3]).
-ifdef(debug).
	-export([sipdecoder/1, response/2, msgencode/2]).
-endif.


-include("sips.hrl").
-include("dialplan.hrl").
-include("sdp/sdp.hrl").
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

	%% ETS tables
	case ets:info(transactions) of
		undefined ->
			ets:new(transactions, [set,named_table,{read_concurrency,true},{keypos,#transaction.key}]);
		_ ->
			ets:delete_all_objects(transactions)
	end,
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
			?DEBUG("new incoming connexion: ~w -> ~w~n", [inet:peername(Socket), inet:sockname(Socket)]),

			%% should use a supervisor to manage a pool of processors
			P = proc_lib:spawn_link(?MODULE, processor, [Socket, Callback]);
			%%gen_tcp:controlling_process(Socket, P),
			
		{error, Reason} ->
			?ERROR("sips::gen_tcp:accept= ~p", [Reason])
	end,

	acceptor(LSocket, Callback).

%% process incoming connection
processor(Socket, {M, C}) ->
	random:seed(now()),

	%% invoke message decoder
	{Status, Messages} = M:C(Socket),

	%%TODO: handle messages
	?DEBUG("processor:end= ~p", [Status]),
	case Status of
		ok ->
			%% spawn a new process to handle message
			%% thus we can receive another data
			%% TODO: handle this smarter
			proc_lib:spawn(fun() ->
				lists:foreach(fun(M) -> 
							handle(M, Socket) end, Messages)
			end),
			processor(Socket, {M,C});
		_ -> 
			%gen_tcp:close(Socket),
			pass
	end.

%% decode SIP message
sipdecoder(Socket) ->
	%% read socket
	?DEBUG("waiting for data ~w~n", [inet:sockname(Socket)]),

	case gen_tcp:recv(Socket, 0) of %, 3000) of
		{ok, Data} -> 
			?DEBUG("~p", [Data]),
			msgdecode([], Data);

		{error, Reason} ->
			?ERROR("sips:gen_tcp::recv= ~p", [Reason]),
			{error, Reason}
	end.

send(undefined, Message) ->
	%WARNING: response message do not have uri value
	S = Message#message.uri,
	send(S, Message);

send(S=#uri{}, Message) ->
	spawn(fun() ->
		io:format("connecting to ~p~n", [S]),

		case gen_tcp:connect(S#uri.host, list_to_integer(S#uri.port),
				[list,{packet,0},{reuseaddr,true},{active,false}]) of
			{ok, Sock} ->
				%io:format("::: ~w~n", [gen_tcp:recv(Sock,0,2000)]),
				send(Sock, Message),
				%io:format("::: ~w~n", [gen_tcp:recv(Sock,0,2000)]),
				%gen_tcp:close(Sock),
				%proc_lib:spawn_link(?MODULE, processor, [Sock, {sips, sipdecoder}]),
				processor(Sock, {sips,sipdecoder}),

				{ok};
			{error, Reason} ->
				io:format("send::fail to connect: ~p", [Reason]),
				{error, Reason}
		end
	end);

send(Sock, Message) ->
	?DEBUG("send message (~w): ~s", [inet:peername(Sock), msgencode(Message)]),
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

			case Status of 
				ok ->
					msgdecode(Mms++[Mn], Rest);
				_  ->
					{error, fail}
			end
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
				Message#message{type=request,version=Version,method=list_to_atom(Method),uri=uri:decode(URI)},
				Next,
				Rest
			);

		_Else ->
			?ERROR("sips:msgdecode= invalid SIP message start-line= ~p", [_Else]),
			{error, invalid_message, []}
	end;
headers_decode(start, Message, [], []) ->
	{error, empty, []};
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
	io:format("~p",[Message]),
	msgencode(start, Message).
msgencode(start, #message{version=V,type=request,method=M,uri=U,headers=H,content=C}) ->
	io:format("~p",[H]),
	string:join(
		lists:append([
			[atom_to_list(M)," ",uri:encode(U)," SIP/",V,"\r\n"],
			lists:map(fun({X,Y}) -> header:encode(X,Y)++"\r\n" end, H),
			["\r\n"],
			[msgencode(payload, C)]
		]),
	"");
msgencode(start, #message{version=V,type=response,status=S,reason=R,headers=H,content=C}) ->
	string:join(
		lists:append([
			["SIP/",V," ",integer_to_list(S)," ",R,"\r\n"],
			lists:map(fun({X,Y}) -> header:encode(X,Y)++"\r\n" end, H),
			["\r\n"],
			[msgencode(payload, C)]
		]),
	"");
msgencode(payload, undefined) ->
	"";
msgencode(payload, P) when is_binary(P) ->
	erlang:binary_to_list(P);
msgencode(_,_) ->
	fail.
%%#message(type=response,type=response,status=200,reason=Trying,

%% response code
response(trying, Msg)    ->
	Msg#message{type=response, status=100, reason="Trying"};
response(ringing, Msg)    ->
	Msg#message{type=response, status=180, reason="Ringing"};
response(ok    , Msg)    ->
	Msg#message{type=response, status=200, reason="OK"};
response(notfound, Msg)  ->
	Msg#message{type=response, status=404, reason="Not Found"};
response('503', Msg)  ->
	Msg#message{type=response, status=503, reason="Service Unavailable"}.
% custom reason
response(Type, Msg, Reason) ->
	Response = response(Type, Msg),
	Response#message{reason=Response#message.reason++" "++Reason}.

% request
request(Method, Message) ->
	Message.


%%
%% Handle SIP REQUESTS
%%

%% REGISTER
handle(M=#message{type=request,method='REGISTER', headers=Headers}, Sock) ->
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
				%send(undefined, response(ok, #message{
					headers=[
						{"Via"           , Via},
						{"From"          , lists:nth(1,dict:fetch("From",Headers))},
						{"To"            ,_To},
						{"Call-id"       , lists:nth(1,dict:fetch("Call-id",Headers))},
						{"Cseq"          , lists:nth(1,dict:fetch("Cseq",Headers))},
						{"Contact"       , lists:nth(1,dict:fetch("Contact",Headers))}, 
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
%% INVITE
handle(M=#message{type=request,method='INVITE', headers=Headers}, Sock) ->
	?DEBUG("handle INVITE", []),

	% lookup caller. Is he registered ?
	From = lists:nth(1,dict:fetch("From", Headers)),
	User = From#address.uri#uri.user,
	?DEBUG("SIP:INVITE= loopkup ~s endpoint", [User]),

	case
		mnesia:dirty_read(endpoints, User)
	of
		% FOUND
		[Endpt] ->
			?DEBUG("Found endpoint: ~p", [Endpt]),
			To = lists:nth(1,dict:fetch("To", Headers)),
			Context = #context{caller=#channel{type=sip, name=User}, socket=Sock,	message=M},
			io:format("dialplan: ~p~n", [list_to_atom(To#address.uri#uri.user)]),
			dialplan:internal(list_to_binary(To#address.uri#uri.user), Context);

		[]      -> 
			?DEBUG("Endpoint not found",[])
	end,

	ok;
%% ACK
handle(M=#message{type=request,method='ACK',headers=Headers}, Sock) ->
	?DEBUG("handle ACK", []),
	% consider call (or what else) established on local side
	ok;

%%
%% Handle SIP Responses
%%

%%    100 Trying
handle(#message{type=response,status=100,headers=H}, _) ->
	% do nothing
	?DEBUG("Received 100 Trying",[]),
	ok;
%%    180 Ringing
handle(M=#message{type=response,status=180,headers=H}, Sock) ->
	?DEBUG("Received 180 Ringing",[]),

	%% Forward Ringing to caller
	Key   = trans_key(msg, M),
	Trans = case ets:lookup(transactions, Key) of
		[] -> 
			% try lookup w/o To tag
			ets:lookup(transactions, trans_key(msg2, M));
		R  ->
			R
	end,

	?DEBUG("lookup transaction ~s: ~w~p", [Key, length(Trans) > 0]),
	case Trans of
		[] ->
			fail;

		[T]  ->
			io:format("trans= ~p ~p~n", [T, T#transaction.s_cid]),
			To = lists:nth(1, dict:fetch("To"  , T#transaction.s_msg#message.headers)),

			send(T#transaction.s_uri, response(ringing, #message{
				headers=[
					{"Via"           , 
						%#via{transport=tcp,host="localhost",port=7779,params=[{branch,header:tag()}]}},
						lists:nth(1, dict:fetch("Via", T#transaction.s_msg#message.headers))},
					{"From"          , 
						lists:nth(1, dict:fetch("From", T#transaction.s_msg#message.headers))},
					{"To"            , To#address{params=To#address.params++[{tag,header:tag()}]}},
					{"Call-id"       , T#transaction.s_cid},
					{"Cseq"          , {1,'INVITE'}},
					{"User-agent"    ,"epbxd"},
					{"Allow"         ,"foobar"},
					{"Content-length",0}
				]
			}, "ring ring..."))
	end,

	%% Send back OK
	ok;
%%		200 OK
handle(M=#message{type=response,status=200,headers=H}, Sock) ->
	?DEBUG("Received 200 OK",[]),

	%% Forward Ringing to caller
	Key   = trans_key(msg, M),
	Trans = case ets:lookup(transactions, Key) of
		[] -> 
			% try lookup w/o To tag
			ets:lookup(transactions, trans_key(msg2, M));
		R  ->
			R
	end,

	?DEBUG("lookup transaction ~s: ~w~p", [Key, length(Trans) > 0]),
	case Trans of
		[] ->
			fail;

		[T]  ->
			io:format("trans= ~p ~p~n", [T, T#transaction.s_cid]),
			% sending OK to the peer			
			To = lists:nth(1, dict:fetch("To"  , T#transaction.s_msg#message.headers)),

			% caller -> callee RTP passthrough
			SDPSession = M#message.content,
			{ok, Rtps} = rtps:start_link([0, {
				SDPSession#sdp_session.connection#sdp_connection.address,
				(hd(SDPSession#sdp_session.medias))#sdp_media.port
			}]),

			Content = sdp:encode(#sdp_session{
				origin=#sdp_origin{
					username= <<"epbxd">>,
					ssid=0,
					ssversion=0,
					address= <<"127.0.0.1">>
				},
				connection = #sdp_connection{address= <<"127.0.0.1">>},
				medias = [#sdp_media{
					port=rtps.getport(Rtps),
					rtpmap=[{'PCMA', [8,8000]}, {'PCMU', [0,8000]}],
					rtcp= #sdp_connection{address= <<"127.0.0.1">>, port=0}
				}]
			}),

			send(T#transaction.s_uri, response(ok, #message{
				headers=[
					{"Via"           , 
						%#via{transport=tcp,host="localhost",port=7779,params=[{branch,header:tag()}]}},
						lists:nth(1, dict:fetch("Via", T#transaction.s_msg#message.headers))},
					{"From"          , 
						lists:nth(1, dict:fetch("From", T#transaction.s_msg#message.headers))},
					{"To"            , To#address{params=To#address.params++[{tag,header:tag()}]}},
					{"Call-id"       , T#transaction.s_cid},
					{"Cseq"          , {1,'INVITE'}},
					{"User-agent"    ,"epbxd"},
					{"Allow"         ,"foobar"},
					{"Contact" 			 ,
						lists:nth(1, dict:fetch("From", T#transaction.s_msg#message.headers))},
					{"Content-type"  , "application/sdp"},
					{"Content-length", erlang:byte_size(Content)}
				],

				content=Content
			})),

			% sending ACK to originator
			_To = lists:nth(1,dict:fetch("To",H)),

			send(Sock, request('ACK', #message{
				type=request,
				method='ACK',
				uri=_To#address.uri,

				headers=[
					{"Via"           , lists:nth(1,dict:fetch("Via",H))},
					{"From"          , lists:nth(1,dict:fetch("From",H))},
					{"To"            , _To},
					{"Call-id"       , lists:nth(1,dict:fetch("Call-id",H))},
					{"Cseq"          , lists:nth(1,dict:fetch("Cseq",H))},
					{"Contact"       , lists:nth(1,dict:fetch("Contact",H))}, 
					{"User-agent"    ,"epbxd"},
					{"Allow"         ,"foobar"},
					{"Content-length",0}
				]
			})),

		ok
	end,

	ok;

handle(Mm=#message{type=request,method=M}, _) ->
	?DEBUG("handle: unknown ~w method - ~w~n", [M,Mm]),
	fail;
handle(#message{type=response,status=S,reason=R}, _) ->
	?DEBUG("handle: unknown ~b/~s response~n", [S,R]),
	fail.


%% Commands
app(dial, Exten, #context{caller=C,socket=Sock,message=M}) ->
	Headers = M#message.headers,

	To   = lists:nth(1,dict:fetch("To", Headers)),
	User = To#address.uri#uri.user,
	?DEBUG("SIP:INVITE= lookup ~s target", [User]),

	case
		mnesia:dirty_read(registrations, User)
	of
		% FOUND
		[Endpt] ->
			?DEBUG("Endpoint registered: ~p", [Endpt]),

			%% Create transaction (registered for both parts
			From = lists:nth(1,dict:fetch("From", Headers)),
			Contact = lists:nth(1,dict:fetch("Contact", Headers)),

			T = #transaction{
				s_cid     = lists:nth(1,dict:fetch("Call-id", Headers)),
				s_fromtag = proplists:get_value("tag", From#address.params),
				s_state   = 'INVITE',
				s_msg     = M,
				s_uri     = Contact#address.uri,

				d_cid     = header:tag(),
				d_fromtag = header:tag(),
				d_state   = 'INVITE'
			},

			% ETS insert needs to be done in process who create ETS table (sips server)
			gen_server:call(sips, trans_key(source, T)),
			gen_server:call(sips, trans_key(dest  , T)),

			_From = #address{
				displayname = "epbxd",
				uri         = #uri{scheme=sip,user="epbxd",host="localhost",port=7779},
				params      = [{tag, T#transaction.d_fromtag}]
			},
			_To = #address{
				uri         = Endpt#registration.uri
			},

			% callee -> caller RTP passthrough
			SDPSession = M#message.content,
			{ok, Rtps} = rtps:start_link([0, {
				SDPSession#sdp_session.connection#sdp_connection.address,
				(hd(SDPSession#sdp_session.medias))#sdp_media.port
			}]),


			Content = sdp:encode(#sdp_session{
				origin=#sdp_origin{
					username= <<"epbxd">>,
					ssid=0,
					ssversion=0,
					address= <<"127.0.0.1">>
				},
				connection = #sdp_connection{address= <<"127.0.0.1">>},
				medias = [#sdp_media{
					port=rtps:getport(Rtps),
					rtpmap=[{'PCMA', [8,8000]}, {'PCMU', [0,8000]}],
					rtcp= #sdp_connection{address= <<"127.0.0.1">>, port=0}
				}]
			}),

			% send INVITE to target peer
			send(undefined, request('INVITE', #message{
				type=request,
				method='INVITE',
				uri=_To#address.uri,

				headers=[
					{"Via"           , #via{transport=tcp,host="localhost",port=7779,params=[{branch,header:tag()}]}},
					{"From"          , _From},
					{"To"            , _To},
					{"Call-id"       , T#transaction.d_cid},
					{"Cseq"          , {1, 'INVITE'}},
					{"User-agent"    ,"epbxd"},
					{"Allow"         ,"foobar"},
					{"Contact" 			 ,_From},
					{"Content-type"  , "application/sdp"},
					{"Content-length", erlang:byte_size(Content)}
				],

				content = Content
			})),

			ok;

		[]      -> 
			?DEBUG("Endpoint not registered",[]),
			_To =	To#address{params=lists:append(To#address.params,[{"tag",header:tag()}])},

			Via = lists:nth(1,dict:fetch("Via",Headers)),

			send(undefined, response('503', #message{
				headers=[
					{"Via"           , Via},
					{"From"          , lists:nth(1,dict:fetch("From",Headers))},
					{"To"            ,_To},
					{"Call-id"       , lists:nth(1,dict:fetch("Call-id",Headers))},
					{"Cseq"          , lists:nth(1,dict:fetch("Cseq",Headers))},
					{"User-agent"    ,"epbxd"},
					{"Allow"         ,"foobar"},
					{"Content-length",0}
				]
			}, ":("))

	end,

	nop;

app(hangup, Exten, Context) ->
	Headers = Context#context.message#message.headers,

	To =  lists:nth(1,dict:fetch("To",Headers)),
	_To =	To#address{params=lists:append(To#address.params,[{"tag",header:tag()}])},

	Via = lists:nth(1,dict:fetch("Via",Headers)),

	send(Context#context.socket, response(notfound, #message{
		headers=[
			{"Via"           , Via},
			{"From"          , lists:nth(1,dict:fetch("From",Headers))},
			{"To"            ,_To},
			{"Call-id"       , lists:nth(1,dict:fetch("Call-id",Headers))},
			{"Cseq"          , lists:nth(1,dict:fetch("Cseq",Headers))},
			{"User-agent"    ,"epbxd"},
			{"Allow"         ,"foobar"},
			{"Content-length",0}
		]
	}, ":(")).



%% transaction operations

trans_key(source, T=#transaction{s_cid=Cid, s_fromtag=From, s_totag=To}) ->
	_From = if From == undefined -> ""; true -> From end,
	_To   = if To   == undefined -> ""; true -> To end,
			
	T#transaction{key=Cid++":"++_From++":"++_To};
trans_key(dest, T=#transaction{d_cid=Cid, d_fromtag=From, d_totag=To}) ->
	_From = if From == undefined -> ""; true -> From end,
	_To   = if To   == undefined -> ""; true -> To end,
			
	T#transaction{key=Cid++":"++_From++":"++_To};
trans_key(msg, #message{headers=H}) ->
	CallID  = lists:nth(1,dict:fetch("Call-id",H)),

	From    = lists:nth(1,dict:fetch("From", H)),
	FromTag = proplists:get_value("tag", From#address.params,""),

	To      = lists:nth(1,dict:fetch("To", H)),
	ToTag   = proplists:get_value("tag", To#address.params,""),

	CallID++":"++FromTag++":"++ToTag;
trans_key(msg2, #message{headers=H}) ->
	CallID  = lists:nth(1,dict:fetch("Call-id",H)),

	From    = lists:nth(1,dict:fetch("From", H)),
	FromTag = proplists:get_value("tag", From#address.params,""),

	CallID++":"++FromTag++":".

handle_call(Msg, _Caller, State) ->
	%io:format("insert transaction: ~p~n", [Msg]),
	ets:insert(transactions, Msg),
	{reply, ok, ok}.

%%
%% unused

code_change(_OldVersion, Library, _Extra) ->
	{ok, Library}.
terminate(_Reason, _Library) ->
	ok.
handle_info(_Msg, Library) ->
	{noreply, Library}.
handle_cast({free, Ch}, State) ->
	{noreply, State}.

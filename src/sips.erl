
-module(sips).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

-export([demo/0, start/1, start_link/3, sample/2, init/1, code_change/3, terminate/2, handle_call/3, handle_info/2,
	handle_cast/2]).

-include("sips.hrl").
-include("utils.hrl").

%%œ-record(server  , {iface=any, proto=tcp, port=5060, socket=null}).
%%-record(session , {callerid, seq}).
%%-record(endpoint, {id, ip, port}).
%% a SIP message
%%-record(message , {version="2.0", type=null, method=null, uri=null, status=null,
%%		reason=null, headers=dict:new(), content=null}).

-define(TCP_OPTIONS,[list, {packet,0},{active,false},{reuseaddr,true}]).


start(Callback) ->
	Server = #server{port=7779},

	%% 2d arg: module callback. ?MODULE == current module name
	%% 3d arg: init parameters
	gen_server:start_link({local, sips}, ?MODULE, [Server, Callback], []).

start_link(Mod, Args, Opts) ->
	Server = #server{port=7779},

	gen_server:start_link({local, sips}, ?MODULE, [Server, {sip, sample}], []).


%% we extract Port variable from Server record
init([Server = #server{port=Port}, Callback]) ->
	?DEBUG("~p", ["init"]),

	%%TODO: manage TCP/UDP switch
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, Socket} -> 
			%% make a copy of Server record, setting socket variable
			_State = Server#server{socket=Socket},
			%% spawn accept process
			%%spawn(?MODULE, loop, [Socket, Callback]),
			loop(Socket, Callback),
			{ok, _State};

		{error, Reason} ->
			{stop, Reason}
	end.

%% accept loop
loop(Socket, {Mod, Callback}) ->
	?DEBUG("~s", ["Accept loop"]),

	case gen_tcp:accept(Socket) of
		{ok, _Socket}   -> 
			Message = Mod:Callback(_Socket, #message{});
			
		{error, Reason} ->
			Message = null,
			{error, Reason}
	end,

	%% handle messages
	handle(Message).


%% sample callback
sample(Socket, Message) ->
	?DEBUG("~p", ["sample callback"]),

	%% read socket
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} -> 
			?DEBUG("~p", [Data]),
	
			%% must decode message
			%%resample(Message, string:tokens(Data, "\r\n"), start),
			%%resample(Message, re:split(Data, "\r\n", [{return,list}]), start),
			_Message = resample(Message, re:split(Data, "\r\n", [{return,list}]), start),
			?DEBUG("~p", [_Message]),
			%%,string:len(_Message#message.content)]),
			%%lists:foldl(resample(X, Message), Message, string:tokens(Data, "\r\n")),

			%% len(x)


			gen_tcp:send(Socket, Data),
			sample(Socket, Message);
		{error, closed} ->
			?DEBUG("~p", ["sample:closed socket"]),
			_Message=Message,
			ok;
		_Else ->
			?DEBUG("~p", ["sample:else"]),
			_Message=Message,
			ok
	end,

	?DEBUG("~p", ["end sample"]),
	_Message.

%%resample(Message, [Method ++ " " ++ URI ++ " " ++ Version|_],start) ->
%%	?DEBUG("tok/start/request= ~p", [Method,URI,Version]),
%%	resample(Message,Next, header);
resample(Message, [Token|Next], start) ->
	%%?DEBUG("tok/start= ~p", [Token]),

	case re:split(Token," ",[{return,list},{parts,3}]) of
		["SIP/"++Version, Status, Reason] ->
			%%?DEBUG("tok/start= ~p", [[response,Status,Reason,Version]]),
			_Message = Message#message{type=response,version=Version,status=Status,reason=Reason};
		[Method,URI,"SIP/"++Version] ->
			%%?DEBUG("tok/start= ~p",[[request,Method,URI,Version]]),
			_Message = Message#message{type=request,version=Version,method=Method,uri=URI};

		_Else ->
			?DEBUG("nomatch= ~p", [_Else]),
			_Message = "",
			ok
	end,

	resample(_Message, Next, header);

resample(Message, [""|Next], header) ->
	?DEBUG("end-of-header: ~p",[Next]),
	Message#message{content=string:join(Next, "\r\n")};

resample(Message, [Token|Next], header) ->
	%%?DEBUG("tok/header= ~p", [Token]),
	[Key, Value] = re:split(Token, ": ", [{return,list},{parts,2}]),
	_Message = Message#message{headers=dict:append(Key,Value,Message#message.headers)},
	resample(_Message,Next, header);

resample(Message, "", header) ->
	%%?DEBUG("end-of-header(last)",[]),
	Message.


%resample(Message, Token, header) ->
%	?DEBUG("tok/end= ~p", [Token]),
%	Message.

handle(Message=#message{type=request,method=Method,headers=Headers}) ->
	?DEBUG("~p", [['handle',Method]]).


demo() ->
	sip:start({sips,sample}).

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

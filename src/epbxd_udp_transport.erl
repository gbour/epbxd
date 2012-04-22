

% we simulate a TCP like listen/accept working, with UDP sockets
% accept sockets is done with matching sender ip/port with process reading

-module(epbxd_udp_transport).
-author("Guillaume Bour <guillaume@bour.cc").
-behaviour(gen_server).

% Cowboy transport API
-export([name/0, messages/0, listen/1, accept/2, recv/3, send/2, setopt/2, 
		controlling_process/2, peername/1, close/1]).
% Extended API
-export([send/3]).
% gen_server interface
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
		terminate/2]).

-include("epbxd_udp_transport.hrl").

-spec name() -> udp.
name() -> udp.

-spec messages() -> {}.
messages() -> {}.

-spec listen(list()) -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
	{port, Port} = lists:keyfind(port, 1, Opts),
	%ListenOpts0  = [binary, {active, false}, {packet, raw}, {reuseaddr, true}],
	ListenOpts0  = [binary, {reuseaddr, true}],
	ListenOpts   =
		case lists:keyfind(ip, 1, Opts) of
			false -> ListenOpts0;
			Ip    -> [Ip|ListenOpts0]
		end,

	%gen_udp:open(Port, ListenOpts).
	gen_server:start_link(?MODULE, [Port, ListenOpts], []).

% int()>0 | infinity
accept(Sock, 0) ->
	accept(Sock, 1);
accept(Sock, Timeout) ->
	Sock ! {accept, self()},
	receive
		{accept, Sock, Ip, Port} -> 
			{ok, {Sock, Ip, Port}}

	after
		Timeout        -> {error, timeout}
	end.

	%Ret = gen_server:call(Sock, accept, Timeout),
	%{ok, []}.

% consider the new controlling process will consume all packets
% must be synchronous to ensure data is copy before end
% TODO: use handle_call() (check process executing backend fun)
controlling_process({Sock, Ip, Port}, Pid) ->
	Sock ! {control, {Ip, Port}, Pid, self()},
	receive 
		{control, ok} -> []
	end,

	ok.

% we only accept 0 Length recv queries
recv(_Sock, 0, Timeout) ->
	receive
		{recv, Packet} ->	{ok, Packet}

	after
		Timeout        -> {error, timeout}
	end.

send(_Sock, _Packet) ->
	{error, not_implemented}.

send({Pid, _Ip, _Port}, Target, Packet) ->
	gen_server:call(Pid, {send, Target, Packet}).

% WARNING: global to all pseudo-sockets
setopt({Sock, _Ip, _Port}, Opts) ->
	Sock ! {setopt, Opts}.

peername({_Sock, Ip, Port}) ->
	{Ip, Port}.

% close fake accept socket
close({Sock, Ip, Port}) ->
	Sock ! {close, {Ip, Port}, self()},
	receive
		{close, ok} -> []
	end,

	flush();

% stop fake listen socket
close(Pid) when is_pid(Pid) ->
	gen_server:cast(Pid, stop).

% emptying mailbox
flush() ->
	receive 
		{recv, _} -> flush()
	after 0 ->
			ok
	end.



% gen_server methods
init([Port, Opts]) ->
	ListenOpts = [{active, true}|Opts],

	case gen_udp:open(Port, ListenOpts) of
		{ok, Socket}    ->
			{ok, #udpsock{socket=Socket}};

		{error, Reason} ->
			{stop, Reason}
	end.

% lock until data received on socket from unknown sender
%handle_call(accept, From, State)  ->
%	receive Data	-> []	end,
%
%	{reply,foobar,State}.
handle_call({send, {Host, Port}, Packet}, From, State=#udpsock{socket=Sock}) ->
	Reply = gen_udp:send(Sock, Host, Port, Packet),

	{reply, Reply, State}.


% receive udp packets
handle_info({udp, _Socket, IP, InPort, Packet},
            State=#udpsock{acceptor=Acceptor,mapping=Mapping,cache1=Cache1,cache2=Cache2}) ->
	%io:format(user,"receive ~p:~p~n",[IP,InPort]),
	{_Acceptor, _Cache1, _Cache2} = case proplists:get_value({IP,InPort}, Mapping) of
		undefined ->
			{_C1, _C2} = case Acceptor of
				undefined ->
					{add_to_cache(Cache1, {IP,InPort}, Packet), Cache2};
				
				Pid       ->
					_C2_ = add_to_cache(Cache2, {IP,InPort}, Packet),

					Pid ! {accept, self(), IP, InPort},

					{Cache1, _C2_}
			end,
			{undefined, _C1, _C2};

		Pid       -> 
			Pid ! {recv, Packet},

			{Acceptor, Cache1, Cache2}
	end,

	State2 = State#udpsock{acceptor=_Acceptor, cache1=_Cache1, cache2=_Cache2},
	{noreply, State2};

handle_info({accept, Pid}, State=#udpsock{cache1=Cache1,cache2=Cache2}) ->
	{Acceptor, _Cache1, _Cache2} = case length(Cache1) of
		0 -> 
			{Pid, Cache1, Cache2};

		_ ->
			[{{IP, Port}, Packets} | _Tail] = Cache1,
			Pid ! {accept, self(), IP, Port},

			{undefined, _Tail, [{{IP,Port}, Packets}|Cache2]}
	end,

	{noreply, State#udpsock{acceptor=Acceptor, cache1=_Cache1, cache2=_Cache2}};

% must be synchronous
handle_info({control, Key, Pid, From}, State=#udpsock{mapping=Mapping,cache2=Cache2}) ->
	% empty cache to fill process mailbox
	_Cache2 = case lists:keytake(Key, 1, Cache2) of
		{value, {Key, Packets}, _Tail} ->
			lists:foreach(fun(P) -> Pid ! {recv, P} end, Packets),
			_Tail;

		false ->
			Cache2
	end,

	From ! {control, ok},
	{noreply, State#udpsock{mapping=[{Key,Pid}|Mapping], cache2=_Cache2}};

handle_info({setopt, Opts}, State=#udpsock{socket=Sock}) ->
	inet:setopt(Sock, Opts),
	{noreply, State};

%
handle_info({close, Key, From}, State=#udpsock{mapping=Mapping,cache1=Cache}) ->
	%io:format(user,"close: ~p ~p~n",[Key, Mapping]),
	{NewMapping, NewCache} = case lists:keytake(Key, 1, Mapping) of
		{value, {Key, Pid}, _Mapping} ->
			% NOTE: Pid process may have died before socket closed
			case erlang:process_info(Pid, messages) of 
				undefined ->
					{_Mapping, Cache};

				{messages, Messages} ->
					% We keep only *recv* messages, and prepend {Key, Msgs} to Cache
					_Cache = if 
						length(Messages) > 0 ->
							[{
								Key, 
								lists:map(fun({_, V}) -> V end,
									lists:filter(fun(V) -> 
										case V of 
											{recv,_} -> true; 
											_        ->	false 
										end 
									end, Messages
								))} | Cache];
						true -> Cache
					end,

					{_Mapping, _Cache}
			end;

		false                         ->
			{Mapping, Cache}
	end,

	From ! {close, ok},

	State2 = State#udpsock{mapping=NewMapping,cache1=NewCache},
	%io:format(user,"/close: ~p~n",[State2]),
	{noreply, State2};

handle_info(_,State)                                     ->
	{noreply, State}.

handle_cast(stop, State)       ->
	{stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
terminate(_Reason, #udpsock{socket=Sock})          ->
	gen_udp:close(Sock),
	ok.

%
% PRIVATE METHODS
%

add_to_cache(Cache, Key, Data) ->
	case lists:keyfind(Key, 1, Cache) of
		{_, Packets} ->
				lists:keyreplace(Key, 1, Cache, {Key, Packets ++ [Data]});

		false             ->
				Cache ++ [{Key, [Data]}]
	end.


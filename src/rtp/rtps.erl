
-module(rtps).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).
-export([getport/1]).

-include("utils.hrl").

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%
% In = Listening port
% Out = {host, port} - passthrouth target
init([In, {Host, Port}]) ->
	case gen_udp:open(In, [binary, {active, true}]) of
		{ok, Socket} ->
			{ok, LPort} = inet:port(Socket),
			?INFO("RTP listening on socket ~p ~p", [Socket, LPort]),
			{ok, {Host, Port, LPort}};

		{error, Reason} ->
			?ERROR("rtps::gen_udp:listen= ~p", [Reason]),
			{stop, Reason}
	end.

getport(Pid) ->
	gen_server:call(Pid, port).

handle_info({udp, Socket, IP, InPortNo, Packet}, {Host, Port, LocalPort}) ->
	?DEBUG("RTP passthrough:: {~p:~b} -> {~p:~b}~n", [IP, InPortNo, Host, Port]),
	gen_udp:send(Socket, erlang:binary_to_list(Host), Port, Packet),
	
	{noreply,{Host,Port,LocalPort}}.

% query local port
handle_call(port, _, {Host, Port, LocalPort}) ->
	{reply, LocalPort, {Host, Port, LocalPort}};

handle_call(Request, From, State) ->
	?DEBUG("call ~p ~p ~p~n",[Request, From, State]),
	{reply, foo, State}.

handle_cast(Info, State) ->
	?DEBUG("cast~n",[]),
	ok.

code_change(_OldVersion, Library, _Extra) ->
	{ok, Library}.
terminate(_Reason, _Library) ->
	ok.

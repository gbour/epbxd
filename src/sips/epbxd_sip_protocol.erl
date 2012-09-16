
-module(epbxd_sip_protocol).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(cowboy_protocol).
% API
-export([start_link/4]).
% Internal use
-export([init/4]).

-include("sips/epbxd_sip.hrl").

start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
	wait_request(Transport, Socket, 2000).

wait_request(Transport, Socket, Timeout) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Packet} -> 
			parse_request(Transport, Socket, epbxd_sip_message:decode(Packet)),
			% looping until timeout
			wait_request(Transport, Socket, Timeout);

		_            -> terminate(Transport, Socket)
	end.

parse_request(Transport, Socket, [])       ->
	ok;
parse_request(Transport, Socket, [M|Tail]) ->
	case M of
		{ok, Message}            ->
			handle(Message, Transport, Socket),
			parse_request(Transport, Socket, Tail);

		%TODO: manage fragmentation
		{error, _Reason, Packet} ->
			io:format(user, "invalid sip message (cause= ~p): ~p~n", [_Reason, Packet]),
			terminate(Transport, Socket)
	end.

handle(Message=#sip_message{type=request,method=M} , Transport, Socket) ->
	epbxd_hooks:run({sip,request,M},  {Message, Transport, Socket});
handle(Message=#sip_message{type=response,status=S}, Transport, Socket) ->
	epbxd_hooks:run({sip,response,S}, {Message, Transport, Socket}).

terminate(Transport, Socket) ->
	Transport:close(Socket),
	ok.

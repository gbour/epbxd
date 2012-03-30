
-module(epbxd_sip_protocol).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(cowboy_protocol).
% API
-export([start_link/4]).
% Internal use
-export([init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
	wait_request(Transport, Socket, 2000).

wait_request(Transport, Socket, Timeout) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Packet} -> parse_request(Transport, Socket, Packet);
		_            -> terminate(Transport, Socket)
	end.

parse_request(Transport, Socket, Buffer) ->
	case epbxd_sip_message:decode(Buffer) of
		{ok, Message}    -> handle(Message);
		%TODO: manage fragmentation
		{error, _Reason} -> terminate(Transport, Socket)
	end.

handle(Message) ->
	ok.

terminate(Transport, Socket) ->
	Transport:close(Socket),
	ok.

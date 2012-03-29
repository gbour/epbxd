
% Sample program
% 
% Manage to receive as much UDP packets as possible on port 9999
% Display UDP throughput (packets/seconds) and global average
% 
% Compare with python matching program

-module(raw_udp_socket_server).
-export([main/0]).

recv(Sock, Monitor) ->
	{ok, _Packet} = gen_udp:recv(Sock, 0, infinity),
	Monitor ! recv,

	recv(Sock, Monitor).

monit(Total, Num) ->
	timer:sleep(1000),

	Throughput = throughput(0),
	Total2     = Total + Throughput,
	Num2       = Num + 1,

	io:format("~b packets/s (~f)~n", [Throughput, Total2/Num2]),
	monit(Total2, Num2).

throughput(C) ->
	receive
		recv -> throughput(C+1)
	after
		0    -> C
	end.

% program entry
main() ->
	Monitor   = spawn(fun() -> monit(0,0) end),

	{ok, LSock} = gen_udp:open(9999, [binary,{reuseaddr,true},{active,false}]),
	recv(LSock, Monitor).




-module(ws_app).
%-behaviour(application).

-export([start/2,stop/1]).

start(normal,_) ->
	application:start(cowboy),
	Dispatch = [
		{'_', [{'_', ws, []}]}
	],
	cowboy:start_listener(http, 10,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),

	%ws_sup:start_link().
	ok.

stop(_State) ->
	ok.



-module(rest_app).
-behaviour(application).

-export([start/2,stop/1]).

start(normal,_) ->
	rest:start_link(),
	Res = rest:add_resource("types.ws"),
	io:format("adding resources= ~p~n", [Res]),

	backoffice:init(),

	application:start(cowboy),
	Dispatch = [
		{'_', [{'_', ws, []}]}
	],
	cowboy:start_listener(http, 10,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),

	rest_sup:start_link().

stop(_State) ->
	ok.


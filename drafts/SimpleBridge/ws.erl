
-module(ws).
-export([init/3,handle/2]).

-include("http.hrl").

init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

handle(Req=#http_req{method='GET',path=Path, buffer=Body}, State) ->
	io:format(user,"Youou! ~p~n",[Path]),

	%{ok,cowboy_http_req:reply(200,[],[],Req),State}.
	RequestBridge = simple_bridge:make_request(cowboy_request_bridge, {Req, "/tmp"}),

	Response  = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),
    Response1 = Response:status_code(200),
    Response2 = Response1:header("Content-Type", "application/json"),
    Response3 = Response2:data(<<"foobar">>),

	Response3:build_response().


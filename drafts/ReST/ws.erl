
-module(ws).
-export([init/3,handle/2]).

-include("http.hrl").
init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

handle(Req=#http_req{method=GET,path=[<<"api">>], buffer=Body}, State) ->
	io:format("Return API~n"),
	Resources = rest:get_all_resources(),

	{ok,cowboy_http_req:reply(200,[], typing2:schema_all(Resources), Req),State};

handle(Req=#http_req{method=GET,path=[<<"api">>,Object,<<"schema">>], buffer=Body}, State) ->
	io:format("Return API :: ~p~n", [Object]),
	Resource = rest:get_resource(erlang:binary_to_atom(Object, latin1)),
	{ok,cowboy_http_req:reply(200,[{<<"Content-Type">>, <<"application/json">>}], typing2:schema(Resource), Req),State};

handle(Req=#http_req{method=GET,path=[<<"api">>,<<"user">>], buffer=Body}, State) ->
	Users = [
		%
		%[{uid,},{name,},{groups,[]}],
		[{uid,1},{name,<<"john">>},{groups,[1,3]}],
		%
		[{uid,2},{name,<<"mary">>},{groups,[2]}]
	],

	{ok, cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], typing2:encode_list(user, Users), Req), State};

handle(Req=#http_req{method=GET,path=[<<"api">>,<<"user">>, Uid], buffer=Body}, State) ->
	User = [{uid,1},{name,<<"john">>},
		{groups, [
			{ref, <<"/api/group/{1}">>},
			{objects, [1,3]}
		]}
	],

	{ok, cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], typing2:encode(user, User), Req), State}.



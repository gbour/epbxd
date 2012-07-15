
-module(ws).
-export([init/3,handle/2]).

-include("http.hrl").
init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

handle(Req=#http_req{method=GET,path=[<<"api">>], buffer=Body}, State) ->
	io:format("Return API~n"),
	Resources = rest:get_all_resources(),

	{ok,cowboy_http_req:reply(200,[], resource:schema_all(Resources), Req),State};

handle(Req=#http_req{method=GET,path=[<<"api">>,Object,<<"schema">>], buffer=Body}, State) ->
	io:format("Return API :: ~p~n", [Object]),
	Resource = rest:get_resource(erlang:binary_to_atom(Object, latin1)),
	{ok,cowboy_http_req:reply(200,[{<<"Content-Type">>, <<"application/json">>}], resource:schema(Resource), Req),State};

handle(Req=#http_req{method=GET,path=[<<"api">>,<<"user">>], buffer=Body}, State) ->
	Users = backoffice:list(user,[]),

	{ok, cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], resource:encode_list(user, Users), Req), State};

handle(Req=#http_req{method=GET,path=[<<"api">>,<<"user">>, Uid], buffer=Body}, State) ->
	Resource = rest:get_resource(user),
	Record   = backoffice:get(user, erlang:list_to_integer(erlang:binary_to_list(Uid)), []),
	
	{ok, cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], resource:encode(Resource, Record), Req), State}.



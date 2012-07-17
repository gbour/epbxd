
-module(ws).
-export([init/3,handle/2]).

-include("http.hrl").
init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

handle(Req=#http_req{method=GET,path=[<<"api">>], buffer=Body}, State) ->
	io:format("Return API~n"),
	Resources = rest:get_all_resources(),

	{ok,cowboy_http_req:reply(200,[], resource:schema_all(Resources), Req),State};

handle(Req=#http_req{method='GET',path=[<<"api">>,Object,<<"schema">>], buffer=Body}, State) ->
	io:format("Return API :: ~p~n", [Object]),
	Resource = rest:get_resource(erlang:binary_to_atom(Object, latin1)),
	{ok,cowboy_http_req:reply(200,[{<<"Content-Type">>, <<"application/json">>}], resource:schema(Resource), Req),State};

handle(Req=#http_req{method='GET',path=[<<"api">>,<<"user">>], buffer=Body}, State) ->
	Users = backoffice:list(user,[]),

	{ok, cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], resource:encode_list(user, Users), Req), State};

handle(Req=#http_req{method='GET',path=[<<"api">>,<<"user">>, Uid], buffer=Body}, State) ->
	Resource = rest:get_resource(user),
	Record   = backoffice:get(user, erlang:list_to_integer(erlang:binary_to_list(Uid)), []),
	
	{ok, cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], resource:encode(Resource, Record), Req), State};

handle(Req=#http_req{method='POST', path=[<<"api">>,<<"user">>], buffer=Body}, State) ->
	Resource = rest:get_resource(user),
	Object   = resource:decode(user, Body, Resource),
	io:format(user, ">> ~p ~p~n", [Body, Object]),

	Ret      = backoffice:set(Object, []),
	io:format(user, ">> backoffice:set= ~p~n", [Ret]),

	{RetCode, Content} = case Ret of
		{ok, Id}     -> {200, erlang:list_to_binary(erlang:integer_to_list(Id))};
		{error, Msg} -> {409, Msg}
	end,

	{ok, cowboy_http_req:reply(RetCode, [{<<"Content-Type">>, <<"application/json">>}],
		Content, Req), State};

handle(Req=#http_req{method='POST', path=[<<"api">>,<<"user">>, _Uid], buffer=Body}, State) ->
	Resource = rest:get_resource(user),
	Object   = resource:decode(user, Body, Resource),

	Uid = erlang:list_to_integer(erlang:binary_to_list(_Uid)),
	io:format(user, "user id= ~p~n", [Uid]),
	Ret = backoffice:update(user, Uid, Object, []),
	{RetCode, Content} = case Ret of
		{ok, Id}            -> {200, <<"">>};
		{error, Msg}        -> {500, <<"fail to update record">>};
		{'not-found', Msg2} -> {404, <<"record not found">>}
	end,

	{ok, cowboy_http_req:reply(RetCode, [{<<"Content-Type">>, <<"application/json">>}],
		Content, Req), State}.

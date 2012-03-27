
-module(epbxd).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(application).
-export([start/2, stop/1]).
-include("utils.hrl").
-include("sips/sips.hrl").

-import(webservice).

start(normal, Args) ->
	config:load(get_config_file()),
	logging:init(config:get(logfile)),
	%%logging:loglevel(config:get("loglevel")),

	% Set node cookie, Try to connect to local ejabberd
	erlang:set_cookie(node(), get_cookie()),
	ejabberd_connect(),

	%%TODO: SHOULD create schema/table only if not exists
	mnesia:create_schema([node()]),
	mnesia:start(),

	% start webservice
	application:start(cowboy),
	Dispatch = [
		{'_', [{'_', webservice, []}]}
	],
	cowboy:start_listener(http, 10,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),

	%% NOTE: cannot debug until logging is initialized
	?DEBUG("app:start",[]),
	application:start(sasl),
	epbxd_sup:start_link();

start(Type,_) ->
	io:format("start2:"++Type),
	{error, badarg}.


stop(State) ->
	ok.

get_config_file() ->
	case application:get_env(config) of
		{ok, File} ->
			File;
		undefined  ->
			empty
	end.

get_cookie() ->
	C = config:get(cookie),

	if
		is_atom(C) -> C;
		is_list(C) -> list_to_atom(C);
		true       -> undefined
	end.

ejabberd_connect() ->
	Node = 'ejabberd@localhost',

	case net_adm:ping(Node) of
		pong ->
			case rpc:call(Node, 'ejabberd_ctl', process, [["status"]]) of
				{badrpc,_} -> fail;
				_          ->	?INFO("Ejabberd is up and available~n",[]),	ok
			end;

		_    -> fail
	end.


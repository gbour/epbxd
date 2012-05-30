
-module(epbxd).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(application).
-export([start/2, stop/1]).
-include("utils.hrl").
-include("sips/sips.hrl").

-import(webservice).

start(normal, _Args) ->
	config:load(get_config_file()),

	% Set node cookie, Try to connect to local ejabberd
	erlang:set_cookie(node(), get_cookie()),
	ejabberd_connect(),

	%%TODO: SHOULD create schema/table only if not exists
	mnesia:create_schema([node()]),
	mnesia:start(),

	% initialize hooks
	hooks_init(),

	% load modules
	modules_load(),


	% start webservice
	application:start(cowboy),
	Dispatch = [
		{'_', [{'_', webservice, []}]}
	],
	cowboy:start_listener(http, 10,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),

	% start SIP/UDP listener
	% TODO: activation and port must be set via configuration file
	cowboy:start_listener(udp, 10,
		epbxd_udp_transport, [{port, 5061}],
		epbxd_sip_protocol, []
	),

	% start SIP/TCP listener
	% TODO
	

	%% NOTE: cannot debug until logging is initialized
	?DEBUG("app:start",[]),
	application:start(sasl),
	% NOTE: replaced by cowboy handler
	% TODO: we need to init mnesia & ETS tables somewhere
	%
	% return {ok, Pid} (of top supervisor)
	epbxd_sup:start_link();
	

start(Type,_) ->
	io:format("start2:"++Type),
	{error, badarg}.


stop(_State) ->
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

hooks_init() ->
	epbxd_hooks:start_link(),

	logging:init([]),
	epbxd_hooks:new(authent, [{onstop,match},{onlast,nomatch}]),

	ok.

modules_load() ->
	% add modules paths to erlang search paths
	code:add_paths(config:get(modules_paths)),

	lists:foreach(fun({Mod, Opts}) ->
			try Mod:start(Opts) of
				_ -> ok
			catch
				_:_ ->
					io:format(user, "Modules: cannot load *~p*~n", [Mod])
			end
		end,
		config:get(modules)
	),

	ok.


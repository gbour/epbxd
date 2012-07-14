
-module(rest).
-behaviour(gen_server).

-include("resource.hrl").

% API
-export([start_link/0, add_resource/1, get_resource/1, get_all_resources/0]).
% gen_server interface
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, rest}, ?MODULE, [], []).

add_resource(Filename) ->
	Resources = resource:load(Filename),
	io:format("loaded resources= ~p~n", [Resources]),
	[ gen_server:call(rest, {add, R}) || R <- Resources ],

	ok.

get_resource(Name) ->
	gen_server:call(rest, {get, Name}).

get_all_resources() ->
	gen_server:call(rest, getall).

%%
%% PRIVATE gen_server interface
%%

init(_Args) ->
	{ok, []}.

handle_call({add, Resource=#resource{name=Name}}, _From, State)    ->
	{reply, ok, [{Name, Resource}|State]};

handle_call({get, Name}, _, State) ->
	{reply, proplists:get_value(Name,State), State};

handle_call(getall,_,State) ->
	{reply, lists:map(fun({K,V}) -> V end, State), State}.

handle_cast(_Req, _State) ->
	{noreply, _State}.

handle_info(_Info, _State) ->
	{noreply, _State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, _State, _Extra) ->
	{ok, _State}.

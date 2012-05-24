
%%	epbxd, Erlang PBX Server
%%	Copyright (C) 2012, Guillaume Bour <guillaume@bour.cc>
%%
%%	This program is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU Affero General Public License as
%%	published by the Free Software Foundation, either version 3 of the
%%	License, or (at your option) any later version.
%%
%%	This program is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU Affero General Public License for more details.
%%
%%	You should have received a copy of the GNU Affero General Public License
%%	along with this program.  If not, see <http://www.gnu.org/licenses/>.

% @doc Hooks manager server
-module(epbxd_hooks).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

% API
-export([start_link/0, add/3, add/4, del/2, del/3, at/2, run/2]).
% gen_server interface
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("utils.hrl").

-define(DEFAULT_PRIORITY, 50).

%% @doc Start callbacks manager as supervised process
%%
%%
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
	gen_server:start_link({local, epbxd_hooks}, ?MODULE, [], []).

%% @doc Add a new callback for a given Hook at default priority (50)
%%
%% @sample
%%		add({sip,request,'INVITE'}, {sip_handler, invite}, [{timeout, 42}]).
%%
-spec add(any(), {atom(), atom()}, list()) -> ok | error.
add(Hookname, Callback, Opts) ->
	gen_server:call(epbxd_hooks, {add, Hookname, ?DEFAULT_PRIORITY, Callback, Opts}).

%% @doc Add a new callback for given hook at given priority
%%
%% @sample
%%		add({sip,request,'INVITE'}, 42, {sip_handler, invite}
%%
%% @args
%%		Priority - from 1 to 100
%%
-spec add(any(), integer(), {atom(), atom()}, list()) -> ok|error.
add(Hookname, Priority, Callback, Opts) when 1 =< Priority andalso Priority =< 100 ->
	gen_server:call(epbxd_hooks, {add, Hookname, Priority, Callback, Opts}).

%% @doc Remove a callback associated with a Hook at default priority (50)
%%
%% @sample
%%		del({sip,request,'INVITE'}, {sip_handler, invite}).
%%
-spec del(any(), {atom(), atom()}) -> ok|error.
del(Hookname, Callback)  ->
	gen_server:call(epbxd_hooks, {del, Hookname, ?DEFAULT_PRIORITY, Callback}).

%% @doc Remove a callback associated with a Hook at given priority
%%
%% @sample
%%		del({sip,request,'INVITE'}, 42, {sip_handler, invite}).
%%
%% @args
%%		Priority - from 1 to 100
%%
-spec del(any(), integer(), {atom(), atom()}) -> ok|error.
del(Hookname, Priority, Callback) when 1 =< Priority andalso Priority =< 100 ->
	gen_server:call(epbxd_hooks, {del, Hookname, Priority, Callback}).

%% @doc Get Callback for Hook at Priority
%%
%% @sample
%%		at({sip,request,'INVITE'}, 42).
%%
%% @args
%%		Priority - from 1 to 100
%%
%% @return
%%		- 'error'     if Hook not set
%%		- 'undefined' if no callback set for this Hook at this Priority
%%		- a Callback else
%%
-spec at(any(), integer()) -> fail | undefined | {atom(),atom()}.
at(Hookname, Priority) when 1 =< Priority andalso Priority =< 100 ->
	case gen_server:call(epbxd_hooks, {get, Hookname}) of
		% Hookname not defined
		undefined -> error;
		Callbacks -> 
			case lists:nth(Priority, Callbacks) of
				{Callback, Opts} -> Callback;
				_                -> undefined
			end
	end.

%% @doc Execute callbacks associated with given hook
%%
%% @sample
%%		{ok, State} = run({sip,request,'INVITE'}, {#sip_message{}, Socket}.
%%
%% @return
%%		{ok, State}		          - all callback correctly executed. Return last callback returned 
%%		                          value
%%		{stop, State, Callback} - last State before stopping-Callback execution. 
%%
-spec run(any(), any()) -> {ok|stop|error, any()}.
run(Hookname, Args) ->
	case gen_server:call(epbxd_hooks, {get, Hookname}) of
		undefined -> undefined;
		Callbacks -> run_(Callbacks, Hookname, Args, undefined, 1)
	end.

run_([], _Hookname, _Args, State, _Prio)              ->
	{ok, State};
run_([undefined | Tail], Hookname, Args, State, Prio) ->
	run_(Tail, Hookname, Args, State, Prio+1);
run_([{{Mod, Fun}, Opts} |Tail], Hookname, Args, State, Prio) ->
	case Mod:Fun({Hookname, Prio}, Args, State, Opts) of
		{ok   , Args2, State2}  ->
			run_(Tail, Hookname, Args2, State2, Prio+1);

		{pass, State3}          ->
			run_(Tail, Hookname, Args, State3, Prio+1);

		{stop , State3}         ->
			{stop, State3, {Prio, {Mod, Fun}}};

		{error, Reason}        ->
			{error, Reason, {Prio, {Mod, Fun}}}
	end.


%%
%% PRIVATE gen_server interface
%%

init(_Args) ->
	{ok, []}.

handle_call({add, Hookname, Priority, Callback, Opts}, _From, State) ->
	{Hooks2, State3} = case lists:keytake(Hookname, 1, State) of
		{value, {Hookname, Hooks}, State2} ->
			{Hooks, State2};

		false                              ->
			{lists:duplicate(100, undefined), State}
	end,

	{Head, [At | Tail]} = lists:split(Priority-1, Hooks2),
	{Reply, Hooks3} = case At of
		undefined ->
			{ok, lists:append([Head, [{Callback, Opts}], Tail])};

		_         ->
			{error, Hooks2}
	end,

	{reply, Reply, [{Hookname, Hooks3} | State3]};

handle_call({del, Hookname, Priority, Callback}, _From, State) ->
	State3 = case lists:keytake(Hookname, 1, State) of
		{value, {Hookname, Hooks}, State2} ->
			{Head, [At | Tail]} = lists:split(Priority-1, Hooks),

			case At of
				% hook callback is matching
				{Callback, _} ->
					[{Hookname, lists:append([Head, [undefined], Tail])} | State2];

				_        ->
					State
			end;

		% Hookname not found
		false                              ->
			State
	end,

	{reply, ok, State3};

handle_call({get, Hookname}, _From, State)          ->
	{reply, proplists:get_value(Hookname, State), State}.

handle_cast(_Req, _State) ->
	{noreply, _State}.

handle_info(_Info, _State) ->
	{noreply, _State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, _State, _Extra) ->
	{ok, _State}.


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
-export([start_link/0, add/2, del/2, run/2]).
% gen_server interface
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("utils.hrl").


%% @doc Start callbacks manager as supervised process
%%
%%
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
	gen_server:start_link({local, epbxd_hooks}, ?MODULE, [], []).

%% @doc Add a new callback for a given Hook
%%
%% @sample
%%		add({sip,request,'INVITE'}, {sip_handler, invite}).
%%
-spec add(any(), {atom(), atom()}) -> ok.
add(Hookname, Callback) ->
	gen_server:call(epbxd_hooks, {add, Hookname, Callback}).

%% @doc Remove a callback associated with a Hook
%%
%% @sample
%%		del({sip,request,'INVITE'}, {sip_handler, invite}).
%%
-spec del(any(), {atom(), atom()}) -> ok.
del(Hookname, Callback) ->
	gen_server:call(epbxd_hooks, {del, Hookname, Callback}).

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
-spec run(any(), any()) -> {ok, any()} | {stop, any()}.
run(Hookname, Args) ->
	case gen_server:call(epbxd_hooks, {get, Hookname}) of
		undefined -> undefined;
		Callbacks -> run_(Callbacks, Hookname, Args, undefined)
	end.

run_([], _Hookname, _Args, State)               ->
	{ok, State};
run_([{Mod, Fun} |Tail], Hookname, Args, State) ->
	case Mod:Fun(Hookname, Args, State) of
		{ok   , State2}  -> 
			run_(Tail, Hookname, Args, State2);

		{error, _Reason} -> 
			run_(Tail, Hookname, Args, State);

		{stop , State3}  -> 
			{stop, State3, {Mod, Fun}}
	end.


%%
%% PRIVATE gen_server interface
%%

init(_Args) ->
	{ok, []}.

handle_call({add, Hookname, Callback}, _From, State) ->
	State3 = case lists:keytake(Hookname, 1, State) of
		{value, {Hookname, RegCallbacks}, State2} ->
			[{Hookname, lists:append(RegCallbacks, [Callback])} | State2];

		false                                  ->
			[{Hookname, [Callback]} | State]
	end,

	{reply, ok, State3};

handle_call({del, Hookname, Callback}, _From, State) ->
	State3 = case lists:keytake(Hookname, 1, State) of
		{value, {Hookname, RegCallbacks}, State2} ->
			[{Hookname, lists:delete(Callback, RegCallbacks)} | State2];

		false                                     ->
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


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
-export([start_link/0, new/2, add/3, add/4, del/2, del/3, at/2, run/2]).
% gen_server interface
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("utils.hrl").

-define(DEFAULT_PRIORITY, 50).
-define(DEFAULT_ONLAST,   ok).
-define(DEFAULT_ONSTOP,   ok).

%% @doc Start callbacks manager as supervised process
%%
%%
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
	gen_server:start_link({local, epbxd_hooks}, ?MODULE, [], []).

%% @doc Define a new hook
%%
%% @sample
%%		new(log, [])
%%		new(authent, [{onstop,ok},{onlast,fail}])
%%
%% @params
%%		Hookname		- hook identifier (MUST BE UNIQUE GLOBALLY)
%%						  i.e:
%%							log
%%							{sip,request,REGISTER}
%%							...
%%		GOpts			- list of key/value tuples
%%							{onstop, ko}			- reply code when callback ask to stop
%%							{onlast, finished}		- reply code when all callbacks executed
%%
%% @return
%%		ok              -
%%		{error, Reason} - something goes wrong. Reason can be:
%%			already_set  : hook already defined
%%
-spec new(any(), list()) -> ok | {error, any()}.
new(Hookname, GOpts) ->
    gen_server:call(epbxd_hooks, {new, Hookname, GOpts}).

%% @doc Add a new callback for a given Hook at default priority (50)
%%
%% @sample
%%		add({sip,request,'INVITE'}, {sip_handler, invite}, [{timeout, 42}]).
%%
%% @params
%%		Hookname
%%		Callback		- {Module, Function} tuple
%%		Opts			- list of anything. Are given to Callback when executed
%%
-spec add(any(), {atom(), atom()}, list()) -> ok | error.
add(Hookname, Callback={_,_}, Opts) ->
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
add(Hookname, Priority, Callback={_,_}, Opts) when 1 =< Priority andalso Priority =< 100 ->
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
%% @params
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
%% @params
%%		Priority - from 1 to 100
%%
%% @return
%%		- {error, not_found}	if Hook not set
%%		- undefined				if no callback set for this Hook at this Priority
%%		- a Callback ({Module,Function}) else
%%
-spec at(any(), integer()) -> fail | undefined | {atom(),atom()}.
at(Hookname, Priority) when 1 =< Priority andalso Priority =< 100 ->
	case gen_server:call(epbxd_hooks, {get, Hookname}) of
        {_, Callbacks} ->
			case lists:nth(Priority, Callbacks) of
				{Callback, _} -> Callback;
				_             -> undefined
			end;

		% Hookname not defined
		_                    -> {error, not_found}
	end.

%% @doc Execute callbacks associated with given hook
%%
%% @sample
%%		{ok, State} = run({sip,request,'INVITE'}, {#sip_message{}, Socket}).
%%
%% @return
%%		{ok, State}							- All callback successfully executed. Return last callback State
%%		{ok, State, {Priority, Callback}}   - A callback stopped execution flow without error
%%											  i.e: authent success, no need to continue
%%		{error, Reason}						- A callback stopped execution flow with error, giving Reason reason
%%
%% @note
%%		returned code may be customised (see new() GOpts param)
%%
-spec run(any(), any()) -> {ok|stop|error, any()}.
run(Hookname, Args) ->
	case gen_server:call(epbxd_hooks, {get, Hookname}) of
		{GOpts, Callbacks} ->
			run_(Callbacks, {Hookname, 1}, Args, undefined, GOpts);

		_                  ->
			undefined
	end.

run_([], _, _, State, GOpts)                                              ->
	{proplists:get_value(onlast, GOpts, ?DEFAULT_ONLAST), State};
run_([undefined | Tail], {Hookname, Prio}, Args, State, GOpts)            ->
	run_(Tail, {Hookname, Prio+1}, Args, State, GOpts);
run_([{{Mod, Fun}, Opts} |Tail], {Hookname, Prio}, Args, State, GOpts)    ->
	case Mod:Fun({Hookname, Prio}, Args, State, Opts) of
        % continue to next callback, with Args as returned value
		{next , Args2, State2}  ->
			run_(Tail, {Hookname, Prio+1}, Args2, State2, GOpts);

        % continue to next callback, with Args as previous value
        {next, State3}          ->
			run_(Tail, {Hookname, Prio+1}, Args, State3, GOpts);

        % stop callbacks execution (but not an error)
		{stop , State3}         ->
			{proplists:get_value(onstop, GOpts, ?DEFAULT_ONSTOP), State3, {Prio, {Mod, Fun}}};

        % stop execution, returning an error
		{error, Reason}         ->
			{error, Reason, {Prio, {Mod, Fun}}}
	end.


%%
%% PRIVATE gen_server interface
%%

init(_Args) ->
	{ok, []}.

handle_call({new, Hookname, GOpts}, _From, State)    ->
	{Reply, State2} = case proplists:get_value(Hookname, State) of
        undefined  ->
            % new hook
			{ok, [{Hookname, {GOpts, lists:duplicate(100, undefined)}} | State]};

		_          ->
			{{error, already_set}, State}
    end,

    {reply, Reply, State2};

handle_call({add, Hookname, Priority, Callback, Opts}, _From, State) ->
	{Reply2, State3} = case lists:keytake(Hookname, 1, State) of
		{value, {Hookname, {GOpts, Callbacks}}, State2} ->
			{Head, [At | Tail]} = lists:split(Priority-1, Callbacks),

			{Reply, Callbacks2} = case At of
				undefined ->
					{ok, lists:append([Head, [{Callback, Opts}], Tail])};

				_         ->
					% a callback is already set for this hook at this priority
					% we do not replace it
					{{error, already_set}, Callbacks}
			end,

			{Reply, [{Hookname, {GOpts, Callbacks2}} | State2]};

		false                                           ->
			% hook not found
			{{error, not_found}, State}
	end,


	{reply, Reply2, State3};

handle_call({del, Hookname, Priority, Callback}, _From, State) ->
	{Reply, State3} = case lists:keytake(Hookname, 1, State) of
		{value, {Hookname, {GOpts, Callbacks}}, State2} ->
			{Head, [At | Tail]} = lists:split(Priority-1, Callbacks),

			case At of
				% hook callback is matching
				{Callback, _} ->
					{ok, [{Hookname, {GOpts, lists:append([Head, [undefined], Tail])}} | State2]};

				_        ->
					% callback is not matching: we do not delete callback found at
					% priority
					{{error, dont_match}, State}
			end;

		% Hookname not found
		false                                           ->
			{{error, not_found}, State}
	end,

	{reply, Reply, State3};

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

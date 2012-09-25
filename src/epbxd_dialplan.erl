
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

% @doc Dialplan handling
-module(epbxd_dialplan).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

% API
-export([start_link/0, add/2, dispatch/2]).
% gen_server interface
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("utils.hrl").
-include("epbxd_dialplan.hrl").
-include("epbxd_channel.hrl").

start_link() ->
	gen_server:start_link({local, epbxd_dialplan}, ?MODULE, [], []).

init(_) ->
	{ok, {retrie, []}}.

add(Re, {Mod, Fun}) ->
	gen_server:call(epbxd_dialplan, {add, Re, {Mod, Fun}}).

%% @doc Dispatch a call following dialplan routing
%%
-spec dispatch(binary(), #call_channel{}) -> any().
dispatch(Extension, Channel) ->
	io:format(user, "dialplan:dispatcher= ~p~n", [Extension]),
	%app_dial:exec(Extension, [], Channel).
	case gen_server:call(epbxd_dialplan, {dispatch, Extension}) of
		{error, Reason} ->
			?DEBUG("dispatcher:fail= ~p", [Reason]),
			{error, Reason};

		{ok, Callbacks} ->
			?DEBUG("dispatcher:success= ~p", [Callbacks]),
			{Mod, Fun} = Callbacks,
			Mod:Fun(Extension, Channel)
	end.

handle_call({add, Re, Action}, _From, State) ->
	State2 = retrie:merge(State, retrie:reduce(retrie:encode(Re, Action))),

	{reply, ok, State2};

handle_call({dispatch, Extension}, _From, State) ->
	?DEBUG("dialplan:dispatch= ~p ~p~n", [State, Extension]),
	{reply, {ok, retrie:match(State, Extension)}, State}.

handle_cast(_Req, _State) ->
	{noreply, _State}.

handle_info(_Info, _State) ->
	{noreply, _State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, _State, _Extra) ->
	{ok, _State}.

%internal(<<"140">>, Context) ->
%	io:format("140~n",[]);
%
%internal(Ext= <<"1",_:2/binary>>, Context) ->
%	io:format("match ~p~n",[Ext]),
%	app:dial(Ext, Context);
%
%%%
%%% T between 0 and 5
%%% 1XX4[0-5]
%internal(Ext= <<"1",_:2/binary,"4",T>>, Context) when T >= $0, T =< 53 -> 
%	io:format("4 match= ~p ~p~n", [Ext,T]);
%
%internal(_, Context) ->
%	io:format("fallback catcher~n",[]),
%	app:hangup(Context).

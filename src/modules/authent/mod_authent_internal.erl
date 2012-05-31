
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

% @doc Erlang internal authent module
-module(mod_authent_internal).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([authent/4]).
% gen_epbxd_module
-export([start/1, stop/0]).

-include("utils.hrl").

%% @doc Start module
%%
%% Initialize module environment
%%
-spec start(any()) -> ok|fail.
start(Opts) ->
	% registering hooks
	epbxd_hooks:add(authent, proplists:get_value(priority, Opts, 50), {?MODULE, authent}, []),
	ok.

%% @doc Stop module
%%
%% Uninstall authent hook
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del(authent, {?MODULE, authent}),
	ok.

%% @doc Authenticate a user using Internal mnesia database
%%
%% @note
%%      at now, Domain and Password are ignored
%%
-spec authent(tuple(), tuple(), any(), list()) -> tuple(ok, any()).
authent({authent, _}, {User, _Domain, _Password}, State, _) ->
	case
		mnesia:dirty_read(endpoints, User)
	of
		[Endpt] ->
			% we found data: stop hook execution, returning result
			{stop, Endpt};

		[]      ->
			% user not found, lets try next hook handler
			{next, State}
	end.

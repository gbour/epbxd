
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

% @doc authent module using external file
%
%       we use file configured in epbxd.cfg
%       i.e: {mod_authent_file, [...,{filename, "/etc/epbxd/users.auth"},{mode, clear}]},
%
%       this file must be readable by epbdx process owner
%
% @note
%   This module is sub-optimal as the file is read again for each request
%
%
-module(mod_authent_file).
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
	epbxd_hooks:add(authent, proplists:get_value(priority, Opts, 50), {?MODULE,	authent}, Opts),
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del(authent, {?MODULE, authent}),
	ok.

%% @doc Authenticate a user using external text file
%%
%% file content is as:
%%      username:realm:password\n
%%
%% where password is encoded at you choice (but the same method for the whole file)
%%
%% @note
%%      . at now, Domain and Password are ignored
%%      . file is reloaded each time
%%
-spec authent(tuple(), tuple(), any(), list()) -> tuple(ok, any()).
authent(_, {User, _Domain, _Password}, State, Opts) ->
	case file:read_file(proplists:get_value(filename, Opts)) of
		% cannot read file (do not exists, no read access, ...)
		{error, _} ->
			{next, State};

		{ok, Content} ->
			io:format(user, "file= ~p~n", [Content]),
			match(utils:bin(User), binstr:split(Content, <<"\n">>), State)
	end.

%
match(_User, [], State)   ->
	{next, State};
match(User, [H|T], State) ->
	case binstr:split(H, <<":">>) of
		[User,_,_] -> {stop, User};
		% related to a bug in binstr:split():: if last part is empty, it returns one field less
		[User,_]   -> {stop, User};

		% line does not match, trying next line
		_          -> match(User, T, State)
	end.


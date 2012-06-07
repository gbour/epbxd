
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

% @doc Modules behaviour specification
-module(gen_epbxd_channel).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-export([behaviour_info/1]).

%% @doc Define modules API
%%
-spec behaviour_info(any()) -> undefined | list({atom(), integer()}).
behaviour_info(callbacks) ->
	[
		{init  , 1},

		% actions
		{dial  , 2},
		{ring  , 2},
		{accept, 2},
		{hangup, 2},

		% events
		{on_called  , 2},
		{on_ringing , 2},
		{on_answered, 2},
		{on_hanguped, 2}
	];
behaviour_info(_Other)    ->
	undefined.


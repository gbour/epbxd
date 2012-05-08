
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

% @doc Dial() application
-module(app_dial).
-author("Guillaume Bour <guillaume@bour.cc>").

%TODO: implement application behaviour

% API
-export([exec/3]).

-include("epbxd_dialplan.hrl").

%-type(string(), list(atom()|tuple(atom(),any())), #call_context{}, tuple()) -> atom().
%NOTE: Account is binary
exec(Account, Opts, Env=#call_context{channel=#call_channel{type=Type}, source={S,T}, request=R}) ->
	io:format("app_dial: searching ~p/~p~n", [Type, Account]),

	% search user
	case
		mnesia:dirty_read(registrations, utils:str(Account))
	of
		[Reg] ->
			io:format(user, "found ~p~n",[Reg]),
			?CALLBACK(Type):request('dial', Reg, Opts, Env);

            

		[]      ->
			% not found. Use generic API to send not-found response
			?CALLBACK(Type):response('not-found', Account, Opts, Env)
	end.
			

%hangup(Context) ->
%	sips:app(hangup, [], Context).

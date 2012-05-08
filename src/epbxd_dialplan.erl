
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

% API
-export([dispatcher/2, callback/1]).

-include("epbxd_dialplan.hrl").

%% @doc Dispatch a call following dialplan routing
%%
-spec dispatcher(binary(), #call_context{}) -> any().
dispatcher(Extension, Context) ->
	io:format(user, "dialplan:dispatcher= ~p~n", [Extension]),
	app_dial:exec(Extension, [], Context).


callback(sip) -> epbxd_sip_sig.



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

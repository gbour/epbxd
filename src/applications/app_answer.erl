
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

% @doc Answer() application
-module(app_answer).
-author("Guillaume Bour <guillaume@bour.cc>").

%TODO: implement application behaviour

% API
-export([exec/2]).

-include("utils.hrl").
-include("epbxd_channel.hrl").

exec(Opts, Chan=#call_channel{state=State,source=Caller}) ->
	% if channel is not in 'pending', we cannot answer!
	case State of
		pending ->
			{ok, RtpContext} = epbxd_channel:accept(Caller, Opts);

		_       ->
			?DEBUG("Cannot answer: channel is not in *pending* state", []),
			{error, 'invalid-state'}
	end.

	%Type = 'sip',
	%io:format("app_answer: searching ~p/~p~n", [Type, Account]),

	%epbxd_channel:accept({Type,Account}, Chan, Opts).
	% not found. Use generic API to send not-found response
	%?CALLBACK(Src#call_peer.type):response('not-found', Account, Opts, Src#call_peer.peer)
			

%hangup(Context) ->
%	sips:app(hangup, [], Context).

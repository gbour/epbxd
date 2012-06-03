
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

% @doc channels generic node
-module(epbxd_channel).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-behaviour(gen_epbxd_channel).
-export([dial/3, ring/2, accept/2, hangup/2]).
%-export([on_called/2, on_ringing/2, on_answered/2, on_hanguped/2]).

-include("epbxd_channel.hrl").

%% @doc 
%%
-spec dial({atom(), string()}, call_channel(), list()) -> ok | {error, atom()}.
dial({Tech,To}, Channel, Opts) ->
	Handler = tech(Tech),

	case
		Handler:dial(Channel#call_channel.source, To, [{channel,Channel} | Opts])
	of
		{ok, Stub}      ->
			mnesia:dirty_write(channels, 
				Channel#call_channel{target=#call_peer{type=Tech, peer=Stub}}
			);

		{error, Reason} ->
			{error, Reason}
	end.

ring(#call_peer{type=Tech,peer=To}, Opts) ->
	Handler = tech(Tech),
	Handler:ring(To, Opts).

accept(#call_peer{type=Tech, peer=To}, Opts) ->
	Handler = tech(Tech),
	Handler:accept(To, Opts).

hangup(#call_peer{type=Tech, peer=To}, Opts) ->
	(tech(Tech)):accept(To, Opts).


tech(sip) -> epbxd_sip_channel.

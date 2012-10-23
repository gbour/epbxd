
%%	epbxd, Erlang PBX server
%%	Copyright (C) 2012, Guillaume Bour <guillaume@bour.cc>
%%
%%	This program is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, version 3.
%%
%%	This program is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
%%	along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%

-type sdp_encoding() :: 'PCMU' | 'PCMA' | 'GSM' | 'G721' | 'G722' | 'G728' | binary().
%%                     encoding        payload type  clock rate
-type sdp_rtpmap()   :: {sdp_encoding(), {integer(), integer()}}.

-record(sdp_origin, {
	username     = <<"epbxd">>  :: binary(),
	ssid         = 0            :: integer(),
	ssversion    = 0            :: integer(),
	nettype      = 'IN'         :: 'IN',
	addrtype     = 'IP4'        :: 'IP4' | 'IP6',
	address      = <<>>         :: binary()
}).
-type sdp_origin()   :: #sdp_origin{}. 

-record(sdp_connection, {
	nettype  = 'IN'    :: 'IN',
	addrtype = 'IP4'   :: 'IP4' | 'IP6',
	address  = <<>>          :: binary(),

	% used for rtcp media attribute only
	port     = undefined     :: undefined | integer()
}).
-type sdp_connection()     :: #sdp_connection{}.

-record(sdp_media,  {
	type   = audio     :: audio | video | text | application | message,
	proto  = 'RTP/AVP' :: udp | 'RTP/AVP' | 'RTP/SAVP',
	port   = 0         :: integer(),
	rtpmap = []        :: list(sdp_rtpmap()),
	rtcp   = undefined :: undefined | sdp_origin(),
	ptime  = 20        :: integer(),
	mode   = sendrecv  :: sendrecv | recvonly | sendonly | inactive,

	attrs      = []    :: list()
}).
-type sdp_media()        :: #sdp_media{}.

-record(sdp_session, {
	version    = 0         :: integer(),
	name       = <<"-">>   :: binary(),
	time       = {0,0}     :: {integer(), integer()},
	origin     = undefined :: undefined | sdp_origin(),
	connection = undefined :: undefined | sdp_connection(),

	medias     = []        :: list(#sdp_media{})
}).
-type sdp_session()      :: #sdp_session{}.

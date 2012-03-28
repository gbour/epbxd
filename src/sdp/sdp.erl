
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

-module(sdp).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([rtpmap/1,decode/1,encode/1]).
%-ifdef(debug).
%	-export([decode_/3, binary_to_addrtype/1, binary_to_media/1, binary_to_proto/1]).
%-endif.

-include("utils.hrl").
-include("sdp.hrl").

-spec rtpmap(sdp_encoding()) -> sdp_rtpmap().
rtpmap('PCMU') -> {'PCMU',  0, 8000};
rtpmap('PCMA') -> {'PCMA',  8, 8000};
rtpmap('GSM')  -> {'GSM' ,  3, 8000};
rtpmap('G721') -> {'G721',  2, 8000};
rtpmap('G722') -> {'G722',  9, 8000};
rtpmap('G728') -> {'G728', 15, 8000};
rtpmap(_)      -> undefined.

-spec binary_to_encoding(binary()) -> atom().
binary_to_encoding(<<"PCMU">>)      -> 'PCMU';
binary_to_encoding(<<"PCMA">>)      -> 'PCMA';
binary_to_encoding(<<"GSM">> )      -> 'GSM';
binary_to_encoding(<<"G721">>)      -> 'G721';
binary_to_encoding(<<"G722">>)      -> 'G722';
binary_to_encoding(<<"G723">>)      -> 'G723';
binary_to_encoding(<<"G728">>)      -> 'G728';
binary_to_encoding(<<"G729">>)      -> 'G729';
binary_to_encoding(C) when is_binary(C) -> erlang:binary_to_atom(C, latin1).

-spec payload_to_encoding(binary()) -> atom().
payload_to_encoding(<<"0">>)  -> 'PCMU';
payload_to_encoding(<<"8">>)  -> 'PCMA';
payload_to_encoding(<<"3">>)  -> 'GSM';
payload_to_encoding(<<"2">>)  -> 'G721';
payload_to_encoding(<<"9">>)  -> 'G722';
payload_to_encoding(<<"15">>) -> 'G728';
payload_to_encoding(_)        -> undefined.

-spec binary_to_addrtype(binary()) -> atom().
binary_to_addrtype(<<"IP4">>) -> 'IP4';
binary_to_addrtype(<<"IP6">>) -> 'IP6'.

-spec binary_to_media(binary()) -> atom().
binary_to_media(<<"audio">>)       -> audio;
binary_to_media(<<"video">>)       -> video;
binary_to_media(<<"text">>)        -> text;
binary_to_media(<<"application">>) -> application;
binary_to_media(<<"message">>)     -> message.

-spec binary_to_proto(binary()) -> atom().
binary_to_proto(<<"udp">>)      -> udp;
binary_to_proto(<<"RTP/AVP">>)  -> 'RTP/AVP';
binary_to_proto(<<"RTP/SAVP">>) -> 'RTP/SAVP'.


-spec decode(binary()) -> sdp_session().
decode(Raw) ->
	decode_(s, binstr:split(Raw, <<"\r\n">>), #sdp_session{}).

%%
%% Session decoding
%%

% Version (v=)
decode_(s, [<<"v=0">>                                    | Tail], Sess) ->
	decode_(s, Tail, Sess);
% Origin (o=)
decode_(s, [<<"o=", Origin/binary>>                      | Tail], Sess) ->
	[User, SessId, SessVersion, <<"IN">>, AddrType, Address] = binstr:split(Origin,	<<$\s>>),

	decode_(s, Tail,
		Sess#sdp_session{origin=#sdp_origin{
		username  = User,
		ssid      = utils:binary_to_integer(SessId),
		ssversion = utils:binary_to_integer(SessVersion),
		addrtype  = binary_to_addrtype(AddrType),
		address   = Address
	}});
% Session Name (s=)
decode_(s, [<<"s=", Name/binary>>                        | Tail], Sess) ->
	decode_(s, Tail, Sess#sdp_session{name=Name});
% Connection Data (c=)
decode_(s, [<<"c=IN ", AddrType:3/binary, $\s, Address/binary>> | Tail], Sess) ->
	decode_(s, Tail, 
		Sess#sdp_session{connection=#sdp_connection{
			addrtype = binary_to_addrtype(AddrType),
			address  = Address
		}}
	);
% Timing (t=)
decode_(s, [<<"t=", Timing/binary>>                      | Tail], Sess) ->
	[Start, End] = [ utils:binary_to_integer(E) || E <- binstr:split(Timing, <<$\s>>) ],
	
	decode_(s, Tail, Sess#sdp_session{time = {Start, End}});
% Media description (m=) 
decode_(s, [M= <<"m=", Rest/binary>>                     | Tail], Sess) ->
	Sess#sdp_session{medias= 
		decode_(m, [M|Tail], [])
	};

% end-of-session (match only if no media(s))
decode_(s, [<<>>], Sess) ->
	Sess;

%%
%% Media(s) decoding
%%

decode_(m, [<<"m=", Rest/binary>>                        | Tail], Medias) ->
	[MediaType, Port, Proto | Payloads] = binstr:split(Rest, <<$\s>>),

	Media2 = decode_(m, Tail, #sdp_media{
		type  = binary_to_media(MediaType),
		proto = binary_to_proto(Proto),
		port  = utils:binary_to_integer(Port)
	}),

	[Media2#sdp_media{rtpmap = map_payloads(Payloads, Media2#sdp_media.rtpmap)} | Medias];
% RTP encodings map (a=rtpmap)
decode_(m, [<<"a=rtpmap:", Map/binary>>                  | Tail], Media) ->
	[Payload, Rest] = binary:split(Map, <<$\s>>),
	[Enc, Clock]     = binary:split(Rest, <<$/>>),

	decode_(m, Tail,
		Media#sdp_media{rtpmap = [{Payload, [Enc, Clock]} | Media#sdp_media.rtpmap]
	});
% RTCP config (a=rtcp)
decode_(m, [<<"a=rtcp:", Rest/binary>>                   | Tail], Media) ->
	[Port, <<"IN">>, AddrType, Address] = binstr:split(Rest, <<$\s>>),

	Media#sdp_media{rtcp = #sdp_connection{
		port     = utils:binary_to_integer(Port),
		addrtype = binary_to_addrtype(AddrType),
		address  = Address
	}};
% Audio Packetisation (a=ptime)
decode_(m, [<<"a=ptime:", Ptime/binary>>                 | Tail], Media) ->
	decode_(m, Tail,
		Media#sdp_media{ptime = utils:binary_to_integer(Ptime)}
	);
% emission/reception modes (a=recvonly|sendrecv|sendonly|inactive)
decode_(m, [<<"a=recvonly">>                             | Tail], Media) ->
	decode_(m, Tail,
		Media#sdp_media{mode = recvonly}
	);
decode_(m, [<<"a=sendrecv">>                             | Tail], Media) ->
	decode_(m, Tail,
		Media#sdp_media{mode = sendrecv}
	);
decode_(m, [<<"a=sendonly">>                             | Tail], Media) ->
	decode_(m, Tail,
		Media#sdp_media{mode = sendonly}
	);
decode_(m, [<<"a=inactive">>                             | Tail], Media) ->
	decode_(m, Tail,
		Media#sdp_media{mode = inactive}
	);

%% unsupported attribute (ignored)
decode_(m, [<<"a=", Other/binary>>                       | Tail], Media) ->
	decode_(m, Tail, Media);
%% end-of-media
decode_(m, [<<>>], Media) ->
	Media.


%% map payloads either using static definitions or dynamic mapping
%-spec max_payloads(list(binary()), list()) -> list()
map_payloads([Payload | Tail], DynMap) ->
	Fmt = case proplists:get_value(Payload, DynMap) of
		undefined         ->
			rtpmap(payload_to_encoding(Payload));
		[Encoding, Clock] ->
			{binary_to_encoding(Encoding), [utils:binary_to_integer(Payload),	utils:binary_to_integer(Clock)]}
	end,

	[ Fmt | map_payloads(Tail, DynMap) ];
map_payloads([], _) ->
	[].


%%%
%%% Session encoding
%%%
-spec encode(sdp_session() | sdp_origin() | sdp_connection() | sdp_media()) -> binary().
encode(#sdp_session{version=V,name=N,time={S,E},origin=O,connection=C,medias=Medias}) ->
	<<
		"v=", (?I2B(V))/binary, "\r\n",
		"o=", (encode(O))/binary, "\r\n",
		"s=", N/binary, "\r\n",
		"c=", (encode(C))/binary, "\r\n",
		"t=", (?I2B(S))/binary, $\s, (?I2B(E))/binary, "\r\n",

		% medias
		(?L2B([ encode(M) || M <- Medias]))/binary,

		"\r\n"
	>>;

encode(#sdp_origin{username=U, ssid=Id, ssversion=Version, nettype=NetType, addrtype=AddrType, address=Addr}) ->
	<<
		U/binary               , $\s,
		(?I2B(Id))/binary      , $\s,
		(?I2B(Version))/binary , $\s,
		(?A2B(NetType))/binary , $\s,
		(?A2B(AddrType))/binary, $\s,
		Addr/binary
	>>;

encode(#sdp_connection{nettype=NetType, addrtype=AddrType, address=Addr, port=Port}) ->
	<<
		(encode(port, Port))/binary,

		(?A2B(NetType))/binary , $\s,
		(?A2B(AddrType))/binary, $\s,
		Addr/binary
	>>;

encode(#sdp_media{type=T, proto=P, port=Port, rtpmap=Map, rtcp=Rtcp, ptime=Pt, mode=Mode, attrs=Attrs}) ->
	Rtpmap2Bin = fun(M) ->
		{Enc, [Type, Clock]} = M,

		{
			<<$\s, (?I2B(Type))/binary>>,
			<<"a=rtpmap:", (?I2B(Type))/binary, $\s, (?A2B(Enc))/binary, $/, (?I2B(Clock))/binary, "\r\n">>
		}
	end,

	{BinPayloads, BinMaps} = lists:unzip(lists:map(Rtpmap2Bin, Map)),


	<<"m=", (?A2B(T))/binary, $\s, (?I2B(Port))/binary, $\s, (?A2B(P))/binary, 
		% payload numbers
		(?L2B(BinPayloads))/binary   , "\r\n",

		% rtp mapping
		(?L2B(BinMaps))/binary       ,

		%
		"a=ptime:",(?I2B(Pt))/binary , "\r\n",
		(encode(rtcp, Rtcp))/binary,
		"a=",(?A2B(Mode))/binary     , "\r\n"
	>>.

-spec encode(rtcp, undefined | sdp_connection()) -> binary().
encode(_, undefined)                 ->
	<<>>;
encode(rtcp, Rtcp=#sdp_connection{}) ->
	<<"a=rtcp:", (encode(Rtcp))/binary, "\r\n">>;
encode(port, Port)                   ->
	<<(?I2B(Port))/binary, $\s>>.


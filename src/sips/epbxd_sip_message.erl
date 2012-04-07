
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

% @doc epbxd API to handle SIP message (code/decode from/to binary stream)
-module(epbxd_sip_message).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-export([decode/1, encode/1]).

-ifdef(debug).
	-export([decode/3]).
-endif.

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

%%
%% DECODING
%% 

%% @doc Decode one or more SIP messages
%%
%%
%% @return [M|Tail]		list of decoded messages 
%%		where M is {ok, #sip_message{...}}
%%    Last element of list MAY BY a {error, invalid, binary tail}
%%
%% TODO: partial decoding (packet fragmentation). cases:
%%		1 packet = 1 complete message
%%		1 packet = 1 message but incomplete (fragmentation)
%%		1 packet = 2+ messages (last may be incomplete)
%% TODO: don't do global fail if one message fail
-spec decode(binary()) -> list({ok, sip_message()} | {error, invalid, binary()}).
decode(Packet) ->
	lists:reverse(
		decode(Packet, [])
	).

decode(<<>>, Acc)   ->
	Acc;
decode(Packet, Acc) ->
	case binstr:split(Packet, <<"\r\n\r\n">>, 2) of
		[Head,Tail] ->
			case decode(#sip_message{}, binstr:split(Head, <<"\r\n">>), Tail) of
				{ok, Message, Rest} ->
					decode(Rest, [{ok, Message} | Acc]);

				{error, invalid}    ->
					[{error, invalid, <<Packet/binary>>} | Acc]
			end;

		_Packet ->
			[{error, invalid, Packet} | Acc]
	end.

%% @doc Decode a single message
%%
%% @private
%% @sample
%% 		epbxd_sip_message:decode(
%%			#sip_message{}, 
%%			[<<"SIP/2.0 200 OK">>, .., <<"User-Agent: Epbxd">>],
%%     	<<"v=0\r\n...>>
%% 		)
%% 
%% TODO: handle incomplete payload
%% TODO: simplify decode(header,...) -> only Headers(raw+decoded) + Rest args
-spec decode(sip_message(), list(binary()), binary()) -> {ok, sip_message()}.
decode(Message=#sip_message{}, [Line|Tail], Rest) ->
	case binstr:split(Line, <<" ">>) of
		% SIP/2.0 200 OK
		[<<"SIP/", Version/binary>>, Status, Reason] ->
			decode(header,
				Message#sip_message{type=response, version=Version,
				                    status=utils:int(Status), reason=Reason},
				Tail,
				Rest);

		% REGISTER sip:192.168.0.194:5060 SIP/2.0
		[Method, URI, <<"SIP/", Version/binary>>] ->
			decode(header,
				Message#sip_message{type=request, version=Version, 
				                    method=utils:atom(Method), uri=epbxd_sip_uri:decode(URI)},
				Tail,
				Rest);

		_Else ->
			%TODO: output to logger system
			%?ERROR("sips:msgdecode= invalid SIP message start-line= ~p", [_Else]),
			{error, invalid, Rest}
	end.

decode(header, Message=#sip_message{headers=Headers}, [Line|Tail], Rest) ->
	[Key, Value] = [ binstr:strip(Item) || Item <- binstr:split(Line, <<":">>, 2) ],
	M = Message#sip_message{headers=
		[epbxd_sip_header:decode(Key, Value) | Headers]
	},

	decode(header, M, Tail, Rest);

% end of headers -> reading (but not decoding) message payload
%
% TODO: Content-Length is expressed in octet, not characters (should be ok with
% binaries)
% TODO: check len(Rest) >= Len (and fail else)
decode(header, Message=#sip_message{headers=Headers}, [], Rest) ->
	% restoring headers original order
	Headers2 = lists:reverse(Headers),

	case proplists:get_value('Content-Length', Headers2) of
		% no 'Content-Length' header == zero-sized payload
		undefined ->
			{ok, Message#sip_message{headers=Headers2}, Rest};

		% zero length = no payload
		0         ->
			{ok, Message#sip_message{headers=Headers2}, Rest};

		Len       ->
			case byte_size(Rest) >= Len of
				true ->
					{ok, 
						Message#sip_message{headers=Headers2, payload=binstr:substr(Rest, 1, Len)},
						binstr:substr(Rest,	Len+1)
					};

				_    -> {error, invalid}
			end
	end.

%%
%% ENCODING
%%

%% @doc Encode SIP message
%%
%% @return Encoded message (iolist)
%%		<<"SIP/2.0 200 OK\r\n...>> =
%%		erlang:iolist_to_binary(epbxd_sip_message:encode(#sip_message{type=response,...})).
%%
-spec encode(#sip_message{}) -> iolist().
encode(#sip_message{version=V,type=request,method=M,uri=U,headers=H}) ->
	Headers = encode(header, H, []),
	[
		utils:str(M), $\s, epbxd_sip_uri:encode(U), $\s, <<"SIP/">>, V, <<"\r\n">>,
		Headers
	];
encode(#sip_message{version=V,type=response,status=S,reason=R,headers=H}) ->
	Headers = encode(header, H, []),

	[
		<<"SIP/">>, V, $\s, utils:str(S), $\s, R, <<"\r\n">>,
		Headers
	].

encode(header, [], Acc)    ->
	[Acc, <<"\r\n">>];
encode(header, [{Header, Value}|Tail], Acc) ->
	encode(header, Tail, 
		[Acc, epbxd_sip_header:encode(Header, Value), <<"\r\n">>]
	).
	


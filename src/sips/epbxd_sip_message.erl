
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
-export([decode/1, encode/1, response/2]).

-ifdef(debug).
	-export([decode/3, to/2]).
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
	case binstr:strpos(Packet, <<"\r\n\r\n">>) of
		0 ->
			[{error, invalid, Packet} | Acc];

		Pos ->
			Head = binstr:substr(Packet, 1, Pos-1),
			Tail = binstr:substr(Packet, Pos+4),

			case decode(#sip_message{}, binstr:split(Head, <<"\r\n">>), Tail) of
				{ok, Message, Rest} ->
					decode(Rest, [{ok, Message} | Acc]);

				{error, invalid}    ->
					[{error, invalid, <<Packet/binary>>} | Acc]
			end
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
	Key2 = epbxd_sip_header:normalize(Key),
	
	{Tuple, Headers2} = case lists:keytake(Key2, 1, Headers) of
		{value, {Key2, Stored}, _Headers} ->
			{epbxd_sip_header:decode(Key2, Value, Stored), _Headers};

		false ->
			{epbxd_sip_header:decode(Key2, Value, undefined), Headers}
	end,

	%io:format(user, "plop ~p ~p~n",[Tuple, Headers2]),
	M = Message#sip_message{headers=[Tuple | Headers2]},
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
	

%%
%% RESPONSES
%%

%% @doc Build a SIP response messages
%%
%%	building response message is based on request,
%%	following process described in RFC 3261, chapter 8.2.6
%%
%% @sample
%%		Response = epbxd_sip_message:response(trying, Request).
%%

% 1xx provisional responses
-spec response(atom(), #sip_message{}) -> #sip_message{}.
response(trying , Req) ->
	response(provisional_, Req, 100, "Trying");
response(ringing, Req) ->
	response(provisional_, Req, 180, "Ringing");

% 2XX
response(ok     , Req) ->
	response(standard_   , Req, 200, "OK");

% 4XX
response('not-found', Req) ->
	response(standard_, Req, 404, "Not found").

%% @doc Generate provisional response (1xx)
%%
%% @private
%%
%%TODO: handle Timestamp header
%%TODO: lists:nth(.. 'Via' ..) will crash if 'Via' header not set in message
-spec response(atom(), #sip_message{}, integer(), string()) -> #sip_message{}.
response(provisional_, Request, Status, Reason) ->
	response(standard_, Request, Status, Reason);

%% @doc Generate standard response
%% @private
response(standard_   , #sip_message{headers=Headers}, Status, Reason) ->
	#sip_message{
		type    = response,
		status  = Status,
		reason  = Reason,
		headers = [
			{'Via'            , proplists:get_value('Via'    , Headers)},
			{'From'           , proplists:get_value('From'   , Headers)},
			{'To'             , to(proplists:get_value('To', Headers), Status)},
			{'Call-ID'        , proplists:get_value('Call-ID', Headers)},
			{'CSeq'           , proplists:get_value('CSeq'   , Headers)},
			{'User-Agent'     , header('User-Agent' )},
			{'Content-Length' , 0}
		]
	}.

%% @doc Append *tag* param in To header except for "100 Trying" provisional response
%%
%% *tag* param is not added for 100/Trying provisional response
%% For all other response, a *tag* param is added, unless already set
%%
%% @private
%% @sample
%%		#sip_address{params=[]}               = epbxd_sip_response:to(#sip_address{params=[]}, 100),
%%		#sip_address{params=[{"tag","1234"}]} = epbxd_sip_response:to(#sip_address{params=[]}, 180),
%%		#sip_address{params=[{"tag","777"}]}  =	epbxd_sip_response:to(#sip_address{params=[{"tag","777"}]}, 180).
%%
-spec to(#sip_address{}, integer()) -> #sip_address{}.
to(To, 100) ->
	To;
to(To=#sip_address{params=Params}, _)   ->
	case proplists:is_defined("tag", Params) of 
		true  ->
			To;
		false ->
			To#sip_address{params=[{"tag", epbxd_sip_header:tag()} | Params]}
	end.

%%
%% Standardized headers
%%
header('Allow')      ->
	false;
header('Supported')  ->
	false;
%%TODO: user-agent must be read from configuration
header('User-Agent') ->
	"Epbxd".

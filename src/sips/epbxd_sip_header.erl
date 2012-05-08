
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

% @doc epbxd API to encode/decode SIP headers (to/from binary stream)
-module(epbxd_sip_header).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([decode/3, decode/2, encode/2, normalize/1, tag/0]).

-include("utils.hrl").
-include("epbxd_sip.hrl").

%%
%% DECODING
%%
%%

% headers multi-line mode
%		both are for standardized headers
%		multi : keep all in a list
%		single: keep only last value
%
%		following is for custom headers
%		custom: if one, return value, else a list of all values
%
mode('Via')            -> multi;
mode('Route')          -> multi;
mode('From')           -> single;
mode('To')             -> single;
mode('Contact')        -> single;
mode('Content-Length') -> single;
mode('CSeq')           -> single;
mode('Max-Forwards')   -> single;
mode(_)                -> custom.

%% @doc Decode SIP header.
%%		decoded value is merge with precedent value(s) following header mode
%%	
-spec decode(atom(), binary(), undefined|list()|binary()) -> tuple(atom(), any()).
decode(Header, Value, Decoded) ->
	{Header, merge_(mode(Header), decode_(Header, Value), Decoded)}.

%% @doc
-spec merge_(atom(), any(), undefined|binary()|string()|list()) -> any().
% Standard one-valued headers
%		keeping only the last value
merge_(single, Value, _)                               ->
	Value;
% Standard multi-valued headers
%		keeping all values in an ordered list
merge_(multi, Value, undefined)                        -> 
	[Value];
merge_(multi, Value, List)                             ->
	List ++ [Value];
% Custom headers:
%		if one value found, return raw (binary) value
%		else if more than one value, return list of raw values
merge_(custom, Value, undefined)                       ->
	Value;
merge_(custom, Value, Decoded) when is_binary(Decoded) ->
	merge_(custom, Value, [Decoded]);
%NOTE: as custom header values are kept as binary, we have no collision between
%			 lists and strings
merge_(custom, Value, Decoded) when is_list(Decoded)   ->
	Decoded ++ [Value].


%% @doc Decode a SIP header (already splitted)
%%
%% @return {Header, Value} where
%%						Header is a normalized atom
%%						Value  is the matching decoded value
%%
%% @sample
%%		{'Content-Length', 42}      = epbxd_sip_header:decode(<<"Content-Length">>, <<"42">>).
%%		{'Content-Length', invalid} = epbxd_sip_header:decode(<<"Content-Length">>, <<"foobar">>).
%%
%% TODO: implement all standard headers
%% TODO: short header form (f -> From)
%% TODO: emit a log info when a not-standard header is found
%% TODO: detect unmatching headers (fROm -> From), emit warning
%%
-spec decode(binary(), binary()) -> tuple(atom(), any()).
decode(Header, Value) ->
	H = normalize(Header),
	{H, decode_(H, Value)}.

%% @doc Normalize SIP header
%%		. convert binary/list to atom
%%		. convert header short-form to long-form
%%
-spec normalize(binary() | atom()) -> atom().
normalize(Header) when is_binary(Header); is_list(Header) ->
	normalize(utils:atom(Header));
normalize('f')    -> 'From';
normalize(Header) -> Header.

%% @doc	Decode header value
%%
%%	Value interpretation is specific to each header
%%
%% From, To, Contact
%%    From: "Bob" <sips:bob@biloxi.com> ;tag=a48s
%%
%% TODO: handle other following forms
%%    From: sip:+12125551212@phone2net.com;tag=887s
%%    From: Anonymous <sip:c8oqz84zk7z@privacy.org>;tag=hyh8
%%
-spec decode_(atom(), binary()) -> any().
decode_('From', Value) ->
	case
		re:run(utils:str(Value),"^\\s*
			(\"(?<display>[^\"]+)\"\\s+<)?
				(?(<display>)|(?<lt><)?)
					(?<uri>[^<>\"\s]+)
				(?(<display>)>|(?(<lt>)>))
					\\s*(?<params>;.*)?$",
			[extended, {capture,[display,uri,params],list}]) of

		{match, [D,U,P]} ->
			case epbxd_sip_uri:decode(U) of
				invalid ->		
					invalid;
				Uri     ->
					#sip_address{displayname=D,uri=Uri,params=epbxd_sip_uri:params(P)}
			end;

		_ ->
			invalid
	end;

decode_('To', Value) ->
	decode_('From', Value);
decode_('Contact', Value) ->
	decode_('From', Value);

%%
%% Content-Length
%%    Content-Length: 1548
%%
decode_('Content-Length', Value) ->
	decode_('Max-Forwards', Value);

%%
%% CSeq header
%%		CSeq: 100 REGISTER
%%
decode_('CSeq', Value) ->
	try
		[Number, Method] = binstr:split(Value, <<" ">>, 2),
		{utils:int(Number), Method}
	of
		Ret   -> Ret
	catch
		_:_   -> invalid
	end;

%%
%% Max-Forwards header
%%    Max-Forwards: 70
%%
decode_('Max-Forwards', Value) ->
	try
		utils:int(Value)
	of
		Ret -> Ret
	catch
		_:_ -> invalid
	end;

%%
%% Via
%%		Via: SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKddnebypp
%%
decode_('Via', Value) ->
	case re:run(utils:str(Value),
		"^\s*SIP/2.0/(?<transport>[^\s]+)\s+(?<host>[^;:\s]+)(:(?<port>\\d+))?(?<params>.*)$",
		[{capture,[transport,host,port,params],list}]) of

		{match, [T,H,Pt,P]} ->
			Pt_ = if 
				Pt == [] -> undefined;
				true     -> utils:int(Pt)
			end,

			#sip_via{transport=via_transport(T),host=H,port=Pt_,params=epbxd_sip_uri:params(P)};
		_ ->
			invalid
	end;

%%
%% Other headers
%%   keep original value
%%
decode_(_, V) ->
	V.

%% @doc VIA transport: string to atom and reverse
%%
%% @private
%% @sample
%%		udp     = via_transport("UDP").
%%		"UDP"   = via_transport(udp).
%%		invalid = via_transport("foobar").
%%
-spec via_transport(string()) -> atom().
via_transport("UDP")  -> udp;
via_transport("TCP")  -> tcp;
via_transport("TLS")  -> tls;
via_transport("SCTP") -> sctp;

via_transport(udp)    -> "UDP";
via_transport(tcp)    -> "TCP";
via_transport(tls)    -> "TLS";
via_transport(sctp)   -> "SCTP";
via_transport(_)      -> invalid.


%%
%% ENCODING HEADERS
%%


%% @doc Encode SIP headers
%%
%% encode() take a header key (atom) and associated value (specific for each header),
%% and return an iolist
%%
%% @sample
%%		<<"CSeq: 100 INVITE">> = iolist_to_binary(encode('CSeq', {100, <<"INVITE">>)).
%%
encode('CSeq', {Seq, Method}) ->
	[<<"CSeq: ">>, utils:str(Seq), $\s, utils:bin(Method)];

encode('Via', #sip_via{transport=T,host=H,port=Pt,params=P}) ->
	[<<"Via: SIP/2.0/">>, via_transport(T), $\s, H, via_port(Pt), epbxd_sip_uri:encode(params,P)];
encode('Via', ListOf)  ->
	encode('Via', ListOf, []);

encode(Header, #sip_address{displayname=undefined,uri=U,params=P}) when
		Header =:= 'From';
		Header =:= 'To';
		Header =:= 'Contact' ->
	[utils:str(Header), <<": <">>, epbxd_sip_uri:encode(U), $>, epbxd_sip_uri:encode(params,P)];
encode(Header, #sip_address{displayname=D,uri=U,params=P}) when
		Header =:= 'From';
		Header =:= 'To';
		Header =:= 'Contact' ->
	[
		utils:str(Header), <<": \"">>, D, <<"\" <">>, epbxd_sip_uri:encode(U), $>,
		epbxd_sip_uri:encode(params,P)
	];

encode(Header, Value) when is_integer(Value) orelse is_atom(Value) ->
	[utils:str(Header), <<": ">>, utils:str(Value)];

encode(Header, Value) when is_binary(Value) ->
	[utils:str(Header), <<": ">>, Value];

%% NOTE: we use io_lib:printable_list() to distinguish strings from lists
%%       It is really not optimal (as the function look through all list items
%%
%%TODO: measure performance loss, optimize
%%      one way of optimization is to explicitely declare standard SIP headers, as we
%%      known Values associated with.
%%      So this method will only be used for custom headers.
encode(Header, Value) ->
	case io_lib:printable_list(Value) of
		true  ->
			[utils:str(Header), <<": ">>, Value];
		false ->
			encode(Header, Value, [])
	end.


%% @doc Lists encoding
%%
%% @private
%%
encode(_Header, [], Acc)        ->
	Acc;
encode(Header, [Val|Tail], [])  ->
	encode(Header, Tail, encode(Header, Val));
encode(Header, [Val|Tail], Acc) ->
	encode(Header, Tail, [Acc, <<"\r\n">>, encode(Header, Val)]).


%% @doc Via port encoding
%%
%% %private
%%
via_port(undefined) ->
	[];
via_port(Pt)        ->
	[$:, utils:str(Pt)].


%% @doc Generate a unique tag value
%% 
%% tag is random 16 characters length hexadecimal string
%%
%% @sample
%%		"df45ed856c5b4909" = epbxd_sip_header:tag().
%%
-spec tag() -> string().
tag() ->
	lists:flatten(
		lists:foldl(
			fun(_,Acc) -> [io_lib:format("~.16b",[random:uniform(16)-1]) |Acc] end, 
			[], lists:seq(1,16)
	)).


%% 
%% Decode SIP headers value
%%
%%
-module(epbxd_sip_header).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([decode/2, encode/2, tag/0]).

-include("utils.hrl").
-include("epbxd_sip.hrl").

%%
%% DECODING
%%
-spec decode(binary(), binary()) -> tuple(atom(), any()).
decode(Header, Value) ->
	H = normalize_(Header),
	{H, decode_(H, Value)}.

normalize_(Header) when is_binary(Header); is_list(Header) ->
	utils:atom(Header);
normalize_('f')    -> 'From';
normalize_(Header) -> Header.

%%
%% From, To, Contact headers
%%    From: "Bob" <sips:bob@biloxi.com> ;tag=a48s
%%
%% TODO: handle other following forms
%%    From: sip:+12125551212@phone2net.com;tag=887s
%%    From: Anonymous <sip:c8oqz84zk7z@privacy.org>;tag=hyh8
%%
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
%% Content-Length header
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
decode_(_, V) ->
	V.


via_transport("UDP")  ->
	udp;
via_transport("TCP")  ->
	tcp;
via_transport("TLS")  ->
	tls;
via_transport("SCTP") ->
	sctp;
via_transport(_A) ->
	invalid.

%%
%% ENCODING HEADERS
%%
encode("Call-id", Value) ->
	encode("Call-ID", Value);

encode("Content-length", V) ->
	encode("Content-Length", V);

encode("Content-type", V) ->
	encode("Content-Type", V);

encode("Cseq", {Seq, Method}) ->
	lists:concat(["CSeq: ",integer_to_list(Seq)," ",Method]);

encode("Max-forwards", V) ->
	encode("Max-Forwards", V);

encode("User-agent", Value) ->
	encode("User-Agent", Value);

encode("Via", #sip_via{transport=T,host=H,port=undefined,params=P}) ->
	lists:concat([
		"Via: SIP/2.0/",string:to_upper(atom_to_list(T))," ",H,uri:format(params,P)
	]);
encode("Via", #sip_via{transport=T,host=H,port=Pt,params=P}) ->
	lists:concat([
		"Via: SIP/2.0/",string:to_upper(atom_to_list(T))," ",H,":",Pt,uri:format(params,P)
	]);

encode(Header, #sip_address{displayname=undefined,uri=U,params=P}) when
		Header =:= "From";
		Header =:= "To";
		Header =:= "Contact" ->
	lists:concat([Header,": <",uri:encode(U),">",uri:format(params,P)]);
encode(Header, #sip_address{displayname=D,uri=U,params=P}) when
		Header =:= "From";
		Header =:= "To";
		Header =:= "Contact" ->
	lists:concat([Header,": \"",D,"\" <",uri:encode(U),">",uri:format(params,P)]);

encode(Header, Value) when is_list(Value) ->
	lists:concat([Header,": ",Value]);
encode(Header, Value) when is_integer(Value) ->
	lists:concat([Header,": ",integer_to_list(Value)]);
encode(_,_) ->
	invalid.

%%
%% generate tag value
%%
tag() ->
	lists:flatten(
		lists:foldl(
			fun(_,Acc) -> [io_lib:format("~.16b",[random:uniform(16)-1]) |Acc] end, 
			[], lists:seq(1,16)
	)).


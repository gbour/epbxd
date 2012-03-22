%% 
%% Decode SIP headers value
%%
%%
-module(header).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([decode/2, encode/2, tag/0]).

-include("utils.hrl").
-include("sips.hrl").

%%
%% DECODING
%%

%%
%% From, To, Contact headers
%%    From: "Bob" <sips:bob@biloxi.com> ;tag=a48s
%%
%% TODO: handle other following forms
%%    From: sip:+12125551212@phone2net.com;tag=887s
%%    From: Anonymous <sip:c8oqz84zk7z@privacy.org>;tag=hyh8
%%
decode("From", Value) ->
	case re:run(Value, "^\s*(\"(?<display>[^\"]+)\"\s+)?<(?<uri>[^>]+)>\s*(?<params>.*)$",
		[{capture,[display,uri,params],list}]) of

		{match, [D,U,P]} ->
			#address{displayname=D,uri=uri:decode(U),params=uri:params(P)};
		_ ->
			invalid
	end;
decode("To", Value) ->
	decode("From", Value);
decode("Contact", Value) ->
	decode("From", Value);

%%
%% Content-Length header
%%    Content-Length: 1548
%%
decode("Content-length", Value) ->
	decode("Max-forwards", Value);

%%
%% CSeq header
%%		CSeq: 100 REGISTER
%%
decode("Cseq", Value) ->
	try
		[Number, Method] = string:tokens(Value, " "),
		{list_to_integer(Number), Method}
	of
		Ret   -> Ret
	catch
		_:_   -> invalid
	end;

%%
%% Max-Forwards header
%%    Max-Forwards: 70
%%
decode("Max-forwards", Value) ->
	try
		list_to_integer(Value)
	of
		Ret -> Ret
	catch
		_:_ -> invalid
	end;

%%
%% Via
%%		Via: SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKddnebypp
%%
decode("Via", Value) ->
	case re:run(Value, "^\s*SIP/2.0/(?<transport>[^\s]+)\s+(?<host>[^;:\s]+)(:(?<port>\\d+))?(?<params>.*)$",
		[{capture,[transport,host,port,params],list}]) of

		{match, [T,H,Pt,P]} ->
			Pt_ = if 
				Pt == [] -> undefined;
				true     -> list_to_integer(Pt)
			end,

			#via{transport=via_transport(T),host=H,port=Pt_,params=uri:params(P)};
		_ ->
			invalid
	end;

%%decode("CSeq", Value) ->
%%	[Number, Method] = string:tokens(Value, " "),
%%	{list_to_integer(Number), Method};

%%
%% Other headers
%%   keep original value
decode(_,V) ->
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

encode("Via", #via{transport=T,host=H,port=undefined,params=P}) ->
	lists:concat([
		"Via: SIP/2.0/",string:to_upper(atom_to_list(T))," ",H,uri:format(params,P)
	]);
encode("Via", #via{transport=T,host=H,port=Pt,params=P}) ->
	lists:concat([
		"Via: SIP/2.0/",string:to_upper(atom_to_list(T))," ",H,":",Pt,uri:format(params,P)
	]);

encode(Header, #address{displayname=undefined,uri=U,params=P}) when
		Header =:= "From";
		Header =:= "To";
		Header =:= "Contact" ->
	lists:concat([Header,": <",uri:encode(U),">",uri:format(params,P)]);
encode(Header, #address{displayname=D,uri=U,params=P}) when
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
	lists:foldl(
		fun(_,Acc) -> [io_lib:format("~.16b",[random:uniform(16)-1]) |Acc] end, 
		[], lists:seq(1,16)
	).


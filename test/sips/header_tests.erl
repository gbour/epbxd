
-module(header_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/sips.hrl").

decode_test() ->
	%% From
	?assertEqual(header:decode("From", "foobar"), invalid),
	?assertEqual(header:decode("From"," \"101\" <sip:101@192.168.0.194:5060> ;tag=e2dafe4bb3"),
		{address,"101",{uri,"sip","101",undefined,"192.168.0.194","5060",[],[]},[{"tag","e2dafe4bb3"}]}),
	?assertEqual(header:decode("From"," <sip:101@192.168.0.194>"),
		{address,[],{uri,"sip","101",undefined,"192.168.0.194",[],[],[]},[]}),

	%% To
	?assertEqual(header:decode("To"," \"101\" <sip:101@192.168.0.194:5060>"),
		{address,"101",{uri,"sip","101",undefined,"192.168.0.194","5060",[],[]},[]}),

	%% Contact
	?assertEqual(header:decode("Contact"," \"101\" <sip:101@10.0.0.13:5060;transport=udp>;+sip.instance=\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\";expires=3600"),
		{address,"101",{uri,"sip","101",undefined,"10.0.0.13","5060",[{"transport","udp"}],[]},[{"+sip.instance","\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\""},{"expires","3600"}]}),

	%% CSeq
	?assertEqual(header:decode("Cseq", "100 INVITE"), {100, "INVITE"}),
	?assertEqual(header:decode("Cseq", "fail")      , invalid),
	?assertEqual(header:decode("Cseq", "zaz INVITE"), invalid),

	%% Max-Forward
	?assertEqual(header:decode("Max-forwards", "70")    , 70),
	?assertEqual(header:decode("Max-forwards", "foobar"), invalid),

	%% Content-Length
	?assertEqual(header:decode("Content-length", "1547")  , 1547),
	?assertEqual(header:decode("Content-length", "foobar"), invalid),

	ok.


decode_via_test() ->
	?assertEqual(header:decode("Via","SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKchhwgyha"),
		{via,udp,"192.168.0.187",5069,[{"branch","z9hG4bKchhwgyha"}]}),
	?assertEqual(header:decode("Via","SIP/2.0/UDP 192.168.0.187"),
		{via,udp,"192.168.0.187",undefined,[]}),
	ok.


%% HEADERS ENCODING

encode_callid_test() ->
	?assertEqual(header:encode("Call-id","ejpkbwsvnzotlba@bour.cc"),
		"Call-ID: ejpkbwsvnzotlba@bour.cc").

encode_cseq_test()    ->
	?assertEqual(header:encode("Cseq",{100,"INVITE"}), "CSeq: 100 INVITE").

encode_useragent_test() ->
	?assertEqual(header:encode("User-agent", "Epbxd"), "User-Agent: Epbxd").

encode_from_test() ->
	?assertEqual(
		header:encode("From",
				#address{displayname="101",uri=#uri{scheme="sip",user="bob",host="biloxi.com"}}),
		"From: \"101\" <sip:bob@biloxi.com>"
	),

	ok.

encode_to_test() ->
	?assertEqual(
		header:encode("To",
				#address{displayname="101",uri=#uri{scheme="sip",user="bob",host="biloxi.com"},
					params=[{"tag","15er75d2"}]}),
		"To: \"101\" <sip:bob@biloxi.com>;tag=15er75d2"
	),

	ok.

encode_contact_test() ->
	?assertEqual(
		header:encode("Contact",
			#address{displayname=undefined,uri=#uri{scheme="sips",user="101",host="192.168.0.10",
					port=445,params=[{"transport","udp"}]},
					params=[{"expires","3600"}]}),
		"Contact: <sips:101@192.168.0.10:445;transport=udp>;expires=3600"
	),

	ok.

encode_via_test() ->
	?assertEqual(header:encode("Via",	#via{transport=udp,host="192.168.0.187",port=5069,
				params=[{branch,"z9hG4bKchhwgyha"}]}),
		"Via: SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKchhwgyha"
	),
	?assertEqual(header:encode("Via",	#via{transport=udp,host="192.168.0.187"}),
		"Via: SIP/2.0/UDP 192.168.0.187"
	),
	ok.

encode_content_length_test() ->
	?assertEqual(header:encode("Content-length", 125), "Content-Length: 125"),
	ok.

encode_content_type_test() ->
	?assertEqual(header:encode("Content-type", "raw"), "Content-Type: raw"),
	ok.

encode_max_forwards_test() ->
	?assertEqual(header:encode("Max-forwards", 70), "Max-Forwards: 70"),
	ok.

encode_default_string_test() ->
	?assertEqual(header:encode("Foo","Bar"), "Foo: Bar").

encode_default_integer_test() ->
	?assertEqual(header:encode("Foo",123), "Foo: 123").

encode_fail_test()    ->
	?assertEqual(header:encode("Foo", false), invalid).


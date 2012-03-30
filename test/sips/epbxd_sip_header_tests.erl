
-module(epbxd_sip_header_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

decode_test() ->
	%% From
	?assertEqual(epbxd_sip_header:decode("From", "foobar")                 , invalid),
	?assertEqual(epbxd_sip_header:decode("From", "<sip:user@host")         , invalid),
	?assertEqual(epbxd_sip_header:decode("From", "sip:user@host>")         , invalid),
	?assertEqual(epbxd_sip_header:decode("From", "\"Doe\" sip:user@host")  , invalid),
	?assertEqual(epbxd_sip_header:decode("From", "\"Doe\" <sip:user@host") , invalid),
	?assertEqual(epbxd_sip_header:decode("From", "\"Doe\" sip:user@host>") , invalid),


	?assertEqual(epbxd_sip_header:decode("From"," \"101\" <sip:101@192.168.0.194:5060> ;tag=e2dafe4bb3"),
		{address,"101",{uri,"sip","101",undefined,"192.168.0.194","5060",[],[]},[{"tag","e2dafe4bb3"}]}),
	?assertEqual(epbxd_sip_header:decode("From"," <sip:101@192.168.0.194>"),
		{address,[],{uri,"sip","101",undefined,"192.168.0.194",[],[],[]},[]}),

	?assertEqual(epbxd_sip_header:decode("From", "sip:101@192.168.0.1"),
		{address,[],{uri,"sip","101",undefined,"192.168.0.1",[],[],[]},[]}),
	?assertEqual(epbxd_sip_header:decode("From", "sip:101@192.168.0.1;transport=udp"),
		{address,[],{uri,"sip","101",undefined,"192.168.0.1",[],[{"transport","udp"}],[]},[]}),

	%% To
	?assertEqual(epbxd_sip_header:decode("To"," \"101\" <sip:101@192.168.0.194:5060>"),
		{address,"101",{uri,"sip","101",undefined,"192.168.0.194","5060",[],[]},[]}),

	%% Contact
	?assertEqual(epbxd_sip_header:decode("Contact"," \"101\" <sip:101@10.0.0.13:5060;transport=udp>;+sip.instance=\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\";expires=3600"),
		{address,"101",{uri,"sip","101",undefined,"10.0.0.13","5060",[{"transport","udp"}],[]},[{"+sip.instance","\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\""},{"expires","3600"}]}),

	%% CSeq
	?assertEqual(epbxd_sip_header:decode("Cseq", "100 INVITE"), {100, "INVITE"}),
	?assertEqual(epbxd_sip_header:decode("Cseq", "fail")      , invalid),
	?assertEqual(epbxd_sip_header:decode("Cseq", "zaz INVITE"), invalid),

	%% Max-Forward
	?assertEqual(epbxd_sip_header:decode("Max-forwards", "70")    , 70),
	?assertEqual(epbxd_sip_header:decode("Max-forwards", "foobar"), invalid),

	%% Content-Length
	?assertEqual(epbxd_sip_header:decode("Content-length", "1547")  , 1547),
	?assertEqual(epbxd_sip_header:decode("Content-length", "foobar"), invalid),

	ok.


decode_via_test() ->
	?assertEqual(epbxd_sip_header:decode("Via","SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKchhwgyha"),
		{via,udp,"192.168.0.187",5069,[{"branch","z9hG4bKchhwgyha"}]}),
	%% _rport_ param have no associated value
	?assertEqual(epbxd_sip_header:decode("Via","SIP/2.0/UDP 192.168.0.187:5069;rport;branch=z9hG4bKchhwgyha"),
		{via,udp,"192.168.0.187",5069,[{"rport",undefined},{"branch","z9hG4bKchhwgyha"}]}),
	?assertEqual(epbxd_sip_header:decode("Via","SIP/2.0/UDP 192.168.0.187"),
		{via,udp,"192.168.0.187",undefined,[]}),
	ok.


%% HEADERS ENCODING

encode_callid_test() ->
	?assertEqual(epbxd_sip_header:encode("Call-id","ejpkbwsvnzotlba@bour.cc"),
		"Call-ID: ejpkbwsvnzotlba@bour.cc").

encode_cseq_test()    ->
	?assertEqual(epbxd_sip_header:encode("Cseq",{100,"INVITE"}), "CSeq: 100 INVITE").

encode_useragent_test() ->
	?assertEqual(epbxd_sip_header:encode("User-agent", "Epbxd"), "User-Agent: Epbxd").

encode_from_test() ->
	?assertEqual(
		epbxd_sip_header:encode("From",
				#sip_address{displayname="101",uri=#sip_uri{scheme="sip",user="bob",host="biloxi.com"}}),
		"From: \"101\" <sip:bob@biloxi.com>"
	),

	ok.

encode_to_test() ->
	?assertEqual(
		epbxd_sip_header:encode("To",
				#sip_address{displayname="101",uri=#sip_uri{scheme="sip",user="bob",host="biloxi.com"},
					params=[{"tag","15er75d2"}]}),
		"To: \"101\" <sip:bob@biloxi.com>;tag=15er75d2"
	),

	ok.

encode_contact_test() ->
	?assertEqual(
		epbxd_sip_header:encode("Contact",
			#sip_address{displayname=undefined,uri=#sip_uri{scheme="sips",user="101",host="192.168.0.10",
					port=445,params=[{"transport","udp"}]},
					params=[{"expires","3600"}]}),
		"Contact: <sips:101@192.168.0.10:445;transport=udp>;expires=3600"
	),

	ok.

encode_via_test() ->
	?assertEqual(epbxd_sip_header:encode("Via",	#sip_via{transport=udp,host="192.168.0.187",port=5069,
				params=[{branch,"z9hG4bKchhwgyha"}]}),
		"Via: SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKchhwgyha"
	),
	?assertEqual(epbxd_sip_header:encode("Via",	#sip_via{transport=udp,host="192.168.0.187"}),
		"Via: SIP/2.0/UDP 192.168.0.187"
	),
	ok.

encode_content_length_test() ->
	?assertEqual(epbxd_sip_header:encode("Content-length", 125), "Content-Length: 125"),
	ok.

encode_content_type_test() ->
	?assertEqual(epbxd_sip_header:encode("Content-type", "raw"), "Content-Type: raw"),
	ok.

encode_max_forwards_test() ->
	?assertEqual(epbxd_sip_header:encode("Max-forwards", 70), "Max-Forwards: 70"),
	ok.

encode_default_string_test() ->
	?assertEqual(epbxd_sip_header:encode("Foo","Bar"), "Foo: Bar").

encode_default_integer_test() ->
	?assertEqual(epbxd_sip_header:encode("Foo",123), "Foo: 123").

encode_fail_test()    ->
	?assertEqual(epbxd_sip_header:encode("Foo", false), invalid).


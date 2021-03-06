
%
% WARNING: results of functions are not valid in a GLOBAL use, because of *MOCK* use
%					 for sub-functions calls
%

-module(epbxd_sip_header_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

bin(IOList) ->
	erlang:iolist_to_binary(IOList).

test_normalize_header() ->
	[
		?_assertEqual('From'    , epbxd_sip_header:normalize('From')),
		?_assertEqual('From'    , epbxd_sip_header:normalize('f')),
		?_assertEqual('X-Custom', epbxd_sip_header:normalize('X-Custom')),

		?_assertEqual('From'    , epbxd_sip_header:normalize(<<"From">>)),
		?_assertEqual('From'    , epbxd_sip_header:normalize("From")),
		?_assertEqual('From'    , epbxd_sip_header:normalize(<<"f">>)),
		?_assertEqual('From'    , epbxd_sip_header:normalize("f"))
	].

test_decode_from_header() ->
	[
		?_assertEqual({'From', #sip_address{displayname=[], uri="uri", params=[]}},
			epbxd_sip_header:decode(<<"From">>, <<"uri">>)),
		?_assertEqual({'From', invalid},
			epbxd_sip_header:decode(<<"From">>, <<"foobar">>)),
		?_assertEqual({'From', invalid},
			epbxd_sip_header:decode(<<"From">>, <<"displayname uri">>))
	].

test_encode_from_header() ->
	[
		?_assertEqual(<<"From: <uri>">>,
				bin(epbxd_sip_header:encode('From', #sip_address{uri="uri"}))),
		?_assertEqual(<<"From: \"jdoe\" <uri>">>,
				bin(epbxd_sip_header:encode('From', #sip_address{displayname="jdoe", uri="uri"}))),
		?_assertEqual(<<"From: <uri>;tag=e58bf">>,
				bin(epbxd_sip_header:encode('From', 
						#sip_address{uri="uri", params="tag=e58bf"}))
		),
		?_assertEqual(<<"From: \"jdoe\" <uri>;tag=e58bf">>,
				bin(epbxd_sip_header:encode('From', 
						#sip_address{displayname="jdoe", uri="uri", params="tag=e58bf"}))
		)
	].

test_decode_to_header() ->
	[
		?_assertEqual({'To', #sip_address{displayname=[], uri="uri", params=[]}},
			epbxd_sip_header:decode(<<"To">>, <<"uri">>)),
		?_assertEqual({'To', invalid},
			epbxd_sip_header:decode(<<"To">>, <<"displayname uri">>))
	].

test_encode_to_header() ->
	[
		?_assertEqual(<<"To: <uri>">>,
				bin(epbxd_sip_header:encode('To', #sip_address{uri="uri"}))),
		?_assertEqual(<<"To: \"jdoe\" <uri>">>,
				bin(epbxd_sip_header:encode('To', #sip_address{displayname="jdoe", uri="uri"}))),
		?_assertEqual(<<"To: <uri>;tag=e58bf">>,
				bin(epbxd_sip_header:encode('To', 
						#sip_address{uri="uri", params="tag=e58bf"}))
		),
		?_assertEqual(<<"To: \"jdoe\" <uri>;tag=e58bf">>,
				bin(epbxd_sip_header:encode('To', 
						#sip_address{displayname="jdoe", uri="uri", params="tag=e58bf"}))
		)
	].

test_decode_contact_header() ->
	[
		?_assertEqual({'Contact', #sip_address{displayname=[], uri="uri", params=[]}},
			epbxd_sip_header:decode(<<"Contact">>, <<"uri">>)),
		?_assertEqual({'Contact', invalid},
			epbxd_sip_header:decode(<<"Contact">>, <<"displayname uri">>))
	].

test_encode_contact_header() ->
	[
		?_assertEqual(<<"Contact: <uri>">>,
				bin(epbxd_sip_header:encode('Contact', #sip_address{uri="uri"}))),
		?_assertEqual(<<"Contact: \"jdoe\" <uri>">>,
				bin(epbxd_sip_header:encode('Contact', #sip_address{displayname="jdoe", uri="uri"}))),
		?_assertEqual(<<"Contact: <uri>;tag=e58bf">>,
				bin(epbxd_sip_header:encode('Contact', 
						#sip_address{uri="uri", params="tag=e58bf"}))
		),
		?_assertEqual(<<"Contact: \"jdoe\" <uri>;tag=e58bf">>,
				bin(epbxd_sip_header:encode('Contact', 
						#sip_address{displayname="jdoe", uri="uri", params="tag=e58bf"}))
		)
	].

test_decode_content_length_header() ->
	[
		?_assertEqual({'Content-Length', 754},
			epbxd_sip_header:decode(<<"Content-Length">>, <<"754">>)),
		?_assertEqual({'Content-Length', invalid},
			epbxd_sip_header:decode(<<"Content-Length">>, <<"foobar">>))
	].

test_encode_content_length_header() ->
	[
		?_assertEqual(<<"Content-Length: 42">>,
			bin(epbxd_sip_header:encode('Content-Length', 42))
		)
	].

test_decode_cseq_header() ->
	[
		?_assertEqual({'CSeq', {100, <<"INVITE">>}},
			epbxd_sip_header:decode(<<"CSeq">>, <<"100 INVITE">>)),
		?_assertEqual({'CSeq', invalid},
			epbxd_sip_header:decode(<<"CSeq">>, <<"foobar INVITE">>)),
		?_assertEqual({'CSeq', invalid},
			epbxd_sip_header:decode(<<"CSeq">>, <<"foobar">>))
	].

test_encode_cseq_header() ->
	[
		?_assertEqual(<<"CSeq: 100 INVITE">>,
			bin(epbxd_sip_header:encode('CSeq', {100, "INVITE"}))
		),
		?_assertEqual(<<"CSeq: 100 INVITE">>,
			bin(epbxd_sip_header:encode('CSeq', {100, <<"INVITE">>}))
		)
	].

test_decode_max_forwards_header() ->
	[
		?_assertEqual({'Max-Forwards', 77},
			epbxd_sip_header:decode(<<"Max-Forwards">>, <<"77">>)),
		?_assertEqual({'Max-Forwards', invalid},
			epbxd_sip_header:decode(<<"Max-Forwards">>, <<"foobar">>))
	].

test_encode_max_forwards_header() ->
	[
		?_assertEqual(<<"Max-Forwards: 70">>,
			bin(epbxd_sip_header:encode('Max-Forwards', 70))
		)
	].

test_decode_via_header() ->
	[
		?_assertEqual({'Via',	#sip_via{transport=tls,host="host",port=9999,params=";params"}},
			epbxd_sip_header:decode(<<"Via">>, <<"SIP/2.0/TLS host:9999;params">>)),
		?_assertEqual({'Via',	#sip_via{transport=invalid,host="host",port=undefined,params=[]}},
			epbxd_sip_header:decode(<<"Via">>, <<"SIP/2.0/PLOP host">>)),
		?_assertEqual({'Via', invalid},
			epbxd_sip_header:decode(<<"Via">>, <<"foobar">>))
	].

test_encode_via_header() ->
	[
		?_assertEqual(<<"Via: SIP/2.0/TLS example.com">>,
			bin(epbxd_sip_header:encode('Via', #sip_via{transport=tls,host="example.com"}))
		),
		?_assertEqual(<<"Via: SIP/2.0/TLS example.com:666">>,
			bin(epbxd_sip_header:encode('Via', #sip_via{transport=tls,host="example.com",port=666}))
		),
		?_assertEqual(<<"Via: SIP/2.0/TLS example.com;received=10.0.0.10">>,
			bin(epbxd_sip_header:encode('Via',
					#sip_via{transport=tls,host="example.com",params="received=10.0.0.10"}))
		),
		?_assertEqual(<<"Via: SIP/2.0/TLS example.com:666;received=10.0.0.10">>,
			bin(epbxd_sip_header:encode('Via',
					#sip_via{transport=tls,host="example.com",port=666,params="received=10.0.0.10"}))
		),
		?_assertEqual(
			<<
				"Via: SIP/2.0/TLS example.com;received=10.0.0.10\r\n",
				"Via: SIP/2.0/UDP gotohell.org:666"
			>>,
			bin(epbxd_sip_header:encode('Via', [
				#sip_via{transport=tls,host="example.com",params="received=10.0.0.10"},
				#sip_via{transport=udp,host="gotohell.org",port=666}
			]))
		)
	].

test_decode_custom_header() ->
	% any custom header is left as-is
	[
		?_assertEqual({'Custom', <<"foobar">>},
			epbxd_sip_header:decode(<<"Custom">>, <<"foobar">>))
	].

test_encode_custom_header() ->
	[
		?_assertEqual(<<"X-Custom: 42">>,
			bin(epbxd_sip_header:encode('X-Custom', 42))
		),
		?_assertEqual(<<"X-Custom: 42">>,
			bin(epbxd_sip_header:encode('X-Custom', "42"))
		),
		?_assertEqual(<<"X-Custom: 42">>,
			bin(epbxd_sip_header:encode('X-Custom', '42'))
		),
		?_assertEqual(<<"X-Custom: 42">>,
			bin(epbxd_sip_header:encode('X-Custom', <<"42">>))
		),
		?_assertEqual(<<"X-Custom: 1\r\nX-Custom: 2\r\nX-Custom: 3">>,
			bin(epbxd_sip_header:encode('X-Custom', [1,2,3]))
		)
	].

test_tag_generation() ->
	% a tag is a 16 chars len hexadecimal string
	[
		?_assertEqual(16, length(epbxd_sip_header:tag()))
	].

test_merge_decode_multi_header() ->
	[
		?_assertEqual(
			{'Via',	[#sip_via{transport=invalid,host="host",port=undefined,params=[]}]},
			epbxd_sip_header:decode('Via', <<"SIP/2.0/PLOP host">>, undefined)
		),
		?_assertEqual(
			{'Via',	[first, #sip_via{transport=invalid,host="host",port=undefined,params=[]}]},
			epbxd_sip_header:decode('Via', <<"SIP/2.0/PLOP host">>, [first])
		)
	].

test_merge_decode_single_header() ->
	[
		?_assertEqual({'Max-Forwards', 77},
			epbxd_sip_header:decode('Max-Forwards', <<"77">>, undefined)
		),
		?_assertEqual({'Max-Forwards', 77},
			epbxd_sip_header:decode('Max-Forwards', <<"77">>, [first])
		)
	].

test_merge_decode_custom_header() ->
	[
		?_assertEqual({'X-Custom', <<"Zzz">>},
			epbxd_sip_header:decode('X-Custom', <<"Zzz">>, undefined)
		),
		?_assertEqual({'X-Custom', [<<"first">>, <<"Zzz">>]},
			epbxd_sip_header:decode('X-Custom', <<"Zzz">>, <<"first">>)
		),
		?_assertEqual({'X-Custom', [first, <<"Zzz">>]},
			epbxd_sip_header:decode('X-Custom', <<"Zzz">>, [first])
		)
	].


headers_test_() ->
	{setup, local,
		% init
		fun() ->
			meck:new(epbxd_sip_uri),
			meck:expect(epbxd_sip_uri, decode, fun(V) ->
				if V =:= "foobar" -> invalid;
					 true           -> V
				end
			end),
			meck:expect(epbxd_sip_uri, params, fun(U) -> U end),

			meck:expect(epbxd_sip_uri, encode, fun(V) -> V end),
			%meck:expect(epbxd_sip_uri, encode, fun(params, []) -> "" end),
			meck:expect(epbxd_sip_uri, encode, fun(params, P) -> 
					if length(P) =:= 0 -> "";
						 true            -> ";"++P 
					end
			end)
		end,
		% cleanup
		fun(_) ->
			meck:unload(epbxd_sip_uri)
		end,
		% funs
		fun(_) ->
			[
				 {"normalizing headers"              , test_normalize_header()}
				,{"decoding 'From' header"           , test_decode_from_header()}
				,{"decoding 'To' header"             , test_decode_to_header()}
				,{"decoding 'Contact' header"        , test_decode_contact_header()}
				,{"decoding 'Content-Length' header" , test_decode_content_length_header()}
				,{"decoding 'CSeq' header"           , test_decode_cseq_header()}
				,{"decoding 'Max-Forwards' header"   , test_decode_max_forwards_header()}
				,{"decoding 'Via' header"            , test_decode_via_header()}
				,{"decoding 'Custom' header"         , test_decode_custom_header()}

				,{"encoding 'From' header"           , test_encode_from_header()}
				,{"encoding 'To' header"             , test_encode_to_header()}
				,{"encoding 'Contact' header"        , test_encode_contact_header()}
				,{"encoding 'Content-Length' header" , test_encode_content_length_header()}
				,{"encoding 'CSeq' header"           , test_encode_cseq_header()}
				,{"encoding 'Max-Forwards' header"   , test_encode_max_forwards_header()}
				,{"encoding 'Via' header"            , test_encode_via_header()}
				,{"encoding 'Custom' header"         , test_encode_custom_header()}

				,{"*tag* generation"                 , test_tag_generation()}

				,{"merge/decode multi  headers"      , test_merge_decode_multi_header()}
				,{"merge/decode single headers"      , test_merge_decode_single_header()}
				,{"merge/decode custom headers"      , test_merge_decode_custom_header()}
			]
		end
	}.

%decode_test() ->
%	%% From
%	?assertEqual(epbxd_sip_header:decode("From", "foobar")                 , invalid),
%	?assertEqual(epbxd_sip_header:decode("From", "<sip:user@host")         , invalid),
%	?assertEqual(epbxd_sip_header:decode("From", "sip:user@host>")         , invalid),
%	?assertEqual(epbxd_sip_header:decode("From", "\"Doe\" sip:user@host")  , invalid),
%	?assertEqual(epbxd_sip_header:decode("From", "\"Doe\" <sip:user@host") , invalid),
%	?assertEqual(epbxd_sip_header:decode("From", "\"Doe\" sip:user@host>") , invalid),
%
%
%	?assertEqual(epbxd_sip_header:decode("From"," \"101\" <sip:101@192.168.0.194:5060> ;tag=e2dafe4bb3"),
%		{address,"101",{uri,"sip","101",undefined,"192.168.0.194","5060",[],[]},[{"tag","e2dafe4bb3"}]}),
%	?assertEqual(epbxd_sip_header:decode("From"," <sip:101@192.168.0.194>"),
%		{address,[],{uri,"sip","101",undefined,"192.168.0.194",[],[],[]},[]}),
%
%	?assertEqual(epbxd_sip_header:decode("From", "sip:101@192.168.0.1"),
%		{address,[],{uri,"sip","101",undefined,"192.168.0.1",[],[],[]},[]}),
%	?assertEqual(epbxd_sip_header:decode("From", "sip:101@192.168.0.1;transport=udp"),
%		{address,[],{uri,"sip","101",undefined,"192.168.0.1",[],[{"transport","udp"}],[]},[]}),
%
%	%% To
%	?assertEqual(epbxd_sip_header:decode("To"," \"101\" <sip:101@192.168.0.194:5060>"),
%		{address,"101",{uri,"sip","101",undefined,"192.168.0.194","5060",[],[]},[]}),
%
%	%% Contact
%	?assertEqual(epbxd_sip_header:decode("Contact"," \"101\" <sip:101@10.0.0.13:5060;transport=udp>;+sip.instance=\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\";expires=3600"),
%		{address,"101",{uri,"sip","101",undefined,"10.0.0.13","5060",[{"transport","udp"}],[]},[{"+sip.instance","\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\""},{"expires","3600"}]}),
%
%	%% CSeq
%	?assertEqual(epbxd_sip_header:decode("Cseq", "100 INVITE"), {100, "INVITE"}),
%	?assertEqual(epbxd_sip_header:decode("Cseq", "fail")      , invalid),
%	?assertEqual(epbxd_sip_header:decode("Cseq", "zaz INVITE"), invalid),
%
%	%% Max-Forward
%	?assertEqual(epbxd_sip_header:decode("Max-forwards", "70")    , 70),
%	?assertEqual(epbxd_sip_header:decode("Max-forwards", "foobar"), invalid),
%
%	%% Content-Length
%	?assertEqual(epbxd_sip_header:decode("Content-length", "1547")  , 1547),
%	?assertEqual(epbxd_sip_header:decode("Content-length", "foobar"), invalid),
%
%	ok.
%
%
%decode_via_test() ->
%	?assertEqual(epbxd_sip_header:decode("Via","SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKchhwgyha"),
%		{via,udp,"192.168.0.187",5069,[{"branch","z9hG4bKchhwgyha"}]}),
%	%% _rport_ param have no associated value
%	?assertEqual(epbxd_sip_header:decode("Via","SIP/2.0/UDP 192.168.0.187:5069;rport;branch=z9hG4bKchhwgyha"),
%		{via,udp,"192.168.0.187",5069,[{"rport",undefined},{"branch","z9hG4bKchhwgyha"}]}),
%	?assertEqual(epbxd_sip_header:decode("Via","SIP/2.0/UDP 192.168.0.187"),
%		{via,udp,"192.168.0.187",undefined,[]}),
%	ok.
%
%
%%% HEADERS ENCODING
%
%encode_callid_test() ->
%	?assertEqual(epbxd_sip_header:encode("Call-id","ejpkbwsvnzotlba@bour.cc"),
%		"Call-ID: ejpkbwsvnzotlba@bour.cc").
%
%encode_cseq_test()    ->
%	?assertEqual(epbxd_sip_header:encode("Cseq",{100,"INVITE"}), "CSeq: 100 INVITE").
%
%encode_useragent_test() ->
%	?assertEqual(epbxd_sip_header:encode("User-agent", "Epbxd"), "User-Agent: Epbxd").
%
%encode_from_test() ->
%	?assertEqual(
%		epbxd_sip_header:encode("From",
%				#sip_address{displayname="101",uri=#sip_uri{scheme="sip",user="bob",host="biloxi.com"}}),
%		"From: \"101\" <sip:bob@biloxi.com>"
%	),
%
%	ok.
%
%encode_to_test() ->
%	?assertEqual(
%		epbxd_sip_header:encode("To",
%				#sip_address{displayname="101",uri=#sip_uri{scheme="sip",user="bob",host="biloxi.com"},
%					params=[{"tag","15er75d2"}]}),
%		"To: \"101\" <sip:bob@biloxi.com>;tag=15er75d2"
%	),
%
%	ok.
%
%encode_contact_test() ->
%	?assertEqual(
%		epbxd_sip_header:encode("Contact",
%			#sip_address{displayname=undefined,uri=#sip_uri{scheme="sips",user="101",host="192.168.0.10",
%					port=445,params=[{"transport","udp"}]},
%					params=[{"expires","3600"}]}),
%		"Contact: <sips:101@192.168.0.10:445;transport=udp>;expires=3600"
%	),
%
%	ok.
%
%encode_via_test() ->
%	?assertEqual(epbxd_sip_header:encode("Via",	#sip_via{transport=udp,host="192.168.0.187",port=5069,
%				params=[{branch,"z9hG4bKchhwgyha"}]}),
%		"Via: SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKchhwgyha"
%	),
%	?assertEqual(epbxd_sip_header:encode("Via",	#sip_via{transport=udp,host="192.168.0.187"}),
%		"Via: SIP/2.0/UDP 192.168.0.187"
%	),
%	ok.
%
%encode_content_length_test() ->
%	?assertEqual(epbxd_sip_header:encode("Content-length", 125), "Content-Length: 125"),
%	ok.
%
%encode_content_type_test() ->
%	?assertEqual(epbxd_sip_header:encode("Content-type", "raw"), "Content-Type: raw"),
%	ok.
%
%encode_max_forwards_test() ->
%	?assertEqual(epbxd_sip_header:encode("Max-forwards", 70), "Max-Forwards: 70"),
%	ok.
%
%encode_default_string_test() ->
%	?assertEqual(epbxd_sip_header:encode("Foo","Bar"), "Foo: Bar").
%
%encode_default_integer_test() ->
%	?assertEqual(epbxd_sip_header:encode("Foo",123), "Foo: 123").
%
%encode_fail_test()    ->
%	?assertEqual(epbxd_sip_header:encode("Foo", false), invalid).
%


-module(uri_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/sips.hrl").

params_test() ->
	?assertEqual([{"transport","udp"}], uri:params(";transport=udp")),
	?assertEqual([], uri:params("")),
	?assertEqual([], uri:params("transport=udp")).

headers_test() ->
	?assertEqual(uri:headers("?a=1&b=2"), [{"a","1"},{"b","2"}]),
	?assertEqual(uri:headers("a=1&b=2") , []),
	?assertEqual(uri:headers("")        , []).

decode_test() ->
	% not an uri
	?assertEqual(uri:decode("foobar"), invalid),

	% Request-URI (no user)
	?assertEqual(uri:decode("sip:biloxi.com"), 
			{uri, "sip",undefined,undefined,"biloxi.com",[],[],[]}),

	?assertEqual(uri:decode("sip:bob@biloxi.com"), 
			{uri, "sip","bob",undefined,"biloxi.com",[],[],[]}),
	?assertEqual(uri:decode("sip:bob:pwd@biloxi.com"), 
			{uri, "sip","bob","pwd","biloxi.com",[],[],[]}),
	?assertEqual(uri:decode("sip:bob@biloxi.com:42"), 
			{uri, "sip","bob",undefined,"biloxi.com","42",[],[]}),
	?assertEqual(uri:decode("sip:bob@biloxi.com;transport=tcp"), 
			{uri, "sip","bob",undefined,"biloxi.com",[],[{"transport","tcp"}],[]}),
	?assertEqual(uri:decode("sip:bob@biloxi.com?a=1"), 
			{uri, "sip","bob",undefined,"biloxi.com",[],[],[{"a","1"}]}),
	?assertEqual(uri:decode("sip:bob:goose@biloxi.com:77;user=phone?callback=foobar"), 
			{uri,"sip","bob","goose","biloxi.com","77",[{"user","phone"}],[{"callback","foobar"}]}).

encode_test() ->
	% Request-URI (no user)
	?assertEqual(uri:encode(#uri{scheme="sip",host="biloxi.com"}),
		"sip:biloxi.com"),

	?assertEqual(uri:encode(#uri{scheme="sip",user="bob",host="biloxi.com"}),
		"sip:bob@biloxi.com"),
	?assertEqual(uri:encode(#uri{scheme="sip",user="bob",password="blowf1sh",host="biloxi.com"}),
		"sip:bob:blowf1sh@biloxi.com"),
	?assertEqual(uri:encode(#uri{scheme="sip",user="bob",host="biloxi.com",port=789}),
		"sip:bob@biloxi.com:789"),
	?assertEqual(uri:encode(#uri{scheme="sip",user="bob",host="biloxi.com",port="789"}),
		"sip:bob@biloxi.com:789"),
	?assertEqual(uri:encode(#uri{scheme="sip",user="bob",host="biloxi.com",
				params=[{"transport","tcp"}]}),
		"sip:bob@biloxi.com;transport=tcp"),
	?assertEqual(uri:encode(#uri{scheme="sip",user="bob",host="biloxi.com",
				headers=[{"ref","http://foo.bar"}]}),
		"sip:bob@biloxi.com?ref=http://foo.bar"),

	%% complete URI
	?assertEqual(uri:encode(#uri{scheme="sip",user="bob",host="biloxi.com",port=442,
				params=[{"transport","tcp"},{"mac","01:e2:f1:11:4a:10"}],
				headers=[{"ref","http://foo.bar"},{"date","2012/05/21"}]}),
		"sip:bob@biloxi.com:442;transport=tcp;mac=01:e2:f1:11:4a:10?ref=http://foo.bar&date=2012/05/21"),

	ok.
	


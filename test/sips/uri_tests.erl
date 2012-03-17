
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
	?assertEqual(uri:decode("sip:bob@biloxi.com"), 
			{uri, "sip","bob",[],"biloxi.com",[],[],[]}),
	?assertEqual(uri:decode("sip:bob:pwd@biloxi.com"), 
			{uri, "sip","bob","pwd","biloxi.com",[],[],[]}),
	?assertEqual(uri:decode("sip:bob@biloxi.com:42"), 
			{uri, "sip","bob",[],"biloxi.com","42",[],[]}),
	?assertEqual(uri:decode("sip:bob@biloxi.com;transport=tcp"), 
		{uri, "sip","bob",[],"biloxi.com",[],[{"transport","tcp"}],[]}),
	?assertEqual(uri:decode("sip:bob@biloxi.com?a=1"), 
			{uri, "sip","bob",[],"biloxi.com",[],[],[{"a","1"}]}),
	?assertEqual(uri:decode("sip:bob:goose@biloxi.com:77;user=phone?callback=foobar"), 
				{uri,
					"sip","bob","goose","biloxi.com","77",[{"user","phone"}],[{"callback","foobar"}]}).

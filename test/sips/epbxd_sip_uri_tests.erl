
-module(epbxd_sip_uri_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

%%
%% DECODING
%%

params_test() ->
	?assertEqual([{"transport","udp"}] , epbxd_sip_uri:params(";transport=udp")),
	?assertEqual([{"rport", undefined}], epbxd_sip_uri:params(";rport")),
	?assertEqual([{"transport","udp"},{"rport", undefined}], epbxd_sip_uri:params(";transport=udp;rport")),
	?assertEqual([]                    , epbxd_sip_uri:params("")),
	?assertEqual([]                    , epbxd_sip_uri:params("transport=udp")).

headers_test() ->
	?assertEqual([{"a","1"}, {"b","2"}], epbxd_sip_uri:headers("?a=1&b=2")),
	?assertEqual([]                    , epbxd_sip_uri:headers("a=1&b=2")),
	?assertEqual([]                    , epbxd_sip_uri:headers("")).

decode_test() ->
	% not an uri
	?assertEqual(invalid, epbxd_sip_uri:decode("foobar")),

	% Request-URI (no user)
	% also check default values
	?assertEqual(
		#sip_uri{scheme="sip",user=undefined,password=undefined,host="biloxi.com",port=undefined,params=[],headers=[]},
		epbxd_sip_uri:decode("sip:biloxi.com")),

	?assertEqual(
		#sip_uri{scheme="sip",user="bob",host="biloxi.com"},
		epbxd_sip_uri:decode("sip:bob@biloxi.com")),
	?assertEqual(
		#sip_uri{scheme="sips",user="bob",password="pwd",host="biloxi.com"},
		epbxd_sip_uri:decode("sips:bob:pwd@biloxi.com")),
	?assertEqual(
		#sip_uri{scheme="sip",user="bob",host="biloxi.com",port="42"},
		epbxd_sip_uri:decode("sip:bob@biloxi.com:42")),
	?assertEqual(
		#sip_uri{scheme="sip",user="bob",host="biloxi.com",params=[{"transport","tcp"}]},
		epbxd_sip_uri:decode("sip:bob@biloxi.com;transport=tcp")),
	?assertEqual(
		#sip_uri{scheme="sip",user="bob",host="biloxi.com",headers=[{"a","1"}]},
		epbxd_sip_uri:decode("sip:bob@biloxi.com?a=1")),
	?assertEqual(
		#sip_uri{scheme="sip",user="bob",password="goose",host="biloxi.com",port="77",params=[{"user","phone"}],
				headers=[{"callback","foobar"}]},
			epbxd_sip_uri:decode("sip:bob:goose@biloxi.com:77;user=phone?callback=foobar")).


%%
%% ENCODING
%%

flat(Bs) ->
	erlang:list_to_bitstring(Bs).

userinfo_test() ->
	?assertEqual(""          , lists:flatten(epbxd_sip_uri:userinfo(undefined, undefined))),
	?assertEqual("jdoe@"     , lists:flatten(epbxd_sip_uri:userinfo("jdoe", undefined))),
	?assertEqual("jdoe:abcd@", lists:flatten(epbxd_sip_uri:userinfo("jdoe", "abcd"))).

port_test() ->
	?assertEqual(""     , lists:flatten(epbxd_sip_uri:port(undefined))),
	?assertEqual(":5060", lists:flatten(epbxd_sip_uri:port(5060))),
	?assertEqual(":5060", lists:flatten(epbxd_sip_uri:port("5060"))).

params_encode_test() ->
	?assertEqual(""        , lists:flatten(epbxd_sip_uri:params(encode, []))),
	?assertEqual(";transport=udp", 
		lists:flatten(epbxd_sip_uri:params(encode, [{"transport","udp"}]))),
	?assertEqual(";a=1;b=2", lists:flatten(epbxd_sip_uri:params(encode, [{"a","1"},{"b","2"}]))),
	?assertEqual(";rport;transport=udp", 
		lists:flatten(epbxd_sip_uri:params(encode, [{"rport",undefined},{"transport","udp"}]))),
	?assertEqual(";transport=udp;rport", 
		lists:flatten(epbxd_sip_uri:params(encode, [{"transport","udp"},{"rport",undefined}]))).

headers_encode_test() ->
	?assertEqual(""        , lists:flatten(epbxd_sip_uri:headers(encode,[]))),
	?assertEqual("?a=1"    , lists:flatten(epbxd_sip_uri:headers(encode,[{"a","1"}]))),
	?assertEqual("?a=1&b=2", lists:flatten(epbxd_sip_uri:headers(encode,[{"a","1"},{"b","2"}]))).

encode_test() ->
	% no user
	?assertEqual(<<"sip:biloxi.com">>     , flat(epbxd_sip_uri:encode(
		#sip_uri{scheme=sip,host="biloxi.com"}
	))),

	?assertEqual(<<"sip:bob@biloxi.com">>     , flat(epbxd_sip_uri:encode(
		#sip_uri{scheme=sip,user="bob",host="biloxi.com"}
	))),
	?assertEqual(<<"sips:bob:pwd@biloxi.com">> , flat(epbxd_sip_uri:encode(
		#sip_uri{scheme=sips,user="bob",password="pwd",host="biloxi.com"}
	))),
	?assertEqual(<<"sip:bob@biloxi.com:42">>   , flat(epbxd_sip_uri:encode(
		#sip_uri{scheme=sip,user="bob",host="biloxi.com",port=42}
	))),

	?assertEqual(<<"sip:bob@biloxi.com;transport=tcp">>, flat(epbxd_sip_uri:encode(
		#sip_uri{scheme=sip,user="bob",host="biloxi.com",params=[{"transport","tcp"}]}
	))),
	?assertEqual(<<"sip:bob@biloxi.com?a=1">>, flat(epbxd_sip_uri:encode(
		#sip_uri{scheme=sip,user="bob",host="biloxi.com",headers=[{"a","1"}]}
	))),

	% complete
	?assertEqual(<<"sip:bob:goose@biloxi.com:77;user=phone?callback=foobar">>, 
		flat(epbxd_sip_uri:encode(
			#sip_uri{scheme=sip,user="bob",password="goose",host="biloxi.com",port=77,
				params=[{"user","phone"}], headers=[{"callback","foobar"}]}
	))),

	ok.


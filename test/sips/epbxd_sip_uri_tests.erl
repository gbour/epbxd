
-module(epbxd_sip_uri_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").


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
%
%encode_test() ->
%	% Request-URI (no user)
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",host="biloxi.com"}),
%		"sip:biloxi.com"),
%
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",user="bob",host="biloxi.com"}),
%		"sip:bob@biloxi.com"),
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",user="bob",password="blowf1sh",host="biloxi.com"}),
%		"sip:bob:blowf1sh@biloxi.com"),
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",user="bob",host="biloxi.com",port=789}),
%		"sip:bob@biloxi.com:789"),
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",user="bob",host="biloxi.com",port="789"}),
%		"sip:bob@biloxi.com:789"),
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",user="bob",host="biloxi.com",
%				params=[{"transport","tcp"}]}),
%		"sip:bob@biloxi.com;transport=tcp"),
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",user="bob",host="biloxi.com",
%				headers=[{"ref","http://foo.bar"}]}),
%		"sip:bob@biloxi.com?ref=http://foo.bar"),
%
%	%% complete URI
%	?assertEqual(epbxd_sip_uri:encode(#sip_uri{scheme="sip",user="bob",host="biloxi.com",port=442,
%				params=[{"transport","tcp"},{"mac","01:e2:f1:11:4a:10"}],
%				headers=[{"ref","http://foo.bar"},{"date","2012/05/21"}]}),
%		"sip:bob@biloxi.com:442;transport=tcp;mac=01:e2:f1:11:4a:10?ref=http://foo.bar&date=2012/05/21"),
%
%	ok.
%	
%

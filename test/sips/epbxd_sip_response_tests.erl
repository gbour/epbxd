
-module(epbxd_sip_response_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

to_test_() ->
	{setup, local,
		% init
		fun() ->
			meck:new(epbxd_sip_header),
			meck:expect(epbxd_sip_header, tag, fun() -> "c12a42" end),

			#sip_address{displayname="jdoe", uri="jdoe", params=[]}
		end,
		% cleanup
		fun(_) ->
			meck:unload(epbxd_sip_header)
		end,
		% tests
		fun(To) ->
			[
				{"100/Trying response do not add *tag* param for *To* header",
					?_assertEqual(To, epbxd_sip_response:to(To, 100))
				},
				{"Other responses add a tag param",
					?_assertEqual(To#sip_address{params=[{"tag","c12a42"}]}, 
						epbxd_sip_response:to(To, 180))
				},
				{"Unless *tag* already set",
					?_assertEqual(To#sip_address{params=[{"tag","d22b19"}]}, 
						epbxd_sip_response:to(To#sip_address{params=[{"tag", "d22b19"}]}, 180))
				}
			]
		end
	}.

response_test_() ->
	{setup, local,
		% init
		fun() ->
			meck:new(epbxd_sip_header),
			meck:expect(epbxd_sip_header, tag, fun() -> "c12a42" end),

			#sip_message{
				type    = request,
				method  = 'INVITE',
				uri     = "sip:101@192.168.0.194",
				headers = [
					{'Via'           , ["via1"]},
					{'Max-Forwards'  , 70},
					{'From'          , "from"},
					{'To'            , #sip_address{displayname="jdoe", uri="jdoe"}},
					{'Call-ID'       , "38cb69e37a1ce0c3"},
					{'CSeq'          , "3298 INVITE"},
					{'User-Agent'    , "Twinkle/1.4.2"},
					{'Content-Type'  , "application/sdp"},
					{'Content-Length', 619}
				]
			}
		end,
		% cleanup
		fun(_) ->
			meck:unload(epbxd_sip_header)
		end,
		% tests
		fun(M) ->
			[
				{"generating 100/Trying response",
					?_assertEqual(
						#sip_message{
							type    = response,
							status  = 100,
							reason  = "Trying",
							headers = [
								{'Via'           , ["via1"]},
								{'From'          , "from"},
								{'To'            , #sip_address{displayname="jdoe", uri="jdoe",	params=[]}},
								{'Call-ID'       , "38cb69e37a1ce0c3"},
								{'CSeq'          , "3298 INVITE"},
								{'User-Agent'    , "Epbxd"},
								{'Content-Length', 0}
							]
						},
						epbxd_sip_response:gen(trying, M)
					)
				},
				{"generating 180/Ringing response",
					?_assertEqual(
						#sip_message{
							type    = response,
							status  = 180,
							reason  = "Ringing",
							headers = [
								{'Via'           , ["via1"]},
								{'From'          , "from"},
								{'To'            , #sip_address{displayname="jdoe", uri="jdoe",
										params=[{"tag","c12a42"}]}},
								{'Call-ID'       , "38cb69e37a1ce0c3"},
								{'CSeq'          , "3298 INVITE"},
								{'User-Agent'    , "Epbxd"},
								{'Content-Length', 0}
							]
						},
						epbxd_sip_response:gen(ringing, M)
					)
				}
			]
		end
	}.

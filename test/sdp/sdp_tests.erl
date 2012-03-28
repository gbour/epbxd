
-module(sdp_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sdp/sdp.hrl").

decode_asterisk_test() ->
	% Asterisk sample
	?assertEqual(sdp:decode(binary:list_to_bin(
			"v=0\r\n"
			"o=root 19113 19113 IN IP4 10.0.0.1\r\n"
			"s=session\r\n"
			"c=IN IP4 10.0.0.1\r\n"
			"t=10 20\r\n"
			"m=audio 14730 RTP/AVP 0 8\r\n"
			"a=rtpmap:0 PCMU/8000\r\n"
			"a=rtpmap:8 PCMA/8000\r\n"
			"a=ptime:20\r\n"
			"a=sendrecv\r\n"
			"\r\n"
		)),
		{sdp_session,0,<<"session">>,{10,20},
			{sdp_origin,<<"root">>,19113,19113,'IN','IP4',<<"10.0.0.1">>},
			{sdp_connection,'IN','IP4',<<"10.0.0.1">>,0},
			[{sdp_media,audio,'RTP/AVP',14730,
				[{'PCMU',[0,8000]},{'PCMA',[8,8000]}],
				undefined,20,sendrecv,[]
			}]
		}
	),

	ok.

decode_thomson_test() ->
	% Thomson ST2030
	?assertEqual(sdp:decode(binary:list_to_bin(
			"v=0\r\n"
			"o=101 6589654 6589654 IN IP4 10.0.0.12\r\n"
			"s=-\r\n"
			"c=IN IP4 10.0.0.12\r\n"
			"t=0 0\r\n"
			"m=audio 41000 RTP/AVP 8 0 18 4\r\n"
			"a=rtpmap:8 PCMA/8000\r\n"
			"a=rtpmap:0 PCMU/8000\r\n"
			"a=rtpmap:18 G729/8000\r\n"
			"a=fmtp:18 annexb=no\r\n"
			"a=rtpmap:4 G723/8000\r\n"
			"a=sendrecv\r\n"
			"\r\n"
		)),
		{sdp_session,0,<<"-">>,{0,0},
			{sdp_origin,<<"101">>,6589654,6589654,'IN','IP4',<<"10.0.0.12">>},
			{sdp_connection,'IN','IP4',<<"10.0.0.12">>,0},
			[{sdp_media,audio,'RTP/AVP',41000,
				[{'PCMA',[8,8000]},{'PCMU',[0,8000]},{'G729',[18,8000]},{'G723',[4,8000]}],
				undefined,20,sendrecv,[]
			}]
		}
	),

	ok.

decode_aastra_test() ->
	% Aastra 6731i
	?assertEqual(sdp:decode(binary:list_to_bin(
			"v=0\r\n"
			"o=MxSIP 0 1 IN IP4 10.0.0.11\r\n"
			"s=SIP Call\r\n"
			"c=IN IP4 10.0.0.11\r\n"
			"t=0 0\r\n"
			"m=audio 3000 RTP/AVP 0 18 106 107 113 110 111 112 98 97 115 96 9 8 101\r\n"
			"a=rtpmap:0 PCMU/8000\r\n"
			"a=rtpmap:18 G729/8000\r\n"
			"a=rtpmap:106 BV16/8000\r\n"
			"a=rtpmap:107 BV32/16000\r\n"
			"a=rtpmap:113 L16/16000\r\n"
			"a=rtpmap:110 PCMU/16000\r\n"
			"a=rtpmap:111 PCMA/16000\r\n"
			"a=rtpmap:112 L16/8000\r\n"
			"a=rtpmap:98 G726-16/8000\r\n"
			"a=rtpmap:97 G726-24/8000\r\n"
			"a=rtpmap:115 G726-32/8000\r\n"
			"a=rtpmap:96 G726-40/8000\r\n"
			"a=rtpmap:9 G722/8000\r\n"
			"a=rtpmap:8 PCMA/8000\r\n"
			"a=rtpmap:101 telephone-event/8000\r\n"
			"a=silenceSupp:off - - - -\r\n"
			"a=fmtp:101 0-15\r\n"
			"a=ptime:30\r\n"
			"a=rtcp:3001 IN IP4 10.0.0.11\r\n"
			"a=sendrecv\r\n"
			"\r\n"
		)),
		{sdp_session,0,<<"SIP Call">>,{0,0},
			{sdp_origin    ,<<"MxSIP">>,0,1,'IN','IP4',<<"10.0.0.11">>},
			{sdp_connection,'IN','IP4',<<"10.0.0.11">>,0},
			[{sdp_media    ,audio,'RTP/AVP',3000,
					[
						{'PCMU'   ,[  0, 8000]}, {'G729'   ,[ 18, 8000]} ,{'BV16'           ,[106,  8000]},
						{'BV32'   ,[107,16000]}, {'L16'    ,[113,16000]} ,{'PCMU'           ,[110, 16000]},
						{'PCMA'   ,[111,16000]}, {'L16'    ,[112, 8000]} ,{'G726-16'        ,[ 98,  8000]},
						{'G726-24',[ 97, 8000]}, {'G726-32',[115, 8000]} ,{'G726-40'        ,[ 96,  8000]},
						{'G722'   ,[  9, 8000]}, {'PCMA'   ,[  8, 8000]} ,{'telephone-event',[101,  8000]}
					],
					{sdp_connection,'IN','IP4',<<"10.0.0.11">>,3001},30,sendrecv,[]
			}]
		}
	),

	ok.



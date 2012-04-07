
-module(epbxd_sip_message_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

test_decode_empty_message() ->
	% not defined
	?_assertError(function_clause, epbxd_sip_message:decode(#sip_message{}, [],	undefined)).

test_decode_invalid_headers() ->
	[
		% invalid Request/Response line
		?_assertEqual({error, invalid, undefined}, epbxd_sip_message:decode(#sip_message{},
				[<<"foobar">>], undefined)
		),
		?_assertError({badmatch, [<<"foobar">>]}, epbxd_sip_message:decode(#sip_message{},
				[<<"INVITE sip:101@192.168.0.194 SIP/2.0">>, <<"foobar">>], undefined)
		),
		?_assertError({badmatch, [<<"foobar">>]}, epbxd_sip_message:decode(#sip_message{},
				[<<"SIP/2.0 200 OK">>, <<"foobar">>], undefined)
		)
	].

test_decode_request() ->
	?_assertEqual({ok, #sip_message{
				type    = request,
				version = <<"2.0">>,
				method  = 'INVITE',
				uri     = <<"sip:101@192.168.0.194">>,
				headers = [
					{'Max-Forwards', <<"70">>},
					{'CSeq'        , <<"338 INVITE">>}
				],
				payload = undefined
		}, undefined},
		
		epbxd_sip_message:decode(#sip_message{}, 
			[
				<<"INVITE sip:101@192.168.0.194 SIP/2.0">>,
				<<"Max-Forwards: 70">>,
				<<"CSeq: 338 INVITE">>
			], undefined)
	).

test_decode_response() ->
	?_assertEqual({ok, #sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [
					{'Call-ID'   , <<"sapisdmefrjxwmp@bour.cc">>},
					{'User-Agent', <<"Epbxd">>}
				],
				payload = undefined
		}, undefined},

		epbxd_sip_message:decode(#sip_message{},
			[
				<<"SIP/2.0 200 OK">>,
				<<"Call-ID: sapisdmefrjxwmp@bour.cc">>,
				<<"User-Agent: Epbxd">>
			], undefined)
	).

test_decode_no_content_length() ->
	% content-length header not defined
	?_assertEqual({ok, #sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [],
				payload = undefined
		}, <<"blahblahblah">>},

		epbxd_sip_message:decode(#sip_message{},[<<"SIP/2.0 200 OK">>], <<"blahblahblah">>)
	).

test_decode_zero_content_length() ->
	% content-length header set to 0
	?_assertEqual({ok, #sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [
					{'Content-Length', 0}
				],
				payload = undefined
		}, <<"blahblahblah">>},

		epbxd_sip_message:decode(#sip_message{},
			[
				<<"SIP/2.0 200 OK">>,
				<<"Content-Length: 0">>
			], <<"blahblahblah">>)
	).


test_decode_positive_content_length() ->
	% content-length header > 0
	?_assertEqual({ok, #sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [
					{'Content-Length', 4}
				],
				payload = <<"blah">>
		}, <<"blahblah">>},

		epbxd_sip_message:decode(#sip_message{},
			[
				<<"SIP/2.0 200 OK">>,
				<<"Content-Length: 4">>
			], <<"blahblahblah">>)
	).

test_decode_exact_content_length() ->
	% content-length header = exact Rest length
	?_assertEqual({ok, #sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [
					{'Content-Length', 12}
				],
				payload = <<"blahblahblah">>
		}, <<>>},

		epbxd_sip_message:decode(#sip_message{},
			[
				<<"SIP/2.0 200 OK">>,
				<<"Content-Length: 12">>
			], <<"blahblahblah">>)
	).

test_decode_split()   ->
	[
		?_assertEqual(
			[{error, invalid, <<"SIP/2.0 200 OK\r\nUser-Agent: Epbxd\r\n\r">>}],
			epbxd_sip_message:decode(<<"SIP/2.0 200 OK\r\nUser-Agent: Epbxd\r\n\r">>)
		),
		?_assertEqual(
			[{ok, #sip_message{type=response, version= <<"2.0">>, status=200,	reason= <<"OK">>,
				headers=[{'User-Agent', <<"Epbxd">>}]}}],
			epbxd_sip_message:decode(<<"SIP/2.0 200 OK\r\nUser-Agent: Epbxd\r\n\r\n">>)
		),
		?_assertEqual(
			[
				{ok, #sip_message{type=response, version= <<"2.0">>, status=200,	reason= <<"OK">>,
					headers=[{'User-Agent', <<"Epbxd">>}]}},
				{error, invalid, <<"xoxo">>}
			],
			epbxd_sip_message:decode(<<"SIP/2.0 200 OK\r\nUser-Agent: Epbxd\r\n\r\nxoxo">>)
		)
	].

test_decode_multi_empty() ->
	% input binary packet is empty
	?_assertEqual([], epbxd_sip_message:decode(<<>>)).

test_decode_multi_1_message() ->
	% binary packet contains one complete SIP message

	?_assertEqual(
		[{ok, #sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [
					{'Call-ID'       , <<"sapisdmefrjxwmp@bour.cc">>},
					{'User-Agent'    , <<"Epbxd">>},
					{'Content-Length', 3}
				],
				payload = <<"v=0">>
		}}],

		epbxd_sip_message:decode(
			<<"SIP/2.0 200 OK\r\n",
				"Call-ID: sapisdmefrjxwmp@bour.cc\r\n",
				"User-Agent: Epbxd\r\n",
				"Content-Length: 3\r\n",
				"\r\n",
				"v=0">>
	)).

test_decode_multi_1_partial() ->
	[
		{"cannot found \"\\r\\n\\r\\n\" sequence (end-of-headers)",
			?_assertEqual(
				[{error, invalid, <<"SIP/2.0 200 OK\r\nCall-ID: sapisdmefrjxwmp@bour.cc\r\n">>}],
				epbxd_sip_message:decode(<<"SIP/2.0 200 OK\r\nCall-ID: sapisdmefrjxwmp@bour.cc\r\n">>)
			)
		},

		{"payload does not match Content-Length value",
			?_assertEqual(
				[{error, invalid, <<"SIP/2.0 200 OK\r\nContent-Length: 10\r\n\r\nv=0">>}],
				epbxd_sip_message:decode(
					<<"SIP/2.0 200 OK\r\n",
					"Content-Length: 10\r\n",
					"\r\n",
					"v=0">>
			))
		}
	].

test_decode_multi_x() ->
	% binary packet contains 2 complete SIP messages

	?_assertEqual(
		[{ok, #sip_message{
				type    = request,
				version = <<"2.0">>,
				method  = 'INVITE',
				uri     = <<"sip:101@192.168.0.194">>,
				headers = [
					{'To'            , <<"<sip:101@192.168.0.194>">>},
					{'Content-Length', 0}
				],
				payload = undefined
			}},
			{ok, #sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [
					{'Call-ID'       , <<"sapisdmefrjxwmp@bour.cc">>},
					{'User-Agent'    , <<"Epbxd">>},
					{'Content-Length', 3}
				],
				payload = <<"v=0">>
		}}],

		epbxd_sip_message:decode(
			<<"INVITE sip:101@192.168.0.194 SIP/2.0\r\n",
				"To: <sip:101@192.168.0.194>\r\n",
				"Content-Length: 0\r\n\r\n",

				"SIP/2.0 200 OK\r\n",
				"Call-ID: sapisdmefrjxwmp@bour.cc\r\n",
				"User-Agent: Epbxd\r\n",
				"Content-Length: 3\r\n",
				"\r\n",
				"v=0">>
	)).

test_decode_multi_x_partial() ->
	% last message of binary packet is incomplete

	?_assertEqual(
		[{ok, #sip_message{
				type    = request,
				version = <<"2.0">>,
				method  = 'INVITE',
				uri     = <<"sip:101@192.168.0.194">>,
				headers = [
					{'To'            , <<"<sip:101@192.168.0.194>">>},
					{'Content-Length', 0}
				],
				payload = undefined
			}},
			{error, invalid, <<"SIP/2.0 200 OK\r\nCall-ID: sapisdmefrjxwmp@bour.cc\r\n",
				"User-Agent: Epbxd\r\nContent-Length: 25\r\n\r\nv=0">>}
		],

		epbxd_sip_message:decode(
			<<"INVITE sip:101@192.168.0.194 SIP/2.0\r\n",
				"To: <sip:101@192.168.0.194>\r\n",
				"Content-Length: 0\r\n\r\n",

				"SIP/2.0 200 OK\r\n",
				"Call-ID: sapisdmefrjxwmp@bour.cc\r\n",
				"User-Agent: Epbxd\r\n",
				"Content-Length: 25\r\n",
				"\r\n",
				"v=0">>
	)).


decode_test_() ->
	{setup, local,
		% init
		fun() ->
			meck:new(uri),
			meck:expect(uri, decode, fun(U) -> U end),

			meck:new(epbxd_sip_header),
			meck:expect(epbxd_sip_header, decode, fun(K,V) -> 
				K2 = binary_to_atom(K, latin1),

				V2 = if
					K2 =:= 'Content-Length' -> list_to_integer(binary_to_list(V));
					true                    -> V
				end,

				{K2, V2}
			end)
		end,
		fun(_) ->
			meck:unload(uri),
			meck:unload(epbxd_sip_header)
		end,
		% tests
		fun(_) ->
			[
 				 {"decode empty message"       , test_decode_empty_message()}
				,{"decode invalid headers"     , test_decode_invalid_headers()}
				,{"decode simple request"      , test_decode_request()}
				,{"decode simple response"     , test_decode_response()}
				,{"decode no content-length"   , test_decode_no_content_length()}
				,{"decode zero content-length" , test_decode_zero_content_length()}
				,{"decode content-length > 0"  , test_decode_positive_content_length()}
				,{"decode content-length == remaining", test_decode_exact_content_length()}

				,{"decode splitting multimessages stream", test_decode_split()}
				,{"decode empty stream"       , test_decode_multi_empty()}
				,{"decode 1 complete message" , test_decode_multi_1_message()}
				,{"decode 1 partial message"  , test_decode_multi_1_partial()}
				,{"decode x messages"         , test_decode_multi_x()}
				,{"decode x partial messages" , test_decode_multi_x_partial()}
				
			]
		end
	}. 

%%
%% ENCODING
%%
test_encode_request() ->
	?_assertEqual(
		<<
			"INVITE sip:101@192.168.0.194 SIP/2.0\r\n",
			"Max-Forwards: 70\r\n",
			"CSeq: 338 INVITE\r\n",
			"\r\n"
		>>,
		
		erlang:iolist_to_binary(
			epbxd_sip_message:encode(#sip_message{
				type    = request,
				version = <<"2.0">>,
				method  = 'INVITE',
				uri     = <<"sip:101@192.168.0.194">>,
				headers = [
					{'Max-Forwards', <<"70">>},
					{'CSeq'        , <<"338 INVITE">>}
				],
				payload = undefined
			})
	)).

test_encode_response() ->
	?_assertEqual(
		<<
			"SIP/2.0 200 OK\r\n",
		  "Call-ID: sapisdmefrjxwmp@bour.cc\r\n",
			"Forwards: 70\r\n",
		  "User-Agent: Epbxd\r\n",
			"\r\n"
		>>,
	
		erlang:iolist_to_binary(
			epbxd_sip_message:encode(#sip_message{
				type    = response,
				version = <<"2.0">>,
				status  = 200,
				reason  = <<"OK">>,
				headers = [
					{'Call-ID'   , <<"sapisdmefrjxwmp@bour.cc">>},
					{'Forwards'  , 70},
					{'User-Agent', <<"Epbxd">>}
				],
				payload = undefined
			})
	)).


encode_test_() ->
	{setup, local,
		% init
		fun() ->
			meck:new(epbxd_sip_header),
			meck:expect(epbxd_sip_header, encode, fun(K, V) ->
				erlang:atom_to_list(K) ++ ": " ++ utils:str(V)
			end),

			meck:new(epbxd_sip_uri),
			meck:expect(epbxd_sip_uri, encode, fun(U) -> U end)
		end,
		% teardown
		fun(_) ->
			meck:unload(epbxd_sip_header),
			meck:unload(epbxd_sip_uri)
		end,
		% tests
		fun(_) ->
			[
				 {"encode request" , test_encode_request()}	
				,{"encode response", test_encode_response()}
			]
		end
	}.

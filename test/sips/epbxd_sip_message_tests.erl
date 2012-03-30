
-module(epbxd_sip_message_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

test_empty_message() ->
	% not defined
	?_assertError(function_clause, epbxd_sip_message:decode(#sip_message{}, [],	undefined)).

test_invalid_headers() ->
	[
		% invalid Request/Response line
		?_assertEqual({error, invalid}, epbxd_sip_message:decode(#sip_message{},
				[<<"foobar">>], undefined)
		),
		?_assertError({badmatch, [<<"foobar">>]}, epbxd_sip_message:decode(#sip_message{},
				[<<"INVITE sip:101@192.168.0.194 SIP/2.0">>, <<"foobar">>], undefined)
		),
		?_assertError({badmatch, [<<"foobar">>]}, epbxd_sip_message:decode(#sip_message{},
				[<<"SIP/2.0 200 OK">>, <<"foobar">>], undefined)
		)
	].

test_request() ->
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

test_response() ->
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

test_no_content_length() ->
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

test_zero_content_length() ->
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


test_positive_content_length() ->
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

test_exact_content_length() ->
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

test_multi_empty() ->
	% input binary packet is empty
	?_assertEqual([], epbxd_sip_message:decode(<<>>)).

test_multi_1_message() ->
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

test_multi_1_partial() ->
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

test_multi_x() ->
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

test_multi_x_partial() ->
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

%					,{"x messages"         , test_multi_x()}
%					,{"x partial messages" , test_multi_x_partial()}

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

				%io:format(user,"header: ~p:~p~n", [K2,V2]),
				{K2, V2}
			end)

		end,
		fun(Args) ->
			meck:unload(uri),
			meck:unload(epbxd_sip_header)
		end,
		% tests
		fun(Args) ->
			[
 				 {"empty message"       , test_empty_message()}
				,{"invalid headers"     , test_invalid_headers()}
				,{"simple request"      , test_request()}
				,{"simple response"     , test_response()}
				,{"no content-length"   , test_no_content_length()}
				,{"zero content-length" , test_zero_content_length()}
				,{"content-length > 0"  , test_positive_content_length()}
				,{"content-length == remaining"  , test_exact_content_length()}

				,{"empty stream"       , test_multi_empty()}
				,{"1 complete message" , test_multi_1_message()}
				,{"1 partial message"  , test_multi_1_partial()}
				,{"x messages"         , test_multi_x()}
				,{"x partial messages" , test_multi_x_partial()}
				
			]
		end
	}. 




%%
%% TODO: we SHOULD test each state transition
%%       here we only test main transaction path
%%

-module(epbxd_sip_server_noninvite_transaction_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

%% extract fsm State variable from Fsm process Pid
get_state(Pid) ->
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
	io:format(user, "~p~n", [State]),
	State.

get_state_name(Pid) ->
	{status,_,_,[_,running,_,_,[_,{data,Props},_]]} = sys:get_status(Pid),
	proplists:get_value("StateName", Props).

main_test_() ->
	{setup, local,
		% init
		fun() ->
			io:format(user, "init~n",[]),
			ets:new(test_states, [set,public,named_table]),

			meck:new(logging),
			meck:expect(logging, log, fun(A,B,C) ->
				io:format(user, "LOG: ~p~p~p~n", [A,B,C])
			end),

			meck:new(epbxd_sip_transaction),
			meck:expect(epbxd_sip_transaction, destroy, fun(_Mod, _Pid, _Trans) -> ok end),
		
			meck:new(fake_transport_udp),
			meck:expect(fake_transport_udp, name, fun() -> udp end),
			meck:new(fake_transport_tcp),
			meck:expect(fake_transport_tcp, name, fun() -> tcp end),
			meck:new(fake_transport_err),
			meck:expect(fake_transport_err, name, fun() -> udp end),

			meck:new(epbxd_sip_routing),
			meck:expect(epbxd_sip_routing, send, fun(Message, Transport, Sock) -> 
				case Transport of
					fake_transport_err -> 
						ets:insert(test_states, {response, undefined}),
						{error, "faking transport failure"};
					_                  ->
						ets:insert(test_states, {response, Message}),
						ok
				end
			end),
			meck:expect(epbxd_sip_routing, is_reliable, fun(TName) ->
				case TName of
					udp -> false;
					tcp -> true
				end
			end),

			meck:new(epbxd_sip_message),
			meck:expect(epbxd_sip_message, response_type, fun(M) -> 
				if M#sip_message.status >= 300 -> M#sip_message.status;
				   M#sip_message.status >= 200 -> final;
				   true                        -> provisional
				end
			end),

			{ok, Pid} = epbxd_sip_server_noninvite_transaction:start_link([]),
			Pid
		end,
		fun(_) ->
			timer:sleep(5000),
			ets:delete(test_states),

			meck:unload(epbxd_sip_message),
			meck:unload(epbxd_sip_routing),
			meck:unload(fake_transport_err),
			meck:unload(fake_transport_tcp),
			meck:unload(fake_transport_udp),
			meck:unload(epbxd_sip_transaction),
			meck:unload(logging),

			ok
		end,
		% tests
		fun(Pid) ->
			{inorder, [
				% initial tests
				 {"fsm process is alive"         , ?_assert(erlang:is_process_alive(Pid))}
				,{"fsm starts at *idle* state"   , ?_assertEqual(idle, get_state_name(Pid))}
				,{"fsm clean init state"         , test_clean_state(Pid, get_state(Pid))}

				,{"fsm get_transaction() fun"    , test_get_transaction(Pid)}
				% cleanup (internal)
				,{"fsm state cleanup (internal)" , test_internal_cleanup(Pid)}

				% fsm states and transitions
				,{"idle -> trying (receiving non-INVITE request)", 
					test_transition(idle, trying, nop, Pid, fake_transport_udp)}
				,{"trying -> proceeding (send provisional response)",
					test_transition(trying, proceeding, nop, Pid, fake_transport_udp)}
				,{"proceeding -> proceeding (request retransmission received)",
					test_transition(proceeding, proceeding, request, Pid, fake_transport_udp)}
				,{"proceeding -> proceeding (response retransmission)",
					test_transition(proceeding, proceeding, response, Pid, fake_transport_udp)}
				,{"proceeding -> completed (2xx response)",
					test_transition(proceeding, completed, '2xx', Pid, fake_transport_udp)}
				,{"completed -> completed (request retrans)",
					test_transition(completed, completed, request, Pid,	fake_transport_udp)}
				,{"completed -> terminated (timerJ timeout)",
					test_transition(completed, terminated, timeout, Pid, fake_transport_udp)}
			]}
		end
	}. 


test_clean_state(Pid, {state,_,_,Transaction}) -> 
	% how to make it silent ?
	{inparallel, [
 		 ?_assertEqual({undefined, undefined}, Transaction#sip_transaction.key)
		,?_assertEqual(infinity   , Transaction#sip_transaction.timerE)
		,?_assertEqual(infinity   , Transaction#sip_transaction.timerF)
		,?_assertEqual(0          , Transaction#sip_transaction.timerK)
		,?_assertEqual(undefined  , Transaction#sip_transaction.request)
		,?_assertEqual(epbxd_sip_server_noninvite_transaction, Transaction#sip_transaction.fsm)
		,?_assertEqual(Pid        , Transaction#sip_transaction.fsmid)
	]}.

test_get_transaction(Pid)  ->
	{state,_,_,T} = get_state(Pid),
	?_assertEqual(T, epbxd_sip_server_noninvite_transaction:get_transaction(Pid)).

test_internal_cleanup(Pid) ->
	State =	epbxd_sip_server_noninvite_transaction:cleanup({state,undefined,undefined,#sip_transaction{fsm=foo,fsmid=bar}}),
	test_clean_state(erlang:self(), State).


test_transition(From=idle, To=trying, _, Pid, Transport) ->
	?_test(begin
		FromState = get_state_name(Pid),

		Req   = #sip_message{type=request, method='BYE'}, 
		Reply = epbxd_sip_server_noninvite_transaction:receipt(Pid, Req, Transport, undefined),

		[
			?assertEqual(From, FromState)

			,?assertEqual(To  , get_state_name(Pid))
			,?assertEqual(ok  , Reply)
			%?assertEqual([{response,Response}], ets:lookup(test_states, response))
		]
	end);

test_transition(From=trying, To=proceeding, _, Pid, Transport) ->
	?_test(begin
		FromState = get_state_name(Pid),

		ets:insert(test_states, {response, unset}),
		Resp  = #sip_message{type=response, status=100},
		Reply = epbxd_sip_server_noninvite_transaction:send(Pid, Resp, Transport, undefined),

		[
			 ?assertEqual(From, FromState)
			,?assertEqual(To , get_state_name(Pid))
			,?assertEqual(ok , Reply)
			,?assertEqual([{response,Resp}], ets:lookup(test_states, response))
		]
	end);

test_transition(From=proceeding, To=proceeding, request, Pid, Transport) ->
	?_test(begin
		FromState = get_state_name(Pid),

		ets:insert(test_states, {response, unset}),
		Resp  = #sip_message{type=response, status=100},

		Req   = #sip_message{type=request, method='BYE'}, 
		Reply = epbxd_sip_server_noninvite_transaction:receipt(Pid, Req, Transport, undefined),

		[
			?assertEqual(From, FromState)

			,?assertEqual(To  , get_state_name(Pid))
			,?assertEqual({stop, ignore}, Reply)
			,?assertEqual([{response,Resp}], ets:lookup(test_states, response))
		]
	end);

test_transition(From=proceeding, To=proceeding, response, Pid, Transport) ->
	?_test(begin
		FromState = get_state_name(Pid),

		ets:insert(test_states, {response, unset}),
		Resp  = #sip_message{type=response, status=101},
		Reply = epbxd_sip_server_noninvite_transaction:send(Pid, Resp, Transport, undefined),

		[
			?assertEqual(From, FromState)

			,?assertEqual(To, get_state_name(Pid))
			,?assertEqual(ok, Reply)
			,?assertEqual([{response,Resp}], ets:lookup(test_states, response))
		]
	end);

test_transition(From=proceeding, To=completed, '2xx', Pid, Transport) ->
	?_test(begin
		FromState = get_state_name(Pid),

		ets:insert(test_states, {response, unset}),
		Resp  = #sip_message{type=response, status=200},
		Reply = epbxd_sip_server_noninvite_transaction:send(Pid, Resp, Transport, undefined),

		[
			?assertEqual(From, FromState)

			,?assertEqual(To, get_state_name(Pid))
			,?assertEqual(ok, Reply)
			,?assertEqual([{response,Resp}], ets:lookup(test_states, response))
		]
	end);

test_transition(From=completed, To=completed, request, Pid, Transport) ->
	?_test(begin
		FromState = get_state_name(Pid),

		ets:insert(test_states, {response, unset}),
		Resp  = #sip_message{type=response, status=200},

		Req   = #sip_message{type=request, method='BYE'}, 
		Reply = epbxd_sip_server_noninvite_transaction:receipt(Pid, Req, Transport, undefined),

		[
			?assertEqual(From, FromState)

			,?assertEqual(To  , get_state_name(Pid))
			,?assertEqual({ok, Req}, Reply)
			,?assertEqual([{response,Resp}], ets:lookup(test_states, response))
		]
	end);

% timerJ = 64*T1 = 32secs
test_transition(From=completed, To=terminated, timeout, Pid, Transport) ->
	{timeout, 35, fun() ->
		timer:sleep(1000),
		FromState = get_state_name(Pid),
		timer:sleep(33000),

		[
			?assertEqual(From, FromState)
			,?assertEqual(idle, get_state_name(Pid))
		]
	end}.




%%
%% TODO: we SHOULD test each state transition
%%       here we only test main transaction path
%%

-module(epbxd_sip_server_invite_transaction_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

%% extract fsm State variable from Fsm process Pid
get_state(Pid) ->
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
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
			meck:expect(epbxd_sip_message, response_type, fun(S) -> 
				if S >= 300 -> S;
				   S >= 200 -> final;
				   true     -> provisional
				end
			end),

			{ok, Pid} = epbxd_sip_server_invite_transaction:start_link([]),
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
				,{"transition: receiving INVITE (transport fail)", test_invite(Pid, fail)}
				% NOTE: test fail as response sent is not valid!
				,{"transition: receiving INVITE (transport udp)" , test_invite(Pid, udp)}
				% loops on same state
				,{"transition: receiving INVITE (proceeding state)"   , test_proceeding(Pid, udp, invite)}
				,{"transition: sending provisional (proceeding state)", test_proceeding(Pid, udp, provisional)}
				,{"transition: sending 3XX (proceeding state)", test_proceeding(Pid, udp, '3xx')}
				,{"completed: receive INVITE", test_completed(Pid, udp, invite)}
				,{"completed: receive ACK", test_completed(Pid, udp, ack)}
				,{"confirmed: timerI timeout", test_confirmed(Pid)}
			]}
		end
	}. 

test_clean_state(Pid, {state,_,_,undefined,Transaction}) -> 
	% how to make it silent ?
	{inparallel, [
 		 ?_assertEqual({undefined, undefined}, Transaction#sip_transaction.key)
		,?_assertEqual(infinity   , Transaction#sip_transaction.timerG)
		,?_assertEqual(infinity   , Transaction#sip_transaction.timerH)
		,?_assertEqual(0          , Transaction#sip_transaction.timerI)
		,?_assertEqual(undefined  , Transaction#sip_transaction.request)
		,?_assertEqual(epbxd_sip_server_invite_transaction  , Transaction#sip_transaction.fsm)
		,?_assertEqual(Pid        , Transaction#sip_transaction.fsmid)
	]}.

test_get_transaction(Pid)  ->
	{state,_,_,_,T} = get_state(Pid),
	?_assertEqual(T, epbxd_sip_server_invite_transaction:get_transaction(Pid)).

test_internal_cleanup(Pid) ->
	State =	epbxd_sip_server_invite_transaction:cleanup({state,0,0,undefined,#sip_transaction{fsm=foo,fsmid=bar}}),
	test_clean_state(erlang:self(), State).

test_invite(Pid, fail) ->
	generic_fsm(Pid, request, idle, fake_transport_err, {idle, {error, "faking transport failure"}, undefined}); 

test_invite(Pid, udp)  ->
	generic_fsm(Pid, request, idle, fake_transport_udp, {proceeding, ok, barfoo}).

test_proceeding(Pid, udp, invite) ->
	generic_fsm(Pid, request, proceeding, fake_transport_udp, {proceeding, {stop, retransmission}, barfoo});

test_proceeding(Pid, udp, provisional) ->
	Resp = #sip_message{type=response,status=100},
	generic_fsm(Pid, response, proceeding, fake_transport_udp, Resp, {proceeding, ok, Resp});

test_proceeding(Pid, udp, '3xx') ->
	Resp = #sip_message{type=response,status=300},
	generic_fsm(Pid, response, proceeding, fake_transport_udp, Resp, {completed, ok, Resp}).

test_completed(Pid, udp, invite) ->
	% same response is reemitted
	Resp = #sip_message{type=response,status=300},
	generic_fsm(Pid, request, completed, fake_transport_udp, {completed, ok, Resp});

test_completed(Pid, udp, ack) ->
	R = #sip_message{type=request, method='ACK'},
	generic_fsm(Pid, request, completed, fake_transport_udp, R, {confirmed, {stop,ack}, unset}).

test_confirmed(Pid) ->
	% wait for timerI expiration -> back to idle state
	% TODO: check transaction reset
	% NOTE: must enclose fun in a {timeout, xx,..} of test fail (maybe a shorted
	% timeout is set by default)
	{timeout, 30, fun() ->
		timer:sleep(6000),
		?assertEqual(idle, get_state_name(Pid))
	end}.

generic_fsm(Pid, request, StateName, Transport, {NextState, NextReply, Response}) ->
	R = #sip_message{type=request, method='INVITE'}, 
	generic_fsm(Pid, request, StateName, Transport, R, {NextState, NextReply, Response}).

generic_fsm(Pid, request, StateName, Transport, Request, {NextState, NextReply, Response}) ->
	% must be executed in a generator as it modifies gen_fsm state
	% (tests must be executed in order)
	?_test(begin
		% cleaning test state
		ets:insert(test_states, {response, unset}),

		Reply = epbxd_sip_server_invite_transaction:receipt(Pid, Request, Transport, undefined),

		[
			?assertEqual(NextState, get_state_name(Pid)),
			?assertEqual(NextReply, Reply),
			?assertEqual([{response,Response}], ets:lookup(test_states, response))
		]
	end);

generic_fsm(Pid, response, StateName, Transport, Message, {NextState, NextReply, Response}) ->
	?_test(begin
		ets:insert(test_states, {response, unset}),

		Reply = epbxd_sip_server_invite_transaction:send(Pid, Message, Transport, undefined),

		[
			?assertEqual(NextState, get_state_name(Pid)),
			?assertEqual(NextReply, Reply),
			?assertEqual([{response, Response}], ets:lookup(test_states, response))
		]
	end).

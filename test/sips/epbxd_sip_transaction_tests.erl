

%%
%% TODO: we SHOULD test each state transition
%%       here we only test main transaction path
%%

-module(epbxd_sip_transaction_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/epbxd_sip.hrl").

%% extract fsm State variable from Fsm process Pid
get_state(Pid) ->
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
	State.

get_state_name(Pid) ->
	{status,_,_,[_,running,_,_,[_,{data,Props},_]]} = sys:get_status(Pid),
	proplists:get_value("StateName", Props).

get_test_() ->
	{setup, local,
		fun() ->
			ets:new(transactions, [set, named_table, public]),

			% we set a pool with ONE available free transaction
			% so poolboy:checkout() will work once, and returns 'full' after
			ets:new(pb_data, [set,named_table,public]),
			ets:insert(pb_data, {a, fsm1}),
			meck:new(poolboy),
			meck:expect(poolboy, checkout, fun(_Mod) ->
				case ets:first(pb_data) of
					'$end_of_table' -> full;
					Key ->
						[{Key,Fsm}] = ets:lookup(pb_data, Key),
						ets:delete(pb_data, Key),
						Fsm
				end
			end),

			ok
		end,
		fun(_) ->
			ets:delete(transactions),
			meck:unload(poolboy)
		end,
		fun(_) ->
			{inorder, [
			 	 {"transaction not found (no checkout)", ?_assertEqual({error, 'not-found'}, epbxd_sip_transaction:get(server,{a,'INVITE'},false))}
				,{"transaction checked out from pool"  , 
						?_assertEqual({epbxd_sip_server_invite_transaction, fsm1}, epbxd_sip_transaction:get(server,{a,'INVITE'},true))}
				,{"transaction checked out from pool (empty pool)", 
						?_assertEqual({error, 'empty-pool'}, epbxd_sip_transaction:get(server,{b,'INVITE'},true))}
				,{"transaction active (found in ets)"  , 
						?_assertEqual({epbxd_sip_server_invite_transaction, fsm1}, epbxd_sip_transaction:get(server,{a,'INVITE'},true))}
			]}
		end
	}.

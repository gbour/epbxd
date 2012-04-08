
-module(epbxd_hooks_tests).
-include_lib("eunit/include/eunit.hrl").

-export([callback1/3, callback2/3, callback3/3]).

-define(EXEC(Desc, Func), {Desc, Func(Args)} end).

get_state(Pid) ->
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
	State.

callback1(_Hookname, _Args, State)  ->
	{ok, {State, callback1}}.

callback2(_Hookname, _Args, State)  ->
	{error, {State, callback2}}.

callback3(_Hookname, _Args, State) ->
	{stop, {State, callback3}}.


test_add_callbacks(Pid) ->
	[
		?_assertEqual(ok, epbxd_hooks:add(hook1, {?MODULE, callback1})),
		?_assertEqual(ok, epbxd_hooks:add(hook1, {?MODULE, callback2})),
		?_assertEqual(ok, epbxd_hooks:add(hook2, {?MODULE, callback1})),
		?_assertEqual(ok, epbxd_hooks:add(hook2, {?MODULE, callback1})),

		?_assertEqual([
				{hook2, [{?MODULE, callback1},{?MODULE, callback1}]},
				{hook1, [{?MODULE, callback1},{?MODULE, callback2}]}
			], 
			get_state(Pid)
		)
	].

test_del_callbacks(Pid)  ->
	[
		?_assertEqual(ok, epbxd_hooks:del(hook1, {?MODULE, callback1})),
		?_assertEqual(ok, epbxd_hooks:del(hook1, {?MODULE, callback3})),

		?_assertEqual([
				{hook1, [{?MODULE, callback2}]},
				{hook2, [{?MODULE, callback1},{?MODULE, callback1}]}
			], 
			get_state(Pid)
		)
	].

test_run_callbacks(Pid) ->
	% NOTE: we need to define a new setup clause, or add() methods 
	% will be executed at very beginning of all test (before test_add_callbacks)
	% which we don't want
	{setup,
		fun() ->
			epbxd_hooks:add(hook10, {?MODULE, callback1}),

			epbxd_hooks:add(hook11, {?MODULE, callback2}),
			epbxd_hooks:add(hook11, {?MODULE, callback1}),

			epbxd_hooks:add(hook12, {?MODULE, callback1}),
			epbxd_hooks:add(hook12, {?MODULE, callback3}),
			epbxd_hooks:add(hook12, {?MODULE, callback1})
		end,
		fun(_) ->
			[
				?_assertEqual(undefined, epbxd_hooks:run(nohook, [])),

				?_assertEqual({ok, {undefined, callback1}}, 
					epbxd_hooks:run(hook10, [])),
				?_assertEqual({ok, {undefined, callback1}}, 
					epbxd_hooks:run(hook11, [])),
				?_assertEqual({stop, {{undefined, callback1}, callback3}, {?MODULE, callback3}}, 
					epbxd_hooks:run(hook12, []))
			]
		end
	}.


epbxd_hooks_test_() ->
	{setup, local,
		% init
		fun() ->
			{ok, Pid} = epbxd_hooks:start_link(),

			Pid
		end,
		% cleanup
		fun(Pid) ->
			ok
		end,
		% tests
		fun(Args) ->
			[
				 {"add callbacks"                 , test_add_callbacks(Args)}
				,{"del callbacks"                 , test_del_callbacks(Args)}
				,{"run callbacks"                 , test_run_callbacks(Args)}
			]
		end
	}.
	


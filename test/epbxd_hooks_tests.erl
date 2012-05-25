
-module(epbxd_hooks_tests).
-include_lib("eunit/include/eunit.hrl").

-export([callback1/4, callback2/4, callback3/4, callback_opts/4]).

-define(EXEC(Desc, Func), {Desc, Func(Args)} end).

get_state(Pid) ->
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
	%io:format(user, "state= ~p~n", [State]),
	State.

internal_hook(Pid, Name, Prio) ->
	lists:nth(Prio, proplists:get_value(Name, get_state(Pid))).

callback1({Name, Prio}, Args, State, _)  ->
	{next, Args++[{Name, Prio, callback1}], {State, {callback1, Prio, Args}}}.

callback2({Name, Prio}, Args, State, _)  ->
	{next, Args++[{Name, Prio, callback2}], {State, {callback2, Prio, Args}}}.

callback3({_Name, Prio}, Args, State, _) ->
	case hd(Args) of
		do_next  ->
			{next, {State, {callback3, Prio, passed}}};

		do_error ->
			{error, because};

		do_stop  ->
			{stop, because}
	end.

callback_opts(_,_,_,Opts) ->
	{ok, [], Opts}.

test_add_callbacks(Pid) ->
	[
		?_assertEqual(ok   , epbxd_hooks:add(hook1, {?MODULE, callback1}, [])),
		% cannot add twice at the same priority (DEFAULT=50)
		?_assertEqual(error, epbxd_hooks:add(hook1,     {?MODULE, callback2}, [])),
		?_assertEqual(error, epbxd_hooks:add(hook1, 50, {?MODULE, callback2}, [])),
		?_assertEqual(ok   , epbxd_hooks:add(hook1, 51, {?MODULE, callback2}, [opt1])),

		?_assertEqual(undefined           , internal_hook(Pid, hook1, 42)),
		?_assertEqual({{?MODULE, callback1}, []}    , internal_hook(Pid, hook1, 50)),
		?_assertEqual({{?MODULE, callback2}, [opt1]}, internal_hook(Pid, hook1, 51)),

		% we can add twice (or more) the same hook
		?_assertEqual(ok, epbxd_hooks:add(hook2, 34, {?MODULE, callback1}, [])),
		?_assertEqual(ok, epbxd_hooks:add(hook2, 42, {?MODULE, callback1}, []))
	].

test_at_callbacks(_Pid)   ->
	[
		% undefined hookname
		?_assertEqual(error               , epbxd_hooks:at(nohook, 1)),
		% empty hook priority
		?_assertEqual(undefined           , epbxd_hooks:at(hook1, 1)),
		?_assertEqual({?MODULE, callback1}, epbxd_hooks:at(hook1, 50)),
		?_assertEqual({?MODULE, callback2}, epbxd_hooks:at(hook1, 51))
	].

test_del_callbacks(Pid)  ->
	[
		% don't return error when callback not found on delete
		?_assertEqual(ok, epbxd_hooks:del(hook1, 42, {?MODULE, callback1})),
		
		?_assertEqual(ok, epbxd_hooks:del(hook1,     {?MODULE, callback1})),
		?_assertEqual(undefined, internal_hook(Pid, hook1, 50)),

		% don't remove if hook don't match
		?_assertEqual(ok, epbxd_hooks:del(hook1, 51, {?MODULE, callback3})),
		?_assertEqual({{?MODULE, callback2}, [opt1]}, internal_hook(Pid, hook1, 51))

	].

test_run_callbacks(_Pid) ->
	% NOTE: we need to define a new setup clause, or add() methods 
	% will be executed at very beginning of all test (before test_add_callbacks)
	% which we don't want
	{setup,
		fun() ->
			epbxd_hooks:add(hook10,     {?MODULE, callback1}, []),

			epbxd_hooks:add(hook11, 20, {?MODULE, callback1}, []),
			epbxd_hooks:add(hook11, 17, {?MODULE, callback2}, []),

			epbxd_hooks:add(hook12,     {?MODULE, callback1}, []),
			epbxd_hooks:add(hook12, 60, {?MODULE, callback3}, []),
			epbxd_hooks:add(hook12, 61, {?MODULE, callback1}, []),

			epbxd_hooks:add(hook_opts, {?MODULE, callback_opts}, [opt1])
		end,
		fun(_) ->
			[
				?_assertEqual(undefined, epbxd_hooks:run(nohook, [])),

				?_assertEqual({ok, {undefined, {callback1, 50, [foo]}}},
					epbxd_hooks:run(hook10, [foo])),
				?_assertEqual({ok, {{undefined, {callback2, 17, [foo]}}, {callback1, 20, [foo, {hook11, 17, callback2}]}}},
					epbxd_hooks:run(hook11, [foo])),

				% pass callback3 - execution continue with previous args
				?_assertEqual({ok, {{{undefined,
						{callback1, 50, [do_next]}},
						{callback3, 60, passed}},
						% callback1/61 received Args as returned by callback1/50
						{callback1, 61,	[do_next, {hook12, 50, callback1}]}}},
					epbxd_hooks:run(hook12, [do_next])),

				% error on callback3 - halt execution with error
				?_assertEqual({error, because, {60, {?MODULE, callback3}}},
					epbxd_hooks:run(hook12, [do_error])),

				% stop on callback3 - halt execution, returning State
				?_assertEqual({ok, because, {60, {?MODULE, callback3}}},
					epbxd_hooks:run(hook12, [do_stop])),

				% validate hooks are receiving opts
				?_assertEqual({ok, [opt1]}, epbxd_hooks:run(hook_opts, nop))
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
		fun(_Pid) ->
			ok
		end,
		% tests
		fun(Args) ->
			[
				 {"add callbacks"                 , test_add_callbacks(Args)}
				,{"at callbacks"                  , test_at_callbacks(Args)}
				,{"del callbacks"                 , test_del_callbacks(Args)}
				,{"run callbacks"                 , test_run_callbacks(Args)}
			]
		end
	}.
	


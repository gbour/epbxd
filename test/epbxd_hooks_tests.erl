
-module(epbxd_hooks_tests).
-include_lib("eunit/include/eunit.hrl").

-export([callback_next/4, callback_stop/4, callback_err/4, callback1/4, callback2/4, callback3/4]).
-export([clb_chk_opts/4, clb_chg_args/4, clb_zap_args/4, clb_stop_args/4]).

-define(EXEC(Desc, Func), {Desc, Func(Args)} end).

get_state(Pid) ->
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
	%io:format(user, "state= ~p~n", [State]),
	State.

internal_hook(gopts, Pid, Name) ->
	hd(tuple_to_list(proplists:get_value(Name, get_state(Pid)))).
internal_hook(clb, Pid, Name, Prio) ->
	{_GOpts, Callbacks} = proplists:get_value(Name,get_state(Pid)),
	lists:nth(Prio, Callbacks).

callback_next(_,_, State,_) ->
	{next, [], State}.

callback_stop(_,_,_,_) ->
	{stop, success}.

callback_err(_,_,_,_) ->
	{error, fail}.

clb_chk_opts(_,_,_, Opts) ->
	case Opts of
		[foobar] ->
			{stop, match};

		_        ->
			{error, fail}
	end.

clb_chg_args(_,_, State, _) ->
	{next, [newarg], State}.

clb_zap_args(_,_, State,_) ->
	{next, State}.

clb_stop_args(_, Args,_,_) ->
	{stop, Args}.


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


test_new_hook(Pid) ->
	[
		% hook not defined
		?_assertEqual({error, not_found}, epbxd_hooks:add(hook0, {a,b}, [])),

		?_assertEqual(ok, epbxd_hooks:new(newook1, [])),
		% defined twice
		?_assertEqual({error, already_set}, epbxd_hooks:new(newook1, [])),

		?_assertEqual(ok, epbxd_hooks:new(newook2, [{gopt, value}])),
		?_assertEqual([{gopt,value}], internal_hook(gopts, Pid, newook2))
	].

test_add_callbacks(Pid) ->
	{setup,
		fun() ->
			epbxd_hooks:new(hook1, []),
			epbxd_hooks:new(hook2, [])
		end,
		fun(_) ->
			[
				{"hook not defined", 
					?_assertEqual({error, not_found}, epbxd_hooks:add(hook0, {a,b}, []))
				},

				{"add callback at default position",
					?_assertEqual(ok, epbxd_hooks:add(hook1, {?MODULE, callback1}, []))
				},

				{"adding twice at default position",
					?_assertEqual({error, already_set}, epbxd_hooks:add(hook1, {a,b}, []))
				},

				{"adding callback as specified position (same as default)",
					?_assertEqual({error, already_set}, epbxd_hooks:add(hook1, 50, {b,c}, []))
				},

				{"callback at specified position with options",
					?_assertEqual(ok, epbxd_hooks:add(hook1, 51, {?MODULE, callback2}, [opt1]))
				},

				{"checking internals",
					{setup, fun() ->
						ok = epbxd_hooks:add(hook2, 17, {u,v}, []),
						ok = epbxd_hooks:add(hook2, 42, {x,y}, [opt1, {17,true}])
					end,
					fun(_) -> [
						?_assertEqual(undefined                 , internal_hook(clb, Pid, hook1, 1)),
						?_assertEqual({{u,v}, []}               , internal_hook(clb, Pid, hook2, 17)),
						?_assertEqual({{x,y}, [opt1, {17,true}]}, internal_hook(clb, Pid, hook2, 42))
					] end
				}}

				% we can add twice (or more) the same hook
				%?_assertEqual(ok, epbxd_hooks:add(hook2, 34, {?MODULE, callback1}, [])),
				%?_assertEqual(ok, epbxd_hooks:add(hook2, 42, {?MODULE, callback1}, []))
			]
		end
	}.

test_at_callbacks(_Pid)   ->
	{setup,
		fun() ->
			ok = epbxd_hooks:new(at1, []),
			ok = epbxd_hooks:add(at1, 12, {a,b}, [])
		end,
		fun(_) ->
			[
				{"undefined hook",
					?_assertEqual({error, not_found}, epbxd_hooks:at(at0, 1))
				},

				{"empty priority",
					?_assertEqual(undefined, epbxd_hooks:at(at1, 1))
				},

				{"found callback",
					?_assertEqual({a,b}, epbxd_hooks:at(at1, 12))
				}
			]
		end
	}.

test_del_callbacks(Pid)  ->
	{setup,
		fun() ->
			ok = epbxd_hooks:new(del1, []),
			ok = epbxd_hooks:add(del1, 15, {a,b}, [])
		end,
		fun(_) ->
			[
				{"undefined hook",
					?_assertEqual({error, not_found}, epbxd_hooks:del(del0, 15, {a,b}))
				},

				{"callback not matching",
					?_assertEqual({error, dont_match}, epbxd_hooks:del(del1, 15, {c,d}))
				},

				{"internal state before deletion",
					?_assertEqual({{a,b},[]}, internal_hook(clb, Pid, del1, 15))
				},

				{"success",
					?_assertEqual(ok, epbxd_hooks:del(del1, 15, {a,b}))
				},

				{"internal state after deletion",
					?_assertEqual(undefined, internal_hook(clb, Pid, del1, 15))
				}
			]
		end
	}.

test_run_callbacks(_Pid) ->
	% NOTE: we need to define a new setup clause, or add() methods 
	% will be executed at very beginning of all test (before test_add_callbacks)
	% which we don't want
	{setup,
		fun() ->
			ok = epbxd_hooks:new(run1, []),
			ok = epbxd_hooks:add(run1, {?MODULE, callback_next}, []),

			ok = epbxd_hooks:new(run2, [{onlast, foo}]),
			ok = epbxd_hooks:add(run2, {?MODULE, callback_next}, []),

			ok = epbxd_hooks:new(run3, [{onstop, bar}]),
			ok = epbxd_hooks:add(run3, 50, {?MODULE, callback_stop}, []),
			ok = epbxd_hooks:add(run3, 51, {?MODULE, callback_next}, []),

			ok = epbxd_hooks:new(run4, []),
			ok = epbxd_hooks:add(run4, {?MODULE, callback_err}, []),

			ok = epbxd_hooks:new(run5, []),
			ok = epbxd_hooks:add(run5, 50, {?MODULE, clb_chk_opts}, [foobar]),

			ok = epbxd_hooks:new(run6, []),
			ok = epbxd_hooks:add(run6, 50, {?MODULE, clb_chg_args}, []),
			ok = epbxd_hooks:add(run6, 51, {?MODULE, clb_stop_args}, []),

			ok = epbxd_hooks:new(run7, []),
			ok = epbxd_hooks:add(run7, 50, {?MODULE, clb_zap_args}, []),
			ok = epbxd_hooks:add(run7, 51, {?MODULE, clb_stop_args}, []),

			ok = epbxd_hooks:new(run8, []),
			ok = epbxd_hooks:add(run8, 15, {?MODULE, callback1}, []),
			ok = epbxd_hooks:add(run8, 60, {?MODULE, callback3}, []),
			ok = epbxd_hooks:add(run8, 61, {?MODULE, callback1}, []),

			epbxd_hooks:add(hook_opts, {?MODULE, callback_opts}, [opt1])
		end,
		fun(_) ->
			[
				{"hook not defined",
					?_assertEqual(undefined, epbxd_hooks:run(run0, []))
				},

				{"executing run1/callback_next",
					?_assertEqual({ok, undefined}, epbxd_hooks:run(run1, []))
				},

				{"custom reply on next (callbacks executed til the end)",
					?_assertEqual({foo, undefined}, epbxd_hooks:run(run2, []))
				},

				{"custom reply on stop (callback stop sequence execution)",
					?_assertEqual({bar, success, {50, {?MODULE, callback_stop}}}, epbxd_hooks:run(run3, []))
				},

				{"error reply (stop execution immediatly)",
					?_assertEqual({error, fail, {50, {?MODULE, callback_err}}}, epbxd_hooks:run(run4, []))
				},

				{"Callback opts check",
					?_assertEqual({ok, match, {50, {?MODULE, clb_chk_opts}}}, epbxd_hooks:run(run5, []))
				},

				{"Callback altering args",
					?_assertEqual({ok, [newarg], {51, {?MODULE, clb_stop_args}}}, epbxd_hooks:run(run6, [oldarg]))
				},

				{"Callback \"zapping\" args",
					?_assertEqual({ok, [oldarg], {51, {?MODULE, clb_stop_args}}}, epbxd_hooks:run(run7, [oldarg]))
				},

				{"Callback sequence #1 (next)",
					?_assertEqual({ok, {{{undefined,
						{callback1, 15, [do_next]}},
						{callback3, 60, passed}},
						% callback1/61 received Args as returned by callback1/50
						{callback1, 61,	[do_next, {run8, 15, callback1}]}}},
					epbxd_hooks:run(run8, [do_next]))
				},

				{"Callback sequence #2 (error)",
					?_assertEqual({error, because, {60, {?MODULE, callback3}}},	epbxd_hooks:run(run8, [do_error]))
				},

				{"Callback sequence #3 (stop)",
					?_assertEqual({ok, because, {60, {?MODULE, callback3}}}, epbxd_hooks:run(run8, [do_stop]))
				}
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
				 {"new hook"                      , test_new_hook(Args)}
				,{"add callbacks"                 , test_add_callbacks(Args)}
				,{"at callbacks"                  , test_at_callbacks(Args)}
				,{"del callbacks"                 , test_del_callbacks(Args)}
				,{"run callbacks"                 , test_run_callbacks(Args)}
			]
		end
	}.
	



-module(mod_authent_internal_tests).
-include_lib("eunit/include/eunit.hrl").
-include("sips/epbxd_sip.hrl").

-export([init_internal/0, down_internal/0]).

init_internal() ->
    meck:new(mnesia),
    meck:expect(mnesia, dirty_read, fun(endpoints, Key) ->
        case Key of
            "bob" -> [{endpoints,"bob"}];
            _     -> []
        end
    end).

down_internal() ->
    meck:unload(mnesia).

authent_unit_test(Mod) ->
    {setup,
        fun() ->
            Init = list_to_atom("init_"++Mod),
            ?MODULE:Init(),

            meck:new(epbxd_hooks),
            meck:expect(epbxd_hooks, add,
                fun(Key,Prio,{Mod2,Fun2}, GOpts) ->
                    ok
                end
            ),
            meck:expect(epbxd_hooks, del,
                fun(Key, {Mod3,Fun3}) ->
                    ok
                end
            ),

            list_to_atom("mod_authent_"++Mod)
        end,
        fun(_) ->
            meck:unload(epbxd_hooks),

            Down = list_to_atom("down_"++Mod),
            ?MODULE:Down()
        end,
        fun(Mod2) ->
            [
                {"loading module", 
                    ?_assertEqual(ok, Mod2:start([]))
                },

                {"authent: user found", 
                    ?_assertEqual({stop, {endpoints,"bob"}}, Mod2:authent({authent,0}, {"bob",nop,nop}, [], []))
                },

                {"authent: user not found", 
                    ?_assertEqual({next, [state]}, Mod2:authent({authent,0}, {"alice",nop,nop}, [state], []))
                },

                {"unloading module", 
                    ?_assertEqual(ok, Mod2:stop())
                }
            ]
        end
    }.

mod_authent_internal_test_() ->
    {setup, local,
        fun() ->
            ok
        end,
        fun(_) ->
            [
                {"Internal authentication (mnesia)", authent_unit_test("internal")}
            ]
        end
    }.

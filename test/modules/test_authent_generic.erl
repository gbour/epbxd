
-module(test_authent_generic).
-include_lib("eunit/include/eunit.hrl").
-include("sips/epbxd_sip.hrl").

-export([authent_unit_test/3]).

authent_unit_test(Mod,Name,Opts) ->
    {setup,
        fun() ->
            Mod:up(),

            meck:new(epbxd_hooks),
            meck:expect(epbxd_hooks, add,
                fun(_Key,_Prio,{_Mod2,_Fun2}, _GOpts) ->
                    ok
                end
            ),
            meck:expect(epbxd_hooks, del,
                fun(_Key, {_Mod3, _Fun3}) ->
                    ok
                end
            ),

            list_to_atom("mod_authent_"++Name)
        end,
        fun(_) ->
            meck:unload(epbxd_hooks),
            Mod:down()
        end,
        fun(Mod2) ->
            [
                {"loading module", 
                    ?_assertEqual(ok, Mod2:start([]))
                },

%                {"authent: user found", 
%                    ?_assertEqual({stop, _Unbound}, Mod2:authent({authent,0}, {"bob",nop,nop}, [], Opts))
%                },

                {"authent: user found", 
					fun() ->
						%NOTE; modules return a {stop, ...} tuple
						%      but we don't know what is the second element
						Ret = Mod2:authent({authent,0}, {"bob",nop,nop}, [], Opts),
	                    ?_assertEqual(stop, element(0, Ret))
					end
                },

                {"authent: user not found", 
                    ?_assertEqual({next, [state]}, Mod2:authent({authent,0}, {"alice",nop,nop}, [state], Opts))
                },


                {"unloading module", 
                    ?_assertEqual(ok, Mod2:stop())
                }
        	]
        end
    }.


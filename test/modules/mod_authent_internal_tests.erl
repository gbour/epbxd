
-module(mod_authent_internal_tests).
-include_lib("eunit/include/eunit.hrl").
-include("sips/epbxd_sip.hrl").

-export([up/0, down/0]).

up() ->
    meck:new(mnesia),
    meck:expect(mnesia, dirty_read, fun(endpoints, Key) ->
        case Key of
            "bob" -> [{endpoints,"bob"}];
            _     -> []
        end
    end).

down() ->
    meck:unload(mnesia).


mod_authent_internal_test_() ->
    {setup, local,
        fun() ->
            ok
        end,
        fun(_) ->
            [
                {"Internal based authentication (mnesia)", 
                    test_authent_generic:authent_unit_test(?MODULE, "internal", [])
                }
            ]
        end
    }.

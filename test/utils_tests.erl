
-module(utils_tests).
-include_lib("eunit/include/eunit.hrl").

title_test() ->
	?assertEqual(<<"Foobar">>, utils:title(<<"foobar">>)),
	?assertEqual(<<"Foobar">>, utils:title(<<"FOOBAR">>)),
	?assertEqual(<<"A">>     , utils:title(<<"a">>)),
	ok.

int_test() ->
	?assertEqual(42    , utils:int(<<"42">>)),
	?assertError(badarg, utils:int(<<"foobar">>)),
	ok.

atom_test() ->
	?assertEqual(foobar, utils:atom(<<"foobar">>)),
	ok.

str_test() ->
	?assertEqual("42"    , utils:str(42)),
	?assertEqual("foobar", utils:str('foobar')),
	?assertEqual("foobar", utils:str(<<"foobar">>)),
	ok.

in_test() ->
	?assertEqual(true , utils:in(42, [12, 42, 34])),
	?assertEqual(false, utils:in(42, [12, 24, 34])),
	ok.


-module(utils).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([title/1, binary_to_integer/1]).

title([H|T]) ->
	string:to_upper([H])++string:to_lower(T).

binary_to_integer(Bin) ->
	erlang:list_to_integer(erlang:binary_to_list(Bin)).

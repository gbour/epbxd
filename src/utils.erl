
-module(utils).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([title/1]).

title([H|T]) ->
	string:to_upper([H])++string:to_lower(T).

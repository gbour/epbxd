
-module(utils).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([title/1, binary_to_integer/1, int/1, atom/1]).

% DEPRECATED
binary_to_integer(Bin) ->
	erlang:list_to_integer(erlang:binary_to_list(Bin)).

% DEPRECATED
title([H|T]) ->
	string:to_upper([H])++string:to_lower(T);


title(<<H:1/binary, T/binary>>) ->
	<<(binstr:to_upper(H))/binary, (binstr:to_lower(T))/binary>>.

int(Val) when is_binary(Val) ->
	erlang:list_to_integer(erlang:binary_to_list(Val)).

atom(Val) when is_binary(Val) ->
	erlang:binary_to_atom(Val, latin1).

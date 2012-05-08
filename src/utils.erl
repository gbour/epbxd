
-module(utils).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([title/1, binary_to_integer/1, int/1, atom/1, str/1, bin/1]).

% DEPRECATED
binary_to_integer(Bin) ->
	erlang:list_to_integer(erlang:binary_to_list(Bin)).

% DEPRECATED
title([H|T]) ->
	string:to_upper([H])++string:to_lower(T);


title(<<H:1/binary, T/binary>>) ->
	<<(binstr:to_upper(H))/binary, (binstr:to_lower(T))/binary>>.

int(Val) when is_binary(Val) ->
	erlang:list_to_integer(erlang:binary_to_list(Val));
int(Val) when is_list(Val)   ->
	erlang:list_to_integer(Val).

atom(Val) when is_binary(Val) ->
	erlang:binary_to_atom(Val, latin1);
atom(Val) when is_list(Val)   ->
	erlang:list_to_atom(Val).

%
%
str(Val) when is_atom(Val)    ->
	erlang:atom_to_list(Val);
str(Val) when is_integer(Val) ->
	erlang:integer_to_list(Val);
str(Val) when is_binary(Val)  ->
	erlang:binary_to_list(Val);
% fallback: we keep value identical
str(Val)                      ->
	Val.

%
%
bin(Val) when is_atom(Val)    ->
	erlang:atom_to_binary(Val, latin1);
bin(Val) when is_list(Val)    ->
	erlang:list_to_binary(Val);
bin(Val) when is_binary(Val)  ->
	Val.

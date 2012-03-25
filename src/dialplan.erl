
%% DIALPLAN

-module(dialplan).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([internal/2]).

internal(<<"140">>, Context) ->
	io:format("140~n",[]);

internal(Ext= <<"1",_:2/binary>>, Context) ->
	io:format("match ~p~n",[Ext]),
	app:dial(Ext, Context);

%%
%% T between 0 and 5
%% 1XX4[0-5]
internal(Ext= <<"1",_:2/binary,"4",T>>, Context) when T >= $0, T =< 53 -> 
	io:format("4 match= ~p ~p~n", [Ext,T]);

internal(_, Context) ->
	io:format("fallback catcher~n",[]),
	app:hangup(Context).

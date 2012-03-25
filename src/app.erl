
-module(app).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([dial/2, hangup/1]).

dial(Exten, Context) ->
	sips:app(dial, Exten, Context).

hangup(Context) ->
	sips:app(hangup, [], Context).


-module(epbxd_dialplan_demo).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([e3k/2]).
-include("utils.hrl").

e3k(Extension, Channel) ->
	?DEBUG("Executing E3K dialplan on ~p/~p", [Extension, ""]),
	%app:dial(Extension, Channel),
	app_answer:exec([], Channel).


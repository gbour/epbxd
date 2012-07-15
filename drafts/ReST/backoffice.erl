
-module(backoffice).
-export([init/0,list/2,get/3]).

-include("backoffice.hrl").

init() ->
	ets:new(users, [set, named_table, {keypos, #user.uid}, public]),

	ets:insert(users, #user{uid=1  ,name= <<"root">> ,groups=[1,2]}),
	ets:insert(users, #user{uid=100,name= <<"gbour">>,groups=[1,3]}),
	ets:insert(users, #user{uid=101,name= <<"jdoe">> ,groups=[3]}),
	ok.

% Object = user
list(user, Opts) ->
	lists:map(fun([Id]) -> [{uid, Id}] end,
		ets:match(users, #user{uid='$1',_='_'})
	).

get(Object, Id, Opts) ->
	%case ets:match_object(users, #user{uid=Id, _='_'}) of
	case ets:lookup(users, Id) of
		[]    -> undefined;
		[Res] -> Res
	end.


% ets:match_object(users, {user,'_',<<"gbour">>,'_'}). 
%


-module(backoffice).
-export([init/0,list/2,get/3,set/2,update/4]).

-include("backoffice.hrl").

init() ->
	ets:new(users, [set, named_table, {keypos, #user.uid}, public]),

	ets:insert(users, #user{uid=1  ,name= <<"root">> ,groups=[1,2]}),
	ets:insert(users, #user{uid=100,name= <<"gbour">>,groups=[1,3]}),
	ets:insert(users, #user{uid=101,name= <<"jdoe">> ,groups=[3]}),

	%ets:new(indexes, [set, named_table, public]),
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

set(Object, Opts) ->
	Ret = ets:insert_new(users, #user{
		uid   = proplists:get_value(uid   , Object),
		name  = proplists:get_value(name  , Object),
		groups= proplists:get_value(groups, Object)
	}),

	case Ret of
		true  -> {ok   , proplists:get_value(uid, Object)};
		false -> {error, "already exists"}
	end.

update(user, Id, Values, Opts) ->
	Ret = case ets:lookup(users, Id) of
		[] -> 'not-found';
		_  ->
			ets:insert(users, #user{
				uid   = Id,
				name  = proplists:get_value(name  , Values),
				groups= proplists:get_value(groups, Values)
			})
	end,

	case Ret of
		true        -> {ok         , Id};
		false       -> {error      , "update fail"};
		'not-found' -> {'not-found', "user not found"}
	end.

% ets:match_object(users, {user,'_',<<"gbour">>,'_'}). 
%

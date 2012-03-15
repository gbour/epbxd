
-module(config).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([load/1, get/1]).

load(Filename) ->
	case file:consult(Filename) of
		{ok, Terms} ->
			%%io:format("~p~n", [Terms]),

			case ets:info(config) of
				undefined ->
					ets:new(config, [set,private,named_table,{read_concurrency,true}]);
				_ ->
					ets:delete_all_objects(config)
			end,
			ets:insert(config, Terms);

		%%{error, Reason}
		Err -> 
			Err
	end.

get(Key) ->
	Entry = ets:lookup(config, Key),
	if 
		length(Entry) > 0 ->
			{Key, Value} = hd(Entry),
			Value;
		true ->
			undefined
	end.


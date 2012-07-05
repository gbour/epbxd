
-module(typing2).
-compile(export_all).
-export([load/1, schema_all/1, schema/1, encode/2, encode_list/2]).

-include("resource.hrl").

load(File) ->
	{ok, [Yaml]} = yaml:load_file(File, [implicit_atoms]),
	Resources = parse_yaml(Yaml,[]),
	Resources.


parse_yaml([], Res) ->
	Res;
parse_yaml([{Name, Desc}|T], Ress) ->
	Res = parse_resource(#resource{name=Name}, Desc),
	io:format("~p: ~p~n", [Name, Res]),
	parse_yaml(T, [Res|Ress]).

parse_resource(Res, []) ->
	Res;
parse_resource(Res, [{record,Record}|T]) ->
	parse_resource(Res#resource{record=Record}, T);
parse_resource(Res, [{key, Key}|T]) ->
	parse_resource(Res#resource{key=Key}, T);
parse_resource(Res, [{fields, Fields}|T]) ->
	Fields2 = lists:map(
		fun({Name, Attrs}) ->
			{Name, parse_field(#field{name=Name}, Attrs)}
		end, Fields),
	parse_resource(Res#resource{fields=Fields2}, T).

parse_field(Field, []) ->
	Field;
parse_field(Field, [{type, Type}|T]) when is_atom(Type) ->
	parse_field(Field#field{type=Type}, T);
parse_field(Field, [{type, <<"list(",Rest/binary>>}|T]) ->
	Type = {list, erlang:binary_to_atom(binary:part(Rest, 0, byte_size(Rest)-1), unicode)},
	parse_field(Field#field{type=Type}, T);
parse_field(Field, [{type, <<"dict(",Rest/binary>>}|T]) ->
	Type = {dict, erlang:binary_to_atom(binary:part(Rest, 0, byte_size(Rest)-1), unicode)},
	parse_field(Field#field{type=Type}, T);
parse_field(Field, [{default, Default}|T]) ->
	parse_field(Field#field{default=Default}, T);
parse_field(Field, [{required, Required}|T]) when is_boolean(Required) ->
	parse_field(Field#field{required=Required}, T);
parse_field(Field, [{unique, Unique}|T]) when is_boolean(Unique) ->
	parse_field(Field#field{unique=Unique}, T);
parse_field(Field, [{desc, Desc}|T]) when is_binary(Desc) ->
	parse_field(Field#field{desc=Desc}, T).
	

check_resource([#resource{key=Key,fields=Fields}|T]) ->
	% check key
	Ret1 = check_resource_key(Key,Fields),
	check_field(Fields),

	ok.

check_resource_key(undefined,_) ->
	true;
check_resource_key(Key, Fields) ->
	proplists:is_defined(Key, Fields).

check_field([{Name, #field{type=Type,default=Default,required=Req,unique=Unk}} | T]) ->
	check_field_default(Type, Default),

	check_field(T).

check_field_default(integer, Value) when is_integer(Value) ->
	true;
check_field_default(string, Value) when is_binary(Value) ->
	true;
check_field_default(boolean, Value) when is_boolean(Value) -> 
	true;
check_field_default(datetime, now) ->
	true;
check_field_default({list,_},[]) ->
	true;
check_field_default(_,_) ->
	false.



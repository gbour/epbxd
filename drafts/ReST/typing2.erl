
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


%%%
%%% DATA SCHEMAS (JSON FORMAT)
%%%

schema_all(Resources) ->
	jsx:encode(schema_all_(Resources,[])).

schema_all_([], Schema) ->
	Schema;
schema_all_([#resource{name=Name}|T], Schema) ->
	Name2 = erlang:atom_to_binary(Name, latin1),
	schema_all_(T, [
		{Name, [
			{base  , <<"/api/",Name2/binary>>},
			{schema, <<"/api/",Name2/binary,"/schema">>}]
		}|Schema
	]).


schema(Resource=#resource{fields=Fields}) ->
	jsx:encode([
			resource_key(Resource),
			{fields, lists:reverse(schema_field(Fields, []))}
	]).

resource_key(#resource{key=undefined}) ->
	<<"">>;
resource_key(#resource{key=Key}) ->
	{key, erlang:atom_to_binary(Key, latin1)}.

schema_field([], Schema) ->
	Schema;
schema_field([{Name, Field}|T], Schema) ->
	schema_field(T,
		[{Name, 
				% use record_info(fields, XX)
				lists:reverse(schema_field_(Field, [type,default,required,unique,desc], []))
		}|Schema
	]).

schema_field_(_,[],Acc) ->
	Acc;
schema_field_(Field=#field{type=Type},[type|T], Acc) when Type == integer; Type == string->
	schema_field_(Field,T, [{type, erlang:atom_to_binary(Type, latin1)}|Acc]);
schema_field_(Field=#field{type=Type},[type|T], Acc) when is_atom(Type) ->
	Name = erlang:atom_to_binary(Type,latin1),
	schema_field_(Field, T, [
		{type, Name}|
		[{schema, <<"/api/",Name/binary,"/schema">>}|Acc]]
	);

schema_field_(Field=#field{type={list,Type}},[type|T], Acc) when Type == integer; Type==string ->
	schema_field_(Field, T, [
		{type, <<"list">>}|
		[{subtype, erlang:atom_to_binary(Type,latin1)}|Acc]
	]);

schema_field_(Field=#field{type={list,Type}},[type|T], Acc)  ->
	Name= erlang:atom_to_binary(Type,latin1),
	schema_field_(Field, T, [
		{type, <<"list">>}|
		[{subtype, Name}|
		[{schema, <<"/api/",Name/binary,"/schema">>}|Acc]]
	]);

schema_field_(Field=#field{required=false, default=Default},[default|T], Acc) when Default =:= undefined ->
	schema_field_(Field,T, [{default, null}|Acc]);
	%schema_field_(Field,T, [{default, erlang:atom_to_binary(Default,latin1)}|Acc]);
schema_field_(Field=#field{default=[]}, [default|T], Acc) ->
	schema_field_(Field,T,[{default, []}|Acc]);
	
schema_field_(Field=#field{required=Required},[required|T], Acc) ->
	schema_field_(Field, T, [{required, Required}|Acc]);
schema_field_(Field=#field{unique=Required},[unique|T], Acc) ->
	schema_field_(Field, T, [{unique, Required}|Acc]);
schema_field_(Field=#field{desc=Desc},[desc|T],Acc) when is_binary(Desc)->
	schema_field_(Field, T, [{desc, Desc}|Acc]);
schema_field_(Field,[_|T], Acc) ->
	schema_field_(Field,T,Acc).


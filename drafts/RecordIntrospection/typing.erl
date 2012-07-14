
-module(typing).
-export([parse/1]).

parse({Record, IncludeFile}) ->
	case
		epp:parse_file(IncludeFile, [],[])
	of
		{error, noent} ->
			pass;

		{ok, Items} ->
			io:format("~p~n", [Items]),
			% undefined if Record not found
			[Fields|Other] = parse_record(Items, Record),
			io:format("~p~n", [[Fields|Other]]),

			{ok, F} = file:open(atom_to_list(Record)++".erl", [write]),
			file:write(F,[Fields,$\n,$\n]),
			lists:foreach(fun(Acc) ->
					[H|T] = Acc,
					io:format(">~p~n", [H]),
					[ file:write(F,[E,$;,$\n]) || E <- lists:reverse(T)],

					R = file:write(F, [H,$.,$\n,$\n]),
					io:format("~p~n", [R])
				end, Other
			),

			ok = file:close(F)
	end.

parse_record([{attribute,_,type,{{record, Record},Fields,[]}} | T], Record) ->
	[FieldsList|Rest] = parse_field(Fields, lists:duplicate(4,[])),
	[["fields() -> [", string:join(lists:reverse(FieldsList),","), "]."] | Rest];
parse_record([_|T], Record) ->
	parse_record(T, Record);
parse_record([], Record) ->
	undefined.

parse_field([], Acc) ->
	Acc;
parse_field([Rec|T], Acc) ->
	parse_field(T, field2def(Rec, Acc)).

field2def({typed_record_field, {record_field,_, {atom,_,FieldName},Default}, Type}, [FieldsAcc,TypeOfAcc, DftAcc, NullAcc]) ->
	TypeOfAcc1 = [["typeof(", atom_to_list(FieldName) ,") -> ",typeof(Type)]|TypeOfAcc],
	DftAcc1    = [["default(" ,atom_to_list(FieldName),") -> ",default(Default) ]|DftAcc],
	NullAcc1   = [["nullable(",atom_to_list(FieldName),") -> ",str(nullable(Type))]|NullAcc],
	FieldsAcc1 = [str(FieldName)|FieldsAcc],

	[FieldsAcc1, TypeOfAcc1, DftAcc1, NullAcc1].

default({bin,_,[{bin_element,_,Default,_,_}]}) ->
	default(Default);
% empty list
default({nil,_}) ->
	<<"[]">>;
default({tuple,_,[]}) ->
	<<"{}">>;
default({_,_,Default}) ->
	str(Default).

typeof({atom,_,undefined}) ->
	undefined;
typeof({type,_,Type,[]})   ->
	str(Type);
typeof({type,_,union,Types}) ->
	Values = lists:filter(fun(T) -> T =/= undefined end, [ typeof(T) || T <- Types ]),
	case erlang:length(Values) of
		1 ->
			hd(Values);
		Any ->
			% rock|paper|scissor -> ["rock","paper","scissor"]
			[$[, [[$",V,$"] || V <- Values], $]]
	end;
typeof({type,_,list,[SubType]}) ->
	[${,<<"list">>,$,,typeof(SubType),$}];
typeof({type,_,dict,[SubType]}) ->
	[${,<<"dict">>,$,,typeof(SubType),$}].

str(X) when is_integer(X) ->
	erlang:integer_to_list(X);
str(X) when is_atom(X) ->
	erlang:atom_to_list(X);
str(X) -> 
	[$",X,$"].


nullable({atom,_,undefined}) -> 
	true;
nullable({type,_,union,Types}) ->
	lists:foldl(fun(X,Y) -> X or Y end, false, [ nullable(T) || T <- Types ]);
nullable(_) ->
	false.



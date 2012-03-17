%% 
%% Decode SIP headers value
%%
%%
-module(uri).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([decode/1]).
-ifdef(debug).
	-export([params/1,headers/1]).
-endif.

-include("utils.hrl").
-include("sips.hrl").

%% TODO: @host is facultative (see tel: URI)
%% TODO: cast port to integer type
decode(Uri) ->
	case re:run(Uri,
			"^(?<scheme>[^:]+):(?<user>[^:@]+)(:(?<pwd>[^@]+))?@(?<host>[^:?;]+)(:(?<port>\\d+))?(?<params>[^?]*)(?<headers>.*)$",[{capture,[scheme,user,pwd,host,port,params,headers],list}]) of
		{match, [Scheme,User,Pwd,Host,Port,Params,Headers]} ->
			#uri{scheme=Scheme,user=User,password=Pwd,host=Host,port=Port,
				params=params(Params),
				headers=headers(Headers)
			};
		_ ->
			error
	end.

params(Params) ->
	case re:run(Params, ";\s*(?<k>[^=]+)=(?<v>[^;]+)",[global,{capture,[k,v],list}]) of
		{match, Matches} ->
			lists:map(fun(E) -> list_to_tuple(E) end, Matches);
		_ ->
			[]
	end.

headers("?"++Headers) ->
	lists:map(
		fun(E) -> list_to_tuple(string:tokens(E, "=")) end, 
		string:tokens(Headers, "&")
	);
headers(_) ->
	[].

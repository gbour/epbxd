%% 
%% Decode SIP headers value
%%
%%
-module(uri).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([decode/1,params/1,encode/1,format/2]).
-ifdef(debug).
	-export([headers/1, password/1]).
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

%%
%% Encode URI
%%
password(undefined) ->
	[];
password(P) when is_list(P) ->
	":"++P.

port(undefined) ->
	[];
port(P) when is_list(P) ->
	":"++P;
port(P) when is_integer(P) ->
	":"++integer_to_list(P).

format(_,undefined) ->
	[];
format(_,[]) ->
	[];
format(params_,[{K,V}|T]) ->
	[";",K,"=",V]++format(params_,T);
format(params,V) ->
	lists:concat(format(params_,V));
format(headers_,[{K,V}|T]) ->
	lists:append([lists:concat([K,"=",V])], format(headers_,T));
format(headers,V) ->
	"?"++string:join(format(headers_, V),"&").

encode(#uri{scheme=S,user=U,password=P,host=H,port=Pt,params=Pm,headers=Hd}) ->
	lists:concat([
		S,":",U,password(P),"@",H,port(Pt),
		format(params,Pm),
		format(headers,Hd)
	]).



%%	epbxd, Erlang PBX Server
%%	Copyright (C) 2012, Guillaume Bour <guillaume@bour.cc>
%%
%%	This program is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU Affero General Public License as
%%	published by the Free Software Foundation, either version 3 of the
%%	License, or (at your option) any later version.
%%
%%	This program is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU Affero General Public License for more details.
%%
%%	You should have received a copy of the GNU Affero General Public License
%%	along with this program.  If not, see <http://www.gnu.org/licenses/>.

% @doc epbxd API to handle SIP URIs (code/decode from/to binary stream)
-module(epbxd_sip_uri).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-export([decode/1, params/1, encode/1, format/2]).
-ifdef(debug).
	-export([headers/1]).
-endif.

-include("utils.hrl").
-include("epbxd_sip.hrl").


%% @doc Convert empty string (as returned by regex) to undefined atom
%%
-spec undefined(string()) -> undefined | string().
undefined(V) when length(V) == 0 ->
	undefined;
undefined(V) ->
	V.

%% @doc Decode an URI (string) 
%%
%% @sample
%%	#sip_uri{scheme=sip, user="john", password=undefined, host="192.168.1.10", port="5061", 
%%	         params =[{"transport", "udp"}],
%%	         headers=[{"appid", "54624"}]} 
%%		= decode("sip:john@192.168.1.10:5061;transport=udp?appid=54624).
%%
%% TODO: @host is facultative (i.e 'tel:' URI)
%% TODO: cast port field to integer
-spec decode(binary() | string()) -> invalid | #sip_uri{}.
decode(Uri) when is_binary(Uri) ->
	decode(utils:str(Uri));
decode(Uri) ->
	case re:run(Uri,
			"^(?<scheme>[^:]+):((?<user>[^:@]+)(:(?<pwd>[^@]+))?@)?(?<host>[^@:?;]+)(:(?<port>\\d+))?(?<params>[^?]*)(?<headers>.*)$",[{capture,[scheme,user,pwd,host,port,params,headers],list}]) of
		{match, [Scheme,User,Pwd,Host,Port,Params,Headers]} ->
			#sip_uri{scheme=Scheme,user=undefined(User),password=undefined(Pwd),host=Host,port=undefined(Port),
				params=params(Params),
				headers=headers(Headers)
			};
		_ ->
			invalid
	end.

%% @doc Decode URI params
%%
%% @sample
%%		[{"tag", "e2daf45"}, {"rport", undefined}] = params(";tag=e2daf45;rport").
%%
%% TODO: should we raise an error if header ';' is not found ?
%%
-spec params(string()) -> list({string(), undefined | string()}).
params(Params) ->
	case re:run(Params, ";\s*(?<k>[^=;]+)(=(?<v>[^;]+))?",[global,{capture,[k,v],list}]) of
		{match, Matches} ->
			lists:map(fun([K, V]) -> list_to_tuple([K, pvalue(V)]) end, Matches);
		_ ->
			[]
	end.

pvalue([]) ->
	undefined;
pvalue(V)  ->
	V.

%% @doc Split URI headers
%%
%% @sample
%%		[{"foo", "bar"}, {"plop", undefined}] = headers("?foo=bar&plop").
%%
-spec headers(string()) -> list({string(), string()}).
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
userinfo(undefined,undefined) ->
	[];
userinfo(U,undefined) ->
	U++"@";
userinfo(U,P) ->
	lists:flatten([U,":",P,"@"]).

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

encode(#sip_uri{scheme=S,user=U,password=P,host=H,port=Pt,params=Pm,headers=Hd}) ->
	lists:concat([
		S,":",userinfo(U,P),H,port(Pt),
		format(params,Pm),
		format(headers,Hd)
	]).


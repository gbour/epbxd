
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
-export([decode/1, params/1, encode/1, encode/2]).
-ifdef(debug).
	-export([headers/1, userinfo/2, port/1, params/2, headers/2]).
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


%% @doc Decode SIP URI scheme
%%
%% "sip" and "sips" are only valid schemes.
%% any other value return *invalid* atom.
%%
%% @private
%%
-spec scheme(decode, string()) -> atom().
scheme(decode, "sip")  -> sip;
scheme(decode, "sips") -> sips;
scheme(decode, _)      -> invalid.

%% @doc Decode SIP URI Port
%%
%% @private
%%
-spec port(decode, atom()|string()) -> atom()|integer().
port(decode, undefined) ->
	undefined;
port(decode, V)         ->
	utils:int(V).


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
			#sip_uri{
				scheme   = scheme(decode, Scheme),
				user     = undefined(User),
				password = undefined(Pwd),
				host     = Host,
				port     = port(decode, undefined(Port)),
				params   = params(Params),
				headers  = headers(Headers)
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
			lists:map(fun([K, V]) -> list_to_tuple([K, undefined(V)]) end, Matches);
		_ ->
			[]
	end.

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

%% @doc Encode URI params
%%
%% @sample
%%		<<";ttl=45;tag=45oxo00">> = erlang:iolist_to_binary(encode(params, [{"ttl", 45},{"tag", "45oxo00"}])).
%%
-spec encode(params, list({string(), any()})) -> iolist().
encode(params, P) ->
	params(encode, P).

%% @doc Encode SIP URI
%%
-spec encode(#sip_uri{}) -> iolist().
encode(#sip_uri{scheme=S,user=U,password=P,host=H,port=Pt,params=Pm,headers=Hd}) ->
	[ scheme(S), $:, userinfo(U,P), H, port(Pt), params(encode, Pm), headers(encode, Hd) ].

%% @doc Encode URI scheme
%% @private
scheme(sip)  -> <<"sip">>;
scheme(sips) -> <<"sips">>.

%% @doc Encode URI userinfo part
%% @private
userinfo(undefined, undefined) ->
	[];
userinfo(User, undefined) ->
	[User, $@];
userinfo(User, Pwd) ->
	[User, $:, Pwd, $@].

%% @doc Encode URI port part
%% @private
port(undefined) ->
	[];
port(P) when is_integer(P) ->
	[$:, utils:str(P)];
port(P) ->
	[$:, P].

%% @doc Encode URI params
%% @private
params(encode, [])     ->
	[];
params(encode, Params) ->
	params(encode, Params, []).

params(encode, [], Acc)                  ->
	Acc;
params(encode, [{Key, undefined} | Tail], Acc) ->
	params(encode, Tail, [Acc, $;, Key]);
params(encode, [{Key, Val} | Tail], Acc) ->
	params(encode, Tail, [Acc, $;, utils:str(Key), $=, utils:str(Val)]).


%% @doc Encode URI headers
%% @private
headers(encode, [])     ->
	[];
headers(encode, Headers) ->
	headers(encode, Headers, []).

headers(encode, [], Acc)                 ->
	[$?, Acc];
headers(encode, [{Key,Val} | Tail], [])  ->
	headers(encode, Tail, [Key, $=, Val]);
headers(encode, [{Key,Val} | Tail], Acc) ->
	headers(encode, Tail, [Acc, $&, Key, $=, Val]).



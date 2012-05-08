
-module(epbxd_sip_sig).
-export([response/4, request/4]).

-include("epbxd_dialplan.hrl").
-include("epbxd_sip.hrl").

response(Type, Extension, Opts, #call_context{source={S,T}, request=R}) ->
	io:format(user, "do response ~p: ~p~n", [Type, R]),
	epbxd_sip_routing:send(epbxd_sip_message:response(Type, R), T, S).

request(Type, Registration, Opts, #call_context{source={_,T}, request=R}) ->
	io:format(user, "do request ~p: ~p ~p~n", [Type, R, T]),
	io:format(user, "~p~n", [epbxd_sip_message:request(Type, Registration,R)]),

	epbxd_sip_routing:send(epbxd_sip_message:request(Type, Registration, R), T, 
		{undefined,Registration#registration.uri#sip_uri.host,undefined}).


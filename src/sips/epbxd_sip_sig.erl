
-module(epbxd_sip_sig).
-export([response/4, request/4]).

-include("epbxd_dialplan.hrl").
-include("epbxd_sip.hrl").

response(Type, Extension, Opts, #call_context{source={S,T}, request=R}) ->
	io:format(user, "do response ~p: ~p~n", [Type, R]),
	epbxd_sip_routing:send(epbxd_sip_message:response(Type, R), T, S).

request(Type, Registration=#registration{uri=Uri}, Opts, #call_context{source={_,T}, request=R}) ->
	io:format(user, "do request ~p: ~p ~p~n", [Type, R, T]),

	% create dialog for called peer
	Dialog = #sip_dialog{
		%callid  = utils:bin(epbxd_sip_header:tag()),
		callid  = utils:bin(epbxd_sip_header:tag()),

		origin  = 'self',
		request = epbxd_sip_message:req2meth(Type),
		status  = epbxd_sip_message:req2meth(Type),
		peer    = Uri,
		created = calendar:universal_time(),
		updated = calendar:universal_time()
	},
	io:format(user, "INVITE Dialog= ~p~n", [Dialog]),
	mnesia:dirty_write(dialogs, Dialog),

	io:format(user, "~p~n", [epbxd_sip_message:request(Type, Registration,R, Dialog)]),
	epbxd_sip_routing:send(epbxd_sip_message:request(Type, Registration, R, Dialog), T,
		{undefined,Registration#registration.uri#sip_uri.host,undefined}).


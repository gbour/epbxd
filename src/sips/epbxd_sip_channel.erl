
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

% @doc SIP gen_channel implementation
-module(epbxd_sip_channel).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-behaviour(gen_epbxd_channel).
-export([init/1, dial/3, ring/2, accept/2, hangup/2]).
%-export([on_called/2, on_ringing/2, on_answered/2, on_hanguped/2]).

-include("epbxd_channel.hrl").
-include("epbxd_sip.hrl").
-include("sdp/sdp.hrl").
-include("rtp/rtp.hrl").

%% @doc 
%%
init(_Opts) ->
	% create SIP hooks
	epbxd_hooks:new({sip,request,'REGISTER'}, []),
	epbxd_hooks:new({sip,request,'INVITE'}, []),
	epbxd_hooks:new({sip,request,'ACK'}, []),
	epbxd_hooks:new({sip,request,'BYE'}, []),
	epbxd_hooks:new({sip,response,180}, []),
	epbxd_hooks:new({sip,response,200}, []),

	epbxd_sip_transaction:init([]),
	ok.

-spec dial(call_peer(), string(), list()) -> ok | {error, atom()}.
dial(_From, To, Opts) ->
	case
		mnesia:dirty_read(registrations, utils:str(To))
	of
		[Reg] ->
			io:format(user, "found ~p~n",[Reg]),

			Chan = proplists:get_value(channel, Opts),
			request(dial, Reg, Opts, Chan);

		[]    ->
			{error, not_found}
	end.

ring(To, _Opts) ->
	response(ringing, To).

accept(To=#sip_stub{ref=Request}, _Opts) ->
	%WARNING: after that, invite transaction is deleted
	%
	Payload = Request#sip_message.payload,
	SdpOffer = sdp:decode(Payload),
	io:format(user, "sdp= ~p~n", [SdpOffer]),
	
	{Type, Codecs} = case sdp:negociate(SdpOffer) of
		[]      -> {'unsupported-media', undef};
		Codecs2 -> {'ok'               , Codecs2}
	end,

	case response(Type, To, Codecs) of
		{ok, Port} ->
			#rtp_context{
				codecs=Codecs,
				port=Port
			};
		_ -> pass
	end.

hangup(_To=#sip_stub{dialog=Dialog}, _Opts) ->
	[Reg] = mnesia:dirty_read(registrations, Dialog#sip_dialog.peer#sip_uri.user),

	epbxd_sip_routing:send(
		epbxd_sip_message:request(bye, Reg, nil, Dialog),
		epbxd_udp_transport, {nil, Reg#registration.uri#sip_uri.host, nil}
	).



response(Type, #sip_stub{socket=S,transport=T,ref=R,transaction={Mod,Fsm}}, Codecs) ->
	% 1. create RTP socket
	{ok, Port, Socket} = epbxd_rtp:alloc(),
	io:format(user, "RTP port= ~p, ~p~n", [Port, inet:sockname(Socket)]),
		
	Session = #sdp_session{
		origin     = #sdp_origin{
			ssid      = random:uniform(99999),
			% TODO: must be increased when session data is modified
			ssversion = random:uniform(9999),
			% TODO: either from configuration, or query the system (on utilise sockname(Socket))
			address   = <<"127.0.0.1">>
		},
		connection = #sdp_connection{
			% TODO: either from configuration, or query the system
			address   = <<"127.0.0.1">>
		},
		medias     = [#sdp_media{
			port      = Port,
			rtpmap    = Codecs
		}]
	},
	io:format(user, "~p~n", [Session]),

	Response = epbxd_sip_message:response(Type, R),
	Payload  = sdp:encode(Session),

	% adding content-type, content-length headers
	Response2 = Response#sip_message{
		headers=lists:append(Response#sip_message.headers, [
				{"Content-Type"  , "application/sdp"},
				{"Content-Length", erlang:size(Payload)}
			]),
		payload=Payload
	},

	Mod:send(Fsm, Response2, T, S),

	% returns RTP socket
	{ok, Socket}
	
response(Type, #sip_stub{socket=S,transport=T,ref=R,transaction={Mod,Fsm}}) ->
	io:format(user, "do response ~p: ~p~n", [Type, R]),
	%epbxd_sip_routing:send(epbxd_sip_message:response(Type, R), T, S).
	Mod:send(Fsm, epbxd_sip_message:response(Type, R), T, S).

request(Type, Registration=#registration{uri=Uri}, _Opts, Channel) ->
	% TODO: transport should be determined from target context (registrations Table)
	T = epbxd_udp_transport,
	io:format(user, "do request ~p: ~p ~n", [Type, T]),

	% create dialog for called peer
	Dialog = #sip_dialog{
		callid  = utils:bin(epbxd_sip_header:tag()),

		origin  = 'self',
		request = epbxd_sip_message:req2meth(Type),
		status  = epbxd_sip_message:req2meth(Type),
		peer    = Uri,
		created = calendar:universal_time(),
		updated = calendar:universal_time(),

		chanid  = Channel#call_channel.id
	},
	io:format(user, "INVITE Dialog= ~p~n", [Dialog]),
	mnesia:dirty_write(dialogs, Dialog),

	io:format(user, "~p~n", [epbxd_sip_message:request(Type, Registration,nil, Dialog)]),
	epbxd_sip_transaction:send(epbxd_sip_message:request(Type, Registration, nil, Dialog), T,
		{undefined,Registration#registration.uri#sip_uri.host,undefined}
	),

	{ok, #sip_stub{dialog=Dialog}}.


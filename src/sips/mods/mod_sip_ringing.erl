
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

% @doc Handle SIP 180/Ringing responses
-module(mod_sip_ringing).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([ringing/3]).
% gen_epbxd_module
-export([start/1, stop/0]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").
-include("epbxd_dialplan.hrl").

%% @doc Start module
%%
%% Initialize module environment
%% Install SIP 180/Ringing response hook
%%
-spec start(list(any())) -> ok|fail.
start(Opts) ->
	% registering hooks
	epbxd_hooks:add({sip,response,180}, {?MODULE, ringing}),
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del({sip,response,180}, {?MODULE, ringing}),
	ok.

%% @doc SIP OK request hook
%%
%% Implement process described in RFC 3261
%%
-spec ringing(tuple(), tuple(), any()) -> tuple(ok, any()).
ringing({Key, Priority}, Args={Resp=#sip_message{headers=Headers}, Sock, Transport}, State) ->
    io:format(user, "receive Ringing request~n",[]),

    % Searching matching dialog
    CallID = proplists:get_value('Call-ID', Headers),
    io:format(user, "Call-ID= ~p~n", [CallID]),

    case
       mnesia:dirty_read(dialogs, CallID)
    of
        % FOUND
        [Dialog] ->
            io:format(user, "Dialog= ~p~n", [Dialog]),
			[Chan] = mnesia:dirty_read(channels, Dialog#sip_dialog.chanid),
            io:format(user, "Channel= ~p~n", [Chan]),

			case Chan#call_channel.action of
				% INVITE action - the callee is OK to take call
				'INVITE' ->
					% Send OK to caller
					Stub = Chan#call_channel.source#call_peer.peer,
					epbxd_sip_routing:send(
						epbxd_sip_message:response(ringing, Stub#sip_stub.ref),
						epbxd_udp_transport, Stub#sip_stub.socket
					)

			end,

			% send ACK to sender
			%[Reg] = mnesia:dirty_read(registrations, Dialog#sip_dialog.peer#sip_uri.user),
			%Req = epbxd_sip_message:request('ack', Reg, nil, Dialog),
			%epbxd_sip_routing:send(Req,
			%	epbxd_udp_transport,{nil,Reg#registration.uri#sip_uri.host,nil}
			%),
            ok;

        []       ->
            io:format(user, "Dialog not found~n", [])
    end,

	{ok, Args, State}.

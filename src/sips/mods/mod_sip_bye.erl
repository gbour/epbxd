
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

% @doc Handle SIP BYE request
-module(mod_sip_bye).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([bye/4]).
% gen_epbxd_module
-export([start/1, stop/0]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").
-include("epbxd_channel.hrl").

%% @doc Start module
%%
%% Initialize module environment
%% Install SIP 180/Ringing response hook
%%
-spec start(list(any())) -> ok|fail.
start(Opts) ->
	% registering hooks
	epbxd_hooks:add({sip,request,'BYE'}, {?MODULE, bye}, Opts),
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del({sip,request,'BYE'}, {?MODULE, bye}),
	ok.

%% @doc SIP OK request hook
%%
%% Implement process described in RFC 3261
%%
-spec bye(tuple(), tuple(), any(), list()) -> tuple(next, any()).
bye(_, {Request=#sip_message{headers=Headers}, _Sock, _Transport}, State, _) ->
	% read transaction from State
	Transaction = proplists:get_value(transaction, State),
	{Mod,Fsm} = Transaction,

    io:format(user, "receive BYE request (trans=~p)~n",[Transaction]),

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

%			case Chan#call_channel.action of
%				% INVITE action
%				'INVITE' ->
%					% Send BYE to peer
%					% NOTE: peer may be target OR source!!!
%					%
%					Peer =
%						if Dialog =:= Chan#call_channel.source#call_peer.peer#sip_stub.dialog ->
%							Chan#call_channel.target;
%						true ->
%							Chan#call_channel.source
%						end,
%					io:format(user, "Peer= ~p~n", [Peer]),
%					epbxd_channel:hangup(Peer, [])
%			end,

			% send OK to originator
			Via = hd(proplists:get_value('Via', Headers)),
			Mod:send(Fsm,
				epbxd_sip_message:response(ok, Request),
				epbxd_udp_transport,{nil, Via#sip_via.host, nil}
			),

			% NOTE: peer dialog will be deleted when OK received
			io:format(user, "delete dialog & channel~n",[]),
			mnesia:dirty_delete_object(dialogs, Dialog),
			mnesia:dirty_delete_object(channels, Chan),
            ok;

        []       ->
            io:format(user, "BYE: Dialog '~p' not found~n", [CallID]),
			% we use Via the answer
			Via = hd(proplists:get_value('Via', Headers)),
			io:format(user, "Via= ~p~n", [Via]),

			%NOTE: responses must be sent through the transaction
			Mod:send(Fsm,
				epbxd_sip_message:response('dont-exists', Request),
				epbxd_udp_transport, {nil,Via#sip_via.host,nil}
			)
    end,

	{next, State}.

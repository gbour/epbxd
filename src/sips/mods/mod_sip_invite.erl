
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

% @doc Handle SIP INVITE requests
-module(mod_sip_invite).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([invite/3]).
% gen_epbxd_module
-export([start/0, stop/0]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").
-include("epbxd_dialplan.hrl").

%% @doc Start module
%%
%% Initialize module environment
%% Install SIP INVITE request hook
%%
-spec start() -> ok|fail.
start() ->
	% registering hooks
	epbxd_hooks:add({sip,request,'INVITE'}, {?MODULE, invite}),
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del({sip, request, 'INVITE'}, {?MODULE, invite}),
	ok.

%% @doc SIP INVITE request hook
%%
%% Implement process described in RFC 3261, section 13.3.1
%%
-spec invite(tuple(), tuple(), any()) -> tuple(ok, any()).
invite(Key, {Request=#sip_message{headers=Headers}, Sock, Transport}, State) ->
	% create dialog for caller
	Dialog = #sip_dialog{
		callid = proplists:get_value('Call-ID', Headers),

		origin  = 'peer',
		request = 'INVITE',
		status  = 'Trying',
		peer    = (proplists:get_value('From', Headers))#sip_address.uri,
		created = calendar:universal_time(),
		updated = calendar:universal_time()
	},
	mnesia:dirty_write(dialogs, Dialog),

	% 100 Trying
	epbxd_sip_routing:send(
		epbxd_sip_message:response(trying, Request),
		Transport, Sock
	),

	% lookup caller. Is he authenticated
	User = (proplists:get_value('From', Headers))#sip_address.uri#sip_uri.user,
	?DEBUG("SIP:INVITE= loopkup '~s' endpoint", [User]),

	case
		mnesia:dirty_read(endpoints, User)
	of
		% FOUND
		[Endpt] ->
			?DEBUG("Found endpoint: ~p", [Endpt]),
			To = (proplists:get_value('To', Headers))#sip_address.uri#sip_uri.user,
			Chan = #call_channel{
				id = {},
				source = #call_peer{type='sip', peer=#sip_stub{
					dialog    = Dialog,
					socket    = Sock,
					transport = Transport,
					ref       = Request
				}},
				target = undefined
			},

			epbxd_dialplan:dispatcher(utils:bin(To), Chan);

		[]      -> 
			?DEBUG("Source endpoint not found",[]),
			epbxd_sip_routing:send(
				epbxd_sip_message:response(forbidden, Request),
				Transport, Sock
			)
	end,

	{ok, done}.

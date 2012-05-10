
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

% @doc Handle SIP 200/OK responses
-module(mod_sip_ok).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([ok/3]).
% gen_epbxd_module
-export([start/0, stop/0]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").
-include("epbxd_dialplan.hrl").

%% @doc Start module
%%
%% Initialize module environment
%% Install SIP 200/OK response hook
%%
-spec start() -> ok|fail.
start() ->
    %% create mnesia table 
    %% (could be ETS, but we need to distribute data between all epbxd nodes)
    mnesia:create_table(dialogs, [
        {attributes , record_info(fields, sip_dialog)},
        {record_name, sip_dialog}
    ]),

	% registering hooks
	epbxd_hooks:add({sip,response,200}, {?MODULE, ok}),
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del({sip,response,200}, {?MODULE, ok}),
	ok.

%% @doc SIP OK request hook
%%
%% Implement process described in RFC 3261, section 13.3.1
%%
-spec ok(tuple(), tuple(), any()) -> tuple(ok, any()).
ok(Key, {Request=#sip_message{headers=Headers}, Sock, Transport}, State) ->
    io:format(user, "receive OK request~n",[]),

    % Searching matching dialog
    CallID = proplists:get_value('Call-ID', Headers),
    io:format(user, "Call-ID= ~p~n", [CallID]),

    case
       mnesia:dirty_read(dialogs, CallID)
    of
        % FOUND
        [Dialog] ->
            io:format(user, "Dialog= ~p~n", [Dialog]),
            ok;

        []       ->
            io:format(user, "Dialog not found~n", [])
    end,

    
	% 100 Trying
%	epbxd_sip_routing:send(
%		epbxd_sip_message:response(trying, Request),
%		Transport, Sock
%	),
%
%	% lookup caller. Is he authenticated
%	User = (proplists:get_value('From', Headers))#sip_address.uri#sip_uri.user,
%	?DEBUG("SIP:INVITE= loopkup '~s' endpoint", [User]),
%
%	case
%		mnesia:dirty_read(endpoints, User)
%	of
%		% FOUND
%		[Endpt] ->
%			?DEBUG("Found endpoint: ~p", [Endpt]),
%			To = (proplists:get_value('To', Headers))#sip_address.uri#sip_uri.user,
%
%			epbxd_dialplan:dispatcher(
%				utils:bin(To),
%				#call_context{
%					channel=#call_channel{type=sip, from=User, to=To},
%					source={Sock, Transport},
%					request=Request
%				}
%			);
%
%		[]      -> 
%			?DEBUG("Source endpoint not found",[]),
%			epbxd_sip_routing:send(
%				epbxd_sip_message:response(forbidden, Request),
%				Transport, Sock
%			)
%	end,

	{ok, done}.

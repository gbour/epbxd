
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

% @doc Handle SIP transactions. Placed BEFORE requests handlers
-module(mod_sip_transaction).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([transaction/4]).
% gen_epbxd_module
-export([start/1, stop/0]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

%% @doc Start module
%%
%% Initialize module environment
%%
-spec start(list(any())) -> ok|fail.
start(Opts) ->
	% registering hooks
	epbxd_hooks:add({sip,response,180}, 20, {?MODULE, transaction}, Opts), % Ringing
	epbxd_hooks:add({sip,response,200}, 20, {?MODULE, transaction}, Opts), % OK
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del({sip,response,180}, 20, {?MODULE, transaction}),
	epbxd_hooks:del({sip,response,200}, 20, {?MODULE, transaction}),
	ok.

%% @doc SIP INVITE request hook
%%
%% Implement process described in RFC 3261, section 13.3.1
%%
%-spec invite(tuple(), tuple(), any()) -> tuple(ok, any()).
transaction(_, {Response=#sip_message{headers=Headers}, Sock, Transport}, State, Opts) ->
	TransId = {
		proplists:get_value("branch", (hd(proplists:get_value('Via', Headers)))#sip_via.params),
		element(2, (proplists:get_value('CSeq', Headers))) % {117, INVITE} -> INVITE
	},

	?DEBUG("Starting transaction handler. Transaction ID= ~p", [TransId]),
	ontrans(ets:lookup(transactions, TransId), Response, State).

ontrans([], Response, State) ->
	?DEBUG("Transaction not found", []),
	{error, transaction_not_found};
ontrans([{_,Fsm}], Response, State) ->
	Transaction = epbxd_sip_transaction:get_transaction(Fsm),
	?DEBUG("Transaction found: ~p", [Transaction]),

	case (Transaction#transaction.fsm):recept(Fsm, Response) of
		{ok, Response} ->
			{next, State};

		{stop, Reason}  ->
			{stop, State}
	end.
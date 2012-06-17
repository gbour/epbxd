
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

% @doc epbxd SIP transactions API
-module(epbxd_sip_transaction).

-export([init/1, send/3, get_transaction/1]).

-include("epbxd_sip.hrl").
-include("utils.hrl").

%% @doc Initialise SIP transactions
%%
-spec init(list()) -> ok | {error, tuple()}.
init(Opts) ->
	ets:new(transactions, [set, named_table, public]),

	% create pools of transactions
	lists:foreach(fun(Trans) ->
			{ok, Pid} = poolboy:start_link([
				{name, {local, Trans}},	{worker_module, Trans},
				{size, 10},	{max_overflow, 10},	{monitor, false}
			])
		end,
		[
			epbxd_sip_client_invite_transaction,
			epbxd_sip_client_noninvite_transaction,
			epbxd_sip_server_invite_transaction,
			epbxd_sip_server_noninvite_transaction
		]
	),

	ok.

%% @doc
%%
send(Request=#sip_message{type=request,method=Method,headers=Headers}, Transport, Socket) ->
	% switching to the good fsm
	Mod = fsm(client, Method),
	Fsm = poolboy:checkout(Mod),
	?DEBUG("transaction:send: fsm= ~p (from ~p)", [Fsm, self()]),

	TransId = {
		proplists:get_value(branch, (hd(proplists:get_value('Via', Headers)))#sip_via.params),
		utils:bin(element(2, (proplists:get_value('CSeq', Headers)))) % {117, INVITE} -> INVITE
	},
	ets:insert(transactions, {TransId, {Mod, Fsm}}),

	Mod:send(Fsm, Request, Transport, Socket),
	ok.

%% @doc
%%
get_transaction({Mod, Fsm}) ->
	Mod:get_transaction(Fsm).

%% @doc
%%
fsm(client,'INVITE')  -> epbxd_sip_client_invite_transaction;
fsm(client,_)         -> epbxd_sip_client_noninvite_transaction.


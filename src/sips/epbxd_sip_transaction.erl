
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

%% @doc
%%
init(Opts) ->
	ets:new(transactions, [set, named_table, public]),
	{ok, Pid} = poolboy:start_link([
		{name, {local, epbxd_sip_client_invite_transaction}},
		{worker_module, epbxd_sip_client_invite_transaction},
		{size, 10},
		{max_overflow, 10},
		{monitor, false}
	]),

	ok.

%% @doc
%%
send(Request=#sip_message{type=request,method=Method,headers=Headers}, Transport, Socket) ->
	% switching to the good fsm
	Fsm = poolboy:checkout(fsm(Method)),
	?DEBUG("transaction:send: fsm= ~p (from ~p)", [Fsm, self()]),

	TransId = {
		proplists:get_value(branch, (hd(proplists:get_value('Via', Headers)))#sip_via.params),
		utils:bin(element(2, (proplists:get_value('CSeq', Headers)))) % {117, INVITE} -> INVITE
	},
	Mod = fsm(Method),
	ets:insert(transactions, {TransId, {Mod, Fsm}}),

	Mod:send(Fsm, Request, Transport, Socket),
	ok.

%% @doc
%%
get_transaction({Mod, Fsm}) ->
	Mod:get_transaction(Fsm).

%% @doc
%%
fsm('INVITE')  -> epbxd_sip_client_invite_transaction.


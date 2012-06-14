
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

% @doc epbxd API to handle SIP message (code/decode from/to binary stream)
-module(epbxd_sip_server_noninvite_transaction).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(gen_fsm).
-behaviour(poolboy_worker).

% public API
-export([start_link/1, send/4, receipt/4, get_transaction/1]).
% private API
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
% states handlers
-export([idle/3, trying/3, proceeding/3, completed/2, completed/3, terminated/2]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

-record(state, {
	timergRef,
	transaction
}).


%% @doc Create server INVITE transaction fsm
%%
-spec start_link(list()) -> {ok, pid()} | {error, any()} | ignore.
start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, []).

%% @doc Init fsm process
%% @private
%%
-spec init(list()) -> {ok, idle, #state{}}.
init(Args) ->
	{ok, idle, #state{transaction=#transaction{fsm=?MODULE, fsmid=self()}}}.

%% @doc Send a SIP message
%%
%% The message go through the fsm.
%% Sending need the fsm to be in idle state
-spec send(pid(), #sip_message{}, atom(), any()) -> ok.
send(Pid, Response=#sip_message{type=response}, Transport, Socket) ->
	gen_fsm:sync_send_event(Pid, {epbxd_sip_message:response_type(Response), Response, Transport, Socket}).

%% @doc Receive a SIP INVITE
%%
%% Go through the transaction fsm
%%
%-spec recept(pid(), #sip_message{}) -> ok.
receipt(Pid, Message, Transport, Socket) ->
	gen_fsm:sync_send_event(Pid, {Message, Transport, Socket}).

%% @doc
%%
-spec get_transaction(pid()) -> #transaction{}.
get_transaction(Pid) ->
	% use handke_info/handle_event instead ?
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
	State#state.transaction.

%%
%% FSM STATES/TRANSITIONS
%%

% @sync
% receive a non-INVITE request from peer
idle({Request=#sip_message{type=request}, Transport, Socket}, _From, State=#state{transaction=Transaction}) ->
	%?DEBUG("fsm: idle->calling (state=~p)",[Transaction]),
	% send 100 Trying to the peer
	%epbxd_sip_routing:send(DOCREATEMESSAGE, Transport, Socket),

	Transaction2 = case epbxd_sip_routing:is_reliable(Transport:name()) of
		false ->
			Transaction#transaction{
				timerG=Transaction#transaction.t1,
				request={Request, Transport, Socket}};

		_     ->
			Transaction#transaction{
				timerH=64*Transaction#transaction.t1,
				request={Request, Transport, Socket}
			}
	end,

	% return ok, so non-INVITE hook execution continue
	{reply, ok, trying, State#state{transaction=Transaction2}}.

trying({provisional, Response, Transport, Socket}, _From, State) ->
	% TODO: send response
	{reply, ok, proceeding, State};

% @sync
% 2xx to 6xx responses
trying({_, Response, Transport, Socket}, _From, State) ->
	% TODO: send response
	{reply, ok, completed, State}.

% @sync
proceeding(Request=#sip_message{type=request}, _From, State) ->
	% TODO: send last response
	{reply, {stop, ignore}, proceeding, State};

% @sync
proceeding({provisional, Response, Transport, Socket}, _From, State) ->
	% TODO: send response to the peer
	{reply, ok, proceeding, State};

% @sync
% 2xx to 6xx response from TU
proceeding({_, Response}, _From, State=#state{transaction=Transaction}) ->
	% TODO: send response to the peer
	{reply, ok, completed, State, State#state.transaction#transaction.timerJ}.

% @async
% timerJ
completed(timeout, State) ->
	{next_state, terminated, State}.

completed(Request=#sip_message{type=request}, _From, State) ->
	% TODO: send last provisional response
	{reply, {ok, Request}, completed, State}.

% @async
% NOTE: do not confuse with terminated() fsm callback
terminated(_, State) ->
	?DEBUG("fsm state change: plop terminated",[]),
	% destroy transaction
	{next_state, idle, #transaction{}}.


%%
%% PRIVATE
%%

handle_event(_Event, StateName, State) ->
	?DEBUG("fsm handle event (~p)", [self()]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	?DEBUG("fsm handle sync event (~p)", [self()]),
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
	?DEBUG("fsm handle info (~p)", [self()]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	?DEBUG("fsm terminate (~p)", [self()]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	?DEBUG("fsm code change (~p)", [self()]),
    {ok, StateName, State}.


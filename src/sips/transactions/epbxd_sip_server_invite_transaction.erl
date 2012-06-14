
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
-module(epbxd_sip_server_invite_transaction).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(gen_fsm).
-behaviour(poolboy_worker).

% public API
-export([start_link/1, send/4, receipt/4, get_transaction/1]).
% private API
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
% states handlers
-export([idle/3, proceeding/3, completed/2, confirmed/2, confirmed/3, completed/3, terminated/2]).

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
% receive an INVITE from peer
idle({Request=#sip_message{type=request, method='INVITE'}, Transport, Socket}, _From, State=#state{transaction=Transaction}) ->
	%?DEBUG("fsm: idle->calling (state=~p)",[Transaction]),
	% send 100 Trying to the peer
	%epbxd_sip_routing:send(DOCREATEMESSAGE, Transport, Socket),

	Transaction2 = case epbxd_sip_routing:is_reliable(Transport:name()) of
		false ->
			Transaction#transaction{
				timerG=Transaction#transaction.t1,
				timerH=64*Transaction#transaction.t1,
				timerI=Transaction#transaction.t4,
				request={Request, Transport, Socket}};

		_     ->
			Transaction#transaction{
				timerH=64*Transaction#transaction.t1,
				request={Request, Transport, Socket}
			}
	end,

	% return ok, so INVITE hook execution continue
	{reply, ok, proceeding, State#state{transaction=Transaction2}}.

proceeding(Request=#sip_message{type=request, method='INVITE'}, _From, State) ->
	{reply, {ok, Request}, proceeding, State};

% @sync
% request retransmission: send last provisional response
%
proceeding({Request=#sip_message{type=request, method='INVITE'}, Transport, Socket}, _From, State) ->
	% TODO: send last provisional response to the peer
	{reply, {stop, ignore}, State};

% @sync
proceeding({provisional, Response, Transport, Socket}, _From, State) ->
	% TODO: send response to the peer
	{reply, ok, proceeding, State};

% @sync
proceeding({successful, Response}, _From, State) ->
	% TODO: send response to the peer
	{reply, ok, terminated, State};

% @sync
% 3xx to 6xx responses
proceeding({_, Response}, _From, State=#state{transaction=Transaction}) ->
	% TODO: send response to the peer
	
	% install TimerH trigger
	% replace by a gen_fsm:send_event_after
	OnTimeout = fun(Pid) ->
		gen_fsm:send_event(Pid, timeoutH)
	end,
	{ok, TRef} = timer:apply_after(Transaction#transaction.timerH, erlang, apply, [OnTimeout, [self()]]),

	% we'll need to cancel this timer when switching to confirmed state, so we need a reference
	TRef2 = gen_fsm:send_event_after(State#state.transaction#transaction.timerG, timeoutG),
	{reply, ok, completed, State#state{timergRef=TRef2}}.

% @async
% timerG
completed(timeoutG, State=#state{transaction=Transaction}) ->
	% TODO: send last response (3xx to 6xx)
	State2 = State#state{
		transaction=Transaction#transaction{
			timerG=min(Transaction#transaction.timerG*2, Transaction#transaction.t2)
	}},
	TRef = gen_fsm:send_event_after(State#state.transaction#transaction.timerG, timeoutG),

	{next_state, completed, State2#state{timergRef=TRef}};

completed(timeoutH, State) ->
	% cancel timerG
	gen_fsm:cancel_timer(State#state.timergRef),

	% inform TU that transaction fails (ACK never received)
	{next_state, terminated, State}.

completed(Request=#sip_message{type=request, method='INVITE'}, _From, State) ->
	% TODO: send last provisional response
	{reply, {ok, Request}, completed, State};

% @sync
completed(#sip_message{type=request, method='ACK'}, _From, State) ->
	% cancel timerG
	gen_fsm:cancel_timer(State#state.timergRef),

	% DO NOT GIVE UP response to TU
	{reply, {stop, ignore}, confirmed, State, State#state.transaction#transaction.timerI}.

% async
% timerI
confirmed(timeout, State) ->
	{next_state, terminated, State};

% @async
% timerH: ignored
confirmed(timeoutH, State) ->
	{next_state, confirmed, State}.

% sync
% absork additional ACKs
confirmed(#sip_message{type=request, method='ACK'}, _From, State) ->
	{reply, {stop, ignore}, confirmed, State}.

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


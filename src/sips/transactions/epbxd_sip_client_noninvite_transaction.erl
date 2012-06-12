
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

% @doc State Machine for non-INVITE client transactions
-module(epbxd_sip_client_noninvite_transaction).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(gen_fsm).
-behaviour(poolboy_worker).

% public API
-export([start_link/1, send/4, recept/2, get_transaction/1]).
% private API
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
% states handlers
-export([idle/3, trying/2, trying/3, proceeding/2, proceeding/3, completed/2, completed/3, terminated/2]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

-record(state, {
	timerfRef,
	transaction
}).


%% @doc Create client INVITE transaction fsm
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
send(Pid, Message, Transport, Socket) ->
	gen_fsm:sync_send_event(Pid, {calling, Message, Transport, Socket}).

%% @doc Receive a SIP response
%%
%% Go through the transaction fsm
%%
-spec recept(pid(), #sip_message{}) -> ok.
recept(Pid, Message=#sip_message{status=Status}) ->
	gen_fsm:sync_send_event(Pid, {epbxd_sip_message:response_type(Status), Message}).

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
idle({trying, Message, Transport, Socket}, From, #state{transaction=Transaction}) ->
	?DEBUG("fsm: idle->traying (state=~p)",[Transaction]),
	% send message	
	epbxd_sip_routing:send(Message, Transport, Socket),

	Transaction2 = case epbxd_sip_routing:is_reliable(Transport:name()) of
		% non-reliable (UDP)
		false ->
			Transaction#transaction{
				timerE=erlang:min(Transaction#transaction.t1, Transaction#transaction.t2),
				timerF=64*Transaction#transaction.t1,
				timerK=Transaction#transaction.t4,
				request={Message, Transport, Socket}};

		_     ->
			Transaction#transaction{
				timerF=64*Transaction#transaction.t1,
				request={Message, Transport, Socket}
			}
	end,

	?DEBUG("timerE= ~p, timerF= ~p", [Transaction2#transaction.timerE, Transaction2#transaction.timerF]),
	% install TimerF trigger
	OnTimeout = fun(Pid) ->
		gen_fsm:send_event(Pid, timeoutf)
	end,
	{ok, TRef} = timer:apply_after(Transaction2#transaction.timerF, erlang, apply, [OnTimeout, [self()]]),

	% if transport is reliable, TimerE == infinity => not timeout
	{reply, ok, calling, #state{transaction=Transaction2, timerfRef=TRef}, Transaction2#transaction.timerE}.

% @async
trying(timeout, State=#state{transaction=Transaction}) ->
	?DEBUG("fsm: trying timeout timerE (=~p)",[Transaction#transaction.timerF]),
	% send message
	%NOTE: request is a tuple; but needs a list as epbxd_sip_routing:send() arguments
	erlang:apply(epbxd_sip_routing,send, utils:list(Transaction#transaction.request)),

	Transaction2 = Transaction#transaction{
		timerE=erlang:min(Transaction#transaction.timerE*2,	Transaction#transaction.t2)
	},
	{next_state, trying, State#state{transaction=Transaction2}, Transaction2#transaction.timerE};

% @async
trying(timeoutf, State) ->
	?DEBUG("fsm: TimerF timed out. Canceling transaction",[]),
	% TODO: inform TU of timerB timeout
	% ??
	
	{next_state, terminated, State}.

% @sync
trying({provisional, Response}, From, State=#state{timerfRef=TRef, transaction=#transaction{t2=T2}}) ->
	{ok, cancel} = timer:cancel(TRef),

	% TODO: give back response to TU
	% just return response
	
	{reply, {ok, Response}, proceeding, State, T2};

% @sync
trying({_, Response}, _From, State=#state{timerfRef=TRef, transaction=#transaction{timerK=TimerK}}) ->
	{ok, cancel} = timer:cancel(TRef),

	% TODO: give up final response to TU
	{reply, {ok, Response}, completed, State, TimerK}.

% @async
proceeding(timeout, State=#state{transaction=Transaction}) ->
	?DEBUG("fsm: proceeding timeout timerE (=~p)",[Transaction#transaction.t2]),
	% send message
	%NOTE: request is a tuple; but needs a list as epbxd_sip_routing:send() arguments
	erlang:apply(epbxd_sip_routing,send, utils:list(Transaction#transaction.request)),

	{next_state, proceeding, State,	Transaction#transaction.t2};

% @async
proceeding(timeoutf, State) ->
	% just ignore
	{next_state, terminated, State}.

% @sync
proceeding({provisional, Response}, _From, State) ->
	% TODO: give up response to TU
	{reply, {ok, Response}, proceeding, State};

% @sync
proceeding({_, Response}, _From, State) ->
	% give up response to TU

	{reply, {ok, Response}, completed, State, State#state.transaction#transaction.timerK}.

% @async
% TimerK
completed(timeout, State) ->
	{next_state, terminated, State}.


% @sync
completed({_, Response=#sip_message{}}, _From, State) ->
	% DO NOTHING
	{reply, {ok, Response}, completed, State}.

% @async
% NOTE: do not confuse with terminated() fsm callback
terminated(_, State) ->
	?DEBUG("fsm state change: terminated",[]),
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


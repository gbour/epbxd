
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
-module(epbxd_sip_client_invite_transaction).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(gen_fsm).
-behaviour(poolboy_worker).

% public API
-export([start_link/1, send/4, recept/2, get_transaction/1]).
% private API
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
% states handlers
-export([idle/3, calling/2, calling/3, proceeding/3, completed/2, completed/3, terminated/2]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

-record(state, {
	timerbRef,
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
idle({calling, Message, Transport, Socket}, _From, #state{transaction=Transaction}) ->
	?DEBUG("fsm: idle->calling (state=~p)",[Transaction]),
	% send message	
	epbxd_sip_routing:send(Message, Transport, Socket),

	Transaction2 = case epbxd_sip_routing:is_reliable(Transport:name()) of
		false ->
			Transaction#transaction{
				timerA=Transaction#transaction.t1,
				timerD=32000,
				request={Message, Transport, Socket}};

		_     ->
			Transaction#transaction{request={Message, Transport, Socket}}
	end,

	?DEBUG("timerA= ~p, timerB= ~p", [Transaction2#transaction.timerA, Transaction2#transaction.timerB]),
	% install TimerB trigger
	OnTimeout = fun(Pid) ->
		gen_fsm:send_event(Pid, timeoutb)
	end,
	{ok, TRef} = timer:apply_after(Transaction2#transaction.timerB, erlang, apply, [OnTimeout, [self()]]),

	% if transport is reliable, TimerA == infinity => not timeout
	{reply, ok, calling, #state{transaction=Transaction2, timerbRef=TRef}, Transaction2#transaction.timerA}.

% @async
calling(timeout, State=#state{transaction=Transaction}) ->
	?DEBUG("fsm: calling timeout timerA (timerA=~p)",[Transaction#transaction.timerA]),
	% send message
	%NOTE: request is a tuple; but needs a list as epbxd_sip_routing:send() arguments
	erlang:apply(epbxd_sip_routing,send, utils:list(Transaction#transaction.request)),

	Transaction2 = Transaction#transaction{timerA=Transaction#transaction.timerA*2},
	{next_state, calling, State#state{transaction=Transaction2}, Transaction2#transaction.timerA};

% @async
calling(timeoutb, State) ->
	?DEBUG("fsm: calling timeoutb. Canceling transaction",[]),
	% TODO: inform TU of timerB timeout
	% ??
	
	{next_state, terminated, State}.

% @sync
calling({provisional, Response}, _From, State=#state{timerbRef=TRef}) ->
	{ok, cancel} = timer:cancel(TRef),

	% TODO: give back response to TU
	% just return response
	
	{reply, {ok, Response}, proceeding, State};

% @sync
calling({successful, Response}, _From, State=#state{timerbRef=TRef}) ->
	{ok, cancel} = timer:cancel(TRef),

	% TODO: give up final response to TU
	{reply, {ok, Response}, terminated, State};

% @sync
calling({_, Response}, _From, State=#state{timerbRef=TRef}) ->
	{ok, cancel} = timer:cancel(TRef),

	% TODO: give up response to TU (TU will send ACK request)
	% TODO: send ACK request

	{reply, {ok, Response}, complete, State, State#transaction.timerD}.

% @sync
proceeding({provisional, Response}, _From, State) ->
	% TODO: give up response to TU
	{reply, {ok, Response}, proceeding, State};

% @sync
proceeding({successful, Response}, _From, State) ->
	% TODO: give up response to TU (TU will send ACK request)
	{reply, {ok, Response}, terminated, State};

% @sync
proceeding({_, Response}, _From, State) ->
	% give up response to TU
	% send ACK request

	{reply, {ok, Response}, completed, State, State#transaction.timerD}.

% @async
proceeding(timeoutb, State) ->
	% just ignore
	{next_state, proceeding, State}.

% @async
completed(timeout, State) ->
	{next_state, terminated, State}.


% @sync
completed({_, Response=#sip_message{status=Status}}, _From, State) when Status > 200 ->
	% DO NOT GIVE UP response to TU
	% TODO: send ACK request
	{reply, {stop, ignore}, completed, State}.

% @async
% NOTE: do not confuse with terminated() fsm callback
terminated(_, State) ->
	?DEBUG("fsm state change: plop terminated",[]),
	% destroy transaction
	{next_state, idle, #transaction{}}.
handle_event(_Event, StateName, State) ->
	?DEBUG("fsm handle event (~p)", [self()]),
    {next_state, StateName, State}.

%%
%% PRIVATE
%%

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


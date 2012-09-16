
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
-export([init/1, cleanup/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
% states handlers
-export([idle/3, proceeding/3, completed/2, completed/3, confirmed/2, confirmed/3]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

-record(state, {
	tRefG,
	tRefH,
	response    = undefined,
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
	{ok, idle, #state{transaction=#sip_transaction{fsm=?MODULE, fsmid=self()}}}.

%% @doc cleanup fsm state data
%%
-spec cleanup(#state{}) -> #state{}.
cleanup(#state{transaction=Transaction}) ->
	epbxd_sip_transaction:destroy(?MODULE, self(), Transaction),
	#state{transaction=#sip_transaction{fsm=?MODULE, fsmid=self()}}.

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
-spec receipt(pid(), sip_message(), atom(), any()) -> ok | {error, atom()}.
receipt(Pid, Message, Transport, Socket) ->
	gen_fsm:sync_send_event(Pid, {Message, Transport, Socket}).

%% @doc
%%
-spec get_transaction(pid()) -> #sip_transaction{}.
get_transaction(Pid) ->
	% use handke_info/handle_event instead ?
	{status,_,_,[_,running,_,_,[_,_,{data,[{_,State}]}]]} = sys:get_status(Pid),
	State#state.transaction.


%%
%% FSM STATES/TRANSITIONS
%%


%% @doc *idle* state :: receive INVITE request
%% @sync
%% @private
%%
-spec idle({sip_message(), atom(), any()}, any(), #state{}) -> {reply, ok|{error, atom()}, atom(), #state{}}.
idle({Request=#sip_message{type=request, method='INVITE'}, Transport, Socket}, _From, StateData=#state{transaction=Transaction}) ->
	%?DEBUG("fsm: idle->calling (state=~p)",[Transaction]),
	% send 100 Trying to the peer
	%%Response = trying(),
	Response= foobar,

	{Reply, NewState, NewStateData} = case
		epbxd_sip_routing:send(Response, Transport, Socket)
	of
		ok              ->
			Transaction2 = case epbxd_sip_routing:is_reliable(Transport:name()) of
				false ->
					Transaction#sip_transaction{
						timerG=Transaction#sip_transaction.t1,
						timerH=64*Transaction#sip_transaction.t1,
						timerI=Transaction#sip_transaction.t4,
						request={Request, Transport, Socket}};

				_     ->
					Transaction#sip_transaction{
						timerH=64*Transaction#sip_transaction.t1,
						request={Request, Transport, Socket}
					}
			end,
			{ok, proceeding, StateData#state{transaction=Transaction2, response={Response, Transport, Socket}}};

		{error, Reason} ->
			{{error, Reason}, idle, cleanup(StateData)}
	end,

	{reply, Reply, NewState, NewStateData}.

%% @doc *proceeding* state :: receive INVITE request (retransmission)
%% @sync
%% @private
%%
%% Send back last provisional response (stop hook execution)
%% 
-spec proceeding({sip_message(), atom(), any()}|{atom(), sip_message(), atom(), any()}, any(), #state{}) -> 
	{reply, ok|{error,atom()}, atom(), #state{}}.
proceeding({Request=#sip_message{type=request, method='INVITE'}, Transport, Socket}, _From,	StateData=#state{response={Response,_,_}}) ->
	{Reply, NewState, NewStateData} = case
		epbxd_sip_routing:send(Response, Transport, Socket)
	of
		ok              ->
			{{stop, retransmission}, proceeding, StateData#state{response={Response,Transport,Socket}}};

		{error, Reason} ->
			{{error, Reason}, idle, cleanup(StateData)}
	end,

	{reply, Reply, NewState, NewStateData};

%% @doc *proceeding* state :: TU sending provisional response
%% @sync
%% @private
%%
proceeding({provisional, Response, Transport, Socket}, _From, StateData) ->
	{Reply, NewState, NewStateData} = case
		epbxd_sip_routing:send(Response, Transport, Socket)
	of
		ok              ->
			{ok, proceeding, StateData};

		{error, Reason} ->
			{{error, Reason}, idle, cleanup(StateData)}
	end,

	{reply, Reply, NewState, NewStateData};

%% @doc *proceeding* state :: TU sending OK (2XX) response
%% @sync
%% @private
%%
proceeding({successful, Response, Transport, Socket}, _From, StateData) ->
	Reply =	epbxd_sip_routing:send(Response, Transport, Socket),
	{reply, Reply, idle, cleanup(StateData)};

%% @doc *proceeding* state :: TU sending 3XX to 6XX response
%% @sync
%% @private
%%
proceeding({_, Response, Transport, Socket}, _From, StateData=#state{transaction=Transaction}) ->
	{Reply, NewState, NewStateData} = case
		epbxd_sip_routing:send(Response, Transport, Socket)
	of
		ok              ->
			% install TimerG/TimerH triggers
			TRefG = gen_fsm:send_event_after(Transaction#sip_transaction.timerG, timeoutG),
			TRefH = gen_fsm:send_event_after(Transaction#sip_transaction.timerH, timeoutH),
			{ok, completed, StateData#state{tRefG=TRefG, tRefH=TRefH, response={Response, Transport, Socket}}};

		{error, Reason} ->
			{{error, Reason}, idle, cleanup(StateData)}
	end,

	{reply, Reply, NewState, NewStateData}.

%% @doc *completed* state :: timerG timeout
%% @private
%% @async
%%
%% Send back last 3xx/6xx response
%%
-spec completed(timeoutG|timeoutH, #state{}) -> {next_state, completed|idle, #state{}}.
completed(timeoutG, StateData=#state{transaction=Transaction, response={Response, Transport, Socket}}) ->
	{NewState, NewStateData} = case
		epbxd_sip_routing:send(Response, Transport, Socket)
	of
		ok              ->
			Transaction2=Transaction#sip_transaction{
				timerG=min(Transaction#sip_transaction.timerG*2, Transaction#sip_transaction.t2)
			},
			TRefG = gen_fsm:send_event_after(Transaction#sip_transaction.timerG, timeoutG),

			{completed, StateData#state{tRefG=TRefG, transaction=Transaction2}};

		{error, Reason} ->
			%TODO : inform TU of error
			{idle, cleanup(StateData)}
	end,

	{next_state, NewState, NewStateData};

%% @doc *completed* state :: timerH timeout
%% @private
%% @async
%%
%% Ends transaction
%%
completed(timeoutH, StateData=#state{tRefG=TRefG}) ->
	_Remains = gen_fsm:cancel_timer(TRefG),

	% inform TU that transaction fails (ACK never received)
	{next_state, idle, cleanup(StateData)}.

%% @doc *completed* state :: receive INVITE request (retransmission)
%% @private
%% @sync
%%
%% Send back last 3xx/6xx response
%%
-spec completed(sip_message(), any(), #state{}) -> 
	{reply, ok|{error,atom()}, atom(), #state{}} | {reply, {stop,ack}, confirmed, #state{}, integer()|infinity}.
completed({Request=#sip_message{type=request, method='INVITE'},_,_}, _From, StateData=#state{tRefG=TRefG, response={Response, Transport, Socket}, transaction=Transaction}) ->
	_Remains = gen_fsm:cancel_timer(TRefG),

	{Reply, NewState, NewStateData} = case
		epbxd_sip_routing:send(Response, Transport, Socket)
	of
		ok              ->
			TRefG2 = gen_fsm:send_event_after(Transaction#sip_transaction.timerG, timeoutG),
			{ok, completed, StateData#state{tRefG=TRefG2}};

		{error, Reason} ->
			{{error, Reason}, idle, cleanup(StateData)}
	end,

	{reply, Reply, NewState, NewStateData};

%% @doc *completed* state :: receive ACK request (end of INVITE transaction)
%% @private
%% @sync
%%
%% Go to *confirmed* state (canceling timerG)
%% NOTE: ACK request is not transmited to TU
completed({#sip_message{type=request, method='ACK'},_,_}, _From, StateData=#state{tRefG=TRefG, tRefH=TRefH, transaction=Transaction}) ->
	% cancel timerG
	_Remains1 = gen_fsm:cancel_timer(TRefG),
	_Remains2 = gen_fsm:cancel_timer(TRefH),

	{_,Transport,_} = Transaction#sip_transaction.request,
	% If transaction is reliable, destroy transaction immediatly
	{NewState, NewStateData, TimeOut} =
		case epbxd_sip_routing:is_reliable(Transport:name()) 
	of
		false ->
			{confirmed, StateData, Transaction#sip_transaction.timerI};
	
		_     ->
			{idle, cleanup(StateData), infinity}
	end,

	{reply, {stop, ack}, NewState, NewStateData, TimeOut}.

%% @doc *confirmed* state :: timerI timeout
%% @private
%% @async
%%
%% destroy transaction
-spec confirmed(timeout, #state{}) -> {next_state, idle, #state{}}.
confirmed(timeout, StateData) ->
	{next_state, idle, cleanup(StateData)}.

%% @doc *confirmed* state :: receive ACK request (retransmission)
%% @private
%% @sync
%%
%% Absorb ACK (not transmitted to TU)
-spec confirmed(sip_message(), any(), #state{}) -> {reply, {stop,retransmission}, confirmed, #state{}}.
confirmed(#sip_message{type=request, method='ACK'}, _From, State) ->
	{reply, {stop, retransmission}, confirmed, State}.


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



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
-export([start_link/1, send/4, receipt/2, get_transaction/1]).
% private API
-export([init/1, cleanup/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
% states handlers
-export([idle/3, calling/2, calling/3, proceeding/3, completed/2, completed/3]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

-record(state, {
	tRefB,
	tRefD,
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
init(_Args) ->
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
-spec send(pid(), sip_message(), atom(), any()) -> ok.
send(Pid, Message, Transport, Socket) ->
	gen_fsm:sync_send_event(Pid, {calling, Message, Transport, Socket}).

%% @doc Receive a SIP response
%%
%% Go through the transaction fsm
%%
-spec receipt(pid(), #sip_message{}) -> ok | {error, atom()}.
receipt(Pid, Message=#sip_message{status=Status}) ->
	gen_fsm:sync_send_event(Pid, {epbxd_sip_message:response_type(Status), Message}).

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

%% @doc idle -> calling :: sending an INVITE Request to a client
%% @sync
%%
-spec idle({calling, sip_message(), tuple(), any()}, any(), #state{}) -> {reply, tuple(), tuple(), #state{}, integer()}.
idle({calling, Request=#sip_message{type=request, method='INVITE'}, Transport, Socket}, _From, StateData=#state{transaction=Transaction}) ->
	?DEBUG("fsm: idle->calling (state=~p)",[Transaction]),
	% send message	
	{Reply, NewState, NewStateData, TimeOut} = case 
		epbxd_sip_routing:send(Request, Transport, Socket) 
	of
		% MESSAGE SENT
		ok             ->
			Transaction2 = case epbxd_sip_routing:is_reliable(Transport:name()) of
				false ->
					Transaction#sip_transaction{
						timerA=Transaction#sip_transaction.t1,
						timerD=32000,
						request={Request, Transport, Socket}};

				_     ->
					% timerA=infinity
					Transaction#sip_transaction{request={Request, Transport, Socket}}
			end,

			?DEBUG("timerA= ~p, timerB= ~p", [Transaction2#sip_transaction.timerA, Transaction2#sip_transaction.timerB]),
			% install TimerB trigger
			TRef = gen_fsm:send_event_after(Transaction2#sip_transaction.timerB, timeoutB),

			% if transport is reliable, TimerA == infinity => not timeout
			{ok, calling, StateData#state{transaction=Transaction2, tRefB=TRef}, Transaction2#sip_transaction.timerA};

		% TRANSPORT ERROR
		{error, Reason} ->
			{{error, Reason}, idle, cleanup(StateData), infinity}
	end,

	{reply, Reply, NewState, NewStateData, TimeOut}.

%% @doc *calling* state :: timerA timeout
%% @async
%%
-spec calling(timeout|timeoutB, #state{}) -> {next_state, calling|idle, #state{}, integer()}|{next_state, idle, #state{}}.
calling(timeout, StateData=#state{transaction=Transaction}) ->
	?DEBUG("fsm: calling timeout timerA (timerA=~p)",[Transaction#sip_transaction.timerA]),
	% resend message
	% NOTE: request is a tuple; but needs a list as epbxd_sip_routing:send() arguments
	{NewState, NewStateData, TimeOut} = case 
		erlang:apply(epbxd_sip_routing, send, utils:list(Transaction#sip_transaction.request))
	of
		% MESSAGE SEND
		ok              ->
			Transaction2 = Transaction#sip_transaction{timerA=Transaction#sip_transaction.timerA*2},
			{calling, StateData#state{transaction=Transaction2}, Transaction2#sip_transaction.timerA};

		% TRANSPORT ERROR: terminate transaction
		{error, _Reason} ->
			% TODO: inform TU
			%
			{idle, cleanup(StateData), infinity}
	end,

	{next_state, NewState, NewStateData, TimeOut};

%% @doc *calling* state :: timerB timeout
%% @async
%%
%-spec calling(timeoutB, #state{}) -> {next_state, terminated, #state{}}.
calling(timeoutB, State) ->
	?DEBUG("fsm: calling timeoutb. Canceling transaction",[]),
	% TODO: inform TU of timerB timeout

	% terminate transaction
	{next_state, idle, cleanup(State)}.

%% @doc *calling* state :: receiving provisional (1XX) response
%% @sync
%%
-spec calling({provisional, sip_message()}, any(), #state{}) -> {reply, {ok, sip_message(), sip_transaction()}, atom(),	#state{}}.
calling({provisional, Response}, _From, State=#state{tRefB=TRef, transaction=Transaction}) ->
	io:format(user,"calling: prov response (~p)~n", [TRef]),
	_Remains = gen_fsm:cancel_timer(TRef),

	% give up response to TU (returns ok, then hooks execution continue)
	{reply, {ok, Response, Transaction}, proceeding, State};

%% @doc *calling* state :: receiving successful (2XX) response
%% @sync
%%
calling({successful, Response}, _From, State=#state{tRefB=TRef, transaction=Transaction}) ->
	_Remains = gen_fsm:cancel_timer(TRef),

	% give up final response to TU
	{reply, {ok, Response, Transaction}, idle, cleanup(State)};

%% @doc *calling* state :: receiving 3XX to 6XX response
%% @sync
%%
calling({_, Response}, _From, State=#state{tRefB=TRef, transaction=Transaction}) ->
	_Remains = gen_fsm:cancel_timer(TRef),

	% install TimerD trigger
	TRefD = gen_fsm:send_event_after(Transaction#sip_transaction.timerD, timeoutD),
	% give up response to TU
	{reply, {ok, Response, Transaction}, completed, State#state{tRefD=TRefD}}.

%% @doc *proceeding* state :: receiving provisional (1XX) response
%% @sync
%%
-spec proceeding({tuple(), sip_message()}, any(), #state{}) -> {reply, {ok, sip_message(), sip_transaction()}, atom(), #state{}}.
proceeding({provisional, Response}, _From, State=#state{transaction=Transaction}) ->
	% give up response to TU
	{reply, {ok, Response, Transaction}, proceeding, State};

%% @doc *proceeding* state :: receiving successful (2XX) response
%% @sync
%%
proceeding({successful, Response}, _From, State=#state{transaction=Transaction}) ->
	% give up response to TU (TU is responsible of sending ACK request)
	{reply, {ok, Response, Transaction}, idle, cleanup(State)};

%% @doc *proceeding* state :: receiving 3XX to 6XX response
%% @sync
%%
proceeding({_, Response}, _From, State=#state{transaction=Transaction}) ->
	% send ACK request

	% install TimerD trigger
	TRefD = gen_fsm:send_event_after(Transaction#sip_transaction.timerD, timeoutD),
	% give up response to TU
	{reply, {ok, Response, Transaction}, completed, State#state{tRefD=TRefD}}.

%% @doc *completed* state :: timerD timetout
%% @async
%%
-spec completed(timeoutD, #state{}) -> {next_state, idle, #state{}}.
completed(timeout, State) ->
	{next_state, idle, cleanup(State)}.

%% @doc *completed* state :: receiving 3XX to 6XX response
%%
%% @sync
-spec completed({atom(), sip_message()}, any(), #state{}) -> {reply, {stop, ignore}, completed, #state{}}.
completed({_, _Response=#sip_message{status=Status}}, _From, State) when Status >= 300 ->
	% DO NOT GIVE UP response to TU
	% TODO: send ACK request
	{reply, {stop, ignore}, completed, State}.


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



-module(epbxd_rtp).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_server).

% API
-export([start_link/1, alloc/0, free/1]).
% gen_server interface
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	available_ports = []
}).

start_link(Args) ->
	io:format(user, "** starting RTP server~n",[]),
	gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).

init({StartRange, EndRange}) ->
	{ok, #state{available_ports=lists:seq(StartRange, EndRange)}}.

alloc() ->
	gen_server:call(?MODULE, alloc).

free(Socket) ->
	gen_server:call(?MODULE, {free, Socket}).


%%
%% PRIVATE
%%

handle_call(alloc, _, State=#state{available_ports=[]}) ->
	{stop, 'no-free-ports', State};
handle_call(alloc, _, State=#state{available_ports=[Port|T]}) ->
	case 
		gen_udp:open(Port, [binary, inet])
	of
		{ok, Socket}    ->
			{reply, {ok, Port, Socket}, State#state{available_ports=T}};

		{error, Reason} ->
			{stop, Reason, {error, Reason}, State#state{available_ports=lists:append(T,[Port])}}
	end;

handle_call({free, Socket}, _, State=#state{available_ports=Ports}) ->
	Port = inet:port(Socket),
	ok = gen_udp:close(Socket),

	{noreply, State#state{available_ports=lists:append(Ports,[Port])}}.


handle_cast(_Req, _State) ->
	{noreply, _State}.

handle_info(_Info, _State) ->
	{noreply, _State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, _State, _Extra) ->
	{ok, _State}.

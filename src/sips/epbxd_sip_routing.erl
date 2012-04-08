
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

% @doc routing SIP Requests/Responses to clients or peers
-module(epbxd_sip_routing).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-export([send/3]).

-include("epbxd_sip.hrl").

%% @doc Send a SIP message to target
%%
%% @return
%%		ok              - message sent 
%%		{error, Reason} - fail to send message, with Reason reason
%%
%% @sample
%%		send(#sip_message{type=response,...}, Transport, Socket)
%%
-spec send(#sip_message{}, module(), inet:socket()|tuple()) -> atom().
send(Message, Transport, Socket) ->
	send(Transport:name(), cleanup(Message), Transport, Socket).

%% @doc Send SIP message through UDP transport
%%
%% Implement UDP response routing described in RFC 3261, section 18.2.2
%% and RFC 3263, section 5
%%
%% @private
%%
%%TODO: use pool of outgoing UDP sockets
%%TODO: response send fallbacks (others methods described in RFCs)
%%TODO: peer ip/host MUST be in Via received= parameter
-spec send(atom(), #sip_message{}, module(), tuple()) -> atom().
send(udp, Message, Transport, CSock={_,Host,_}) ->
	io:format(user, "~p~n", [Message]),
	Port = via_port(udp, Message),

	{ok, Sock} = gen_udp:open(0, [binary,{reuseaddr,true},{active,false}]),
	Ret = gen_udp:send(Sock, Host, Port, epbxd_sip_message:encode(Message)),
	gen_udp:close(Sock),

	Ret.


%% @doc Cleanup message to send
%%	
%% Remove headers with values eq 'undefined' atom
%%
%% @private
%% @sample
%%		#sip_message{headers=[{1,2}]} = cleanup(#sip_message{headers=[{1,	2},{3, undefined}]}).
%%
-spec cleanup(sip_message()) -> sip_message().
cleanup(Message=#sip_message{headers=H}) ->
	Message#sip_message{headers=lists:filter(fun({K, V}) -> V =/= undefined end, H)}.


%% @doc Get peer SIP port
%%
%% The port is taken from top-most Via URI is set, else we use default port 
%% for user protocol
%%
%% @private
%% @sample
%%		5060 = via_port(udp, #sip_message{headers=[{"Via", [#sip_via{}]}]}),
%%		  42 = via_port(udp, #sip_message{headers=[{"Via", [#sip_via{port=42}]}]}).
%%
-spec via_port(atom(), #sip_message{}) -> integer().
via_port(Proto, #sip_message{headers=Headers}) ->
	[Via|Tail] = proplists:get_value('Via', Headers),

	case Via#sip_via.port of
		undefined -> default_port(Proto);
		Port      -> Port
	end.

%% @doc Default port by protocol
%%
%% @private
%%
-spec default_port(atom()) -> integer().
default_port(udp) -> 5060;
default_port(tcp) -> 5060;
default_port(tls) -> 5061.

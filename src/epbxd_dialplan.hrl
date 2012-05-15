
-include("sips/epbxd_sip.hrl").

%-record(call_channel, {
%	type,                       %% sip, sccp, ...
%	from,
%	to
%}).

-type peer_type() :: 'sip'.
-type peer_stub() :: sip_stub().

-record(call_peer, {
	type = 'sip'       :: peer_type(),
	peer = undefined   :: undefined | peer_stub()
}).
-type call_peer() :: #call_peer{}.

-record(call_channel, {
	id     = {}        :: tuple(),
	% active application
	action = undefined :: undefined | atom(),

	source = undefined :: undefined | call_peer(),
	target = undefined :: undefined | call_peer()
}).
-type call_channel() :: #call_channel{}.

-define(CALLBACK(T), (epbxd_dialplan:callback(T))).

% outdated
-record(context, {caller, socket, message}).
-record(channel, {type, name}).

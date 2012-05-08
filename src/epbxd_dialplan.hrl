
-record(call_channel, {
	type,                       %% sip, sccp, ...
	from,
	to
}).

-record(call_context, {
	channel,
	source,
	request
}).

-define(CALLBACK(T), (epbxd_dialplan:callback(T))).

% outdated
-record(context, {caller, socket, message}).
-record(channel, {type, name}).

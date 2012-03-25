
-record(channel, {
	type,                       %% sip, sccp, ...
	name
}).

-record(context, {
		caller,
		socket,
		message
}).

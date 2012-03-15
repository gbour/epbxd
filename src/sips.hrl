
%% SIP server 
-record(server  , {
		iface=any,  %% interface name. eth0, vlan1, lo, any (all interfaces)
		proto=tcp,  %% one of tcp,udp,tls
		port =5060, %% service port
		socket      %% associated socket (once opened)
}).

%% a SIP message
-record(message , {
		version = "2.0", 
		type,                 %% request or response
		method,               %% request only. One of REGISTER, INVITE, BYE, ...
		uri,                  %% request only.
		status,               %% response only. status code (alà HTTP)
		reason,               %% response only.
		headers = dict:new(), %% message headers 
		content               %% message content
}).

-record(session , {callerid, seq}).
-record(endpoint, {id, ip, port}).


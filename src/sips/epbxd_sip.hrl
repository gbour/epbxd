
%% SIP server 
-record(server  , {
		iface=any,  %% interface name. eth0, vlan1, lo, any (all interfaces)
		proto=tcp,  %% one of tcp,udp,tls
		port =5060, %% service port
		socket      %% associated socket (once opened)
}).

%% a SIP message
-record(sip_message , {
		version = "2.0", 
		type,                 %% request or response
		method,               %% request only. One of REGISTER, INVITE, BYE, ...
		uri,                  %% request only.
		status,               %% response only. status code (alà HTTP)
		reason,               %% response only.
		headers = [],         %% message headers 
		payload               %% message content
}).
-type sip_message()   :: #sip_message{}. 

-record(session , {callerid, seq}).
%%-record(endpoint, {id, ip, port}).

%% account
-record(endpoint, {
		name,
		desc,
		password,
		domain,
		ssl                   %% true,false,ondemand
}).

%% endpoint registration record
-record(registration, {
		name,
%		proto = sip,iax,sccp
%		uri = contact uri
		uri,
		macaddr, 
% in uri
%		transport,
%		ip, 
%		port, 
		ua,
		timeout,              %% timeout datetime
		ping                  %% ping datetime
}).

%% To, From, Contact header
-record(sip_address, {
		displayname,
		uri,
		params      = []
}).

%% Via header
-record(sip_via, {
		transport,
		host,
		port,
		params = []
}).

%% SIP URI
-record(sip_uri, {
		scheme,               %% sip, sips, tel, ...
		user,
		password,
		host,
		port,
		params  = [],
		headers = []
}).

%% Transaction
-record(transaction, {
	key,

	% source
	s_cid,
	s_fromtag,
	s_totag,
	s_state,
	s_msg,
	s_uri,

	% destination
	d_cid,
	d_fromtag,
	d_totag,
	d_state,
	d_msg,
	d_uri
}).



% headers by order
-define(HEADERS_ORDER, [
		% Header         , use Request headers as datasource
		%                  (Custom -> Request? -> Default)
		{'Via'           , request},
		{'Max-Forwards'  , default},
		{'From'          , request},
		{'To'            , request},
		{'Call-ID'       , request},
		{'CSeq'          , request},
		{'Expires'       , default},
		{'Contact'       , request},
		{'Allow'         , default},
		{'Supported'     , default},
		{'Date'          , default},
		{'User-Agent'    , default}%,
%		{'Content-Type'  , default},
%		{'Content-Length', default}
	]).


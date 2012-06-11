
-ifndef(EPBXD_SIP_HRL).
-define(EPBXD_SIP_HRL, 1).

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
%% TODO: registration is not SIP specific
%%       SHOULD contains a type field ('sip', ...) and placed in a more generic place
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
-type sip_uri() :: #sip_uri{}.

%% Transaction
-record(transaction_old, {
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

-record(transaction, {
	key = {undefined,undefined},		% {Method, Branch}

	% RTT. Default to 500ms
	t1      = 500,
	timerA  = infinity,
	% 64*T1 == 32secs
	timerB  = 32000,
	%
	timerD  = 0,
	request = undefined,

	fsm     = undefined,
	fsmid   = undefined
}).

%% Dialog
-type sip_request()       :: 'INVITE' | 'BYE' | 'CANCEL' | 'OPTIONS'.
-type sip_dialog_origin() :: 'self' | 'peer'.
-type sip_dialog_status() :: 'Trying' | 'Ringing' | 'OK' | sip_request().

-record(sip_dialog, {
	% for now, we use Call-ID as dialog key
	%NOTE: may be completed with URI (id, host, port) AND to tag ?
	callid     = <<>>         :: binary(),

	% datas
	% who initiate the dialog
	origin     = 'self'       :: sip_dialog_origin(),
	% which request initiate the dialog
	request    = undefined    :: undefined | sip_request(),
	% current status
	status     = undefined    :: undefined | sip_dialog_status(),
	% dialog peer (my be target)
	peer       = undefined    :: undefined | sip_uri(),

	% timestamps (output of calendar:unilocal_time()
	created    = {}           :: tuple(),
	updated    = {}           :: tuple(),

	% backreference
	chanid     = undefined    :: undefined | integer()
}).
-type sip_dialog() :: #sip_dialog{}.

-record(sip_stub, {
	dialog    = undefined :: undefined | sip_dialog(),

	socket    = undefined,
	transport = undefined,
	ref       = undefined
}).
-type sip_stub() :: #sip_stub{}.

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

-endif.


-record(udpsock, {
	socket,
	acceptor,
	mapping=[],
	cache1=[],    % unaffected cache (no virtual "child socket")
	cache2=[]     % affected cache (virtual "child socket" created with accept() but not yet control process
}).


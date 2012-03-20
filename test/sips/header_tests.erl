
-module(header_tests).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
	%% From
	?assertEqual(header:decode("From", "foobar"), invalid),
	?assertEqual(header:decode("From"," \"101\" <sip:101@192.168.0.194:5060> ;tag=e2dafe4bb3"),
		{address,"101",{uri,"sip","101",[],"192.168.0.194","5060",[],[]},[{"tag","e2dafe4bb3"}]}),
	?assertEqual(header:decode("From"," <sip:101@192.168.0.194>"),
		{address,[],{uri,"sip","101",[],"192.168.0.194",[],[],[]},[]}),

	%% To
	?assertEqual(header:decode("To"," \"101\" <sip:101@192.168.0.194:5060>"),
		{address,"101",{uri,"sip","101",[],"192.168.0.194","5060",[],[]},[]}),

	%% Contact
	?assertEqual(header:decode("Contact"," \"101\" <sip:101@10.0.0.13:5060;transport=udp>;+sip.instance=\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\";expires=3600"),
		{address,"101",{uri,"sip","101",[],"10.0.0.13","5060",[{"transport","udp"}],[]},[{"+sip.instance","\"<urn:uuid:00000000-0000-1000-8000-00085D13F3F9>\""},{"expires","3600"}]}),

	%% CSeq
	?assertEqual(header:decode("Cseq", "100 INVITE"), {100, "INVITE"}),
	?assertEqual(header:decode("Cseq", "fail")      , invalid),
	?assertEqual(header:decode("Cseq", "zaz INVITE"), invalid),

	%% Max-Forward
	?assertEqual(header:decode("Max-forwards", "70")    , 70),
	?assertEqual(header:decode("Max-forwards", "foobar"), invalid),

	%% Content-Length
	?assertEqual(header:decode("Content-length", "1547")  , 1547),
	?assertEqual(header:decode("Content-length", "foobar"), invalid),

	ok.

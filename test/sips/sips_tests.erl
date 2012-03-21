
-module(sips_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sips/sips.hrl").


response100_test() ->
	ok.

response200_test() ->
	ok.

msgdecode_test() -> 
	%% content only
	?assertEqual(sips:msgdecode(header, #message{}, ["","foo","bar"]),
		{message,"2.0",undefined,undefined,undefined,undefined,undefined,dict:from_list([]),
			"foo\r\nbar"
	}),

	%% one header
	?assertEqual(sips:msgdecode(header, #message{}, ["Foo: Bar",""]), 
		{message,"2.0",undefined,undefined,undefined,undefined,undefined,
			dict:from_list([{"Foo",["Bar"]}]),[]
	}),

	%% several headers
	?assertEqual(sips:msgdecode(header, #message{}, [
				"CSeq: 100 INVITE",
				"Max-Forwards: 70",
				"",
				"v=0",
				"o=twinkle 1684153369 1202521382 IN IP4 192.168.0.187"]),
		{message,"2.0",undefined,undefined,undefined,undefined,undefined,
			dict:from_list([{"Cseq",[{100,"INVITE"}]},{"Max-forwards",[70]}]),
			"v=0\r\no=twinkle 1684153369 1202521382 IN IP4 192.168.0.187"
	}),

	%% same header twice
	?assertEqual(sips:msgdecode(header, #message{}, [
				"Via: Roma",
				"CSeq: 100 INVITE",
				"Via: Paris",
				"",
				"v=0",
				"o=twinkle 1684153369 1202521382 IN IP4 192.168.0.187"]),
		{message,"2.0",undefined,undefined,undefined,undefined,undefined,
			dict:from_list([{"Cseq",[{100,"INVITE"}]},{"Via",["Roma","Paris"]}]),
			"v=0\r\no=twinkle 1684153369 1202521382 IN IP4 192.168.0.187"
	}),

	%% full SIP request message
	?assertEqual(sips:msgdecode(start, #message{}, [
				"INVITE sip:101@192.168.0.194 SIP/2.0",
				"Via: Roma",
				"Max-Forwards: 70",
				"To: <sip:101@192.168.0.194>",
				"CSeq: 338 INVITE",
				"",
				"v=0",
				"o=twinkle 1684153369 1202521382 IN IP4 192.168.0.187"
			]),
		{message,"2.0",request,'INVITE',"sip:101@192.168.0.194",undefined,undefined,
			dict:from_list([
					{"Via",["Roma"]},
					{"Max-forwards",[70]},
					{"To",[{address,[],{uri,"sip","101",[],"192.168.0.194",[],[],[]},[]}]},
					{"Cseq",[{338,"INVITE"}]}
			]),
			"v=0\r\no=twinkle 1684153369 1202521382 IN IP4 192.168.0.187"
	}),

	%% full SIP response
	?assertEqual(sips:msgdecode(start, #message{}, [
				"SIP/2.0 100 Trying",
				"Via: SIP/2.0/UDP",
				"CSeq: 100 REGISTER",
				"User-Agent: Epbxd",
				"Content-Length: 0",
				""
			]),
		{message,"2.0",response,undefined,undefined,"100","Trying",
			dict:from_list([
					{"Via",["SIP/2.0/UDP"]},
					{"Cseq",[{100,"REGISTER"}]},
					{"User-agent",["Epbxd"]},
					{"Content-length",[0]}
			]),
			""
	}),


	ok.

msgencode_test() ->
	%%
	%% SIP Response
	%%
	?assertEqual(sips:msgencode(start, #message{
		type=response,
		status=200,
		reason="OK",
		%% using a proplist to keep order
		headers=[
				{"Via"           ,"SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKddnebypp"},
				{"From"          ,#address{displayname="104",
						uri=#uri{scheme="sip",user="104",host="192.168.0.194"},params=[{tag,"eyxie"}]
				}},
				{"To"            ,#address{displayname="104",
						uri=#uri{scheme="sip",user="104",host="192.168.0.194"},params=[{tag,"as0e9a3d03"}]
				}},
				{"Call-id"       ,"sapisdmefrjxwmp@bour.cc"},
				{"Cseq"          ,{100,"REGISTER"}},
				{"User-agent"    ,"Epbxd"},
				{"Allow"         ,"INVITE, ACK, CANCEL, OPTIONS, BYE, REFER, SUBSCRIBE,NOTIFY,INFO"},
				{"Supported"     ,"replaces"},
				{"Expires"       ,3600},
				{"Contact" 			 ,#address{uri=#uri{scheme=sip,user=104,host="192.168.0.187",port=5069},
						params=[{"expires",3600}]
				}},
				{"Date"          ,"Tue, 29 Nov 2011 10:10:35 GMT"},
				{"Content-length",0}
		]}),


		"SIP/2.0 200 OK\r\n"
		"Via: SIP/2.0/UDP 192.168.0.187:5069;branch=z9hG4bKddnebypp\r\n"
		"From: \"104\" <sip:104@192.168.0.194>;tag=eyxie\r\n"
		"To: \"104\" <sip:104@192.168.0.194>;tag=as0e9a3d03\r\n"
		"Call-ID: sapisdmefrjxwmp@bour.cc\r\n"
		"CSeq: 100 REGISTER\r\n"
		"User-Agent: Epbxd\r\n"
		"Allow: INVITE, ACK, CANCEL, OPTIONS, BYE, REFER, SUBSCRIBE,NOTIFY,INFO\r\n"
		"Supported: replaces\r\n"
		"Expires: 3600\r\n"
		"Contact: <sip:104@192.168.0.187:5069>;expires=3600\r\n"
		"Date: Tue, 29 Nov 2011 10:10:35 GMT\r\n"
		"Content-length: 0\r\n"
		"\r\n"
	),

	ok.


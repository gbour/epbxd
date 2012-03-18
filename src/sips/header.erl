%% 
%% Decode SIP headers value
%%
%%
-module(header).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([decode/2]).

-include("utils.hrl").
-include("sips.hrl").


%%
%% From, To, Contact headers
%%    From: "Bob" <sips:bob@biloxi.com> ;tag=a48s
%%
%% TODO: handle other following forms
%%    From: sip:+12125551212@phone2net.com;tag=887s
%%    From: Anonymous <sip:c8oqz84zk7z@privacy.org>;tag=hyh8
%%
decode("From", Value) ->
	case re:run(Value, "^\s*\"(?<display>[^\"]+)\"\s+<(?<uri>[^>]+)>\s*(?<params>.*)$",
		[{capture,[display,uri,params],list}]) of

		{match, [D,U,P]} ->
			#address{displayname=D,uri=uri:decode(U),params=uri:params(P)};
		_ ->
			invalid
	end;
decode("To", Value) ->
	decode("From", Value);
decode("Contact", Value) ->
	decode("From", Value);

%%
%% CSeq header
%%		CSeq: 100 REGISTER
%%
decode("CSeq", Value) ->
	try
		[Number, Method] = string:tokens(Value, " "),
		{list_to_integer(Number), Method}
	of
		Ret   -> Ret
	catch
		_:_   -> invalid
	end;

%%decode("CSeq", Value) ->
%%	[Number, Method] = string:tokens(Value, " "),
%%	{list_to_integer(Number), Method};

%%
%% Other headers
%%   keep original value
decode(_,V) ->
	V.

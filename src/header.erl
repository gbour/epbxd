%% 
%% Decode SIP headers value
%%
%%
-module(header).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([decode/2, encode/2]).

-include("utils.hrl").
-include("sips.hrl").

%% From: sip:+12125551212@phone2net.com;tag=887s
%% From: Anonymous <sip:c8oqz84zk7z@privacy.org>;tag=hyh8
decode("From", Value) ->
	%% From: "Bob" <sips:bob@biloxi.com>;tag=a48s
	case re:run(Value, "^\s*\"([^\"]+)\"\s+<([^:]+):([^;>]+)(;transport=[^>]+)?>\s*(;tag=.*)?$",[{capture,all,list}]) of
		{match, [_, Display, Scheme, Username]} ->
			#uri{displayname=Display, scheme=Scheme, username=Username};
	 	{match, [_, Display, Scheme, Username, ";transport="++Transport]} ->
			#uri{displayname=Display, scheme=Scheme, username=Username, transport=Transport};
	 	{match, [_, Display, Scheme, Username, ";transport="++Transport,";tag="++Tag]} ->
			#uri{displayname=Display, scheme=Scheme, username=Username, transport=Transport, tag=Tag};
		{match, [_, Display, Scheme, Username, [], ";tag="++Tag]} ->
			#uri{displayname=Display, scheme=Scheme, username=Username, tag=Tag};
		_ ->
			error
	end;
decode("To", Value) ->
	decode("From", Value);

decode("CSeq", Value) ->
	case re:run(Value, "^(\\d+) (.*)$", [{capture,all,list}]) of
		{match, [_, Sequence, Method]} ->
			{list_to_integer(Sequence), Method};
		_ ->
			error
	end;

decode(_,V) ->
	?DEBUG("NOT MATCH",[]),
	V.



encode("CSeq", {Sequence, Method}) ->
	io_lib:fwrite("~b ~s", [Sequence, Method]);
encode("Allow", _) ->
	"INVITE, ACK, CANCEL, BYE".


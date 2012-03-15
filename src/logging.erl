
%%
%% we cannot deactivace log level at the moment
%% 2 ways of doing this:
%%   - dynamic code compilation (like in ejabberd)
%%     (dynamic_compile:from_string/1 + code:load_binary/3)
%%   - building our own gen_server (based on disk_log)

-module(logging).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([init/1, log/3]).

init(Filename) ->
	%%io:format("~p~n", [Filename]),
	%% disable stdout printing
	error_logger:tty(false),
	%% log to filename
	error_logger:logfile({open, Filename}).


loglevel(Level) ->
	false.

log(fatal, Format, Data) ->
	error_logger:error_msg("[fatal] "++Format, Data);
log(error, Format, Data) ->
	error_logger:error_msg(Format, Data);
log(warning, Format, Data) ->
	error_logger:warning_msg(Format, Data);
log(notice, Format, Data) ->
	error_logger:info_msg("[notice] "++Format, Data);
log(info, Format, Data) ->
	error_logger:info_msg(Format, Data);
log(debug, Format, Data) ->
	error_logger:info_msg("[debug] "++Format, Data);
log(_,Format, Data) ->
	false.



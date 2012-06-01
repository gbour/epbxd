
%%
%% we cannot deactivace log level at the moment
%% 2 ways of doing this:
%%   - dynamic code compilation (like in ejabberd)
%%     (dynamic_compile:from_string/1 + code:load_binary/3)
%%   - building our own gen_server (based on disk_log)

-module(logging).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([init/1, log/3]).

init(_Opts) ->
	epbxd_hooks:new(log, []).

loglevel(Level) ->
	false.

log(Level, Format, Data) ->
	epbxd_hooks:run(log, {Level,Format,Data}).

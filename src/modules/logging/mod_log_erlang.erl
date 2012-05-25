
%%	epbxd, Erlang PBX Server
%%	Copyright (C) 2012, Guillaume Bour <guillaume@bour.cc>
%%
%%	This program is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU Affero General Public License as
%%	published by the Free Software Foundation, either version 3 of the
%%	License, or (at your option) any later version.
%%
%%	This program is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU Affero General Public License for more details.
%%
%%	You should have received a copy of the GNU Affero General Public License
%%	along with this program.  If not, see <http://www.gnu.org/licenses/>.

% @doc Erlang based logger
-module(mod_log_erlang).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([log/4]).
% gen_epbxd_module
-export([start/1, stop/0]).

-include("utils.hrl").

%% @doc Start module
%%
%% Initialize module environment
%% Install SIP REGISTER request hook
%%
-spec start(any()) -> ok|fail.
start(Opts) ->
	error_logger:tty(false),
	error_logger:logfile({open, proplists:get_value(filename, Opts)}),

	% registering hooks
	epbxd_hooks:add(log, proplists:get_value(priority, Opts), {?MODULE, log}, Opts),
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del(log, {?MODULE, register}),
	ok.

%% @doc 
%%
-spec log(tuple(), tuple(), any(), list()) -> tuple(ok, any()).
log({log, _Priority}, {Level, Format, Data}, State, _) ->
	log_(Level, Format, Data),
	{next, State}.

-spec log_(atom(), string(), list(any())) -> ok|false.
log_(fatal  , Format, Data) ->
	error_logger:error_msg("[fatal] "++Format, Data);
log_(error  , Format, Data) ->
	error_logger:error_msg(Format, Data);
log_(warning, Format, Data) ->
	error_logger:warning_msg(Format, Data);
log_(notice , Format, Data) ->
	error_logger:info_msg("[notice] "++Format, Data);
log_(info   , Format, Data) ->
	error_logger:info_msg(Format, Data);
log_(debug  , Format, Data) ->
	error_logger:info_msg("[debug] "++Format, Data);
log_(_      , _Format, _Data) ->
	false.



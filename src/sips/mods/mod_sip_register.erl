
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

% @doc Handle SIP REGISTER requests
-module(mod_sip_register).
-author("Guillaume Bour <guillaume@bour.cc>").
%-behaviour(gen_epbxd_module).

% API
% hooks
-export([register/4]).
% gen_epbxd_module
-export([start/1, stop/0]).

-include("utils.hrl").
-include("sips/epbxd_sip.hrl").

%% @doc Start module
%%
%% Initialize module environment
%% Install SIP REGISTER request hook
%%
-spec start(list(any())) -> ok|fail.
start(Opts) ->
	%% create mnesia tables
	mnesia:create_table(registrations, 
		[{attributes,record_info(fields,registration)},{record_name,registration}]),
	mnesia:create_table(endpoints, [{attributes,record_info(fields,endpoint)},{record_name,endpoint}]),
	lists:foreach(fun(EP) ->
				mnesia:dirty_write(endpoints, #endpoint{name=proplists:get_value(name,EP)})
		end, config:get(endpoints)
	),

	% registering hooks
	epbxd_hooks:new({sip,request,'REGISTER'}, []),
	epbxd_hooks:add({sip,request,'REGISTER'}, {?MODULE, register}, Opts),
	ok.

%% @doc Stop module
%%
%% Uninstall hooks
%%
-spec stop() -> ok|fail.
stop() ->
	epbxd_hooks:del({sip, request, 'REGISTER'}, {?MODULE, register}),
	ok.

%% @doc SIP REGISTER request hook
%%
%% Implement process described in RFC 3261, section 10.3
%%
%% Send 100 Trying provisional response
%% Search User (To header) in endpoints Mnesia table
%%		if found
%%			insert User in registrations table
%%			send 200 response
%%		if not found, send 404 response
%%
%%TODO: Provisional response must be optional (RFC strict compliance)
%%TODO: registration: set expiration date (current datetime + expires value)
%%TODO: expire must be configurable (global + per endpoint)
%%TODO: compliance with RFC 3261#10.3
%%TODO: implement authentication
-spec register(tuple(), tuple(), any(), list()) -> tuple(ok, any()).
register(_, Args={Request=#sip_message{headers=Headers}, Sock, Transport}, State, _) ->
	epbxd_sip_routing:send(
		epbxd_sip_message:response(trying, Request),
		Transport, Sock
	),

	% lookup caller. Is he registered ?
	User = (proplists:get_value('To', Headers))#sip_address.uri#sip_uri.user,
	?DEBUG("SIP:REGISTER= loopkup ~s endpoint", [User]),

	% default registration expiry (in seconds)
	Expires = 3600,

	Response = on_auth(epbxd_hooks:run(authent, {User, undefined, undefined}), Args, User),
	epbxd_sip_routing:send(Response, Transport, Sock),

	{next, State}.

%% User found: 
%%   register user context
%%   send 200/OK response
%%
on_auth({match, Endpt, {_, {Mod,Fun}}}, {Request=#sip_message{headers=Headers}, Sock, Transport}, User) ->
	logging:log(debug, "Found endpoint (in ~p module): ~p", [Mod, Endpt]),

	Contact = proplists:get_value('Contact', Headers),
	mnesia:dirty_write(registrations,#registration{name=User,	uri=Contact#sip_address.uri}),

	epbxd_sip_message:response(ok, Request, [{'Expires', 3600}]);

%% User not found.
%% Send 404/not found response
%%
on_auth({nomatch, _}, {Request,_,_}, _User) ->
	logging:log(debug, "Endpoint not found. Returning 404", []),

	epbxd_sip_message:response('not-found', Request).


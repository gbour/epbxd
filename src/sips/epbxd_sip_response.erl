
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
-module(epbxd_sip_response).
-author("Guillaume Bour <guillaume@bour.cc>").

% API
-export([gen/2]).
% DEBUG
-ifdef(debug).
	-export([to/2]).
-endif.

-include("sips/epbxd_sip.hrl").

%% @doc Generate SIP response messages
%%
%% @sample
%%		Response = epbxd_sip_response:gen(trying, Request).
%%
% 1xx provisional responses
-spec gen(atom(), #sip_message{}) -> #sip_message{}.
gen(trying , Req) ->
	gen(provisional_, Req, 100, "Trying");
gen(ringing, Req) ->
	gen(provisional_, Req, 180, "Ringing").

%% @doc Generate provisional response (1xx)
%%
-spec gen(atom(), #sip_message{}, integer(), string()) -> #sip_message{}.
gen(provisional_, Req=#sip_message{headers=Headers}, Status, Reason) ->
	#sip_message{
		type    = response,
		status  = Status,
		reason  = Reason,
		headers = [
			{'Via'            , proplists:get_value('Via'    , Headers)},
			{'From'           , proplists:get_value('From'   , Headers)},
			{'To'             , to(proplists:get_value('To', Headers), Status)},
			{'Call-ID'        , proplists:get_value('Call-ID', Headers)},
			{'CSeq'           , proplists:get_value('CSeq'   , Headers)},
			{'User-Agent'     , header('User-Agent' )},
			{'Content-Length' , 0}
		]
	}.

%% @doc Append tag params in To header except for "100 Trying" provisional response
%%
%% *tag* param is not added for 100/Trying provisional response
%% For all other response, a *tag* param is added, unless already set
%%
%% %sample
%%		#sip_address{params=[]}               = epbxd_sip_response:to(#sip_address{params=[]} 100)
%%		#sip_address{params=[{"tag","1234"}]} = epbxd_sip_response:to(#sip_address{params=[]} 180)
%%
-spec to(#sip_address{}, integer()) -> #sip_address{}.
to(To, 100) ->
	To;
to(To=#sip_address{params=Params}, _)   ->
	case proplists:is_defined("tag", Params) of 
		true  ->
			To;
		false ->
			To#sip_address{params=[{"tag", epbxd_sip_header:tag()} | Params]}
	end.


header('Allow')      ->
	false;
header('Supported')  ->
	false;
header('User-Agent') ->
	"Epbxd".


-module(epbxd_sup).
-author("Guillaume Bour <guillaume@bour.cc>").

-behaviour(supervisor).
-export([start_link/0, init/1]).

-include("utils.hrl").

% This is top supervisor
%	We currently start no services, as we use cowboy handlers,
% but we need all the same a supervisor for application return value.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}.
%	{ok, {
%			{one_for_one, 10, 60000},
%			[{sips_id,
%					{sips, start_link, [a,b,c]},
%					permanent,
%					5000,
%					worker,
%					[sips]
%			}]
%	}}.


-module(ws).
-export([init/3,handle/2]).

-include("http.hrl").
init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.


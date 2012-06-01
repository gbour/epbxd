
-module(mod_authent_file_tests).
-include_lib("eunit/include/eunit.hrl").
-include("sips/epbxd_sip.hrl").

-export([up/0, down/0, tests/1]).

up() ->
	%NOTE: mocking up native module fails
%	meck:new(file, [unstick,passthrough]),
%    meck:expect(file, read_file, fun(Filename) ->
%		case Filename of
%			% the good one
%			"/tmp/foobar" ->
%				<<"bob::\nmary:acme:rT5*p">>;
%
%			% not given as option
%			undefined -> 
%				{error, not_defined};
%
%			_ ->
%				{error, undefined}	
%		end
%    end).
	file:write_file("/tmp/authent_file.test", <<"bob::\nmary:acme:rT5*p">>).

down() ->
    %meck:unload(file).
	file:delete("/tmp/authent_file.test").

tests(_) ->
	ok.

mod_authent_file_test_() ->
    {setup, local,
        fun() ->
            ok
        end,
        fun(_) ->
            [
                {"Text file based authentication", 
					test_authent_generic:authent_unit_test(?MODULE, "file", [{filename,"/tmp/authent_file.test"},{mode,clear}])
				}
            ]
        end
    }.

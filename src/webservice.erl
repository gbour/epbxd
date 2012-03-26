
-module(webservice).
-export([init/3, handle/2, terminate/2,sip_users/2,sip_registrations/2]).

-include("http.hrl").
-include("jsonerl.hrl").
-include("stdlib-1.18/include/qlc.hrl").
-include("sips/sips.hrl").

init({tcp, http}, Req, Opts) ->
	{ok, Req, undefined_state}.

%handle(Req=#http_req{method='GET',path=[<<"foo">>,Bar]}, State) -> 
%	io:format("GET request ~p~n",[Req#http_req.path]),
%	{ok, Req, State};

handle(Req=#http_req{method=M,path=[Context, Object], buffer=Body}, State) ->
	io:format("plop ~p_~p~n",[Context,Object]),
	{ok, Resp} = try 
		apply(webservice,binary_to_atom(<<Context/binary,"_",Object/binary>>, latin1), [M,Body]) 
	of
		D -> 
			io:format("foo ~p~n",[D]),
			cowboy_http_req:reply(200, [], jsonerl:encode(D), Req)
	catch
		Exception:Reason -> 
			io:format("~p:~p~n", [Exception, Reason]),
			cowboy_http_req:reply(404,Req)
	end,

	{ok, Resp, State};

handle(Req=#http_req{method=M,path=[Context, Object, Action], buffer=Body}, State) ->
	io:format("Unhandled: ~s:~s:~s~n",[Context, Object, Action]),
	{ok, Req, State}; %dispatch(M, Context, Object, Action, Body), State}.

handle(Req, State) ->
	{ok, cowboy_http_req:reply(404,Req), State}.

terminate(Req, State) ->
	ok.


sip_users('GET', Body) ->
	T = fun() ->
			qlc:eval(qlc:q([E || E <- mnesia:table(endpoints)]))
	end,
	{atomic, Res} = mnesia:transaction(T),
	lists:map(
		fun(E) -> 
				?record_to_struct(endpoint,	E#endpoint{name=list_to_atom(E#endpoint.name)}) 
		end, Res
	).

sip_registrations('GET',Body) ->
	{atomic, Res} = mnesia:transaction(
		fun() ->
			qlc:eval(qlc:q([E || E <- mnesia:table(registrations)]))
		end
	),

	lists:map(
		fun(R) ->
			?record_to_struct(registration, R#registration{name=list_to_atom(R#registration.name)})
		end, Res
	).

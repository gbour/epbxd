
-define(DEBUG(Fmt, Args), 
	logging:log(debug  , lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(INFO(Fmt, Args), 
	logging:log(info   , lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(NOTICE(Fmt, Args), 
	logging:log(notice , lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(WARNING(Fmt, Args), 
	logging:log(warning, lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(ERROR(Fmt, Args), 
	logging:log(error  , lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).


-define(L2B(List), erlang:list_to_binary(List)).
-define(I2B(Int) , erlang:list_to_binary(erlang:integer_to_list(Int))).
-define(A2B(Atom), erlang:atom_to_binary(Atom, latin1)).

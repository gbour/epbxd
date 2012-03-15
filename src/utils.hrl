
-define(DEBUG(Fmt, Args), 
	logging:log(debug, lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(INFO(Fmt, Args), 
	logging:log(info, lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(NOTICE(Fmt, Args), 
	logging:log(notice, lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(WARNING(Fmt, Args), 
	logging:log(warning, lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).
-define(ERROR(Fmt, Args), 
	logging:log(error, lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]), Args)).


-define(DEBUG(Fmt, Args), 
	_Fmt = lists:concat(["(", ?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]),
	logging:log(debug, _Fmt, Args)).
-define(INFO(Fmt, Args), 
	_Fmt = lists:concat([?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]),
	logging:log(info, _Fmt, Args)).
-define(NOTICE(Fmt, Args), 
	_Fmt = lists:concat([?MODULE, ":", ?LINE, "):: ", Fmt, "~n"]),
	logging:log(notice, _Fmt, Args)).
-define(WARNING(Fmt, Args), 
	_Fmt = lists:concat([?MODULE, ":", ?LINE ,"):: ", Fmt, "~n"]),
	logging:log(warning, _Fmt, Args)).
-define(ERROR(Fmt, Args), 
	_Fmt = lists:concat([?MODULE, ":", ?LINE ,"):: ", Fmt, "~n"]),
	logging:log(error, _Fmt, Args)).

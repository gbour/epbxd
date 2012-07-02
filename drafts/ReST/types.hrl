
% epp:parse_file("types.hrl",["./"],[]).
%
-record(user, {
	id     = undefined        :: undefined|integer(),
	name   = 'foo'            :: string(),

	groups = list()           :: list(#group{})
}).
-type user() :: #user{}.

-record(group, {
	id     = undefined        :: undefined|integer(),
	name   = undefined        :: undefined|string(),
%	foo :: integer(),

	users  = []               :: list(user())
}).


-record(sample, {
	%field1 = 1,
	field2 = 2 :: integer(),
	field3 = 3 :: undefined|integer(),
	field4 = 4 :: integer()|undefined,

	field5 = true      :: boolean(),
	field6 = <<"foo">> :: string(),
	% list of integers
	field7 = []        :: list(integer()),
	field8 = []        :: [integer()],
	% dict
	field10 = {}        :: dict(integer())
}).

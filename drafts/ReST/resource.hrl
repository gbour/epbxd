
-record(field, {
	name :: atom(),
	type :: atom(),
	default :: any(),
	required = true :: boolean(),
	unique = false :: boolean(),
	desc :: binary()	
}).

-record(resource, {
	name   :: atom(),
	record :: undefined|atom(),
	key    :: atom()|list(atom()),

	fields :: list(#field{})
}).

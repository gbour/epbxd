{application, epbxd, [
	{description, "IPBX made with Erlang"},
	{vsn, "1.0.0"},
	{modules, [config, logging]},
	{applications, [kernel, stdlib]},
	{env, [
		{config, "/etc/epbxd.cfg"}
	]},
	{mod, {epbxd, []}}
]}.

{application, epbxd, [
	{description, "ejabberd-based IPBX"},
	{vsn, "1.0.0"},
	{modules, [config, logging]},
	{applications, [kernel, stdlib]},
	{env, [
		%%{config, "../etc/epbx.cfg"}
		{config, "/tmp/epbx.cfg"}
	]},
	{mod, {epbxd, []}}
]}.

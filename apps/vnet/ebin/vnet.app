{application, 'vnet', [
	{description, "Virtue Network Component"},
	{vsn, "0.1.0"},
	{modules, ['vnet_app','vnet_sup']},
	{registered, [vnet_sup]},
	{applications, [kernel,stdlib]},
	{mod, {vnet_app, []}},
	{env, []}
]}.
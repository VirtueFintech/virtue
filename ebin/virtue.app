{application, 'virtue', [
	{description, "Virtue Trust Coin"},
	{vsn, "0.1.0"},
	{modules, ['virtue_app','virtue_sup']},
	{registered, [virtue_sup]},
	{applications, [kernel,stdlib]},
	{mod, {virtue_app, []}},
	{env, []}
]}.
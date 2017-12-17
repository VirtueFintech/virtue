{application, 'vkey', [
	{description, "Virtue User Addressing"},
	{vsn, "0.1.0"},
	{modules, ['vkey_app','vkey_sup']},
	{registered, [vkey_sup]},
	{applications, [kernel,stdlib,base58]},
	{mod, {vkey_app, []}},
	{env, []}
]}.
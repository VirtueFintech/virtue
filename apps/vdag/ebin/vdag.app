{application, 'vdag', [
	{description, "Virtue DAG Module"},
	{vsn, "0.1.0"},
	{modules, ['vdag_app','vdag_sup']},
	{registered, [vdag_sup]},
	{applications, [kernel,stdlib]},
	{mod, {vdag_app, []}},
	{env, []}
]}.
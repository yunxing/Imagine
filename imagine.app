{application, imagine,
[
	{description, "Imagine"},
	{vsn, "0.1.3"},
	{modules, [
		imagine,
		imagine_sup,
		imagine_app,
		room,
		remote,
		roomManager,
		utility_server,
		gmail,
		misultin,
		misultin_acceptor,
		misultin_acceptors_sup,
		misultin_http,
		misultin_req,
		misultin_server,
		misultin_socket,
		misultin_utility,
		misultin_websocket,
		misultin_ws
	]},
	{registered, []},
	{mod, {imagine_app, []}},
	{env, []},
	{applications, [kernel, stdlib]}
]}.
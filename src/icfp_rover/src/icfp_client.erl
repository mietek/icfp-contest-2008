-module(icfp_client).
-behavior(application).
-export([start/2, stop/1]).
-export([connect/0, connect/2, set_controller/1, stop_trial/0]).
-export([command/1, commands/1]).

start(_, _) ->
	{ok, Host} = application:get_env(icfp_rover, host),
	{ok, Port} = application:get_env(icfp_rover, port),
	icfp_client_sup:start_link(Host, Port).

stop(_) ->
	ok.

connect() ->
	icfp_client_proc:connect().

connect(Host, Port) ->
	icfp_client_proc:connect(Host, Port).

set_controller(Controller) when is_function(Controller) ->
	icfp_client_proc:set_controller(Controller),
	icfp_motor:set_controller(Controller).

stop_trial() ->
	exit(whereis(icfp_client_proc), kill).	

command(Cmd) ->
	icfp_client_proc:send(Cmd).

commands(Cmds) ->
	lists:map(fun command/1, Cmds).

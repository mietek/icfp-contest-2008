-module(icfp_client_sup).
-behavior(supervisor).
-export([start_link/2]).
-export([init/1]).

start_link(Host, Port) ->
	supervisor:start_link(?MODULE, [Host, Port]).

init([Host, Port]) ->
	Event = {event, {icfp_event, start_link, []},
		transient, 2000, worker, [icfp_event]},
	Client = {client, {icfp_client_proc, start_link, [Host, Port]},
		transient, 2000, worker, [icfp_client_proc]},
	Motion = {motion, {icfp_motion, start_link, []},
		transient, 2000, worker, [icfp_motion]},
	Cont = {cont, {icfp_controller, start_link, []},
		transient, 2000, worker, [icfp_controller]},
	{ok, {{one_for_all, 1, 10}, [Event, Client, Motion, Cont]}}.

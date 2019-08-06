-module(icfp_controller).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([idle/2, in_motion/2]).
-export([init/1, terminate/3]).
-export([handle_event/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3]).
-export([code_change/4]).

-record(state, {map_x, map_y, rel_map_offset,  target = {0,0}, path = []}).

start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init(event) ->
	{ok, {0,0}};
init([]) ->
	ets:new(?MODULE, [ordered_set, named_table, public]),
	ok = icfp_event:add_handler(?MODULE, event),
	{ok, idle, #state{}}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

handle_info(_, StateName, State) ->
	{next_state, StateName, State}.

idle({init, MapSizeX, MapSizeY, RelOffset}, State) ->
	Target = icfp_lib:conv_coords(State#state.target, RelOffset),
	icfp_motion:set_waypoint(Target),
	{next_state, in_motion, State#state{
		rel_map_offset = RelOffset,
		map_x = MapSizeX, 
		map_y = MapSizeY, 
		path = [Target]}
	};
idle(E, State) ->
	io:format("IDLE: ~p~n", [E]),
	{next_state, idle, State}.

% in_motion(waypoint_reached, State) ->
%   case State#state.path of
%       [OldWP,NextWP|Path] ->
%           io:format("Reached WayPoint ~p -> ~p~n", [OldWP, NextWP]),
%           icfp_motion:set_waypoint(NextWP),
%           {next_state, in_motion, State#state{path = [NextWP|Path]}};
%       [Target] ->
%           io:format("Reached Target ~p~n", [Target]),
%           {next_state, in_motion, State#state{path = []}}
%   end;
% in_motion(world_update, State) ->
%   CurPos = icfp_motion:current_pos(),
%   TargetPos = hd(State#state.path),
%   case obstructed(CurPos, TargetPos) of
%       false ->    
%           {next_state, in_motion, State};
%       Object ->
%           io:format("Should detour ~p~n", [Object]),
%           {next_state, in_motion, State}
%   end;
in_motion(boulder, State) ->
	Target = icfp_lib:conv_coords(
		State#state.target,
		State#state.rel_map_offset
	),
	icfp_motion:set_waypoint(Target),
	{next_state, in_motion, State#state{path = [Target]}};
in_motion(restart, State) ->
	Target = icfp_lib:conv_coords(
		State#state.target,
		State#state.rel_map_offset
	),
	icfp_motion:set_waypoint(Target),
	{next_state, in_motion, State#state{path = [Target]}};
in_motion(_, State) ->
	{next_state, in_motion, State}.

terminate(_, _, _State) ->
	ets:delete(?MODULE),
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%% Gen event
handle_event({i, _, {MapSizeX, MapSizeY}, _, _}, _) ->
	RelOffset = {MapSizeX / 2, MapSizeY / 2},
	gen_fsm:send_event(?MODULE, {init, MapSizeX, MapSizeY, RelOffset}),
	{ok, RelOffset};
handle_event({t, _, _, {CurX, CurY, Dir, _}, Objs}, RelOffset) ->
    Pos = {CurX, CurY},
    NewPos = icfp_field:get_next_pos(Pos, icfp_lib:deg_to_rad(Dir), Objs),
    if
        NewPos /= Pos ->
            icfp_motion:set_waypoint(icfp_lib:conv_coords(NewPos, RelOffset));
        true ->
            ok
    end,
    % F = fun({T, X, Y, R}) ->
    %   {icfp_lib:conv_coords({X, Y}, RelOffset), R, T}
    % end,
    % ets:insert(?MODULE, [F(O) || O <- Objs, element(1, O) /= m]),
	gen_fsm:send_event(?MODULE, world_update),
	{ok, RelOffset};
handle_event({b, _}, State) ->
	gen_fsm:send_event(?MODULE, boulder),
	{ok, State};
handle_event({e, _, _}, State) ->
	gen_fsm:send_event(?MODULE, restart),
	{ok, State};
handle_event({motion, waypoint_reached}, State) ->
	gen_fsm:send_event(?MODULE, waypoint_reached),
	{ok, State};
handle_event(_, State) ->
	{ok, State}.

is_unobstructed(CurrentPos, TargetPos, ObjPos) ->
	MyAng = icfp_lib:car_ang(CurrentPos, TargetPos),
	ObjAng = icfp_lib:car_ang(ObjPos, TargetPos),
	abs(MyAng - ObjAng) > 0.1.

obstructed(CurPos, TargetPos) ->
	ets:safe_fixtable(?MODULE, false),
	Result = (catch obstructed_loop(CurPos, TargetPos, ets:first(?MODULE))),
	ets:safe_fixtable(?MODULE, false),
	Result.

obstructed_loop(_CurPos, _TargetPos, '$end_of_table') ->
	false;
obstructed_loop(CurPos, TargetPos, ObjPos) ->
	case is_unobstructed(CurPos, TargetPos, ObjPos) of
		true -> obstructed_loop(CurPos, TargetPos, ets:next(?MODULE, ObjPos));
		false -> hd(ets:lookup(?MODULE, ObjPos))
	end.


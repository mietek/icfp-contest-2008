-module(icfp_motion).
-behaviour(gen_fsm).
-export([start_link/0]).
-export([current_pos/0, set_waypoint/1]).
-export([handle_event/2]).
-export([idle/2, action/2]).
-export([init/1, terminate/3]).
-export([handle_event/3, handle_sync_event/4, handle_info/3]).
-export([code_change/4]).

-import(icfp_lib, [deg_to_rad/1, rad_to_deg/1]).

-define(CTRL_LEFT(CTRL),        (hd(tl(CTRL)) == $l)).
-define(CTRL_HARD_LEFT(CTRL),   (hd(tl(CTRL)) == $L)).
-define(CTRL_RIGHT(CTRL),       (hd(tl(CTRL)) == $r)).
-define(CTRL_HARD_RIGHT(CTRL),  (hd(tl(CTRL)) == $R)).
-define(CTRL_FWD(CTRL),         (hd(CTRL) == $a)).
-define(CTRL_BREAK(CTRL),       (hd(CTRL) == $b)).
-define(TURN_HARD(CUR, TARGET), (abs(CUR - TARGET) > 20)).
-define(BREAK(DIST),            (abs(DIST) < 10)).
-define(TURNING(CTRL),          ((hd(tl(CTRL)) == $l) or (hd(tl(CTRL)) == $L) or
									(hd(tl(CTRL)) == $r) or (hd(tl(CTRL)) == $R))).

-record(state, {
	pos,					%% Current position of the Rover
	waypoint,				%% Target Waypoint
	last_correction = now()	%% Last corrctive manouver
}).

start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

set_waypoint(WP) ->
	gen_fsm:send_event(?MODULE, {set_waypoint, WP}).

current_pos() ->
	gen_fsm:sync_send_all_state_event(?MODULE, current_pos).

%% Gen Event init (ugly)
init(event) ->
	{ok, {0, 0}};
%% Gen FSM init
init([]) ->
	ok = icfp_event:add_handler(?MODULE, event),
	{ok, idle, #state{}}.

idle({set_waypoint, WP}, State) ->
	{next_state, action, State#state{last_correction = {0,0,0}, waypoint = WP}};
idle({update, _, CurPos, _}, State) ->
	{next_state, idle, State#state{pos = CurPos}};
idle(_, State) ->
	{next_state, idle, State}.

action({set_waypoint, WP}, State) ->
	{next_state, action, State#state{last_correction = {0,0,0}, waypoint = WP}};
action({update, CurCtrl, CurPos, CurAng}, State) ->
	TDiff = abs(timer:now_diff(now(), State#state.last_correction)),
	case is_waypoint_reached(CurPos, State#state.waypoint) of
		true -> 
			stop(CurCtrl),
			NextState = action,
			icfp_event:notify({motion, waypoint_reached}),
			{next_state, action, State#state{pos = CurPos}};
		false when TDiff > 500 ->
			correct_path(CurCtrl, CurPos, CurAng, State#state.waypoint),
			{next_state, action, State#state{pos = CurPos, last_correction = now()}};
		false ->
			{next_state, action, State#state{pos = CurPos}}
	end;
action(_, State) ->
	{next_state, action, State}.

handle_sync_event(current_pos, _From, StateName, State) ->
	{reply, State#state.pos, StateName, State};
handle_sync_event(_, _From, StateName, State) ->
	{reply, ok, StateName, State}.

handle_event(_, StateName, State) ->
	{next_state, StateName, State}.

handle_info(_, StateName, State) ->
	{next_state, StateName, State}.

terminate(_, _, _) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%% Gen event
handle_event({t, _T, Ctrl, {CurX, CurY, CurV, _CurS}, _}, RelOffset) ->
	gen_fsm:send_event(?MODULE, {update,
		atom_to_list(Ctrl), 
		icfp_lib:conv_coords({CurX, CurY}, RelOffset),
		CurV
	}),
	{ok, RelOffset};
handle_event({i, _, {MapSizeX, MapSizeY}, _, _}, _) ->
	{ok, {MapSizeX / 2, MapSizeY / 2}};
handle_event(_, RelOffset) ->
	{ok, RelOffset}.

is_waypoint_reached({CurX, CurY}, {TargetX, TargetY}) ->
	abs(TargetX - CurX) < 5 andalso  abs(TargetY - CurY) < 5.

correct_path(CurCtrl, CurPos, CurAng, WayPoint) ->
	TargetAngle = icfp_lib:car_ang(CurPos, WayPoint),
	TargetDist = icfp_lib:car_dist(CurPos, WayPoint),
	RadCurAng = deg_to_rad(CurAng),
	if
		abs(RadCurAng - TargetAngle) > 0.04, TargetDist > 0 ->
			turn(CurCtrl, RadCurAng, TargetAngle),
			forward(CurCtrl, TargetDist),
			action;
		abs(RadCurAng - TargetAngle) > 0.04 ->
			turn(CurCtrl, RadCurAng, TargetAngle),
			action;
		TargetDist > 0 ->
			forward(CurCtrl, TargetDist),
			action;
		true ->
			action
	end.

turn(CurCtrl, CurAng, TargetAng) ->
	Dest = 
		math:cos(TargetAng) * math:sin(CurAng) -
		math:cos(CurAng) * math:sin(TargetAng),
	if
		Dest > 0.45 -> turn(CurCtrl, hard_right);
		Dest > 0.0 -> turn(CurCtrl, right);
		Dest < -0.45 -> turn(CurCtrl, hard_left);
		Dest < 0.0 -> turn(CurCtrl, left);
		true       -> turn(CurCtrl, straight)
	end.

forward([C|_], TargetDist) ->
	icfp_client:commands([a]).

turn([_, $R], hard_right) -> ok;
turn([_, $r], hard_right) -> icfp_client:commands([r]);
turn([_, $-], hard_right) -> icfp_client:commands([r,r]);
turn([_, $l], hard_right) -> icfp_client:commands([r,r,r]);
turn([_, $L], hard_right) -> icfp_client:commands([r,r,r,r]);
turn([_, $R], right) -> icfp_client:commands([l]);
turn([_, $r], right) -> ok;
turn([_, $-], right) -> icfp_client:commands([r]);
turn([_, $l], right) -> icfp_client:commands([r,r]);
turn([_, $L], right) -> icfp_client:commands([r,r,r]);
turn([_, $R], straight) -> icfp_client:commands([l,l]);
turn([_, $r], straight) -> icfp_client:commands([l]);
turn([_, $-], straight) -> ok;
turn([_, $l], straight) -> icfp_client:commands([r]);
turn([_, $L], straight) -> icfp_client:commands([r, r]);
turn([_, $R], left) -> icfp_client:commands([l,l,l]);
turn([_, $r], left) -> icfp_client:commands([l,l]);
turn([_, $-], left) -> icfp_client:commands([l]);
turn([_, $l], left) -> ok;
turn([_, $L], left) -> icfp_client:commands([r]);
turn([_, $R], hard_left) -> icfp_client:commands([l,l,l,l]);
turn([_, $r], hard_left) -> icfp_client:commands([l,l,l]);
turn([_, $-], hard_left) -> icfp_client:commands([l,l]);
turn([_, $l], hard_left) -> icfp_client:commands([l]);
turn([_, $L], hard_left) -> ok.

stop([$a,$R]) -> icfp_client:commands([b,b,l,l]);
stop([$a,$r]) -> icfp_client:commands([b,b,l]);
stop([$a,$L]) -> icfp_client:commands([b,b,r,r]);
stop([$a,$l]) -> icfp_client:commands([b,b,r]);
stop([$a,$-]) -> icfp_client:commands([b,b]);
stop([$b,$R]) -> icfp_client:commands([l,l]);
stop([$b,$r]) -> icfp_client:commands([l]);
stop([$b,$L]) -> icfp_client:commands([r,r]);
stop([$b,$l]) -> icfp_client:commands([r]);
stop([$b,$-]) -> ok;
stop([$-,$R]) -> icfp_client:commands([l,l]);
stop([$-,$r]) -> icfp_client:commands([l]);
stop([$-,$-]) -> ok;
stop([$-,$L]) -> icfp_client:commands([r,r]);
stop([$-,$l]) -> icfp_client:commands([r]).


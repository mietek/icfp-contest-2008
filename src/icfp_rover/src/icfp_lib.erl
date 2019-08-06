-module(icfp_lib).
-export([car_ang/2, car_dist/2, conv_coords/2]).
-export([deg_to_rad/1, rad_to_deg/1, norm_ang/1]).

%% @spec car_ang(pos(), pos()) -> float()
%% @doc Calcultate the polar angle based on a cartesian coordinates.
car_ang({CurX, CurY}, {TargetX, TargetY}) ->
	DX = TargetX - CurX,
	DY = TargetY - CurY,
	math:atan2(DY, DX).

%% @spec car_dist(pos(), pos()) -> float()
%% @doc Calcultate the between two cartesian coordinates.
car_dist({CurX, CurY}, {TargetX, TargetY}) ->
	DX = TargetX - CurX,
	DY = TargetY - CurY,
	math:sqrt(math:pow(DX, 2) + math:pow(DY, 2)).

%% @spec conv_coords(pos(), rel_offset()) -> pos()
%% @doc Moves the origo of the coordinates relative to rel_offset.
conv_coords({X, Y}, {RelOrgOffsetX, RelOrgOffsetY}) ->
	{X + RelOrgOffsetX, Y + RelOrgOffsetY}.

deg_to_rad(V) ->
	V * (math:pi() / 180).

rad_to_deg(R) ->
	R * (180 / math:pi()).

norm_ang(Angle) when Angle < 0 -> 360 + Angle;
norm_ang(Angle)                -> Angle.

-module(icfp_field).
-compile(export_all).

add_vec({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.
sub_vec({X1, Y1}, {X2, Y2}) -> {X1 - X2, Y1 - Y2}.
mul_vec({X, Y}, A) -> {X * A, Y * A}.
div_vec({X, Y}, A) -> {X / A, Y / A}.
vec_len({X, Y}) -> math:sqrt(X * X + Y * Y).
rnd_vec({X, Y}) -> {round(X), round(Y)}.
vec_ang({X, Y}) -> math:atan2(Y, X).
dot_vec({X1, Y1}, {X2, Y2}) -> X1 * X2 + Y1 * Y2.
nor_vec(V) -> div_vec(V, vec_len(V)).
dir_vec(Src, Dst) -> nor_vec(sub_vec(Dst, Src)).

deg_to_rad(D) -> D * (math:pi() / 180).
rad_to_deg(R) -> R * (180 / math:pi()).

is_crater({c, _, _, _}) -> true;
is_crater(_) -> false.
is_boulder({b, _, _, _}) -> true;
is_boulder(_) -> false.
is_martian({m, _, _, _, _}) -> true;
is_martian(_) -> false.

get_next_pos(Pos, Dir, Objs) ->
    DirVec = {math:cos(Dir), math:sin(Dir)},
    Craters = lists:filter(fun is_crater/1, Objs),
    CraterForce = get_objs_force(Pos, DirVec, Craters),
    Boulders = lists:filter(fun is_boulder/1, Objs),
    BoulderForce = get_objs_force(Pos, DirVec, Boulders),
    Martians = lists:filter(fun is_martian/1, Objs),
    MartianForce = get_objs_force(Pos, DirVec, Martians),
    HomeForce = get_home_force(Pos),
    lists:foldl(fun add_vec/2, Pos, [CraterForce, BoulderForce, MartianForce, HomeForce]).

get_objs_force(Pos, DirVec, Objs) ->
    Forces = lists:map(fun(Obj) -> get_obj_force(Pos, DirVec, Obj) end, Objs),
    lists:foldl(fun add_vec/2, {0, 0}, Forces).

get_obj_force(Pos, DirVec, {b, ObjX, ObjY, _ObjRad}) ->
    ObjPos = {ObjX, ObjY},
    Force = get_force(ObjPos, Pos, 100000),
    ObjVec = dir_vec(Pos, ObjPos),
    Scale = dot_vec(ObjVec, DirVec),
    mul_vec(Force, Scale);
get_obj_force(Pos, DirVec, {c, ObjX, ObjY, _ObjRad}) ->
    ObjPos = {ObjX, ObjY},
    Force = get_force(ObjPos, Pos, 100000),
    ObjVec = dir_vec(Pos, ObjPos),
    Scale = dot_vec(ObjVec, DirVec),
    mul_vec(Force, Scale);
get_obj_force(_, _, _) ->
    {0, 0}.

get_home_force(Pos) ->
    Force = get_force(Pos, {0, 0}, 1000000),
    % io:format("~p~n", [Force]),
    Force.

get_force(Src, Dst, Fac) ->
    Delta = sub_vec(Dst, Src),
    Len = vec_len(Delta),
    mul_vec(Delta, Fac / math:pow(Len, 3)).
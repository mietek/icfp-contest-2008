-module(icfp_client_proto).
-export([parse/1, encode/1]).

parse(P) ->
	parse_packet(P).

encode(T) ->
	[type(T), $;].

parse_packet([Type|P]) ->
	case string:chr(P, $;) of
		0 ->
			{[], [Type|P]};
		N ->
			{Args, Rest} = lists:split(N - 1, P),
			{parse_args(type(Type), lists:map(fun typecast/1, string:tokens(Args, " "))), tl(Rest)}
	end.

parse_args(i, [X, Y, Limit, SMin, SMax, Speed, Turn, HTurn]) ->
	{i, Limit, {X, Y}, {SMin, SMax}, {Speed, Turn, HTurn}};
parse_args(t, [T, Ctrl, X, Y, V, S|O]) ->
	{t, T, Ctrl, {X, Y, ang_norm(V), S}, parse_object(O, [])};
parse_args(b, [T]) ->
	{b, T};
parse_args(c, [T]) ->
	{c, T};
parse_args(s, [T]) ->
	{s, T};
parse_args(e, [T, S]) ->
	{e, T, S}.

parse_object([], Acc) ->
	lists:reverse(Acc);
parse_object([m, X, Y, D, S|T], Acc) ->
	parse_object(T, [{m, X, Y, D, S}|Acc]);
parse_object([O, X, Y, R|T], Acc) ->
	parse_object(T, [{O, X, Y, R}|Acc]).

typecast([$-, N|T]) when N >= $0, N =< $9 ->
	-1 * typecast([N|T]);
typecast([H|_] = N) when H >= $0, H =< $9 ->
	case string:chr(N, $.) of
		0 ->
			list_to_integer(N);
		_ ->
			list_to_float(N)
	end;
typecast(H) when is_list(H) ->
	list_to_atom(H).

type($I) -> i;
type($T) -> t;
type($B) -> b;
type($C) -> c;
type($K) -> k;
type($E) -> e;
type($S) -> s;
type(a)  -> $a;
type(b)  -> $b;
type(l)  -> $l;
type(r)  -> $r;
type(al) -> [$a, $l];
type(ar) -> [$a, $r];
type(bl) -> [$b, $l];
type(br) -> [$b, $r];
type(noop) -> [].

ang_norm(V) when V < 0 -> 360 + V;
ang_norm(V)            -> V.


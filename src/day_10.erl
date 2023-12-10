-module(day_10).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Map = parse_map(Lines),
    {ok, Start} = find_start(Map),
    Loop = find_loop(Start, Map),
    length(Loop) div 2;

run(two, Lines = [FirstLine | _]) ->
    Map = parse_map(Lines),
    {ok, Start} = find_start(Map),
    Loop = find_loop(Start, Map),
    Height = length(Lines),
    Width = size(FirstLine),
    NewMap = map_from_loop(Loop, Width, Height),
    find_enclosed(NewMap).


parse_map(Lines) ->
    maps:from_list([{{X, Y}, direction(C)} ||
                    {Y, Line} <- lists:enumerate(Lines),
                    {X, C} <- lists:enumerate(binary_to_list(Line))]).


direction($S) -> start;
direction($|) -> ns;
direction($-) -> ew;
direction($L) -> ne;
direction($J) -> nw;
direction($7) -> sw;
direction($F) -> se;
direction(_) -> none.


find_start(Map) ->
    Locations = maps:filter(fun
            (_, start) -> true;
            (_, _) -> false
        end,
        Map),
    case maps:to_list(Locations) of
        [{Start, start}] -> {ok, Start};
        _ -> {error, nostart}
    end.


find_loop(Start, Map) ->
    {Location, Direction} = find_next(Start, start, none, Map),
    find_loop(Location, Direction, Map, [{Start, start}]).


find_loop(Location, Direction, Map, Result = [{Previous, _} | _]) ->
    case find_next(Location, Direction, Previous, Map) of
        {_, start} ->
            [{Location, Direction} | Result];
        {NextLoc, NextDir} ->
            find_loop(NextLoc, NextDir, Map, [{Location, Direction} | Result])
    end.


find_next({X, Y}, Direction, Previous, Map) ->
    Neighbours = maps:with([{X, Y + 1}, {X, Y - 1}, {X + 1, Y}, {X - 1, Y}], Map),
    Filtered = maps:filter(fun
            (_, none) -> false;
            (Loc, _) when Loc == Previous -> false;
            (_, _) -> true
        end,
        Neighbours),
    find_connected(Direction, {X, Y}, maps:to_list(Filtered)).


find_connected(start, {_, Y}, [{{X2, Y}, ew} | _]) -> {{X2, Y}, ew};
find_connected(start, {X, _}, [{{X, Y2}, ns} | _]) -> {{X, Y2}, ns}; 
find_connected(start, {X, Y}, [{{X2, Y}, ne} | _]) when X2 < X -> {{X2, Y}, ne};
find_connected(start, {X, Y}, [{{X2, Y}, se} | _]) when X2 < X -> {{X2, Y}, se};
find_connected(start, {X, Y}, [{{X2, Y}, nw} | _]) when X2 > X -> {{X2, Y}, nw};
find_connected(start, {X, Y}, [{{X2, Y}, sw} | _]) when X2 > X -> {{X2, Y}, sw};
find_connected(start, {X, Y}, [{{X, Y2}, se} | _]) when Y2 < Y -> {{X, Y2}, se};
find_connected(start, {X, Y}, [{{X, Y2}, sw} | _]) when Y2 < Y -> {{X, Y2}, sw};
find_connected(start, {X, Y}, [{{X, Y2}, ne} | _]) when Y2 > Y -> {{X, Y2}, ne};
find_connected(start, {X, Y}, [{{X, Y2}, nw} | _]) when Y2 > Y -> {{X, Y2}, nw};

find_connected(ns, {X, _}, [{{X, Y2}, start} | _]) -> {{X, Y2}, start};
find_connected(ns, {X, _}, [{{X, Y2}, ns} | _]) -> {{X, Y2}, ns};
find_connected(ns, {X, Y}, [{{X, Y2}, se} | _]) when Y2 < Y -> {{X, Y2}, se};
find_connected(ns, {X, Y}, [{{X, Y2}, sw} | _]) when Y2 < Y -> {{X, Y2}, sw};
find_connected(ns, {X, Y}, [{{X, Y2}, ne} | _]) when Y2 > Y -> {{X, Y2}, ne};
find_connected(ns, {X, Y}, [{{X, Y2}, nw} | _]) when Y2 > Y -> {{X, Y2}, nw};

find_connected(ew, {_, Y}, [{{X2, Y}, start} | _]) -> {{X2, Y}, start};
find_connected(ew, {_, Y}, [{{X2, Y}, ew} | _]) -> {{X2, Y}, ew};
find_connected(ew, {X, Y}, [{{X2, Y}, ne} | _]) when X2 < X -> {{X2, Y}, ne};
find_connected(ew, {X, Y}, [{{X2, Y}, se} | _]) when X2 < X -> {{X2, Y}, se};
find_connected(ew, {X, Y}, [{{X2, Y}, nw} | _]) when X2 > X -> {{X2, Y}, nw};
find_connected(ew, {X, Y}, [{{X2, Y}, sw} | _]) when X2 > X -> {{X2, Y}, sw};

find_connected(ne, {X, Y}, [{{X2, Y}, start} | _]) when X2 > X -> {{X2, Y}, start};
find_connected(ne, {X, Y}, [{{X2, Y}, ew} | _]) when X2 > X -> {{X2, Y}, ew};
find_connected(ne, {X, Y}, [{{X2, Y}, nw} | _]) when X2 > X -> {{X2, Y}, nw};
find_connected(ne, {X, Y}, [{{X2, Y}, sw} | _]) when X2 > X -> {{X2, Y}, sw};
find_connected(ne, {X, Y}, [{{X, Y2}, start} | _]) when Y2 < Y -> {{X, Y2}, start};
find_connected(ne, {X, Y}, [{{X, Y2}, ns} | _]) when Y2 < Y -> {{X, Y2}, ns};
find_connected(ne, {X, Y}, [{{X, Y2}, se} | _]) when Y2 < Y -> {{X, Y2}, se};
find_connected(ne, {X, Y}, [{{X, Y2}, sw} | _]) when Y2 < Y -> {{X, Y2}, sw};

find_connected(nw, {X, Y}, [{{X2, Y}, start} | _]) when X2 < X -> {{X2, Y}, start};
find_connected(nw, {X, Y}, [{{X2, Y}, ew} | _]) when X2 < X -> {{X2, Y}, ew};
find_connected(nw, {X, Y}, [{{X2, Y}, ne} | _]) when X2 < X -> {{X2, Y}, ne};
find_connected(nw, {X, Y}, [{{X2, Y}, se} | _]) when X2 < X -> {{X2, Y}, se};
find_connected(nw, {X, Y}, [{{X, Y2}, start} | _]) when Y2 < Y -> {{X, Y2}, start};
find_connected(nw, {X, Y}, [{{X, Y2}, ns} | _]) when Y2 < Y -> {{X, Y2}, ns};
find_connected(nw, {X, Y}, [{{X, Y2}, se} | _]) when Y2 < Y -> {{X, Y2}, se};
find_connected(nw, {X, Y}, [{{X, Y2}, sw} | _]) when Y2 < Y -> {{X, Y2}, sw};

find_connected(se, {X, Y}, [{{X2, Y}, start} | _]) when X2 > X -> {{X2, Y}, start};
find_connected(se, {X, Y}, [{{X2, Y}, ew} | _]) when X2 > X -> {{X2, Y}, ew};
find_connected(se, {X, Y}, [{{X2, Y}, nw} | _]) when X2 > X -> {{X2, Y}, nw};
find_connected(se, {X, Y}, [{{X2, Y}, sw} | _]) when X2 > X -> {{X2, Y}, sw};
find_connected(se, {X, Y}, [{{X, Y2}, start} | _]) when Y2 > Y -> {{X, Y2}, start};
find_connected(se, {X, Y}, [{{X, Y2}, ns} | _]) when Y2 > Y -> {{X, Y2}, ns};
find_connected(se, {X, Y}, [{{X, Y2}, ne} | _]) when Y2 > Y -> {{X, Y2}, ne};
find_connected(se, {X, Y}, [{{X, Y2}, nw} | _]) when Y2 > Y -> {{X, Y2}, nw};

find_connected(sw, {X, Y}, [{{X2, Y}, start} | _]) when X2 < X -> {{X2, Y}, start};
find_connected(sw, {X, Y}, [{{X2, Y}, ew} | _]) when X2 < X -> {{X2, Y}, ew};
find_connected(sw, {X, Y}, [{{X2, Y}, ne} | _]) when X2 < X -> {{X2, Y}, ne};
find_connected(sw, {X, Y}, [{{X2, Y}, se} | _]) when X2 < X -> {{X2, Y}, se};
find_connected(sw, {X, Y}, [{{X, Y2}, start} | _]) when Y2 > Y -> {{X, Y2}, start};
find_connected(sw, {X, Y}, [{{X, Y2}, ns} | _]) when Y2 > Y -> {{X, Y2}, ns};
find_connected(sw, {X, Y}, [{{X, Y2}, ne} | _]) when Y2 > Y -> {{X, Y2}, ne};
find_connected(sw, {X, Y}, [{{X, Y2}, nw} | _]) when Y2 > Y -> {{X, Y2}, nw};

find_connected(Dir, Loc, [_ | Options]) -> find_connected(Dir, Loc, Options).


map_from_loop([{Start, _} | Loop], Width, Height) ->
    % scale up new map by three, easier to squeeze between the pipes like this
    Map = erlang:make_tuple(3 * Height, erlang:make_tuple(3 * Width, 0)),
    draw_map(Loop, Start, Start, Map).


draw_map([], From, To, Map) ->
    draw_line(From, To, Map);

draw_map([{To, _} | Loop], From, Start, Map) ->
    draw_map(Loop, To, Start, draw_line(From, To, Map)).


draw_line({X, Y0}, {X, Y1}, Map) when Y0 > Y1 ->
    draw_line({X, Y1}, {X, Y0}, Map);

draw_line({X0, Y}, {X1, Y}, Map) when X0 > X1 ->
    draw_line({X1, Y}, {X0, Y}, Map);

draw_line({X, Y}, {X, _}, Map) ->
    NewX = (X - 1) * 3 + 2,
    NewY = (Y - 1) * 3 + 2,
    draw_point(NewX, NewY, draw_point(NewX, NewY + 1, draw_point(NewX, NewY + 2, draw_point(NewX, NewY + 3, Map))));

draw_line({X, Y}, {_, Y}, Map) ->
    NewX = (X - 1) * 3 + 2,
    NewY = (Y - 1) * 3 + 2,
    draw_point(NewX, NewY, draw_point(NewX + 1, NewY, draw_point(NewX + 2, NewY, draw_point(NewX + 3, NewY, Map)))).


draw_point(X, Y, Map) ->
    setelement(Y, Map, setelement(X, element(Y, Map), 1)).


find_enclosed(Map) ->
    % due to resizing the map it is surrounded by zeros, we can start everywhere
    Filled = fill_outside(1, 1, Map),
    NewMap = shrink(Filled),
    length([X || Line <- tuple_to_list(NewMap), X <- tuple_to_list(Line), X == 0]).


shrink(Map) ->
    Height = size(Map) div 3,
    Width = size(element(1, Map)) div 3,
    NewMap = erlang:make_tuple(Height, erlang:make_tuple(Width, 0)),
    shrink(1, 1, Width, Height, Map, NewMap).

shrink(_, Y, _, Height, _, NewMap) when Y > Height ->
    NewMap;
shrink(X, Y, Width, Height, OldMap, NewMap) when X > Width ->
    shrink(1, Y + 1, Width, Height, OldMap, NewMap);
shrink(X, Y, Width, Height, OldMap, Map) ->
    OldX = (X - 1) * 3 + 2,
    OldY = (Y - 1) * 3 + 2,
    Value = element(OldX, element(OldY, OldMap)),
    NewMap = setelement(Y, Map, setelement(X, element(Y, Map), Value)),
    shrink(X + 1, Y, Width, Height, OldMap, NewMap).


fill_outside(X, Y, Map) ->
    Height = size(Map),
    Width = size(element(1, Map)),
    fill_outside(X, Y, Width, Height, Map).


fill_outside(X, Y, Width, Height, Map) ->
    Row = element(Y, Map),
    case element(X, Row) of
        1 -> Map;
        2 -> Map;
        0 ->
            lists:foldl(fun({X0, Y0}, NewMap) ->
                    fill_outside(X0, Y0, Width, Height, NewMap)
                end,
                setelement(Y, Map, setelement(X, Row, 2)),
                neighbours(X, Y, Width, Height))
    end.


neighbours(X, Y, Width, Height) ->
    Points = [{X - 1, Y - 1},
              {X + 1, Y + 1},
              {X + 1, Y - 1},
              {X - 1, Y + 1},
              {X - 1, Y},
              {X + 1, Y},
              {X, Y - 1},
              {X, Y + 1}],
    [{X0, Y0} || {X0, Y0} <- Points, X0 >= 1, X0 =< Width, Y0 >= 1, Y0 =< Height].

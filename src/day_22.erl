-module(day_22).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Input = parse(Lines),
    Grid = settle(Input),
    Bricks = maps:fold(fun(K, V, Result) -> Result#{V => [K | maps:get(V, Result, [])]} end, #{}, Grid),
    Dependencies = find_dependencies(Bricks, Grid),
    Independent = sets:size(sets:subtract(sets:from_list(maps:keys(Bricks)), sets:from_list(maps:keys(Dependencies)))),
    Independent + maps:fold(fun(N, Deps, Result) ->
            X = maps:fold(fun(_, Y, R) ->
                        sets:subtract(R, sets:from_list(Y))
                    end,
                    sets:from_list(Deps),
                    maps:without([N], Dependencies)),
            case sets:size(X) of
                0 -> Result + 1;
                _ -> Result
            end
        end,
        0,
        Dependencies);

run(two, Lines) ->
    Input = parse(Lines),
    Grid = settle(Input),
    Bricks = maps:fold(fun(K, V, Result) -> Result#{V => [K | maps:get(V, Result, [])]} end, #{}, Grid),
    Dependencies = find_dependencies(Bricks, Grid),
    % naive assumption to only consider the lowest bricks
    % doesn't work, there could be a brick lying on two of the lowest bricks
    Start = [N || {N, _} <- maps:get(1, Input, [])],
    lists:max(disintegrate(Start, Dependencies)).


parse(Lines) ->
    {_, Bricks} = parse(Lines, []),
    Bricks.


parse([], Result) ->
    lists:foldl(fun(Item, {N, Bricks}) ->
            Z = element(3, Item),
            {N + 1, Bricks#{Z => [{N, Item} | maps:get(Z, Bricks, [])]}}
        end,
        {1, #{}},
        lists:keysort(3, Result));
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    Pattern = "(\\d+),(\\d+),(\\d+)~(\\d+),(\\d+),(\\d+)",
    {match, Match} = re:run(Line, Pattern, [{capture, all_but_first, binary}]),
    [X1, Y1, Z1, X2, Y2, Z2] = [binary_to_integer(N) || N <- Match],
    parse(Lines, [{X1, Y1, Z1, X2 - X1, Y2 - Y1, Z2 - Z1} | Result]).


settle(Bricks) ->
    % place first layer in grid
    Grid = lists:foldl(fun({N, Brick}, Grid) ->
            place(N, Brick, Grid)
        end,
        #{},
        maps:get(1, Bricks, [])),
    settle(2, lists:max(maps:keys(Bricks)), Bricks, Grid).


settle(Z, Height, _, Grid) when Z > Height ->
    Grid;
settle(Z, Height, Bricks, Grid) ->
    case maps:get(Z, Bricks, []) of
        [] -> settle(Z + 1, Height, Bricks, Grid);
        Layer ->
            NewGrid = settle_layer(Layer, Grid),
            settle(Z + 1, Height, Bricks, NewGrid)
    end.


settle_layer([], Grid) ->
    Grid;
settle_layer([{N, {X, Y, Z, Dx, Dy, Dz}} | Layer], Grid) ->
    case intersect(X, Y, Z - 1, Dx, Dy, Dz, Grid) of
        true ->
            NewGrid = place(N, {X, Y, Z, Dx, Dy, Dz}, Grid),
            settle_layer(Layer, NewGrid);
        false ->
            settle_layer([{N, {X, Y, Z - 1, Dx, Dy, Dz}} | Layer], Grid)
    end.


place(N, {X, Y, Z, 0, 0, 0}, Grid) ->
    Grid#{{X, Y, Z} => N};
place(N, {X, Y, Z, Dx, 0, 0}, Grid) ->
    lists:foldl(fun(D, NewGrid) -> NewGrid#{{X + D, Y, Z} => N} end, Grid, lists:seq(0, Dx));
place(N, {X, Y, Z, 0, Dy, 0}, Grid) ->
    lists:foldl(fun(D, NewGrid) -> NewGrid#{{X, Y + D, Z} => N} end, Grid, lists:seq(0, Dy));
place(N, {X, Y, Z, 0, 0, Dz}, Grid) ->
    lists:foldl(fun(D, NewGrid) -> NewGrid#{{X, Y, Z + D} => N} end, Grid, lists:seq(0, Dz)).


intersect(_, _, 0, _, _, _, _) ->
    true;
intersect(X, Y, Z, 0, 0, 0, Grid) ->
    maps:is_key({X, Y, Z}, Grid);
intersect(X, Y, Z, Dx, 0, 0, Grid) ->
    lists:any(fun(D) -> maps:is_key({X + D, Y, Z}, Grid) end, lists:seq(0, Dx));
intersect(X, Y, Z, 0, Dy, 0, Grid) ->
    lists:any(fun(D) -> maps:is_key({X, Y + D, Z}, Grid) end, lists:seq(0, Dy));
intersect(X, Y, Z, 0, 0, Dz, Grid) ->
    lists:any(fun(D) -> maps:is_key({X, Y, Z + D}, Grid) end, lists:seq(0, Dz)).


find_dependencies(Bricks, Grid) ->
    find_dependencies(maps:to_list(Bricks), Grid, []).


find_dependencies([], _, Deps) ->
    lists:foldl(fun({A, B}, Result) ->
            Result#{A => [B | maps:get(A, Result, [])]}
        end,
        #{},
        Deps);
find_dependencies([{N, Brick} | Bricks], Grid, Deps) ->
    NewDeps = [{N, M} || M <- [maps:get({X, Y, Z + 1}, Grid, none) || {X, Y, Z} <- Brick],
                         M /= none, M /= N],
    find_dependencies(Bricks, Grid, NewDeps ++ Deps).


disintegrate(Start, Dependencies) ->
    [length(lists:uniq(disintegrate(N, Dependencies, []))) || N <- Start].


disintegrate(N, Dependencies, Path) ->
    case maps:get(N, Dependencies, []) of
        [] ->
            [N | Path];
        Deps ->
            lists:foldl(fun(D, P) ->
                    disintegrate(D, Dependencies, P)
                end,
                [N | Path],
                Deps)
    end.

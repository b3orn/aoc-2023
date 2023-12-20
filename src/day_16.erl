-module(day_16).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Grid = parse(Lines),
    Tiles = process({1, 1, 1, 0}, Grid),
    %print_tiles(Tiles, size(element(1, Grid)), size(Grid)),
    length(lists:uniq([{X, Y} || {X, Y, _, _} <- maps:keys(Tiles)]));

run(two, Lines) ->
    Grid = parse(Lines),
    find_optimal_coverage(Grid).


parse(Lines) ->
    parse(Lines, []).


parse([], Result) ->
    list_to_tuple(lists:reverse(Result));
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    parse(Lines, [list_to_tuple(binary_to_list(Line)) | Result]).


find_optimal_coverage(Grid) ->
    Height = size(Grid),
    Width = size(element(1, Grid)),
    Top = [{X, 1, 0, 1} || X <- lists:seq(1, Width)],
    Bottom = [{X, Height, 0, -1} || X <- lists:seq(1, Width)],
    Left = [{1, Y, 1, 0} || Y <- lists:seq(1, Height)],
    Right = [{Width, Y, -1, 0} || Y <- lists:seq(1, Height)],
    find_optimal_coverage(Top ++ Bottom ++ Left ++ Right, Grid, 0).


find_optimal_coverage([], _, Coverage) ->
    Coverage;
find_optimal_coverage([Start | Tail], Grid, Coverage) ->
    Tiles = process(Start, Grid),
    case length(lists:uniq([{X, Y} || {X, Y, _, _} <- maps:keys(Tiles)])) of
        NewCoverage when NewCoverage > Coverage ->
            find_optimal_coverage(Tail, Grid, NewCoverage);
        _ ->
            find_optimal_coverage(Tail, Grid, Coverage)
    end.


process(Beam, Grid) ->
    process([Beam], Grid, #{}).


process([], _, Tiles) ->
    Tiles;
process(Beams, Grid, Tiles) ->
    {NewBeams, NewTiles} = follow_beams(Beams, [], Grid, Tiles),
    process(NewBeams, Grid, NewTiles).


follow_beams([], Beams, _, Tiles) ->
    {Beams, Tiles};
follow_beams([{X, Y, Dx, Dy} | Tail], Beams, Grid, Tiles) ->
    {NewBeams, NewTiles} = case maps:get({X, Y, Dx, Dy}, Tiles, undefined) of
        undefined ->
            case grid_get(X, Y, Grid) of
                undefined ->  % beam ends
                    {[], Tiles};
                $| when Dx == 1; Dx == -1 ->  % beam splits
                    {[{X, Y + 1, 0, 1}, {X, Y - 1, 0, -1}], Tiles#{{X, Y, Dx, Dy} => 1}};
                $- when Dy == 1; Dy == - 1->  % beam splits
                    {[{X + 1, Y, 1, 0}, {X - 1, Y, -1, 0}], Tiles#{{X, Y, Dx, Dy} => 1}};
                $/ ->  % beam reflects
                    {[{X - Dy, Y - Dx, -Dy, -Dx}], Tiles#{{X, Y, Dx, Dy} => 1}};
                $\\ ->  % beam reflects
                    {[{X + Dy, Y + Dx, Dy, Dx}], Tiles#{{X, Y, Dx, Dy} => 1}};
                _ ->  % beam continues
                    {[{X + Dx, Y + Dy, Dx, Dy}], Tiles#{{X, Y, Dx, Dy} => 1}}
            end;
        _ ->
            {[], Tiles}
    end,
    follow_beams(Tail, NewBeams ++ Beams, Grid, NewTiles).


grid_get(0, _, _) -> undefined;
grid_get(_, 0, _) -> undefined;
grid_get(_, Y, Grid) when Y > size(Grid) -> undefined;
grid_get(X, Y, Grid) when X > size(element(Y, Grid)) -> undefined;
grid_get(X, Y, Grid) -> element(X, element(Y, Grid)).


print_tiles(Tiles, Width, Height) ->
    utils:print_grid(maps:fold(fun({X, Y, _, _}, _, Result) ->
            setelement(Y, Result, setelement(X, element(Y, Result), $#))
        end,
        erlang:make_tuple(Height, erlang:make_tuple(Width, $.)),
        Tiles)).
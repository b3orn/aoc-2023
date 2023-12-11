-module(day_11).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    find_shortest_path_sum(Lines, 2);

run(two, Lines) ->
    find_shortest_path_sum(Lines, 1000000).


find_shortest_path_sum(Lines, Scale) ->
    Map = [binary_to_list(Line) || Line <- Lines],
    {EmptyRows, EmptyColumns} = find_empty_space(Map),
    Galaxies = find_galaxies(Map, EmptyRows, EmptyColumns, Scale),
    Paths = find_paths(Galaxies),
    lists:sum([L || {_, L} <- Paths]).


find_empty_space(Map) ->
    EmptyRows = find_empty_rows(Map),
    EmptyColumns = find_empty_columns(Map),
    {EmptyRows, EmptyColumns}.


find_empty_rows(Map) ->
    find_empty_rows(Map, 1, #{}).


find_empty_rows([], _, Result) ->
    Result;
find_empty_rows([Row | Map], N, Result) ->
    case [C || C <- Row, C == $#] of
        [] -> find_empty_rows(Map, N + 1, Result#{N => true});
        _ -> find_empty_rows(Map, N + 1, Result)
    end.


find_empty_columns(Map = [First | _]) ->
    Width = length(First),
    find_empty_columns(Map, 1, Width, #{}).


find_empty_columns(_, N, Width, Result) when N > Width ->
    Result;
find_empty_columns(Map, N, Width, Result) ->
    Column = [lists:nth(N, Row) || Row <- Map],
    case [C || C <- Column, C == $#] of
        [] -> find_empty_columns(Map, N + 1, Width, Result#{N => true});
        _ -> find_empty_columns(Map, N + 1, Width, Result)
    end.


find_galaxies(Map, EmptyRows, EmptyColumns, Scale) ->
    find_galaxies(Map, 1, 1, EmptyRows, EmptyColumns, Scale, []).


find_galaxies([], _, _, _, _, _, Result) ->
    Result;
find_galaxies([Row | Map], Y, ScaledY, EmptyRows, EmptyColumns, Scale, Result) ->
    case maps:get(Y, EmptyRows, false) of
        true ->
            find_galaxies(Map, Y + 1, ScaledY + Scale, EmptyRows, EmptyColumns, Scale, Result);
        false ->
            Galaxies = [{X, ScaledY} || X <- find_galaxies_in_row(Row, 1, 1, EmptyColumns, Scale, [])],
            find_galaxies(Map, Y + 1, ScaledY + 1, EmptyRows, EmptyColumns, Scale, Result ++ Galaxies)
    end.


find_galaxies_in_row([], _, _, _, _, Result) ->
    lists:reverse(Result);
find_galaxies_in_row([$# | Row], X, ScaledX, Empty, Scale, Result) ->
    find_galaxies_in_row(Row, X + 1, ScaledX + 1, Empty, Scale, [ScaledX | Result]);
find_galaxies_in_row([$. | Row], X, ScaledX, Empty, Scale, Result) ->
    case maps:get(X, Empty, false) of
        true ->
            find_galaxies_in_row(Row, X + 1,  ScaledX + Scale, Empty, Scale, Result);
        false ->
            find_galaxies_in_row(Row, X + 1,  ScaledX + 1, Empty, Scale, Result)
    end.


find_paths(Galaxies) ->
    find_paths(Galaxies, 1, Galaxies, []).


find_paths([], _, _, Result) ->
    Result;
find_paths([{X0, Y0} | Locations], N, Galaxies, Result) ->
    Paths = [{{N, M}, abs(X0 - X1) + abs(Y0 - Y1)} || {M, {X1, Y1}} <- lists:enumerate(Galaxies), M > N],
    find_paths(Locations, N + 1, Galaxies, Result ++ Paths).

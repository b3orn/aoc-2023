-module(day_23).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Grid = parse(Lines),
    Start = find_field(1, Grid),
    End = find_field(size(Grid), Grid),
    Paths = find_paths(Start, End, Grid),
    lists:max(Paths).


parse(Lines) ->
    parse(Lines, []).


parse([], Result) ->
    list_to_tuple(lists:reverse(Result));
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    parse(Lines, [list_to_tuple(binary_to_list(Line)) | Result]).


find_field(N, Grid) ->
    Row = element(N, Grid),
    [Start] = [{X, N} || X <- lists:seq(1, size(Row)), element(X, Row) == $.],
    Start.


find_paths({X, Y}, End, Grid) ->
    find_paths(X, Y, Grid, End, #{}).


find_paths(X, Y, _, {X, Y}, Path) ->
    [map_size(Path)];
find_paths(X, Y, Grid, End, Path) ->
    CurrentPath = Path#{{X, Y} => true},
    lists:foldl(fun({U, V}, Result) ->
            find_paths(U, V, Grid, End, CurrentPath) ++ Result
        end,
        [],
        candidates(X, Y, Grid, CurrentPath)).


candidates(X, Y, Grid, Path) ->
    Candidates = [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}],
    Height = size(Grid),
    Width = size(element(1, Grid)),
    lists:filter(fun
            ({U, _}) when U < 1; U > Width -> false;
            ({_, V}) when V < 1; V > Height -> false;
            ({U, V}) when element(U, element(V, Grid)) == $# -> false;
            ({U, V}) when U > X, element(U, element(V, Grid)) == $< -> false;
            ({U, V}) when U < X, element(U, element(V, Grid)) == $> -> false;
            ({U, V}) when V > Y, element(U, element(V, Grid)) == $^ -> false;
            ({U, V}) when V < Y, element(U, element(V, Grid)) == $v -> false;
            ({U, V}) when map_get({U, V}, Path) -> false;
            (_) -> true
        end,
        Candidates).

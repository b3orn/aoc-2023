-module(day_21).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Grid = parse(Lines),
    {X, Y} = find_start(Grid),
    NewGrid = setelement(Y, Grid, setelement(X, element(Y, Grid), $.)),
    Options = find_options(64, {X, Y}, NewGrid),
    length(Options);

run(two, Lines) ->
    Grid = parse(Lines),
    {X, Y} = find_start(Grid),
    NewGrid = setelement(Y, Grid, setelement(X, element(Y, Grid), $.)),
    find_options_repeating(26501365, {X, Y}, NewGrid).


parse(Lines) ->
    parse(Lines, []).


parse([], Result) ->
    list_to_tuple(lists:reverse(Result));
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    parse(Lines, [list_to_tuple(binary_to_list(Line)) | Result]).


find_start(Grid) ->
    find_start(1, 1, size(element(1, Grid)), size(Grid), Grid).


find_start(_, Y, _, Height, _) when Y > Height ->
    undefined;
find_start(X, Y, Width, Height, Grid) when X > Width ->
    find_start(1, Y + 1, Width, Height, Grid);
find_start(X, Y, Width, Height, Grid) ->
    case element(X, element(Y, Grid)) of
        $S -> {X, Y};
        _ -> find_start(X + 1, Y, Width, Height, Grid)
    end.


find_options(N, Start, Grid) ->
    find_options(N, [Start], [], Grid).


find_options(0, Options, _, _) ->
    Options;
find_options(N, [], Options, Grid) ->
    find_options(N - 1, lists:uniq(Options), [], Grid);
find_options(N, [{X, Y} | Tail], Options, Grid) ->
    Candidates = candidates(X, Y, Grid),
    find_options(N, Tail, Candidates ++ Options, Grid).


candidates(X, Y, Grid) ->
    Candidates = [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}],
    Height = size(Grid),
    Width = size(element(1, Grid)),
    [{U, V} || {U, V} <- Candidates,
               U > 0, U =< Width,
               V > 0, V =< Height,
               element(U, element(V, Grid)) /= $#].


find_options_repeating(N, Start, Grid) ->
    find_options_repeating(N, [Start], #{}, #{}, #{}, Grid).


find_options_repeating(0, Options, _, _, VisitedB, _) ->
    map_size(maps:merge(maps:from_list([{K, true} || K <- Options]), VisitedB));
find_options_repeating(N, [], Options, VisitedA, VisitedB, Grid) ->
    NewVisitedA = maps:merge(VisitedA, Options),
    find_options_repeating(N - 1, maps:keys(Options), #{}, VisitedB, NewVisitedA, Grid);
find_options_repeating(N, [{X, Y} | Tail], Options, VisitedA, VisitedB, Grid) ->
    Candidates = candidates_repeating(X, Y, Grid, VisitedA),
    find_options_repeating(N, Tail, maps:merge(Candidates, Options), VisitedA, VisitedB, Grid).


candidates_repeating(X, Y, Grid, Visited) ->
    Candidates = [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}],
    Height = size(Grid),
    Width = size(element(1, Grid)),
    Filtered = lists:filter(fun({U, V}) ->
            Coord = {((U rem Width) + Width) rem Width,
                     ((V rem Height) + Height) rem Height},
            {NewU, NewV} = case Coord of
                {0, 0} -> {Width, Height};
                {0, B} -> {Width, B};
                {A, 0} -> {A, Height};
                {A, B} -> {A, B}
            end,
            element(NewU, element(NewV, Grid)) /= $# andalso not maps:is_key({U, V}, Visited)
        end,
        Candidates),
    maps:from_list([{C, true} || C <- Filtered]).

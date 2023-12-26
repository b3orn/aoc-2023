-module(day_13).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Patterns = parse(Lines),
    find_all_reflections(Patterns, 0);

run(two, Lines) ->
    Patterns = parse(Lines),
    find_all_reflections(Patterns, 1).


parse(Lines) ->
    parse(Lines, [], []).


parse([], [], Result) ->
    [compress_pattern(Pattern) || Pattern <- lists:reverse(Result)];
parse([], Pattern, Result) ->
    parse([], [], [list_to_tuple(lists:reverse(Pattern)) | Result]);
parse([<<>> | Lines], Pattern, Result) ->
    parse(Lines, [], [list_to_tuple(lists:reverse(Pattern)) | Result]);
parse([Line | Lines], Pattern, Result) ->
    Row = list_to_tuple([if C == $# -> 1; true -> 0 end || <<C>> <= Line]),
    parse(Lines, [Row | Pattern], Result).


compress_pattern(Pattern) ->
    Height = size(Pattern),
    Width = size(element(1, Pattern)),
    Rows = [create_bitpattern(tuple_to_list(element(Y, Pattern))) || Y <- lists:seq(1, Height)],
    Cols = [create_bitpattern([element(X, element(Y, Pattern)) || Y <- lists:seq(1, Height)]) || X <- lists:seq(1, Width)],
    {Width, Height, list_to_tuple(Rows), list_to_tuple(Cols)}.


create_bitpattern(Items) ->
    lists:foldl(fun(N, R) -> (R bsl 1) + N end, 0, Items).



find_all_reflections(Patterns, NBitErrors) ->
    lists:sum([find_reflections(Pattern, NBitErrors) || Pattern <- Patterns]).


find_reflections({Width, Height, Rows, Cols}, NBitErrors) ->
    H = check_reflection(Height, Rows, NBitErrors),
    V = check_reflection(Width, Cols, NBitErrors),
    100 * H + V.


check_reflection(N, Pattern, NBitErrors) ->
    check_reflection(1, N, Pattern, NBitErrors).

check_reflection(I, N, _, _) when I > N - 1 ->
    0;
check_reflection(I, N, Pattern, NBitErrors) ->
    W = min(I, N - I),
    L = span(Pattern, I - W, I),
    R = lists:reverse(span(Pattern, I, I + W)),
    BitErrors = lists:sum([bit_count(A bxor B) || {A, B} <- lists:zip(L, R)]),
    case BitErrors of
        NBitErrors -> I;
        _ -> check_reflection(I + 1, N, Pattern, NBitErrors)
    end.


span(Data, Start, Stop) ->
    [element(I + 1, Data) || I <- lists:seq(Start, Stop - 1)].


bit_count(N) ->
    bit_count(N, 0).


bit_count(0, Count) ->
    Count;
bit_count(N, Count) ->
    case N band 1 of
        1 -> bit_count(N bsr 1, Count + 1);
        _ -> bit_count(N bsr 1, Count)
    end.

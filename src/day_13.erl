-module(day_13).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Patterns = split_patterns(Lines),
    find_reflections(Patterns).


split_patterns(Lines) ->
    split_patterns(Lines, [], []).


split_patterns([], [], Result) ->
    lists:reverse([list_to_tuple(Pattern) || Pattern <- Result]);
split_patterns([], Pattern, Result) ->
    lists:reverse([lists:reverse(Pattern) | Result]);
split_patterns([<<>> | Lines], Pattern, Result) ->
    split_patterns(Lines, [], [lists:reverse(Pattern) | Result]);
split_patterns([Line | Lines], Pattern, Result) ->
    NewLine = list_to_tuple(binary_to_list(Line)),
    split_patterns(Lines, [NewLine | Pattern], Result).


find_reflections(Patterns) ->
    lists:sum([find_reflection(Pattern) || Pattern <- Patterns]).


find_reflection(Pattern) ->
    Height = size(Pattern),
    Width = size(element(1, Pattern)),
    HorizontalReflection = find_horizontal_reflection(Pattern, Height),
    VerticalReflection = find_vertical_reflection(Pattern, Width, Height),
    VerticalReflection + 100 * HorizontalReflection.


find_vertical_reflection(Pattern, Width, Height) ->
    Rotated = list_to_tuple([
        list_to_tuple([element(X, element(Y, Pattern)) || Y <- lists:seq(1, Height)])
        || X <- lists:seq(1, Width)]),
    find_horizontal_reflection(Rotated, Width).


find_horizontal_reflection(Pattern, Height) ->
    Patterns = generate_patterns(Height),
    verify_patterns(Patterns, Pattern).


generate_patterns(N) when N rem 2 == 1 ->
    generate_upper_patterns(generate_lower_patterns(N - 1), N);
generate_patterns(N) ->
    generate_upper_patterns(generate_lower_patterns(N), N).


generate_lower_patterns(0) -> [];
generate_lower_patterns(N) ->
    Patterns = [{N div 2,
                 [{I, N - (I - 1)} || I <- lists:seq(1, N div 2)]}],
    Patterns ++ generate_lower_patterns(N - 2).


generate_upper_patterns(LowerPatterns, N) ->
    generate_upper_patterns(LowerPatterns, N, []).


generate_upper_patterns([], _, Result) ->
    NewResult = lists:uniq(fun({P, Pattern}) ->
            {P, [if A > B -> {B, A}; true -> {A, B} end || {A, B} <- Pattern]}
        end,
        Result),
    lists:sort(fun({_, A}, {_, B}) ->
            length(A) > length(B)
        end,
        NewResult);
generate_upper_patterns([{P, Pattern} | Tail], N, Result) ->
    NewPattern = [{N - (A - 1), N - (B - 1)} || {A, B} <- Pattern],
    generate_upper_patterns(Tail, N, [{P, Pattern}, {N - P, NewPattern} | Result]).


verify_patterns([], _) -> 0;
verify_patterns([{Result, Pattern} | Tail], Grid) ->
    Match = lists:foldl(fun
            (_, false) -> false;
            ({A, B}, _) -> element(A, Grid) == element(B, Grid)
        end,
        true,
        Pattern),
    case Match of
        true ->
            Result;
        _ ->
            verify_patterns(Tail, Grid)
    end.

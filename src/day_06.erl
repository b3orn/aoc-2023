-module(day_06).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Trimmed = [lists:last(binary:split(Line, <<":">>)) || Line <- Lines, Line /= <<>>],
    [Times, Distances] = [parse_numbers(Line) || Line <- Trimmed],
    Races = lists:zip(Times, Distances),
    Options = find_options(Races),
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, Options);

run(two, Lines) ->
    Trimmed = [lists:last(binary:split(Line, <<":">>)) || Line <- Lines, Line /= <<>>],
    [Times, Distances] = [parse_numbers(binary:replace(Line, <<" ">>, <<"">>, [global])) || Line <- Trimmed],
    Races = lists:zip(Times, Distances),
    Options = find_options(Races),
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, Options).


-spec parse_numbers(binary()) -> [non_neg_integer()].
parse_numbers(Numbers) ->
    ParsedNumbers = binary:split(Numbers, <<" ">>, [global, trim_all]),
    lists:map(fun erlang:binary_to_integer/1, ParsedNumbers).


find_options(Races) ->
    find_options(Races, []).


find_options([], Result) ->
    lists:reverse(Result);

find_options([{Time, RecordDistance} | Races], Result) ->
    X = math:sqrt(math:pow(Time, 2)/4 - RecordDistance),
    Tmin = case round(math:floor(Time/2 - X)) of
        T1 when T1 * (Time - T1) > RecordDistance -> T1;
        T1 -> T1 + 1
    end,
    Tmax = case round(math:ceil(Time/2 + X)) of
        T2 when T2 * (Time - T2) < RecordDistance -> T2;
        T2 -> T2 - 1
    end,
    Options = Tmax - Tmin,
    find_options(Races, [Options | Result]).

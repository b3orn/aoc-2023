#!/usr/bin/env escript


-spec main([string()]) -> ok.
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Races = parse_file(Lines),
    Options = find_options(Races),
    Result = lists:foldl(fun(X, Acc) -> X * Acc end, 1, Options),
    io:format("~p~n", [Result]);

main(_) ->
    io:format("usage: escript solution.escript filename~n").


-spec parse_file([binary()]) -> [{pos_integer(), pos_integer()}].
parse_file([TimesLine, DistanceLine]) ->
    Times = parse_line(TimesLine),
    Distances = parse_line(DistanceLine),
    lists:zip(Times, Distances).


-spec parse_line(binary()) -> [non_neg_integer()].
parse_line(Line) ->
    [_, Values] = binary:split(Line, <<" ">>),
    parse_numbers(Values).


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
    Tmin = round(math:ceil(Time/2 - X)),
    Tmax = round(math:ceil(Time/2 + X)),
    Options = Tmax - Tmin,
    find_options(Races, [Options | Result]).

-module(day_09).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Data = [parse_numbers(Line) || Line <- Lines],
    lists:sum([extrapolate_forward(Line) || Line <- Data]);

run(two, Lines) ->
    Data = [parse_numbers(Line) || Line <- Lines],
    lists:sum([extrapolate_backward(Line) || Line <- Data]).


parse_numbers(Line) ->
    [binary_to_integer(C) || C <- binary:split(Line, <<" ">>, [global, trim_all])].


diff(Data) ->
    Length = length(Data),
    A = lists:sublist(Data, Length - 1),
    B = lists:sublist(Data, 2, Length - 1),
    [Y - X || {X, Y} <- lists:zip(A, B)].


extrapolate_forward(Data) ->
    extrapolate_forward(Data, 0).


extrapolate_forward(Data, Result) ->
    Diff = diff(Data),
    case [N || N <- Diff, N /= 0] of
        [] -> Result + lists:last(Data);
        _ -> extrapolate_forward(Diff, Result + lists:last(Data))
    end.


extrapolate_backward(Data = [First | _]) ->
    Diff = diff(Data),
    case [N || N <- Diff, N /= 0] of
        [] -> First;
        _ -> First - extrapolate_backward(Diff)
    end.

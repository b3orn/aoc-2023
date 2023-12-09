-module(day_01).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Digits = [[C - $0 || <<C>> <= Line, C >= $0, C =< $9] || Line <- Lines],
    lists:sum([F * 10 + lists:last(D) || D = [F | _] <- Digits]);

run(two, Lines) ->
    Digits = [convert_line(Line, []) || Line <- Lines],
    lists:sum([F * 10 + lists:last(D) || D = [F | _] <- Digits]).


-spec convert_line(binary(), [non_neg_integer()]) -> [non_neg_integer()].
convert_line(<<>>, Result) ->
    lists:reverse(Result);
convert_line(<<C, Line/binary>>, Result) when C >= $0 andalso C =< $9 ->
    convert_line(Line, [C - $0 | Result]);
convert_line(<<_, Line/binary>> = <<"one", _/binary>>, Result) ->
    convert_line(Line, [1 | Result]);
convert_line(<<_, Line/binary>> = <<"two", _/binary>>, Result) ->
    convert_line(Line, [2 | Result]);
convert_line(<<_, Line/binary>> = <<"three", _/binary>>, Result) ->
    convert_line(Line, [3 | Result]);
convert_line(<<_, Line/binary>> = <<"four", _/binary>>, Result) ->
    convert_line(Line, [4 | Result]);
convert_line(<<_, Line/binary>> = <<"five", _/binary>>, Result) ->
    convert_line(Line, [5 | Result]);
convert_line(<<_, Line/binary>> = <<"six", _/binary>>, Result) ->
    convert_line(Line, [6 | Result]);
convert_line(<<_, Line/binary>> = <<"seven", _/binary>>, Result) ->
    convert_line(Line, [7 | Result]);
convert_line(<<_, Line/binary>> = <<"eight", _/binary>>, Result) ->
    convert_line(Line, [8 | Result]);
convert_line(<<_, Line/binary>> = <<"nine", _/binary>>, Result) ->
    convert_line(Line, [9 | Result]);
convert_line(<<_, Line/binary>>, Result) ->
    convert_line(Line, Result).

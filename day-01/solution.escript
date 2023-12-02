#!/usr/bin/env escript

main([]) ->
    io:format("usage: escript solution.escript filename~n");

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Result = convert_lines(Lines, 0),
    io:format("~w~n", [Result]).


convert_lines([], Result) ->
    Result;

convert_lines([Line | Lines], Result) ->
    Digits = convert_line(binary_to_list(Line), []),
    [First | _] = Digits,
    Last = lists:last(Digits),
    convert_lines(Lines, Result + 10 * First + Last).


convert_line([], Result) ->
    lists:reverse(Result);

% part one
convert_line([X | Line], Result) when X >= $0 andalso X =< $9 ->
    convert_line(Line, [X - $0 | Result]);

% part two
convert_line([_ | Line] = [$o, $n, $e | _], Result) ->
    convert_line(Line, [1 | Result]);

convert_line([_ | Line] = [$t, $w, $o | _], Result) ->
    convert_line(Line, [2 | Result]);

convert_line([_ | Line] = [$t, $h, $r, $e, $e | _], Result) ->
    convert_line(Line, [3 | Result]);

convert_line([_ | Line] = [$f, $o, $u, $r | _], Result) ->
    convert_line(Line, [4 | Result]);

convert_line([_ | Line] = [$f, $i, $v, $e | _], Result) ->
    convert_line(Line, [5 | Result]);

convert_line([_ | Line] = [$s, $i, $x | _], Result) ->
    convert_line(Line, [6 | Result]);

convert_line([_ | Line] = [$s, $e, $v, $e, $n | _], Result) ->
    convert_line(Line, [7 | Result]);

convert_line([_ | Line] = [$e, $i, $g, $h, $t | _], Result) ->
    convert_line(Line, [8 | Result]);

convert_line([_ | Line] = [$n, $i, $n, $e | _], Result) ->
    convert_line(Line, [9 | Result]);

convert_line([_ | Line], Result) ->
    convert_line(Line, Result).

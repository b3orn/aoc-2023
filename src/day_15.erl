-module(day_15).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    [Input | _] = Lines,
    Steps = binary:split(Input, <<",">>, [global, trim_all]),
    lists:sum([hash(Step) || Step <- Steps]);

run(two, Lines) ->
    [Input | _] = Lines,
    Steps = binary:split(Input, <<",">>, [global, trim_all]),
    Boxes = maps:to_list(process(Steps)),
    lists:sum([(1 + BoxId) * I * F || {BoxId, Lenses} <- Boxes,
                                      {I, {_, F}} <- lists:enumerate(Lenses)]).


hash(S) -> hash(S, 0).


hash(<<>>, Result) ->
    Result;
hash(<<C, Tail/binary>>, Result) ->
    hash(Tail, 17 * (Result + C) rem 256).


process(Steps) ->
    process(Steps, #{}).


process([], Boxes) ->
    Boxes;
process([Step | Steps], Boxes) ->
    NewBoxes = case re:run(Step, "(\\w+)(-|=)(\\d+)?", [{capture, all_but_first, binary}]) of
        {match, [Label, <<"=">>, Number]} ->
            BoxId = hash(Label),
            Box = maps:get(BoxId, Boxes, []),
            F = binary_to_integer(Number),
            case lists:keymember(Label, 1, Box) of
                true ->
                    Boxes#{BoxId => lists:keyreplace(Label, 1, Box, {Label, F})};
                _ ->
                    Boxes#{BoxId => Box ++ [{Label, F}]}
            end;
        {match, [Label, <<"-">>]} ->
            BoxId = hash(Label),
            Boxes#{BoxId => lists:keydelete(Label, 1, maps:get(BoxId, Boxes, []))}
    end,
    process(Steps, NewBoxes).

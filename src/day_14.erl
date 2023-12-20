-module(day_14).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Grid = parse(Lines),
    Height = size(Grid),
    Width = size(element(1, Grid)),
    Tilted = tilt(Grid, Width, Height),
    calculate_load(Tilted, Width, Height);

run(two, Lines) ->
    Grid = parse(Lines),
    Height = size(Grid),
    Width = size(element(1, Grid)),
    Tilted = tilt_loop(1000000000, Grid, Width, Height),
    calculate_load(Tilted, Width, Height).


parse(Lines) ->
    parse(Lines, []).


parse([], Result) ->
    list_to_tuple(lists:reverse(Result));
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    parse(Lines, [list_to_tuple(binary_to_list(Line)) | Result]).


tilt(Grid, Width, Height) ->
    tilt(1, Grid, Width, Height).


tilt(X, Grid, Width, _) when X > Width ->
    Grid;
tilt(X, Grid, Width, Height) ->
    tilt(X + 1, tilt_column(X, Grid, Height), Width, Height).


tilt_column(X, Grid, Height) ->
    tilt_column(2, X, Grid, Height).


tilt_column(Y, _, Grid, Height) when Y > Height ->
    Grid;
tilt_column(1, X, Grid, Height) ->
    tilt_column(2, X, Grid, Height);
tilt_column(Y, X, Grid, Height) ->
    Row = element(Y, Grid),
    case element(X, Row) of
        $. -> tilt_column(Y + 1, X, Grid, Height);
        $# -> tilt_column(Y + 1, X, Grid, Height);
        $O ->
            case find_tilt_offset(Y, X, Grid, 1) of
                0 -> tilt_column(Y + 2, X, Grid, Height);
                Offset ->
                    Grid1 = setelement(Y, Grid, setelement(X, Row, $.)),
                    Row2 = element(Y - Offset, Grid1),
                    Grid2 = setelement(Y - Offset, Grid1, setelement(X, Row2, $O)),
                    tilt_column(Y + 1, X, Grid2, Height)
            end
    end.


find_tilt_offset(1, _, _, _) ->
    0;
find_tilt_offset(Y, _, _, Offset) when Y - Offset == 0 ->
    Offset - 1;
find_tilt_offset(Y, X, Grid, Offset) ->
    case element(X, element(Y - Offset, Grid)) of
        $# -> Offset - 1;
        $O -> Offset - 1;
        $. -> find_tilt_offset(Y, X, Grid, Offset + 1)
    end.


tilt_loop(N, Grid, Width, Height) ->
    tilt_loop(N, Grid, Width, Height, #{}).


tilt_loop(0, Grid, _, _, _) ->
    Grid;
tilt_loop(N, Grid, Width, Height, State) ->
    North = tilt(Grid, Width, Height),
    West = tilt(rotate(North, Width, Height), Height, Width),
    South = tilt(rotate(West, Height, Width), Width, Height),
    East = tilt(rotate(South, Width, Height), Height, Width),
    Tilted = rotate(East, Height, Width),
    case maps:get(Tilted, State, undefined) of
        undefined ->
            tilt_loop(N - 1, Tilted, Width, Height, State#{Tilted => N});
        LoopStart ->
            % loop detected
            LoopLength = LoopStart - N,
            FinalIndex = LoopStart - (N rem LoopLength) + 1,
            {Result, _} = lists:keyfind(FinalIndex, 2, maps:to_list(State)),
            Result
    end.


calculate_load(Grid, Width, Height) ->
    calculate_load(1, Grid, Width, Height, 0).


calculate_load(Y, _, _, Height, Result) when Y > Height ->
    Result;
calculate_load(Y, Grid, Width, Height, Result) ->
    Load = (Height - Y + 1) * count_rocks(element(Y, Grid), Width),
    calculate_load(Y + 1, Grid, Width, Height, Result + Load).


count_rocks(Row, Width) ->
    count_rocks(1, Row, Width, 0).


count_rocks(X, _, Width, Result) when X > Width ->
    Result;
count_rocks(X, Row, Width, Result) when element(X, Row) == $O ->
    count_rocks(X + 1, Row, Width, Result + 1);
count_rocks(X, Row, Width, Result) ->
    count_rocks(X + 1, Row, Width, Result).


rotate(Grid, Width, Height) ->
    lists:foldl(fun({X, Y}, NewGrid) ->
            Value = element(X, element(Height - Y + 1, Grid)),
            Row = element(X, NewGrid),
            setelement(X, NewGrid, setelement(Y, Row, Value))
        end,
        erlang:make_tuple(Width, erlang:make_tuple(Height, 0)),
        [{X, Y} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)]).

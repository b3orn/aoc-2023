-module(day_18).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Plan = parse(Lines),
    Coords = dig_trench(Plan, 1, 1, [{{1, 1}, {1, 1}}]),
    {Width, Height, Shifted} = shift_origin(Coords),
    Map = day_10:map_from_loop(Shifted, Width, Height),
    day_10:find_enclosed(Map) + length(Coords) - 1.


parse(Lines) ->
    parse(Lines, []).


parse([], Result) ->
    lists:reverse(Result);
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    case re:run(Line, "(U|D|R|L) (\\d+) \\((.*?)\\)", [{capture, all_but_first, binary}]) of
        nomatch ->
            parse(Lines, Result);
        {match, [Direction, Length, Color]} ->
            parse(Lines, [{Direction, binary_to_integer(Length), Color} | Result])
    end.


dig_trench([], _, _, Map) ->
    Map;
dig_trench([{<<"R">>, L, _} | Plan], X, Y, Map) ->
    Points1 = [_ | Points2] = [{X1, Y} || X1 <- lists:seq(X, X + L)],
    dig_trench(Plan, X + L, Y, Map ++ lists:zip(Points1, Points2, trim));
dig_trench([{<<"L">>, L, _} | Plan], X, Y, Map) ->
    Points1 = [_ | Points2] = [{X1, Y} || X1 <- lists:seq(X, X - L, -1)],
    dig_trench(Plan, X - L, Y, Map ++ lists:zip(Points1, Points2, trim));
dig_trench([{<<"U">>, L, _} | Plan], X, Y, Map) ->
    Points1 = [_ | Points2] = [{X, Y1} || Y1 <- lists:seq(Y, Y - L, -1)],
    dig_trench(Plan, X, Y - L, Map ++ lists:zip(Points1, Points2, trim));
dig_trench([{<<"D">>, L, _} | Plan], X, Y, Map) ->
    Points1 = [_ | Points2] = [{X, Y1} || Y1 <- lists:seq(Y, Y + L)],
    dig_trench(Plan, X, Y + L, Map ++ lists:zip(Points1, Points2, trim)).


shift_origin(Coords) ->
    {From, To} = lists:unzip(Coords),
    {X, Y} = lists:unzip(From ++ To),
    ShiftX = -(lists:min(X) - 1),
    ShiftY = -(lists:min(Y) - 1),
    Width = lists:max(X) + ShiftX,
    Height = lists:max(Y) + ShiftY,
    Shifted = [{{X0 + ShiftX, Y0 + ShiftY}, {X1 + ShiftX, Y1 + ShiftY}} || {{X0, Y0}, {X1, Y1}} <- Coords],
    {Width, Height, Shifted}.

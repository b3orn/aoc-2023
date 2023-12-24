-module(day_24).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Data = parse(Lines),
    Params = [{D, lin_eq_2d(D)} || D <- Data],
    Min = 200000000000000,
    Max = 400000000000000,
    count_intersections(Params, Min, Max).


parse(Lines) ->
    parse(Lines, []).


parse([], Result) ->
    lists:reverse(Result);
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    [Position, Velocity] = binary:split(Line, <<"@">>, [trim_all]),
    parse(Lines, [{parse_vector(Position), parse_vector(Velocity)} | Result]).


parse_vector(Line) ->
    Parts = binary:split(Line, <<",">>, [global, trim_all]),
    Parsed = [list_to_integer(string:strip(binary_to_list(P))) || P <- Parts],
    list_to_tuple(Parsed).


count_intersections(Data, Min, Max) ->
    count_intersections(Data, Min, Max, 0).


count_intersections([_], _, _, Result) ->
    Result;
count_intersections([{P, EqP} | Tail], Min, Max, Result) ->
    NewResult = lists:foldl(fun({Q, EqQ}, Res) ->
            case intersect_2d(EqP, EqQ) of
                {X, Y} when X > Min, Y > Min, X < Max, Y < Max ->
                    case intersect_time_2d(P, {X, Y}) of
                        T1 when T1 > 0 ->
                            case intersect_time_2d(Q, {X, Y}) of
                                T2 when T2 > 0 ->
                                    Res + 1;
                                _ ->
                                    Res
                            end;
                        _ ->
                            Res
                    end;
                _ -> Res
            end
        end,
        Result,
        Tail),
    count_intersections(Tail, Min, Max, NewResult).


lin_eq_2d({{PX, PY, _}, {VX, VY, _}}) ->
    M = VY/VX,
    B = PY - M * PX,
    {M, B}.


intersect_2d({M, _}, {M, _}) -> undefined;
intersect_2d({M1, B1}, {M2, B2}) ->
    X = (B2 - B1) / (M1 - M2),
    {X, M1 * X + B1}.


intersect_time_2d({{X, _, _}, {VX, _, _}}, {XI, _}) ->
    (XI - X) / VX.

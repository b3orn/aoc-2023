-module(day_12).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Data = parse_lines(Lines),
    find_options(Data);

run(two, Lines) ->
    Data = parse_lines(Lines),
    Unfolded = unfold(Data),
    find_options(Unfolded).


parse_lines(Lines) ->
    parse_lines(Lines, []).


parse_lines([], Result) ->
    lists:reverse(Result);
parse_lines([Line | Lines], Result) ->
    [Springs, Numbers] = binary:split(Line, <<" ">>, [global, trim_all]),
    Damaged = [binary_to_integer(N) || N <- binary:split(Numbers, <<",">>, [global, trim_all])],
    parse_lines(Lines, [{binary_to_list(Springs), Damaged} | Result]).


unfold(Lines) ->
    unfold(Lines, []).


unfold([], Result) ->
    lists:reverse(Result);
unfold([{Springs, Damaged} | Lines], Result) ->
    UnfoldedSprings = string:join(string:copies([Springs], 5), "?"),
    UnfoldedDamaged = string:copies(Damaged, 5),
    unfold(Lines, [{UnfoldedSprings, UnfoldedDamaged} | Result]).


find_options(Lines) ->
    lists:sum(utils:parallel(fun({S, D}) -> find_options(S, D) end, Lines)).


find_options(Springs, Damaged) ->
    {_, Result} = find_options(Springs, Damaged, 0, #{}),
    Result.


find_options([], [], _, Map) ->
    {Map, 1};
find_options([], _, _, Map) ->
    {Map, 0};
find_options([$# | _], [], _, Map) ->
    {Map, 0};
find_options(Springs, [], _, Map) ->
    case lists:member($#, Springs) of
        true -> {Map, 0};
        _ -> {Map, 1}
    end;
find_options([$#], [D], N, Map) when D == N + 1 ->
    {Map, 1};
find_options([$# | _], [D | _], N, Map) when N > D ->
    {Map, 0};
find_options([$# | Springs], Damaged, N, Map) ->
    find_options(Springs, Damaged, N + 1, Map);
find_options([$. | Springs], Damaged, 0, Map) ->
    find_options(Springs, Damaged, 0, Map);
find_options([$. | Springs], [D | Damaged], D, Map) ->
    find_options(Springs, Damaged, 0, Map);
find_options([$? | Springs], Damaged, N, Map) ->
    {MapLeft, Left} = test_option([$# | Springs], Damaged, N, Map),
    {MapRight, Right} = test_option([$. | Springs], Damaged, N, MapLeft),
    {MapRight, Left + Right};
find_options(_, _, _, Map) ->
    {Map, 0}.


test_option(Springs, Damaged, N, Map) ->
    case maps:get({Springs, Damaged, N}, Map, none) of
        none ->
            {NewMap, Result} = find_options(Springs, Damaged, N, Map),
            {NewMap#{{Springs, Damaged, N} => Result}, Result};
        Value ->
            {Map, Value}
    end.
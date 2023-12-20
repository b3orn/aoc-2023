-module(day_17).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Grid = parse(Lines),
    Height = size(Grid),
    Width = size(element(1, Grid)),
    best_path(Grid, Width, Height).


parse(Lines) ->
    parse(Lines, []).


parse([], Result) ->
    list_to_tuple(lists:reverse(Result));
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    parse(Lines, [list_to_tuple([C - $0 || <<C>> <= Line]) | Result]).


best_path(Grid, Width, Height) ->
    Queue = #{0 => [{{1, 1}, 1, 0, 0}, {{1, 1}, 0, 1, 0}]},
    State = #{{{1, 1}, 1, 0, 0} => 0, {{1, 1}, 0, 1, 0} => 0},
    best_path(Queue, State, Grid, Width, Height).

best_path(Queue, State, Grid, Width, Height) ->
    Loss = lists:min(maps:keys(Queue)),
    case process_options(maps:get(Loss, Queue, []), Loss, maps:without([Loss], Queue), State, Grid, Width, Height) of
        {undefined, undefined, FinalLoss} ->
            FinalLoss;
        {NewState, NewQueue, _} ->
            best_path(NewQueue, NewState, Grid, Width, Height)
    end.


process_options([], _, Queue, State, _, _, _) ->
    {State, Queue, undefined};
process_options([{{X, Y}, _, _, _} | _], Loss, _, _, _, X, Y) ->
    {undefined, undefined, Loss};
process_options([{Point, Dx, Dy, D} | Tail], Loss, Queue, State, Grid, Width, Height) ->
    Candidates = candidates(Point, Dx, Dy, D, Grid, Width, Height, State, Loss),
    NewState = State#{{Point, Dx, Dy, D} => Loss},
    NewQueue = merge(Candidates, Queue),
    process_options(Tail, Loss, NewQueue, NewState, Grid, Width, Height).


candidates({X, Y}, Dx, Dy, D, Grid, Width, Height, State, Loss) ->
    Candidates = [{{X + Dx, Y + Dy}, Dx, Dy, D + 1},
                  {{X - Dy, Y + Dx}, Dy, Dx, 1},
                  {{X + Dy, Y - Dx}, Dy, Dx, 1}],
    group(lists:filtermap(fun
            ({{X0, _}, _, _, _}) when X0 < 1 -> false;
            ({{_, Y0}, _, _, _}) when Y0 < 1 -> false;
            ({{X0, _}, _, _, _}) when X0 > Width -> false;
            ({{_, Y0}, _, _, _}) when Y0 > Height -> false;
            ({_, _, _, D0}) when D0 > 3 -> false;
            ({_, 0, 0, 0}) -> false;
            ({P, Dx0, Dy0, D0}) ->
                NewLoss = Loss + grid_get(P, Grid),
                case maps:get({P, Dx0, Dy0, D0}, State, Width * Height) of
                    L when L > NewLoss -> {true, {NewLoss, {P, Dx0, Dy0, D0}}};
                    _ -> false
                end
        end,
        Candidates)).


grid_get({X, Y}, Grid) ->
    element(X, element(Y, Grid)).


group(List) ->
    lists:foldl(fun({K, V}, Res) ->
            Res#{K => [V | maps:get(K, Res, [])]}
        end,
        #{},
        List).


merge(MapA, MapB) ->
    maps:fold(fun(K, V, Res) ->
            case V ++ maps:get(K, Res, []) of
                [] -> maps:without([K], Res);
                L -> Res#{K => lists:reverse(lists:keysort(1, lists:uniq(L)))}
            end
        end,
        MapB,
        MapA).

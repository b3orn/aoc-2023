-module(day_20).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Modules = parse(Lines),
    State = init_state(Modules),
    put(rx, #{}),
    {_, High, Low} = process_loop(1000, State, Modules),
    High * Low;
run(two, Lines) ->
    % lcm of nodes leading into &lb -> rx
    Modules = parse(Lines),
    State = init_state(Modules),
    Keys = maps:keys(maps:get(<<"lb">>, State)),
    put(rx, maps:from_list([{K, []} || K <- Keys])),
    N = 20000,
    put(n, N),
    process_loop(N, State, Modules),
    Loops = [V1 - V2 || [V1, V2 | _] <- maps:values(get(rx))],
    utils:least_common_multiple(Loops).


parse(Lines) ->
    parse(Lines, #{}).


parse([], Result) ->
    Result;
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([<<"broadcaster -> ", Dest/binary>> | Lines], Result) ->
    parse(Lines, Result#{broadcaster => split(Dest, <<",">>)});
parse([<<"%", Line/binary>> | Lines], Result) ->
    [Name, Dest] = split(Line, <<"->">>),
    parse(Lines, Result#{Name => {flipflop, split(Dest, <<",">>)}});
parse([<<"&", Line/binary>> | Lines], Result) ->
    [Name, Dest] = split(Line, <<"->">>),
    parse(Lines, Result#{Name => {conjunction, split(Dest, <<",">>)}}).


split(Line, Pattern) ->
    [list_to_binary(string:strip(binary_to_list(P))) || P <- binary:split(Line, Pattern, [global, trim_all])].


init_state(Modules) ->
    init_state(maps:to_list(Modules), Modules, #{}).


init_state([], _, State) ->
    State;
init_state([{Name, {flipflop, _}} | Tail], Modules, State) ->
    init_state(Tail, Modules, State#{Name => off});
init_state([{Name, {conjunction, _}} | Tail], Modules, State) ->
    Inputs = lists:foldl(fun
            ({Input, {_, Dest}}, Result) ->
                case lists:member(Name, Dest) of
                    true -> Result#{Input => low};
                    _ -> Result
                end;
            ({Input, Dest}, Result) ->
                case lists:member(Name, Dest) of
                    true -> Result#{Input => low};
                    _ -> Result
                end
        end,
        #{},
        maps:to_list(Modules)),
    init_state(Tail, Modules, State#{Name => Inputs});
init_state([_ | Tail], Modules, State) ->
    init_state(Tail, Modules, State).


process_loop(N, State, Modules) ->
    process_loop(N, State, Modules, 0, 0).


process_loop(0, State, _, High, Low) ->
    {State, High, Low};
process_loop(N, State, Modules, High, Low) ->
    {NewState, NewHigh, NewLow} = process(State, Modules),
    put(rx, maps:map(fun(_, Value) ->
        lists:map(fun
                (high) -> get(n) - N;
                (Val) -> Val
            end,
            Value)
        end,
        get(rx))),
    process_loop(N - 1, NewState, Modules, High + NewHigh, Low + NewLow).


process(State, Modules) ->
    Input = [{button, broadcaster, low}],
    process(Input, State, Modules, 0, 1).

process([], State, _, High, Low) ->
    {State, High, Low};
process(Input, State, Modules, High, Low) ->
    {NewState, Output} = step(Input, State, Modules),
    NewHigh = High + length([1 || {_, _, high} <- Output]),
    NewLow = Low + length([1 || {_, _, low} <- Output]),
    process(Output, NewState, Modules, NewHigh, NewLow).


step(Input, State, Modules) ->
    step(Input, [], State, Modules).


step([], Output, State, _) ->
    {State, Output};
step([{Sender, <<"output">>, Signal} | Input], Output, State, Modules) ->
    io:format("~s -> output: ~p~n", [Sender, Signal]),
    step(Input, Output, State, Modules);
step([{_, broadcaster, Signal} | Input], Output, State, Modules) ->
    Destinations = maps:get(broadcaster, Modules),
    step(Input, Output ++ [{broadcaster, D, Signal} || D <- Destinations], State, Modules);
step([{Sender, Name, Signal} | Input], Output, State, Modules) ->
    {NewState, NewOutput} = case maps:get(Name, Modules, undefined) of
        undefined ->
            {State, []};
        {flipflop, Destinations} ->
            step_flipflop(Signal, Name, State, Destinations);
        {conjunction, Destinations} ->
            step_conjunction(Sender, Signal, Name, State, Destinations)
    end,
    case maps:get(Name, get(rx), none) of
        none -> ok;
        Last ->
            case lists:keyfind(<<"lb">>, 2, NewOutput) of
                {_, _, high} ->
                    put(rx, maps:update(Name, [high | Last], get(rx)));
                _ ->
                    ok
            end
    end,
    step(Input, Output ++ NewOutput, NewState, Modules).


step_flipflop(low, Name, State, Destinations) ->
    case maps:get(Name, State) of
        off ->
            {State#{Name => on}, [{Name, D, high} || D <- Destinations]};
        on ->
            {State#{Name => off}, [{Name, D, low} || D <- Destinations]}
    end;
step_flipflop(_, _, State, _) ->
    {State, []}.


step_conjunction(Sender, Signal, Name, State, Destinations) ->
    NewState = State#{Name => maps:put(Sender, Signal, maps:get(Name, State))},
    AllHigh = lists:all(fun
            (high) -> true;
            (low) -> false
        end,
        maps:values(maps:get(Name, NewState))),
    NewSignal = case AllHigh of
        true -> low;
        false -> high
    end,
    {NewState, [{Name, D, NewSignal} || D <- Destinations]}.

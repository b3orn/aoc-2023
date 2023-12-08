#!/usr/bin/env escript


-spec main([string()]) -> ok.
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    {Instructions, Nodes} = parse_file(Lines),
    Paths = traverse_map(Instructions, Nodes),
    Steps = [length(Path) - 1 || Path <- Paths],
    TotalSteps = least_common_multiple(Steps),
    io:format("~p~n~p~n", [Steps, TotalSteps]);

main(_) ->
    io:format("usage: escript solution.escript filename~n").


parse_file([Instructions | Rest]) ->
    {binary_to_list(Instructions), parse_nodes(Rest, #{})}.

parse_nodes([], Result) -> Result;
parse_nodes([Line | Lines], Result) ->
    case re:run(Line, "(\\w+) = \\((\\w+), (\\w+)\\)", [{capture, all_but_first, binary}]) of
        {match, [Node, Left, Right]} ->
            parse_nodes(Lines, Result#{Node => {Left, Right}});
        _ ->
            parse_nodes(Lines, Result)
    end.


traverse_map(Instructions, Nodes) ->
    StartNodes = maps:filter(fun(<<_, _, "A">>, _) -> true; (_, _) -> false end, Nodes),
    parallel(fun({Node, Loc}) ->
            traverse_map(Instructions, Loc, Instructions, Nodes, [Node])
        end,
        maps:to_list(StartNodes)).


traverse_map([], Location, Instructions, Nodes, Path) ->
    traverse_map(Instructions, Location, Instructions, Nodes, Path);

traverse_map([Dir | Instructions], Loc, AllInstructions, Nodes, Path) ->
    Next = case Dir of
        $L -> element(1, Loc);
        $R -> element(2, Loc)
    end,
    case Next of
        <<_, _, "Z">> ->
            [Next | Path];
        _ ->
            NewLoc = maps:get(Next, Nodes),
            traverse_map(Instructions, NewLoc, AllInstructions, Nodes, [Next | Path])
    end.


least_common_multiple(Numbers) ->
    Factorized = [frequencies(factorize(N)) || N <- Numbers],
    Factors = lists:foldl(fun(Factors, Result) ->
            maps:fold(fun(Factor, Count, R) ->
                    R#{Factor => max(maps:get(Factor, R, 0), Count)}
                end,
                Result,
                Factors)
        end,
        #{},
        Factorized),
    io:format("~p~n", [Factors]),
    round(maps:fold(fun(K, V, R) -> R * math:pow(K, V) end, 1, Factors)).


factorize(N) -> factorize(N, primes(), []).

factorize(1, _, Result) ->
    Result;
factorize(N, [P | _], Result) when N rem P == 0 ->
    factorize(N div P, primes(), [P | Result]);
factorize(N, [_ | Primes], Result) ->
    factorize(N, Primes, Result).


frequencies(Values) ->
    lists:foldl(fun(K, S) -> S#{K => maps:get(K, S, 0) + 1} end, #{}, Values).


% first 60 primes are enough
primes() ->
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
     73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
     179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281].


-spec parallel(function(), [any()]) -> [any()].
parallel(Fun, Values) ->
    gather(divide(Fun, Values)).


-spec divide(function(), [any()]) -> [pid()].
divide(Fun, Values) ->
    Self = self(),
    [spawn(fun() -> Self ! {self(), Fun(Value)} end) || Value <- Values].


-spec gather([pid()]) -> [any()].
gather(Pids) ->
    gather(Pids, Pids, #{}).

gather([], Pids, Results) ->
    [maps:get(Pid, Results) || Pid <- Pids];

gather(Pids, AllPids, Results) ->
    receive
        {Pid, Value} ->
            gather(lists:delete(Pid, Pids), AllPids, Results#{Pid => Value})
    end.

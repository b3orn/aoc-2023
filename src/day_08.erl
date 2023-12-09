-module(day_08).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    {Instructions, Nodes} = parse_file(Lines),
    Path = traverse_map_one(Instructions, Nodes),
    length(Path) - 1;

run(two, Lines) ->
    {Instructions, Nodes} = parse_file(Lines),
    Paths = traverse_map_two(Instructions, Nodes),
    Steps = [length(Path) - 1 || Path <- Paths],
    utils:least_common_multiple(Steps).


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


traverse_map_one(Instructions, Nodes) ->
    Start = maps:get(<<"AAA">>, Nodes),
    traverse_map_one(Instructions, Start, Instructions, Nodes, [<<"AAA">>]).


traverse_map_one([], Location, Instructions, Nodes, Path) ->
    traverse_map_one(Instructions, Location, Instructions, Nodes, Path);

traverse_map_one([Dir | Instructions], Loc, AllInstructions, Nodes, Path) ->
    Next = case Dir of
        $L -> element(1, Loc);
        $R -> element(2, Loc)
    end,
    case Next of
        <<"ZZZ">> ->
            [<<"ZZZ">> | Path];
        _ ->
            NewLoc = maps:get(Next, Nodes),
            traverse_map_one(Instructions, NewLoc, AllInstructions, Nodes, [Next | Path])
    end.


traverse_map_two(Instructions, Nodes) ->
    StartNodes = maps:filter(fun(<<_, _, "A">>, _) -> true; (_, _) -> false end, Nodes),
    utils:parallel(fun({Node, Loc}) ->
            traverse_map_two(Instructions, Loc, Instructions, Nodes, [Node])
        end,
        maps:to_list(StartNodes)).


traverse_map_two([], Location, Instructions, Nodes, Path) ->
    traverse_map_two(Instructions, Location, Instructions, Nodes, Path);

traverse_map_two([Dir | Instructions], Loc, AllInstructions, Nodes, Path) ->
    Next = case Dir of
        $L -> element(1, Loc);
        $R -> element(2, Loc)
    end,
    case Next of
        <<_, _, "Z">> ->
            [Next | Path];
        _ ->
            NewLoc = maps:get(Next, Nodes),
            traverse_map_two(Instructions, NewLoc, AllInstructions, Nodes, [Next | Path])
    end.

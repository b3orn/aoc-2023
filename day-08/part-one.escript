#!/usr/bin/env escript


-spec main([string()]) -> ok.
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    {Instructions, Nodes} = parse_file(Lines),
    Path = traverse_map(Instructions, Nodes),
    Steps = length(Path) - 1,
    io:format("~p~n", [Steps]);

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
    Start = maps:get(<<"AAA">>, Nodes),
    traverse_map(Instructions, Start, Instructions, Nodes, [<<"AAA">>]).


traverse_map([], Location, Instructions, Nodes, Path) ->
    traverse_map(Instructions, Location, Instructions, Nodes, Path);

traverse_map([Dir | Instructions], Loc, AllInstructions, Nodes, Path) ->
    Next = case Dir of
        $L -> element(1, Loc);
        $R -> element(2, Loc)
    end,
    case Next of
        <<"ZZZ">> ->
            [<<"ZZZ">> | Path];
        _ ->
            NewLoc = maps:get(Next, Nodes),
            traverse_map(Instructions, NewLoc, AllInstructions, Nodes, [Next | Path])
    end.

-module(day_25).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    % find connections between random nodes, count used edges
    % split graph at three most used edges
    Graph = parse(Lines),
    Nodes = graph_nodes(Graph),
    Edges = graph_edges(Graph),
    NewEdges = simulate(10 * map_size(Edges), Graph, Edges, Nodes),
    [{E1, _}, {E2, _}, {E3, _} | _] = lists:reverse(lists:keysort(2, maps:to_list(NewEdges))),
    io:format("~p ~p ~p~n", [E1, E2, E3]),
    SplitGraph = split_graph([E1, E2, E3], Graph),
    PartialGraphs = partial_graphs(tuple_to_list(Nodes), SplitGraph),
    Sizes = [map_size(G) || G <- PartialGraphs],
    io:format("~p~n", [Sizes]),
    lists:foldl(fun(A, B) -> A * B end, 1, Sizes).


parse(Lines) ->
    parse(Lines, #{}).


parse([], Result) ->
    maps:fold(fun(Key, Values, Res) ->
            lists:foldl(fun(Component, R) ->
                    R#{Component => lists:uniq([Key | maps:get(Component, R, [])])}
                end,
                Res,
                Values)
        end,
        Result,
        Result);
parse([<<>> | Lines], Result) ->
    parse(Lines, Result);
parse([Line | Lines], Result) ->
    [Name, Components] = binary:split(Line, <<":">>, [trim_all]),
    parse(Lines, Result#{Name => binary:split(Components, <<" ">>, [global, trim_all])}).


graph_nodes(Graph) ->
    list_to_tuple(maps:keys(maps:fold(fun(Key, Nodes, Result) ->
            lists:foldl(fun(Node, Res) ->
                    Res#{Node => true}
                end,
                Result#{Key => true},
                Nodes)
        end,
        #{},
        Graph))).


graph_edges(Graph) ->
    maps:fold(fun(Key, Nodes, Edges) ->
            lists:foldl(fun(Node, NewEdges) ->
                    case maps:is_key({Node, Key}, NewEdges) of
                        true -> NewEdges;
                        false -> NewEdges#{{Key, Node} => 0}
                    end
                end,
                Edges,
                Nodes)
        end,
        #{},
        Graph).


simulate(N, Graph, Edges, Nodes) ->
    simulate(N, Graph, Edges, Nodes, #{}, 0).

simulate(0, _, Edges, _, _, _) ->
    Edges;
simulate(_, _, Edges, _, _, 10) ->
    Edges;
simulate(N, Graph, Edges, Nodes, Visited, Error) ->
    Start = element(rand:uniform(size(Nodes)), Nodes),
    End = element(rand:uniform(size(Nodes)), Nodes),
    case maps:is_key({Start, End}, Visited) of
        true ->
            simulate(N, Graph, Edges, Nodes, Visited, Error + 1);
        _ ->
            Path = shortest_path(Start, End, Graph),
            NewEdges = maps:fold(fun({A, B}, _, Res) ->
                    case maps:get({A, B}, Res, undefined) of
                        undefined ->
                            #{{B, A} := Count} = Res,
                            Res#{{B, A} => Count + 1};
                        Count ->
                            Res#{{A, B} => Count + 1}
                    end
                end,
                Edges,
                Path),
            NewVisited = Visited#{{Start, End} => true, {End, Start} => true},
            simulate(N - 1, Graph, NewEdges, Nodes, NewVisited, 0)
    end.


shortest_path(Start, End, Graph) ->
    shortest_path(#{0 => [Start]}, #{Start => #{}}, End, Graph).


shortest_path(Queue, State, End, Graph) ->
    Cost = lists:min(maps:keys(Queue)),
    #{Cost := Options} = Queue,
    Queue2 = maps:without([Cost], Queue),
    case check_options(Options, Queue2, State, Cost, End, Graph) of
        {NewQueue, NewState, undefined} ->
            shortest_path(NewQueue, NewState, End, Graph);
        {_, _, Path} ->
            Path
    end.


check_options([], Queue, State, _, _, _) ->
    {Queue, State, undefined};
check_options([End | _], _, State, _, End, _) ->
    {undefined, undefined, maps:get(End, State)};
check_options([Node | Nodes], Queue, State, Cost, End, Graph) ->
    #{Node := Connections} = Graph,
    #{Node := Path} = State,
    {Next, NewState} = lists:foldl(fun(Con, {Q, S}) ->
            case maps:get(Con, S, undefined) of
                undefined -> {[Con | Q], S#{Con => Path#{{Node, Con} => true}}};
                _ -> {Q, S}
            end
        end,
        {[], State},
        Connections),
    NewQueue = Queue#{Cost + 1 => Next ++ maps:get(Cost + 1, Queue, [])},
    check_options(Nodes, NewQueue, NewState, Cost, End, Graph).


split_graph([], Graph) ->
    Graph;
split_graph([{A, B} | Edges], Graph) ->
    #{A := NodesA, B := NodesB} = Graph,
    NewGraph = Graph#{A => lists:delete(B, NodesA), B => lists:delete(A, NodesB)},
    split_graph(Edges, NewGraph).


partial_graphs(Nodes, Graph) ->
    partial_graphs(Nodes, Graph, []).


partial_graphs([], _, Graphs) ->
    Graphs;
partial_graphs([Node | Nodes], Graph, Graphs) ->
    NewGraph = build_graph(Node, Graph, #{}),
    NewNodes = lists:filter(fun(N) -> not maps:is_key(N, NewGraph) end, Nodes),
    partial_graphs(NewNodes, Graph, [NewGraph | Graphs]).


build_graph(Node, Graph, NewGraph) ->
    #{Node := Nodes} = Graph,
    lists:foldl(fun(N, G) ->
            case maps:get(N, G, undefined) of
                undefined -> build_graph(N, Graph, G);
                _ -> G
            end
        end,
        NewGraph#{Node => Nodes},
        Nodes).

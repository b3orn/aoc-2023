-module(day_19).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    {Workflows, Parts} = parse(Lines),
    Processed = process(Parts, Workflows),
    lists:sum([lists:sum(maps:values(P)) || P <- Processed]).


parse(Lines) ->
    parse(Lines, #{}, []).


parse([], Workflows, Parts) ->
    {Workflows, Parts};
parse([<<>> | Lines], Workflows, Parts) ->
    parse(Lines, Workflows, Parts);
parse([Line = <<"{", _/binary>> | Lines], Workflows, Parts) ->
    parse(Lines, Workflows, parse_part(Line) ++ Parts);
parse([Line | Lines], Workflows, Parts) ->
    parse(Lines, parse_workflow(Line, Workflows), Parts).


parse_part(Line) ->
    case re:run(Line, "(x|m|a|s)=(\\d+)", [global, {capture, all_but_first, binary}]) of
        nomatch -> [];
        {match, Match} ->
            [maps:from_list([{K, binary_to_integer(V)} || [K, V] <- Match])]
    end.


parse_workflow(Line, Workflows) ->
    case re:run(Line, "(\\w+)\\{(.*?)\\}", [{capture, all_but_first, binary}]) of
        nomatch -> Workflows;
        {match, [Name, Rules]} ->
            Workflows#{Name => parse_rules(binary:split(Rules, <<",">>, [global]), [])}
    end.


parse_rules([], Result) ->
    lists:reverse(Result);
parse_rules([<<"A">> | Rules], Result) ->
    parse_rules(Rules, [<<"A">> | Result]);
parse_rules([<<"R">> | Rules], Result) ->
    parse_rules(Rules, [<<"R">> | Result]);
parse_rules([Rule | Rules], Result) ->
    case re:run(Rule, "(x|m|a|s)(<|>)(\\d+):(\\w+)", [{capture, all_but_first, binary}]) of
        {match, [Attr, Op, Value, Next]} ->
            parse_rules(Rules, [{Op, Attr, binary_to_integer(Value), Next} | Result]);
        _ ->
            parse_rules(Rules, [Rule | Result])
    end.


process(Parts, Workflows) ->
    process(Parts, Workflows, []).


process([], _, Result) ->
    Result;
process([Part | Parts], Workflows, Result) ->
    process(Parts, Workflows, process_part(Part, <<"in">>, Workflows) ++ Result).


process_part(Part, Name, Workflows) ->
    Workflow = maps:get(Name, Workflows),
    case run_workflow(Part, Workflow) of
        <<"R">> ->
            [];
        <<"A">> ->
            [Part];
        Next ->
            process_part(Part, Next, Workflows)
    end.


run_workflow(_, []) ->
    <<"A">>;
run_workflow(Part, [{<<"<">>, Attr, Value, Next} | Workflow]) ->
    case maps:get(Attr, Part) < Value of
        true -> Next;
        _ ->
            run_workflow(Part, Workflow)
    end;
run_workflow(Part, [{<<">">>, Attr, Value, Next} | Workflow]) ->
    case maps:get(Attr, Part) > Value of
        true -> Next;
        _ ->
            run_workflow(Part, Workflow)
    end;
run_workflow(_, [Next | _]) ->
    Next.

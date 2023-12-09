-module(aoc).
-feature(maybe_expr, enable).

-export([main/1]).


-spec main([string()]) -> ok.
main(["list"]) ->
    maybe
        {ok, Days} = find_days(),
        io:format("~s~n", [lists:join(" ", Days)])
    end;

main(["all"]) ->
    maybe
        {ok, Days} = find_days(),
        lists:foreach(fun(Day) -> main([Day]) end, Days)
    end;

main([Day]) ->
    DayDir = lists:flatten(["day-", string:pad(Day, 2, leading, "0")]),
    Path = filename:join([root_dir(), "data", DayDir, "input.txt"]),
    main([Day, Path]);

main([Day, Filename]) ->
    DayNo = list_to_integer(Day),
    ModuleName = lists:flatten(["day_", string:pad(Day, 2, leading, "0")]),
    maybe
        {ok, Data} ?= file:read_file(Filename),
        Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
        {module, Module} ?= code:load_file(list_to_atom(ModuleName)),
        lists:foreach(fun(Part) ->
                Start = erlang:monotonic_time(nanosecond),
                Result = catch Module:run(Part, Lines),
                Stop = erlang:monotonic_time(nanosecond),
                Duration = (Stop - Start) / 1000000,
                case load_result(DayNo, Part) of
                    undefined ->
                        io:format("~2..0w ~w ~.3fms: ~w~n", [DayNo, Part, Duration, Result]);
                    Result ->
                        io:format("~2..0w ~w ~.3fms: ~w ✓~n", [DayNo, Part, Duration, Result]);
                    Expected ->
                        io:format("~2..0w ~w ~.3fms: ~w, expected ~w~n", [DayNo, Part, Duration, Result, Expected])
                end
            end,
            Module:parts())
    else
        Error ->
            io:format("Error: ~p~n", [Error])
    end;

main(_) ->
    io:format("usage: aoc [DAY [FILENAME]|list|all]~n").


find_days() ->
    maybe
        {ok, Filenames} ?= file:list_dir(filename:dirname(code:which(?MODULE))),
        {ok, lists:sort(lists:filtermap(fun(Filename) ->
                case re:run(Filename, "day_(\\d+)\\.beam", [{capture, all_but_first, list}]) of
                    {match, [Day]} -> {true, Day};
                    _ -> false
                end
            end,
            Filenames))}
    end.


load_result(Day, Part) ->
    Filename = filename:join([root_dir(), "data", "results.txt"]),
    maybe
        {ok, [Results]} ?= file:consult(Filename),
        {Day, Parts} ?= lists:keyfind(Day, 1, Results),
        {Part, Result} ?= lists:keyfind(Part, 1, Parts),
        Result
    else
        _ ->
            undefined
    end.


root_dir() ->
    filename:dirname(filename:dirname(code:which(?MODULE))).

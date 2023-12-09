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
    Root = filename:dirname(filename:dirname(code:which(?MODULE))),
    DayDir = lists:flatten(["day-", string:pad(Day, 2, leading, "0")]),
    Path = filename:join([Root, "data", DayDir, "input.txt"]),
    main([Day, Path]);

main([Day, Filename]) ->
    DayNo = string:pad(Day, 2, leading, "0"),
    ModuleName = lists:flatten(["day_", DayNo]),
    maybe
        {ok, Data} ?= file:read_file(Filename),
        Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
        {module, Module} ?= code:load_file(list_to_atom(ModuleName)),
        lists:foreach(fun(Part) ->
                Start = erlang:monotonic_time(nanosecond),
                Result = catch Module:run(Part, Lines),
                Stop = erlang:monotonic_time(nanosecond),
                Duration = (Stop - Start) / 1000000,
                io:format("~s ~p ~.3fms: ~p~n", [DayNo, Part, Duration, Result])
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

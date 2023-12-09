-module(aoc).
-feature(maybe_expr, enable).

-export([main/1]).


-spec main([string()]) -> ok.
main([Day, Filename]) ->
    ModuleName = lists:flatten(["day_", string:pad(Day, 2, leading, "0")]),
    maybe
        {ok, Data} ?= file:read_file(Filename),
        Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
        {module, Module} ?= code:load_file(list_to_atom(ModuleName)),
        lists:foreach(fun(Part) ->
                Start = erlang:monotonic_time(nanosecond),
                Result = Module:run(Part, Lines),
                Stop = erlang:monotonic_time(nanosecond),
                Duration = (Stop - Start) / 1000000,
                io:format("~p ~.3fms: ~p~n", [Part, Duration, Result])
            end,
            Module:parts())
    else
        Error ->
            io:format("Error: ~p~n", [Error])
    end;

main(_) ->
    io:format("usage: aoc day filename~n").

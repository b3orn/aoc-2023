#!/usr/bin/env escript


-spec main([string()]) -> ok.
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Parsed = parse_file(Lines),
    Mapped = map_seeds(maps:get(seeds, Parsed), Parsed),
    MinLocation = lists:min([L || {_, L} <- Mapped]),
    io:format("~p~n", [MinLocation]);

main(_) ->
    io:format("usage: escript solution.escript filename~n").


parse_file(Lines) ->
    parse_file(Lines, undefined, #{seeds => [],
                                   soil => [],
                                   fertilizer => [],
                                   water => [],
                                   light => [],
                                   temperature => [],
                                   humidity => [],
                                   location => []}).


parse_file([], _, Result) ->
    Result;

parse_file([<<"seeds:", Line/binary>> | Lines], undefined, Result) ->
    parse_file(Lines, undefined, Result#{seeds => parse_numbers(Line)});

parse_file([Line | Lines], State, Result) ->
    case re:run(Line, "(.*?) map:", [{capture, all_but_first, binary}]) of
        {match, [Key]} ->
            parse_file(Lines, mapping_keys(Key), Result);
        nomatch ->
            Mapping = lists:append(maps:get(State, Result), parse_mapping(Line)),
            parse_file(Lines, State, maps:update(State, Mapping, Result))
    end.


parse_mapping(Line) ->
    case parse_numbers(Line) of
        [] -> [];
        Numbers -> [list_to_tuple(Numbers)]
    end.


mapping_keys(<<"seed-to-soil">>) -> soil;
mapping_keys(<<"soil-to-fertilizer">>) -> fertilizer;
mapping_keys(<<"fertilizer-to-water">>) -> water;
mapping_keys(<<"water-to-light">>) -> light;
mapping_keys(<<"light-to-temperature">>) -> temperature;
mapping_keys(<<"temperature-to-humidity">>) -> humidity;
mapping_keys(<<"humidity-to-location">>) -> location.


-spec parse_numbers(binary()) -> [non_neg_integer()].
parse_numbers(Numbers) ->
    ParsedNumbers = binary:split(Numbers, <<" ">>, [global, trim_all]),
    lists:map(fun erlang:binary_to_integer/1, ParsedNumbers).


map_seeds(Seeds, Mappings) ->
    map_seeds(Seeds, Mappings, []).


map_seeds([], _, Result) ->
    lists:reverse(Result);

map_seeds([Seed | Seeds], Mappings, Result) ->
    Keys = [soil, fertilizer, water, light, temperature, humidity, location],
    Mapped = lists:foldl(fun(Key, Value) ->
            map_value(Value, maps:get(Key, Mappings))
        end,
        Seed,
        Keys),

    map_seeds(Seeds, Mappings, [{Seed, Mapped} | Result]).


-spec map_value(number(), [{non_neg_integer(), non_neg_integer(), non_neg_integer()}]) -> number().
map_value(Value, []) ->
    Value;

map_value(Value, [{Dest, Start, Length} | _]) when Value >= Start, Value < Start + Length ->
    Dest + Value - Start;

map_value(Value, [_ | Mappings]) ->
    map_value(Value, Mappings).

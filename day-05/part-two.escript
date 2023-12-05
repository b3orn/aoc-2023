#!/usr/bin/env escript


-spec main([string()]) -> ok.
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Parsed = parse_file(Lines),
    Mappings = [maps:get(Key, Parsed) || Key <- [soil, fertilizer, water, light, temperature, humidity, location]],
    Mapped = map_seeds(maps:get(seeds, Parsed), Mappings),
    MinLocation = lists:min([Start || {Start, _} <- Mapped]),
    io:format("~p~n", [length(Mapped)]),
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
    Seeds = parse_numbers(Line),
    parse_file(Lines, undefined, Result#{seeds => parse_seeds(Seeds, [])});

parse_file([Line | Lines], State, Result) ->
    case re:run(Line, "(.*?) map:", [{capture, all_but_first, binary}]) of
        {match, [Key]} ->
            parse_file(Lines, mapping_keys(Key), Result);
        nomatch ->
            Mapping = lists:append(maps:get(State, Result), parse_mapping(Line)),
            parse_file(Lines, State, maps:update(State, Mapping, Result))
    end.


parse_seeds([], Result) ->
    lists:reverse(Result);

parse_seeds([Start, Length | Seeds], Result) ->
    parse_seeds(Seeds, [{Start, Length} | Result]).


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


map_seeds(Ranges, []) ->
    Ranges;

map_seeds(Ranges, [Mapping | Mappings]) ->
    io:format("~p~n", [length(Ranges)]),
    NewRanges = lists:flatten([map_range(Range, Mapping, Mapping) || Range <- Ranges]),
    map_seeds(NewRanges, Mappings).


map_range(Range, [], _) ->
    [Range];

% range start is within map
map_range({Start, Length}, [{Dest, SrcStart, SrcLength} | _], Mappings) when Start >= SrcStart, Start < SrcStart + SrcLength ->
    MappableLength = SrcStart + SrcLength - Start,
    case Length - MappableLength of
        N when N =< 0 -> % range fits completely into map
            [{Dest + Start - SrcStart, Length}];
        N -> % start of the range is within map, tail is not, split the range
            [{Dest + Start - SrcStart, MappableLength} | map_range({SrcStart + SrcLength, N}, Mappings, Mappings)]
    end;

% range start is outside of map, but range and map overlap
map_range({Start, Length}, [{Dest, SrcStart, SrcLength} | _], Mappings) when SrcStart > Start, SrcStart < Start + Length ->
    PrefixLength = SrcStart - Start,
    Prefix = map_range({Start, PrefixLength}, Mappings, Mappings),
    case Length - PrefixLength of
        N when N =< SrcLength -> % end of range is within map
            [{Dest, N} | Prefix];
        N -> % end of map is beyond end of map
            [{Dest, SrcLength}, Prefix | map_range({SrcStart + SrcLength, N - SrcLength}, Mappings, Mappings)]
    end;

map_range(Range, [_ | Mappings], AllMappings) ->
    map_range(Range, Mappings, AllMappings).

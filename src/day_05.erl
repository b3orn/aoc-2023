-module(day_05).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Parsed = parse_file(Lines),
    Mapped = map_seeds(maps:get(seeds, Parsed), Parsed),
    lists:min([L || {_, L} <- Mapped]);

run(two, Lines) ->
    Parsed = parse_file(Lines),
    Seeds = parse_seeds(maps:get(seeds, Parsed), []),
    Mappings = [maps:get(Key, Parsed) || Key <- [soil, fertilizer, water, light, temperature, humidity, location]],
    Mapped = map_seed_ranges(Seeds, Mappings),
    lists:min([L || {L, _} <- Mapped]).


parse_file(Lines) ->
    parse_file(Lines, undefined, #{}).


parse_file([], _, Result) ->
    Result;

parse_file([<<>> | Lines], State, Result) ->
    parse_file(Lines, State, Result);

parse_file([<<"seeds:", Line/binary>> | Lines], undefined, Result) ->
    parse_file(Lines, undefined, Result#{seeds => parse_numbers(Line)});

parse_file([Line | Lines], State, Result) ->
    
    case re:run(Line, "(.*?) map:", [{capture, all_but_first, binary}]) of
        {match, [Key]} ->
            parse_file(Lines, mapping_keys(Key), Result);
        nomatch ->
            Mapping = maps:get(State, Result, []) ++ parse_mapping(Line),
            parse_file(Lines, State, Result#{State => Mapping})
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


map_seed_ranges(Ranges, []) ->
    Ranges;

map_seed_ranges(Ranges, [Mapping | Mappings]) ->
    NewRanges = lists:flatten([map_range(Range, Mapping, Mapping) || Range <- Ranges]),
    map_seed_ranges(NewRanges, Mappings).


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

-module(day_04).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    lists:sum([Points || {_, {Points, _}} <- parse_cards(Lines)]);

run(two, Lines) ->
    InitialCards = parse_cards(Lines),
    play_cards(InitialCards).


-spec parse_cards(binary()) -> [non_neg_integer()].
parse_cards(Lines) ->
    parse_cards(Lines, []).


-spec parse_cards(binary(), [non_neg_integer()]) -> [non_neg_integer()].
parse_cards([], Result) ->
    lists:reverse(Result);

parse_cards([Line | Lines], Result) ->
    Match = re:run(Line, "Card\\s+(\\d+): (.*)\\|(.*)", [{capture, all_but_first, binary}]),
    case Match of
        nomatch ->
            parse_cards(Lines, Result);
        {match, [Card, Winning, Numbers]} ->
            CardId = binary_to_integer(Card),
            ParsedWinning = parse_numbers(Winning),
            ParsedNumbers = parse_numbers(Numbers),
            parse_cards(Lines, [{CardId, calc_score(ParsedWinning, ParsedNumbers)} | Result])
    end.


-spec calc_score([non_neg_integer()], [non_neg_integer()]) -> non_neg_integer().
calc_score(Winning, Numbers) ->
    Intersection = sets:to_list(
        sets:intersection(
            sets:from_list(Winning),
            sets:from_list(Numbers))),
    case length(Intersection) of
        0 -> {0, 0};
        N -> {round(math:pow(2, N - 1)), N}
    end.


-spec parse_numbers(binary()) -> [non_neg_integer()].
parse_numbers(Numbers) ->
    ParsedNumbers = binary:split(Numbers, <<" ">>, [global, trim_all]),
    lists:map(fun erlang:binary_to_integer/1, ParsedNumbers).


-spec play_cards(Cards) -> Cards
    when Cards :: [{pos_integer(), non_neg_integer()}].
play_cards(Cards) ->
    play_cards(Cards, list_to_tuple(Cards), 0).


-spec play_cards(Cards, Cards, Cards) -> Cards
    when Cards :: [{pos_integer(), non_neg_integer()}].
play_cards([], _, Result) ->
    Result;
play_cards([{Card, {_, N}} | Cards], AllCards, Result) ->
    NewCards = [element(I, AllCards) || I <- lists:seq(Card + 1, Card + N)],
    play_cards(NewCards ++ Cards, AllCards, Result + 1).

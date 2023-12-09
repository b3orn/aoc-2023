-module(day_07).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Hands = parse_hands(Lines),
    ClassifiedHands = classify_hands_one(Hands),
    Result = process_hands(ClassifiedHands),
    lists:sum(Result);

run(two, Lines) ->
    Hands = parse_hands(Lines),
    DevaluedJokers = [{lists:map(fun(11) -> 1; (C) -> C end, Cards), Bid} || {Cards, Bid} <- Hands],
    ClassifiedHands = classify_hands_two(DevaluedJokers),
    Result = process_hands(ClassifiedHands),
    lists:sum(Result).


parse_hands(Lines) ->
    parse_hands(Lines, []).


parse_hands([], Result) ->
    lists:reverse(Result);

parse_hands([Line | Lines], Result) ->
    [Cards, Bid] = binary:split(Line, <<" ">>, [global, trim_all]),
    ParsedCards = parse_cards(Cards),
    parse_hands(Lines, [{ParsedCards, binary_to_integer(Bid)} | Result]).


parse_cards(Card) ->
    parse_cards(binary_to_list(Card), []).


parse_cards([], Result) -> lists:reverse(Result);
parse_cards([$A | Cards], Result) -> parse_cards(Cards, [14 | Result]);
parse_cards([$K | Cards], Result) -> parse_cards(Cards, [13 | Result]);
parse_cards([$Q | Cards], Result) -> parse_cards(Cards, [12 | Result]);
parse_cards([$J | Cards], Result) -> parse_cards(Cards, [11 | Result]);
parse_cards([$T | Cards], Result) -> parse_cards(Cards, [10 | Result]);
parse_cards([C | Cards], Result) -> parse_cards(Cards, [C - $0 | Result]).


process_hands(Hands) ->
    N = length(Hands),
    Sorted = lists:reverse(lists:sort(fun
            ({CardsA, Type, _}, {CardsB, Type, _}) ->
                compare_cards(CardsA, CardsB);
            ({_, TypeA, _}, {_, TypeB, _}) ->
                TypeA =< TypeB
        end,
        Hands)),
    [Bid * (N - (I - 1)) || {I, {_, _, Bid}} <- lists:enumerate(Sorted)].


compare_cards([], []) -> false;
compare_cards([C | CardsA], [C | CardsB]) -> compare_cards(CardsA, CardsB);
compare_cards([A | _], [B | _]) -> A =< B.


classify_hands_one(Hands) ->
    [{Cards, classify_cards_one(Cards), Bid} || {Cards, Bid} <- Hands].


classify_hands_two(Hands) ->
    [{Cards, classify_cards_two(Cards), Bid} || {Cards, Bid} <- Hands].


classify_cards_one(Cards) ->
    case lists:reverse(lists:sort(maps:values(utils:frequencies(Cards)))) of
        [5] -> 6; % five of a kind
        [4, _] -> 5; % four of a kind
        [3, 2] -> 4; % full house
        [3 | _] -> 3; % three of a kind
        [2, 2, _] -> 2; % two pair
        [2 | _] -> 1; % one pair
        _ -> 0 % high card
    end.


classify_cards_two(Cards) ->
    case lists:sort(Cards) of
        [X, X, X, X, X] -> 6; % five of a kind
        [1, X, X, X, X] -> 6; % five of a kind with joker
        [1, 1, X, X, X] -> 6; % five of a kind with joker
        [1, 1, 1, X, X] -> 6; % five of a kind with joker
        [1, 1, 1, 1, _] -> 6; % five of a kind with joker
        [X, X, X, X, _] -> 5; % four of a kind
        [_, X, X, X, X] -> 5; % four of a kind
        [1, X, X, X, _] -> 5; % four of a kind with joker
        [1, 1, X, X, _] -> 5; % four of a kind with joker
        [1, _, X, X, X] -> 5; % four of a kind with joker
        [1, 1, _, X, X] -> 5; % four of a kind with joker
        [1, 1, 1, _, _] -> 5; % four of a kind with joker
        [X, X, Y, Y, Y] -> 4; % full house
        [X, X, X, Y, Y] -> 4; % full house
        [1, X, X, Y, Y] -> 4; % full house with joker
        [X, X, X, _, _] -> 3; % three of a kind
        [_, X, X, X, _] -> 3; % three of a kind
        [_, _, X, X, X] -> 3; % three of a kind
        [1, X, X, _, _] -> 3; % three of a kind with joker
        [1, _, X, X, _] -> 3; % three of a kind with joker
        [1, _, _, X, X] -> 3; % three of a kind with joker
        [1, 1, _, _, _] -> 3; % three of a kind with joker
        [X, X, Y, Y, _] -> 2; % two pair
        [X, X, _, Y, Y] -> 2; % two pair
        [_, X, X, Y, Y] -> 2; % two pair
        [X, X, _, _, _] -> 1; % one pair
        [_, X, X, _, _] -> 1; % one pair
        [_, _, X, X, _] -> 1; % one pair
        [_, _, _, X, X] -> 1; % one pair
        [1, _, _, _, _] -> 1; % one pair with joker
        _ -> 0 % high card
    end.

#!/usr/bin/env escript


-spec main([string()]) -> ok.
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Hands = parse_hands(Lines),
    Result = process_hands(Hands),
    Sum = lists:sum(Result),
    io:format("~p~n", [Sum]);

main(_) ->
    io:format("usage: escript solution.escript filename~n").


parse_hands(Lines) ->
    parse_hands(Lines, []).


parse_hands([], Result) ->
    lists:reverse(Result);

parse_hands([Line | Lines], Result) ->
    [Cards, Bid] = binary:split(Line, <<" ">>, [global, trim_all]),
    ParsedCards = parse_cards(Cards),
    parse_hands(Lines, [{ParsedCards,
                         classify_cards(ParsedCards),
                         binary_to_integer(Bid)} | Result]).

parse_cards(Card) ->
    parse_cards(binary_to_list(Card),[]).


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


classify_cards(Cards) ->
    case lists:sort(Cards) of
        [X, X, X, X, X] -> 6; % five of a kind
        [X, X, X, X, _] -> 5; % four of a kind
        [_, X, X, X, X] -> 5; % four of a kind
        [X, X, Y, Y, Y] -> 4; % full house
        [X, X, X, Y, Y] -> 4; % full house
        [X, X, X, _, _] -> 3; % three of a kind
        [_, X, X, X, _] -> 3; % three of a kind
        [_, _, X, X, X] -> 3; % three of a kind
        [X, X, Y, Y, _] -> 2; % two pair
        [X, X, _, Y, Y] -> 2; % two pair
        [_, X, X, Y, Y] -> 2; % two pair
        [X, X, _, _, _] -> 1; % one pair
        [_, X, X, _, _] -> 1; % one pair
        [_, _, X, X, _] -> 1; % one pair
        [_, _, _, X, X] -> 1; % one pair
        _ -> 0 % high card
    end.

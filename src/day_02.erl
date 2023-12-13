-module(day_02).

-export([parts/0, run/2]).


-define(N_RED, 12).
-define(N_GREEN, 13).
-define(N_BLUE, 14).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    {Result, _} = process_lines(Lines, 0, 0),
    Result;

run(two, Lines) ->
    {_, Result} = process_lines(Lines, 0, 0),
    Result.


-spec process_lines([binary()], non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
process_lines([], GameIdSum, PowerSum) ->
    {GameIdSum, PowerSum};

process_lines([<<>> | Lines], GameIdSum, PowerSum) ->
    process_lines(Lines, GameIdSum, PowerSum);

process_lines([Line | Lines], GameIdSum, PowerSum) ->
    {NewGameIdSum, NewPowerSum} = process_line(Line, GameIdSum, PowerSum),
    process_lines(Lines, NewGameIdSum, NewPowerSum).


-spec process_line(binary(), non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
process_line(Line, GameIdSum, PowerSum) ->
    {match, Match} = re:run(Line,
                            "Game (\\d+):\\s*(.*)",
                            [{capture, [1, 2], list}]),
    [GameID, GameString] = Match,
    {match, Game} = re:run(GameString,
                           "(\\d+)\\s*(red|green|blue)",
                           [global, {capture, all_but_first, list}]),
    case process_game(Game, true, 0, 0, 0) of
        {true, Power} ->
            {GameIdSum + list_to_integer(GameID), PowerSum + Power};
        {_, Power} ->
            {GameIdSum, PowerSum + Power}
    end.


-spec process_game([[string()]], boolean(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {boolean(), non_neg_integer()}.
process_game([], Valid, MaxRed, MaxGreen, MaxBlue) ->
    {Valid, MaxRed * MaxGreen * MaxBlue};

process_game([[N, "red"] | Rest], Valid, MaxRed, MaxGreen, MaxBlue) ->
    X = list_to_integer(N),
    process_game(Rest, is_valid(red, X, Valid), max(MaxRed, X), MaxGreen, MaxBlue);

process_game([[N, "green"] | Rest], Valid, MaxRed, MaxGreen, MaxBlue) ->
    X = list_to_integer(N),
    process_game(Rest, is_valid(green, X, Valid), MaxRed, max(MaxGreen, X), MaxBlue);

process_game([[N, "blue"] | Rest], Valid, MaxRed, MaxGreen, MaxBlue) ->
    X = list_to_integer(N),
    process_game(Rest, is_valid(blue, X, Valid), MaxRed, MaxGreen, max(MaxBlue, X)).


-spec is_valid(red | green | blue, non_neg_integer(), boolean()) -> boolean().
is_valid(_, _, false) ->
    false;
is_valid(red, X, _) when X =< ?N_RED ->
    true;
is_valid(green, X, _) when X =< ?N_GREEN ->
    true;
is_valid(blue, X, _) when X =< ?N_BLUE ->
    true;
is_valid(_, _, _) ->
    false.

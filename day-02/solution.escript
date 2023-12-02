#!/usr/bin/env escript

-define(N_RED, 12).
-define(N_GREEN, 13).
-define(N_BLUE, 14).


main([]) ->
    io:format("usage: escript solution.escript filename~n");

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    {GameIdSum, PowerSum} = process_lines(Lines, 0, 0),
    io:format("game id sum: ~w~npower sum: ~w~n", [GameIdSum, PowerSum]).


process_lines([], GameIdSum, PowerSum) ->
    {GameIdSum, PowerSum};

process_lines([Line | Lines], GameIdSum, PowerSum) ->
    {NewGameIdSum, NewPowerSum} = process_line(binary_to_list(Line), GameIdSum, PowerSum),
    process_lines(Lines, NewGameIdSum, NewPowerSum).


process_line(Line, GameIdSum, PowerSum) ->
    {match, Match} = re:run(Line,
                            "Game (\\d+):\\s*(.*)",
                            [{capture, [1, 2], list}]),
    [GameID, GameString] = Match,

    {match, Game} = re:run(GameString,
                           "(\\d+)\\s*(red|green|blue)",
                           [global, {capture, all_but_first, list}]),

    {Valid, Power} = process_game(Game, true, 0, 0, 0),

    case Valid of
        true ->
            {GameIdSum + list_to_integer(GameID), PowerSum + Power};
        _ ->
            {GameIdSum, PowerSum + Power}
    end.

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

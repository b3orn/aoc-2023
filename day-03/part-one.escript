#!/usr/bin/env escript


-spec main([string()]) -> ok.
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Positions = find_symbols(Lines),
    PartNumbers = find_part_numbers(Lines, Positions),
    io:format("~p~n", [lists:sum(PartNumbers)]);

main(_) ->
    io:format("usage: escript part-one.escript filename~n").


-spec find_symbols([string()]) -> [{non_neg_integer(), non_neg_integer()}].
find_symbols(Lines) ->
    find_symbols(Lines, 0, []).


-spec find_symbols([binary()], non_neg_integer(), Positions) -> Positions
    when Positions :: [{non_neg_integer(), non_neg_integer()}].
find_symbols([], _, Positions) ->
    Positions;

find_symbols([Line | Lines], Y, Positions) ->
    NewPositions = case re:run(Line, "([^\\d\\.])", [global, {capture, all_but_first}]) of
        nomatch -> [];
        {match, Matches} -> lists:map(fun([{X, _}]) -> {X, Y} end, Matches)
    end,
    find_symbols(Lines, Y + 1, lists:append(Positions, NewPositions)).


-spec find_part_numbers([binary()], [{non_neg_integer(), non_neg_integer()}]) ->
    [non_neg_integer()].
find_part_numbers(Lines, PartPositions) ->
    find_part_numbers(Lines, PartPositions, 0, []).


-spec find_part_numbers([binary()], [{non_neg_integer(), non_neg_integer()}], non_neg_integer(), [non_neg_integer()]) ->
    [non_neg_integer()].
find_part_numbers([], _, _, PartNumbers) ->
    PartNumbers;
 
find_part_numbers([Line | Lines], Positions, Y, PartNumbers) ->
    NewPartNumbers = case re:run(Line, "(\\d+)", [global, {capture, all_but_first}]) of
        nomatch -> [];
        {match, Matches} ->
            lists:filtermap(fun([{X, Length}]) ->
                case is_adjacent(X, Y, Length, Positions) of
                    true -> {true, binary_to_integer(binary:part(Line, X, Length))};
                    false -> false
                end
            end,
            Matches)
    end,
    find_part_numbers(Lines, Positions, Y + 1, lists:append(PartNumbers, NewPartNumbers)).


-spec is_adjacent(non_neg_integer(), non_neg_integer(), pos_integer(), [{non_neg_integer(), non_neg_integer()}]) ->
    boolean().
is_adjacent(_, _, _, []) ->
    false;

is_adjacent(X, Y, _, [{SymX, Y} | _]) when X == SymX + 1 ->
    true;

is_adjacent(X, Y, Length, [{SymX, Y} | _]) when X + Length == SymX ->
    true;

is_adjacent(X, Y, Length, [{SymX, SymY} | _]) when Y == SymY + 1, SymX >= X - 1, SymX =< X + Length ->
    true;

is_adjacent(X, Y, Length, [{SymX, SymY} | _]) when Y == SymY - 1, SymX >= X - 1, SymX =< X + Length ->
    true;

is_adjacent(X, Y, Length, [_ | Positions]) ->
    is_adjacent(X, Y, Length, Positions).

-module(day_03).

-export([parts/0, run/2]).


-spec parts() -> [atom()].
parts() -> [one, two].


-spec run(one | two, [binary()]) -> integer().
run(one, Lines) ->
    Positions = find_symbols(Lines),
    PartNumbers = find_part_numbers(Lines, Positions),
    lists:sum([N || {N, _} <- PartNumbers]);

run(two, Lines) ->
    Positions = find_gears(Lines),
    PartNumbers = find_part_numbers(Lines, Positions),
    GearRatios = lists:filtermap(fun(Pos) ->
            case [N || {N, P} <- PartNumbers, P == Pos] of
                [A, B] -> {true, A * B};
                _ -> false
            end
        end,
        Positions),
    lists:sum(GearRatios).


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


-spec find_gears([string()]) -> [{non_neg_integer(), non_neg_integer()}].
find_gears(Lines) ->
    find_gears(Lines, 0, []).


-spec find_gears([binary()], non_neg_integer(), Positions) -> Positions
    when Positions :: [{non_neg_integer(), non_neg_integer()}].
find_gears([], _, Positions) ->
    Positions;

find_gears([Line | Lines], Y, Positions) ->
    NewPositions = case re:run(Line, "(\\*)", [global, {capture, all_but_first}]) of
        nomatch -> [];
        {match, Matches} -> lists:map(fun([{X, _}]) -> {X, Y} end, Matches)
    end,
    find_gears(Lines, Y + 1, lists:append(Positions, NewPositions)).


-spec find_part_numbers([binary()], [{non_neg_integer(), non_neg_integer()}]) ->
    [non_neg_integer()].
find_part_numbers(Lines, PartPositions) ->
    find_part_numbers(Lines, PartPositions, 0, []).


-spec find_part_numbers([binary()], [{non_neg_integer(), non_neg_integer()}], non_neg_integer(), Result) -> Result
    when Result :: [{non_neg_integer(), {non_neg_integer(), non_neg_integer()}}].
find_part_numbers([], _, _, PartNumbers) ->
    PartNumbers;
 
find_part_numbers([Line | Lines], Positions, Y, PartNumbers) ->
    NewPartNumbers = case re:run(Line, "(\\d+)", [global, {capture, all_but_first}]) of
        nomatch -> [];
        {match, Matches} ->
            lists:filtermap(fun([{X, Length}]) ->
                case is_adjacent(X, Y, Length, Positions) of
                    {SymX, SymY} -> {true, {binary_to_integer(binary:part(Line, X, Length)), {SymX, SymY}}};
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
    {SymX, Y};

is_adjacent(X, Y, Length, [{SymX, Y} | _]) when X + Length == SymX ->
    {SymX, Y};

is_adjacent(X, Y, Length, [{SymX, SymY} | _]) when Y == SymY + 1, SymX >= X - 1, SymX =< X + Length ->
    {SymX, SymY};

is_adjacent(X, Y, Length, [{SymX, SymY} | _]) when Y == SymY - 1, SymX >= X - 1, SymX =< X + Length ->
    {SymX, SymY};

is_adjacent(X, Y, Length, [_ | Positions]) ->
    is_adjacent(X, Y, Length, Positions).
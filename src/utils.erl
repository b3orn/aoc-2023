-module(utils).

-export([primes/1,
         factorize/1,
         least_common_multiple/1,
         frequencies/1,
         parallel/2,
         divide/2,
         gather/1,
         print_grid/1]).


-spec primes(pos_integer()) -> [pos_integer()].
primes(N) ->
    Table = primes_table(),
    case lists:sort([P || {P} <- ets:tab2list(Table)]) of
        [] ->
            fill_prime_table(2, N, [2]);
        Primes ->
            fill_prime_table(lists:last(Primes), N, Primes)
    end.


primes_table() ->
    primes_table(ets:info(primes)).

primes_table(undefined) ->
    ets:new(primes, [public, named_table, set, {keypos, 1}]);
primes_table(_) ->
    primes.


fill_prime_table(Start, Stop, Primes) when Start >= Stop ->
    Primes;

fill_prime_table(Start, Stop, Primes) ->
    fill_prime_table(lists:seq(Start, Stop), Primes).


fill_prime_table([], Primes) ->
    ets:insert(primes_table(), [{P} || P <- Primes]),
    lists:sort(Primes);

fill_prime_table([C | Candidates], Primes) ->
    case lists:min([C rem P || P <- Primes]) of
        0 -> fill_prime_table(Candidates, Primes);
        _ -> fill_prime_table(Candidates, [C | Primes])
    end.


-spec least_common_multiple([pos_integer()]) -> pos_integer().
least_common_multiple(Numbers) ->
    Factorized = [frequencies(factorize(N)) || N <- Numbers],
    Factors = lists:foldl(fun(Factors, Result) ->
            maps:fold(fun(Factor, Count, R) ->
                    R#{Factor => max(maps:get(Factor, R, 0), Count)}
                end,
                Result,
                Factors)
        end,
        #{},
        Factorized),
    round(maps:fold(fun(K, V, R) -> R * math:pow(K, V) end, 1, Factors)).


-spec factorize(pos_integer()) -> [pos_integer()].
factorize(N) -> factorize(N, primes(N), []).

factorize(1, _, Result) ->
    Result;
factorize(N, [P | _], Result) when N rem P == 0 ->
    factorize(N div P, primes(N), [P | Result]);
factorize(N, [_ | Primes], Result) ->
    factorize(N, Primes, Result).


-spec frequencies(list()) -> map().
frequencies(Values) ->
    lists:foldl(fun(K, S) -> S#{K => maps:get(K, S, 0) + 1} end, #{}, Values).


-spec parallel(function(), [any()]) -> [any()].
parallel(Fun, Values) ->
    gather(divide(Fun, Values)).


-spec divide(function(), [any()]) -> [pid()].
divide(Fun, Values) ->
    Self = self(),
    [spawn(fun() -> Self ! {self(), Fun(Value)} end) || Value <- Values].


-spec gather([pid()]) -> [any()].
gather(Pids) ->
    gather(Pids, Pids, #{}).

gather([], Pids, Results) ->
    [maps:get(Pid, Results) || Pid <- Pids];

gather(Pids, AllPids, Results) ->
    receive
        {Pid, Value} ->
            gather(lists:delete(Pid, Pids), AllPids, Results#{Pid => Value})
    end.


print_grid(Grid) ->
    lists:foreach(fun(Line) ->
            io:format("~s~n", [tuple_to_list(Line)])
        end,
        tuple_to_list(Grid)).

-module(factorial).

-export([factorial/1, factorial_t/1]).

-define(options, [memory, heap_size, total_heap_size, stack_size]).

factorial(0) -> 1;
factorial(Num) ->
    if
        Num rem 1000 == 0 ->
            Res = erlang:process_info(self(), ?options),
            Res2 = erlang:memory(),
            io:format("~p~n", [Res ++ Res2]);
        true -> ok
    end,
    Num * factorial(Num - 1).

factorial_t(Num) -> factorial_t(Num, 1).

factorial_t(0, Acc) -> Acc;
factorial_t(Num, Acc) ->
    if
        Num rem 1000 == 0 ->
            Res = erlang:process_info(self(), ?options),
            Res2 = erlang:memory(),
            io:format("~p~n", [Res ++ Res2]);
        true -> ok
    end,
    factorial_t(Num - 1, Num * Acc).
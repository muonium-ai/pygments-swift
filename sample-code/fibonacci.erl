%% Fibonacci (Erlang)

-module(fib).
-export([fib/1, main/0]).

fib(N) when N < 2 -> N;
fib(N) -> fib(N - 1) + fib(N - 2).

main() ->
  lists:foreach(fun(I) -> io:format("~p ~p~n", [I, fib(I)]) end, lists:seq(0, 11)).

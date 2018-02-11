-module(double).
-compile(export_all).

start() ->
    register(double, spawn(fun() -> double() end)).


double() -> 
    receive
    N when is_integer(N) -> io:format("~p~n", [N * 2]), double();
    _ -> erlang:error(badarg)
    end.

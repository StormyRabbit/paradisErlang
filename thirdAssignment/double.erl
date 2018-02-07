-module(double).
-compile(export_all).

double_port() -> 4567.

test() ->
    double:start(),
    double ! 2,
    double ! asd,
    hooray.

start() -> 
    register(double, spawn(fun() -> double() end)).


double() -> 
    receive
    N when is_integer(N) -> io:format("~p~n", [N * 2]), double();
    _ -> erlang:error(invalid_input), double()
    end.

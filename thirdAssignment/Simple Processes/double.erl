-module(double).
-compile(export_all).


% starts the double process and registers it with the atm double.
start() -> register(double, spawn(fun() -> double() end)).

% receives Integers and prints the *2 value to stdOut.
% if anything but an integer is received an badarg error is thrown.
double() -> 
    receive
    N when is_integer(N) -> io:format("~p~n", [N * 2]), double();
    _ -> erlang:error(badarg)
    end.

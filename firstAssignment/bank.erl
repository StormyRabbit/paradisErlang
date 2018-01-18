-module(bank).
-compile(export_all).

test() ->
    io:format("Starting tests~n"),
    Pid = new(),
    io:format("Testing add~n"),
    ok = add(Pid, 10),
    ok = add(Pid, 20),
    io:format("Testing balance~n"),
    30 = balance(Pid),
    io:format("Testing withdraw~n"),
    ok = withdraw(Pid, 15),
    io:format("Testing balance after withdraw~n"),
    15 = balance(Pid),
    insufficient_funds = withdraw(Pid, 20),
    horray.

new() ->
    spawn(fun() -> bank(0) end).

balance(Pid) -> 
    rpc(Pid, balance).

add(Pid, Amount) -> 
    rpc(Pid, {add, Amount}).

withdraw(Pid, Amount) -> 
    rpc(Pid, {withdraw, Amount}).

rpc(Pid, Msg) ->
    %% sends Msg to Pid and recieves the response.
    Pid ! {self(), Msg},
    receive Any -> Any end.

bank(X) ->
    receive
	{From, {add, Y}} ->
	    From ! ok,
	    bank(X+Y);
	{From, {withdraw, Y}} ->
	    From ! ok,
	    bank(X-Y);
	{From, balance} ->
	    From ! X;
    {From, _} ->  % catch allt annat
        From ! error
    end.

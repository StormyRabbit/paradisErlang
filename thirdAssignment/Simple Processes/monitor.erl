-module(monitor).
-compile(export_all).

start() ->
    spawn(fun() -> double_monitor() end),
    spawn(fun() -> double_tester() end),
    hooray.

double_tester() ->
    io:format("double tester started!~n"),
    timer:sleep(round(timer:seconds(rand:uniform(20)))),
    Value = round(rand:uniform(20)),
    case Value rem 2 == 0 of
    true -> double ! Value;
    false -> double ! asd
    end,
    double_tester().

double_monitor() ->
    link(whereis(double)),
    process_flag(trap_exit, true),
    io:format("started watching...~n"),
    receive
    {'EXIT', Pid, Why} -> 
    io:format("ERROR: ~p~n", [Why]),
    io:format("RESTARTING CRASHED PROCESS...~n"), double:start(), 
    double_monitor()
    end.
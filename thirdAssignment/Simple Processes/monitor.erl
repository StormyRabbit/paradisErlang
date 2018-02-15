-module(monitor).
-compile(export_all).


% To start:
% Compile and run double.erl (double:start())
% then compile and run this file, monitor:start().

% Startfunction that starts the monitor aswell as the tester.
start() ->
    spawn(fun() -> double_monitor() end),
    spawn(fun() -> double_tester() end),
    hooray.

% Loop that randoms a number and if its even it sends it to the double process.
% if its odd it sends an atom, crashing the process.
double_tester() ->
    io:format("double tester started!~n"),
    timer:sleep(round(timer:seconds(rand:uniform(20)))),
    Value = round(rand:uniform(20)),
    case Value rem 2 == 0 of
    true -> double ! Value;
    false -> double ! asd
    end,
    double_tester().

% Loop for monitoring the double process.
% links with the double process and traps all exits.
% If an exit is recieved, the Reason is printed to stdOut
% and the process is restarted.
double_monitor() ->
    link(whereis(double)),
    process_flag(trap_exit, true),
    io:format("started watching...~n"),
    receive
    {'EXIT', Pid, Why} -> 
    io:format("ERROR: ~p~n", [Why]),
    % I assume this is a restart, not sure if there is an actual 
    % difference between "starting a new" and restaring a crashed.
    io:format("RESTARTING CRASHED PROCESS...~n"), double:start(), 
    double_monitor()
    end.
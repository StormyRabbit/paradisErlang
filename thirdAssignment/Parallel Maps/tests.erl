-module(tests).
-compile(export_all).

test() ->
    List = [a,b,c,d,e,f,g],
    MaxWorkers = 2,
    L = length(List),
    Number = Lrem MaxWorkers + L div MaxWorkers,
    {Work, Rem} = lists:split(Number, List),
    io:format("WORKLOAD: ~p~n REMAINING: ~p~n", [Work, Rem]).
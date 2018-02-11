-module(pmap_max).
-compile(export_all).

smap(_, []) -> [];
smap(F, [H|T])-> [F(H) | smap(F, T)].

test() ->
    List = [1,2,3,4,5,6,7],
    Test_F = fun(X) -> X+1 end,
    io:format("SMAP: ~p~n", [smap(Test_F, List)]),
    io:format("PMAP: ~p~n", [pmap_max(Test_F, List, 3)]),
    hooray.


pmap_max(F, L, MaxWorkers) -> 
    Pids = spawn_workers(F, L, [], MaxWorkers),
    gather(Pids).

gather([Pid|T]) ->
    receive 
        {Pid, Ret} ->  gather(T) ++ Ret
    end;
gather([])-> [].

spawn_workers(_, [], Pids, _) -> Pids;
spawn_workers(_, _, Pids, 0) -> Pids;
spawn_workers(F, L, Pids, MaxWorkers) -> 
    S = self(),
    {Work, Rem} = calculate_workload(L, MaxWorkers),
    Pid = spawn(fun() -> worker_process(S, F, Work) end),
    spawn_workers(F, Rem, [Pid] ++ Pids, MaxWorkers - 1).

calculate_workload(List, MaxWorkers) -> 
    L = length(List),
    Number = L rem MaxWorkers + L div MaxWorkers,
    lists:split(Number, List).

worker_process(Parent, Func, Work) -> 
    Asd = do_work(Func, Work),
    Parent ! {self(), Asd}.

do_work(_, []) -> [];
do_work(Func, [H|T]) -> [Func(H) | do_work(Func, T)].



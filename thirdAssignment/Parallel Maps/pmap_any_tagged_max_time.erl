-module(pmap_any_tagged_max_time).
-compile(export_all).

test() ->
    [{2,1},{20,6765},{35,9227465}] = pmap_any_tagged_max_time(fun fib/1, [35,2,456,20], 2000),
    hooray.

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

pmap_any_tagged_max_time(F, L, MaxTime) -> 
    spawn_workers(F, L, MaxTime),
    gather(length(L), [], MaxTime).

gather(0, Result, _) -> Result;
gather(L, Result, MaxTime) ->
    receive 
        Any -> gather(L-1, Result ++ [Any], MaxTime)
    after
        MaxTime -> Result
    end.

spawn_workers(_, [], _) -> [];
spawn_workers(F, [H|T], MaxTime) -> 
    S = self(),
    spawn(fun() -> worker_process(S, F, H, MaxTime) end),
    spawn_workers(F, T, MaxTime).

worker_process(Parent, Func, Work, MaxTime) -> 
    S = self(),
    Pid = spawn(fun() -> do_work(S, Func, Work) end),
    receive 
        Any -> Parent ! Any
    after 
        MaxTime -> exit(Pid)
    end.

do_work(Parent, Func, Work) -> 
    Result = Func(Work),
    Parent ! {Work, Result}.



-module(pmap_any_tagged).
-compile(export_all).

smap(_, []) -> [];
smap(F, [H|T])-> [F(H) | smap(F, T)].

test() ->
    [{2,1},{20,6765},{35,9227465}] = pmap_any_tagged(fun fib/1 ,[35,2,20]),
    hooray.

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

pmap_any_tagged(F, L) -> 
    spawn_workers(F, L),
    gather(length(L), []).

gather(0, Result) -> Result;
gather(L, Result) ->
    receive 
        Any -> gather(L-1, Result ++ [Any])
    end;
gather(_, [])-> [].

spawn_workers(F, []) -> [];
spawn_workers(F, [H|T]) -> 
    S = self(),
    spawn(fun() -> worker_process(S, F, H) end),
    spawn_workers(F, T).

worker_process(Parent, Func, Work) -> 
    Result = Func(Work),
    Parent ! {Work, Result}.



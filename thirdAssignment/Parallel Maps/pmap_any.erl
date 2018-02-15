-module(pmap_any).
-compile(export_all).

smap(_, []) -> [];
smap(F, [H|T])-> [F(H) | smap(F, T)].

test() ->
    [1,6765,9227465] = pmap_any(fun fib/1 ,[35,2,20]),
    hooray.

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

pmap_any(F, L) -> 
    spawn_workers(F, L),
    gather(length(L), []).


% Gathers all the results
gather(0, Result) -> Result;
gather(L, Result) ->
    receive 
        Any -> gather(L-1, Result ++ [Any])
    end;
gather(_, [])-> [].

% creates the workers, one for each element in the list.
spawn_workers(F, []) -> [];
spawn_workers(F, [H|T]) -> 
    S = self(),
    spawn(fun() -> worker_process(S, F, H) end),
    spawn_workers(F, T).
% the function that does the actual work, sends it to the parent 
% as a msg.
worker_process(Parent, Func, Work) -> 
    Result = Func(Work),
    Parent ! Result.



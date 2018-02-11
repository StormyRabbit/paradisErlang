-module(msg_ring).
-compile(export_all).
smap(_, [])    -> [];
smap(F, [H|T]) -> [F(H) | smap(F, T)].

test() -> 
  15 = start(3,5), hooray.

start(N, M) ->
  S = self(),
  Result = register(first, spawn(fun() -> first_loop(S, self()) end)),
  Pids = create_links(N-1, []),
  connect_chain(Pids),
  [H|_] = Pids,
  whereis(first) ! {next, H},
  whereis(first) ! {0, M},
  receive 
    Any -> Any
  end.

create_links(0, Pids) -> Pids;
create_links(N, Pids) ->
  NewLink = spawn(fun() -> loop(self()) end),
  create_links(N-1, [NewLink|Pids]). 

connect_chain(L) when length(L) == 1 ->
  [H|_] = L,
  H ! {next, whereis(first)};
connect_chain([F|[S|T]]) -> 
  F ! {next, S}, connect_chain([S|T]).

first_loop(S, Next) -> 
  receive 
    {next, Asd} -> first_loop(S, Asd);
    {I, 0} ->  S ! I; 
    {I, M} -> Next ! {I+1, M-1}, first_loop(S, Next)
  end.

loop(Next) -> 
  receive
    {next, Asd} -> loop(Asd);
    {I, M} -> Next ! {I+1, M}, loop(Next)
  end.

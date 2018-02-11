-module(msg_ring).
-compile(export_all).
smap(_, [])    -> [];
smap(F, [H|T]) -> [F(H) | smap(F, T)].

test() -> start(3,5).

start(N, M) ->
  register(first, spawn(fun() -> ring() end)),
  Pids = createWorkers(N-1, []),
  connect_ring(Pids).

createWorkers(0, Pids) -> Pids;
createWorkers(N, Pids) -> createWorkers(N-1, [spawn(fun() -> ring() end)|Pids]).

connect_ring(Pids) ->
  [Last|Tail] = lists:reverse(Pids),
  link(whereis(first), Last),
  connect_middle_section(Pids).

connect_middle_section([First|[Second|T]]) -> link(First, Second), connect_middle_section([Second|T]).

first_loop() -> 
  receive 
    {I, 0} -> io:format("GOT SOMETHING~n"), I; 
    {I, M} -> io:format("GOT SOMETHING~n"), {I+1, M-1}, first_loop()
  end.

ring() -> 
  receive
    {I, M} -> io:format("GOT SOMETHING~n"), {I+1, M}, ring()
  end.
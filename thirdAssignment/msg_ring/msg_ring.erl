-module(msg_ring).
-compile(export_all).
smap(_, [])    -> [];
smap(F, [H|T]) -> [F(H) | smap(F, T)].

test() -> 
  15 = start(3,5), hooray.

% start function that creates 
% a one-way process link containing of N links,
% and then transmitts a msg that goes trough the ring M number of times.
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

% creates all the processes needed 
create_links(0, Pids) -> Pids;
create_links(N, Pids) ->
  NewLink = spawn(fun() -> loop(self()) end),
  create_links(N-1, [NewLink|Pids]). 
% Does the actual connecting of the different processes.
% When the Last Node is reached its connected to the registered process, first.
connect_chain(L) when length(L) == 1 ->
  [H|_] = L, 
  H ! {next, whereis(first)};
connect_chain([F|[S|T]]) -> 
  F ! {next, S}, connect_chain([S|T]).

% special mailbox for the firstNode,
% responsible for keeping track of the number of loops.
first_loop(S, Next) -> 
  receive 
    {next, Asd} -> first_loop(S, Asd);
    {I, 0} ->  S ! I; 
    {I, M} -> Next ! {I+1, M-1}, first_loop(S, Next)
  end.
% mailbox loop thingy for the links, just sends the msg
% down the chain and increasing I with 1.
loop(Next) -> 
  receive
    {next, Asd} -> loop(Asd);
    {I, M} -> Next ! {I+1, M}, loop(Next)
  end.

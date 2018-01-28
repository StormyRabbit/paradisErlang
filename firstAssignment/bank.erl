-module(bank).
-compile(export_all).
% A simple banking system used as an exercise for the PARADIs course
% at Stockholms University.
% Supports adding and reducing a value bound to a function called bank.
test() ->
    Pid = new(),
    % Testing add
    ok = add(Pid, 10),
    ok = add(Pid, 20),
    cant_add_below_zero = add(Pid, -100),
    30 = balance(Pid),
    %testing withdraw
    ok = withdraw(Pid, 15),
    cant_withdraw_below_zero = withdraw(Pid, -100),
    15 = balance(Pid),
    insufficient_funds = withdraw(Pid, 20),
    ok = withdraw(Pid, 15),


    % incorrect param tests
    {'EXIT', _} = (catch add(Pid, asd)),
    {'EXIT', _} = (catch balance(asd)),
    {'EXIT', _} = (catch withdraw(Pid, asd)),

    % all tests done
    hooray.

new() ->
    % Creates a new 'bank' with 0 as current funds.
    spawn(fun() -> bank(0) end).

balance(Pid) -> 
    rpc(Pid, balance).

add(Pid, Amount) -> 
    % adds the Amount to the Pid value.
    case is_integer(Amount) of
        true -> rpc(Pid, {add, Amount});
        false -> erlang:error(badarg)
    end.

withdraw(Pid, Amount) -> 
    % reduces the banks amount with Amount.
    case is_integer(Amount) of
        true -> rpc(Pid, {withdraw, Amount});
        false -> erlang:error(badarg)
    end.

rpc(Pid, Msg) ->
    % sends Msg to Pid and recieves the response.
    Pid ! {self(), Msg},
    receive Any -> Any end.

bank(X) ->
    % programs Main function, receives everything sent by RPC 
    % and updates X accordingly.
    receive
        % add messages
        {From, {add, Y}} when Y < 0 ->
            From ! cant_add_below_zero,
            bank(X);
        {From, {add, Y}} -> 
            From ! ok, 
            bank(X+Y);
        
        % Withdraw messages
        {From, {withdraw, Y}} when Y > X ->
            From ! insufficient_funds,
            bank(X);
        {From, {withdraw, Y}} when Y < 0 ->
            From ! cant_withdraw_below_zero,
            bank(X);
        {From, {withdraw, Y}} -> 
            From ! ok, 
            bank(X-Y);

        % Balance messages
        {From, balance} -> 
            From ! X,
            bank(X);
        
        % Catch anything undefined
        {From, _ } ->
            From ! erlang:error(badarg),
            bank(X)
    end.

-module(multi_bank).
-compile(export_all).

test() ->
    Pid = new(),
    %% account creation tests
    ok = createAccount(Pid, lasse),
    duplicate_account_error = createAccount(Pid, lasse),
    duplicate_account_error = createAccount(Pid, lasse),
    ok = createAccount(Pid, maria),

    % test add balance & withdraw
    ok = add(Pid, lasse, 100),
    100 = balance(Pid, lasse),
    ok = add (Pid, maria, 200),
    200 = balance(Pid, maria),
    horray.

new() ->
    spawn(fun() -> bank(#{}) end).

lend(Pid, From, To, Amount) ->
    rpc(Pid, {From, To, Amount} ).

balance(Pid, Who) -> 
    rpc(Pid, {Who, balance} ).

add(Pid, Who, Amount) -> 
    rpc(Pid, {add, Who, Amount}).

withdraw(Pid, Who, Amount) -> 
    rpc(Pid, {withdraw, Who, Amount}).


createAccount(Pid, Who) ->
    rpc(Pid, {createAccount, Who}).

rpc(Pid, Msg) ->
    %% sends Msg to Pid and recieves the response.
    Pid ! {self(), Msg},
    receive Any -> Any end.


bank(X) ->
    receive
        %% account creation
        {From, {createAccount, Name}} ->
        case maps:is_key(Name, X) of
            true -> 
                From ! duplicate_account_error,
                bank(X);
            false -> 
                From ! ok,
                bank(maps:put(Name, 0, X))
        end;

        %% Amount operations

        {From, {add, Name, Amount}} -> 
            From ! ok,
            io:format("Amount ~n~p\n", [Amount]),
            io:format("Name ~n~p\n", [Name]),
            io:format("current amount ~n~p", [maps:get(Name, X)]),
            newBalance = maps:get(Name, X) + Amount,
            bank(maps:update(Name, newBalance, X));

        {From, {withdraw, Name, Amount}} -> 
            io:format("withdrawing.."),
            balance = maps:get(Name, X),
            newBalance = balance - Amount,
            From ! ok,
            bank(maps:update(Name, newBalance));
        
        {From, {lend, Lender, Borrower, Amount}} ->
            io:format("lending.."),
            %% todo check if both exists
            mapAfterLenderReduction = maps:get(Lender, X) - Amount,
            mapAfterBorrowerIncrease = maps:get(Borrower, mapAfterLenderReduction) + Amount,
            From ! ok,
            bank(mapAfterBorrowerIncrease);
        {From, {balance, Name}} -> 
            io:format("checking balance.."),
            From ! maps:get(Name, X),
            bank(X)
    end.

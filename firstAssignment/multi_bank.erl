-module(multi_bank).
-compile(export_all).

test() ->
    Pid = new(),
    %% account creation tests
    ok = createAccount(Pid, lasse),
    duplicate_account_error = createAccount(Pid, lasse),
    duplicate_account_error = createAccount(Pid, lasse),
    ok = createAccount(Pid, maria),

    horray.

new() ->
    spawn(fun() -> bank(#{}) end).

lend(Pid, From, To, Amount) ->
    rpc(Pid, {From, To, Amount} ).

balance(Pid) -> 
    rpc(Pid, balance).

add(Pid, Amount) -> 
    rpc(Pid, {add, Amount}).

withdraw(Pid, Amount) -> 
    rpc(Pid, {withdraw, Amount}).


createAccount(Pid, Name) ->
    rpc(Pid, {createAccount, Name}).

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
            io:format("adding.."),
            balance = maps:get(Name, X),
            newBalance = balance + Amount,
            From ! ok,
            bank(maps:update(Name, newBalance));

        {From, {withdraw, Name, Amount}} when maps:get(Name, X) > Amount ->
            From ! insufficient_funds,
            bank(X); 
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

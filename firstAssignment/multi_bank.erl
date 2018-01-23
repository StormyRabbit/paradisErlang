-module(multi_bank).
-compile(export_all).

test() ->
    Pid = new(),
    %% account creation tests
    ok = createAccount(Pid, lasse),
    duplicate_account_error = createAccount(Pid, lasse),
    ok = createAccount(Pid, maria),

    % test add balance & withdraw
    ok = add(Pid, lasse, 100),
    100 = balance(Pid, lasse),
    ok = add (Pid, maria, 200),
    200 = balance(Pid, maria),
    no_known_account_error = balance(Pid, joe),
    0 = withdraw(Pid, lasse, 100),
    100 = withdraw(Pid, maria, 100),
    horray.

new() ->
    spawn(fun() -> bank(maps:new()) end).

lend(Pid, From, To, Amount) ->
    rpc(Pid, {From, To, Amount} ).

balance(Pid, Who) -> 
    rpc(Pid, {balance, Who} ).

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
            case maps:is_key(Name, X) of 
                true -> 
                    From ! ok,
                    NewBalance = maps:get(Name, X) + Amount,
                    NewMap = maps:update(Name, NewBalance, X),
                    bank(NewMap);
                false ->
                    From ! no_known_account_error,
                    bank(X)
            end;

        {From, {withdraw, Name, Amount}} ->
            case maps:is_key(Name, X) of
                true -> 
                    %% account exists, check if balance is enough
                    case maps:get(Name, X) > Amount of 
                        true -> 
                            NewBalance = maps:get(Name, X) - Amount,
                            NewMap = maps:update(Name, NewBalance, X),
                            From ! ok,
                            bank(NewMap);
                        false ->
                            From ! insufficient_funds,
                            bank(X)
                    end;
                false -> 
                    From ! no_known_account_error,
                    bank(X)
            end;
    
        {From, {lend, Lender, Borrower, Amount}} ->
            case maps:is_key(Lender, X) of
                true -> case maps:is_key(Borrower, X) of
                        true -> case maps:get(Lender, X) > Amount of 
                                true ->
                                    LenderBalance = maps:get(Lender, X) - Amount,
                                    BorrowerBalance = maps:get(Borrower, X) + Amount,
                                    
                                false -> From ! insufficient_funds, bank(X),
                        false-> From ! no_known_account_error, bank(X),
                false -> From ! no_known_account_error, bank(X)
            end; % end lend
            
        {From, {balance, Name}} -> 
            case maps:is_key(Name, X) of
                true -> 
                    From ! maps:get(Name, X),
                    bank(X);
                false -> 
                    From ! no_known_account_error,
                    bank(X)
            end % end balance
        end. % recieve end

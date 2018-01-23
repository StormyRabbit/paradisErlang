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
    ok = withdraw(Pid, lasse, 100),
    ok = withdraw(Pid, maria, 100),

        % Lend tests
    ok = lend(Pid, maria, lasse, 50),
    50 = balance(Pid, maria),
    50 = balance(Pid, lasse),

        % delete account test
    remaining_funds_error = deleteAcount(Pid, lasse),
    ok = withdraw(Pid, lasse, 50),
    ok = deleteAcount(Pid, lasse),
    [maria] = listAccounts(Pid),

        % incorrect param tests
    {'EXIT', _} = (catch add(Pid, lasse, asd)),
    {'EXIT', _} = (catch balance(asd, asd)),
    {'EXIT', _} = (catch withdraw(Pid, maria, asd)),
    {'EXIT', _} = (catch add(Pid, joe, asd)),
    {'EXIT', _} = (catch withdraw(Pid, joe, asd)),

    horray.

new() ->
    spawn(fun() -> bank(maps:new()) end).

listAccounts(Pid) ->
    rpc(Pid, listAccounts).

deleteAcount(Pid, Who) -> 
    rpc(Pid, {deleteAcount, Who}).

lend(Pid, From, To, Amount) ->
    case is_integer(Amount) of
        true -> rpc(Pid, {lend, From, To, Amount});
        false -> erlang:error(badarg)
    end.

balance(Pid, Who) -> 
    rpc(Pid, {balance, Who} ).

add(Pid, Who, Amount) -> 
    case is_integer(Amount) of
        true -> rpc(Pid, {add, Who, Amount});
        false -> erlang:error(badarg)
    end.

withdraw(Pid, Who, Amount) -> 
    case is_integer(Amount) of 
        true -> rpc(Pid, {withdraw, Who, Amount});
        false -> erlang:error(badarg)
    end.


createAccount(Pid, Who) ->
    rpc(Pid, {createAccount, Who}).

rpc(Pid, Msg) ->
    %% sends Msg to Pid and recieves the response.
    Pid ! {self(), Msg},
    receive Any -> Any end.


bank(AccountsMap) ->
    receive

            % Account related operations

        {From, {createAccount, Name}} ->
        case maps:is_key(Name, AccountsMap) of
            true -> 
                From ! duplicate_account_error, 
                bank(AccountsMap);
            false -> 
                From ! ok,
                bank(maps:put(Name, 0, AccountsMap))
        end;

        {From, {deleteAcount, Name}} ->
        % 
        case maps:is_key(Name, AccountsMap) of
            true -> 
                case maps:get(Name, AccountsMap) > 0 of 
                true -> 
                    From ! remaining_funds_error, 
                    bank(AccountsMap);
                false -> 
                    NewMap = maps:remove(Name, AccountsMap),
                    From ! ok, 
                    bank(NewMap)
                end;
            false ->
                From ! no_known_account_error,
                bank(AccountsMap)
        end;

        {From, listAccounts} -> 
            From ! maps:keys(AccountsMap),
            bank(AccountsMap);


            % Amount operations
            
        {From, {add, Name, Amount}} -> 
            %adds Amount to Name if it exists.
            case maps:is_key(Name, AccountsMap) of 
                true ->  % Account exists, increase amount bound to Name.
                    From ! ok,
                    NewBalance = maps:get(Name, AccountsMap) + Amount,
                    NewMap = maps:update(Name, NewBalance, AccountsMap),
                    bank(NewMap);
                false -> 
                    From ! no_known_account_error,
                    bank(AccountsMap)
            end;

        {From, {withdraw, Name, Amount}} ->
        % Reduces the amount specified from the Name account if enough funds is present.
            case maps:is_key(Name, AccountsMap) of
                true -> 
                    %% account exists, check if balance is enough
                    case maps:get(Name, AccountsMap) >= Amount of 
                        true -> % Everything checks out, reduce Amount from Name.
                            NewBalance = maps:get(Name, AccountsMap) - Amount,
                            NewMap = maps:update(Name, NewBalance, AccountsMap),
                            From ! ok,
                            bank(NewMap);
                        false -> % Not enough funds
                            From ! insufficient_funds,
                            bank(AccountsMap)
                    end;
                false -> % Account does not exist
                    From ! no_known_account_error,
                    bank(AccountsMap)
            end;
    
        {From, {lend, Lender, Borrower, Amount}} ->
        % Tests if accounts Lender and Borrower exists, then checks if Lender has enough funds 
        % to Borrow Amount. Lastly Transfers Amount from Lender To borrower
            case maps:is_key(Lender, AccountsMap) of
                true -> case maps:is_key(Borrower, AccountsMap) of
                        true -> case maps:get(Lender, AccountsMap) > Amount of 
                                true -> % This is where the actual lend operation takes place
                                    LenderBalance = maps:get(Lender, AccountsMap) - Amount,
                                    BorrowerBalance = maps:get(Borrower, AccountsMap) + Amount,
                                    NewMap = maps:update(Lender, LenderBalance, AccountsMap),
                                    From ! ok,
                                    bank(maps:update(Borrower, BorrowerBalance, NewMap));
                                false -> From ! insufficient_funds, bank(AccountsMap) % Lender insufficient_funds
                                end;
                        false -> From ! no_known_account_error, bank(AccountsMap) % Borrower does not exist
                        end;
                false -> From ! no_known_account_error, bank(AccountsMap) % Lender does not exist
            end; % end lend
            
        {From, {balance, Name}} -> 
        % Check if Name is a valid account if true return balance, otherwise return no_known_account_error.
            case maps:is_key(Name, AccountsMap) of
                true -> 
                    From ! maps:get(Name, AccountsMap),
                    bank(AccountsMap);
                false -> 
                    From ! no_known_account_error,
                    bank(AccountsMap)
            end % end balance
        end. % recieve end

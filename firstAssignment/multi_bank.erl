-module(multi_bank).
-compile(export_all).
% Lasse Sjöblom [larseriksjoblom@gmail.com]
% doc Code for assignment 1 at PARADIS course at Stockholms University.
% This program implements a simple version of a bank system.
% Supports adding, withdrawing lending and checking the amount related to a specific account.
% account is saved in a map with the username as an atom as key.
% 


test() ->
    % Unit tests, creates a new bank containg in empty map 
    % and tests adding users and manipulating the balance.
    % also does invalid parameter testing, outputs hooray if all tests are passed. 
    Pid = new(),

    % account creation tests
    io:format("Testing account creation...~n",[]),
    ok = createAccount(Pid, lasse),
    duplicate_account_error = createAccount(Pid, lasse),
    ok = createAccount(Pid, maria),
    io:format("Account creation tests PASSED~n",[]),

    % test add, balance & withdraw
    io:format("Testing add...~n",[]),
    ok = add(Pid, lasse, 100),
    ok = add (Pid, maria, 200),
    no_known_account_error = add (Pid, joe, 200),
    io:format("add tests PASSED~n",[]),

    io:format("Testing withdraw...~n", []),
    ok = withdraw(Pid, lasse, 100),
    ok = withdraw(Pid, maria, 100),
    no_known_account_error = withdraw(Pid, joe, 200),
    io:format("Withdraw tests PASSED~n", []),

    io:format("Testing balance...~n", []),
    0 = balance(Pid, lasse),
    100 = balance(Pid, maria),
    no_known_account_error = balance(Pid, joe),
    io:format("Balance tests PASSED~n", []),


    % Lend tests
    io:format("Testing lend...~n", []),
    ok = lend(Pid, maria, lasse, 50),
    50 = balance(Pid, maria),
    50 = balance(Pid, lasse),
    io:format("Lend test PASSED~n", []),

    % delete account test
    io:format("Testing account deletion...~n", []),
    remaining_funds_error = deleteAcount(Pid, lasse),
    ok = withdraw(Pid, lasse, 50),
    ok = deleteAcount(Pid, lasse),
    [maria] = listAccounts(Pid),
    io:format("Account deletion tests PASSED~n", []),

    % incorrect param tests
    io:format("Testing incorrect parameter types~n",[]),
    {'EXIT', _} = (catch add(Pid, lasse, asd)),
    {'EXIT', _} = (catch balance(asd, asd)),
    {'EXIT', _} = (catch withdraw(Pid, maria, asd)),
    {'EXIT', _} = (catch add(Pid, joe, asd)),
    {'EXIT', _} = (catch withdraw(Pid, joe, asd)),
    io:format("Incorrect parameter types tests PASSED~n",[]),

    horray.

new() ->
    % spawns a new bank insntance with a empty map
    spawn(fun() -> bank(maps:new()) end).

listAccounts(Pid) ->
    %  sends a request to get a list of all registered accounts.
    rpc(Pid, listAccounts).

deleteAcount(Pid, Who) -> 
    %  Sends a request to get delete a registered account.
    %  Pid, reference to the bank instance.
    %  Who, a atom that represents the account name.
    rpc(Pid, {deleteAcount, Who}).

lend(Pid, From, To, Amount) ->
    % Function to lend money from From to  To for the  Amount .
    % Pid, the Pid reference to the bank instance.
    % From, a atom that represents the account name that will lend money to the borrower.
    % To, a atom that represents the account name that will borrow money from the lender.
    case is_integer(Amount) of
        true -> rpc(Pid, {lend, From, To, Amount});
        false -> erlang:error(badarg)
    end.

balance(Pid, Who) -> 
    % Function to get the balance currently stored connected to Who.
    % Pid, the Pid reference to the bank instance.
    % Who, a atom that represents the account name that will be checked.
    rpc(Pid, {balance, Who} ).

add(Pid, Who, Amount) -> 
    % Function to get the balance currently stored connected to Who.
    % Pid, the Pid reference to the bank instance.
    % Who, a atom that represents the account name that will have money added to it.
    % Amount, the amount to be added to the account.
    case is_integer(Amount) of
        true -> rpc(Pid, {add, Who, Amount});
        false -> erlang:error(badarg)
    end.

withdraw(Pid, Who, Amount) -> 
    % Function to withdraw a specific amount from a specific account.
    % Pid, the Pid reference to the bank instance.
    % Who, a atom that represents the account name that will have money withdrawn.
    % Amount, the amount to be withdrawn to the account.
    case is_integer(Amount) of 
        true -> rpc(Pid, {withdraw, Who, Amount});
        false -> erlang:error(badarg)
    end.


createAccount(Pid, Who) ->
    % Function to create a new account.
    % Pid, the Pid reference to the bank instance.
    % Who, a atom that represents the account name that will be created.
    rpc(Pid, {createAccount, Who}).

rpc(Pid, Msg) ->
    % Function that sends anything in Msg to Pid and catches any return value.
    % Pid, the Pid reference to the bank instance.
    % Msg, the Msg, either a tuple or a atom connected to a specific keyword.
    Pid ! {self(), Msg},
    receive Any -> Any end.


bank(AccountsMap) ->
    % The banking process, supports receiving multiple types of operations, 
    % such as add, withdraw and lend.
    receive
    % Account related operations
        {From, {createAccount, Name}} ->
        case maps:is_key(Name, AccountsMap) of
            true -> % account is already registered 
                From ! duplicate_account_error, 
                bank(AccountsMap);
            false -> % accout name is free to register
                From ! ok,
                bank(maps:put(Name, 0, AccountsMap))
        end;

        {From, {deleteAcount, Name}} ->
        case maps:is_key(Name, AccountsMap) of
            true -> % accout exists
                case maps:get(Name, AccountsMap) > 0 of 
                true -> % money still left on account, abort deletion
                    From ! remaining_funds_error, 
                    bank(AccountsMap);
                false -> % account is empty, removes
                    NewMap = maps:remove(Name, AccountsMap),
                    From ! ok, 
                    bank(NewMap)
                end;
            false -> % no such account 
                From ! no_known_account_error,
                bank(AccountsMap)
        end;

        {From, listAccounts} -> % returns a list of the keys 
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

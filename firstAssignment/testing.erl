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
    % ok = add(Pid, lasse, 100),
    %100 = balance(Pid, lasse),
    %   ok = add (Pid, maria, 200),
    %  200 = balance(Pid, maria),
    horray.

new() ->
    spawn(fun() -> bank(maps:new()) end).

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
                Account = spawn(fun() -> account({Name, 0}) end),
                From ! ok,
                bank(maps:put(Name, Account))
        end;

    end.

account(AccInfo) ->
    receive

    end.
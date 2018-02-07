-module(arithmetic_evaluator).
-export([test/0, eval/1, safe_eval/1]).

% This code is written for the PARADIS course at Stockholms University vt18
% Module for evaulating arithmetic expressions.
% Supports + - / * in the form of nested tuples using the keywords 
% such as plus, times, divide, minus.
% Lasse Sjöblom - larseriksjoblom@gmail.com

test() ->
    % eval testing
    3 = eval({plus,1,2}),
    7 = eval({plus,3,4}),
    -5 = eval({times, {plus,2,3}, {minus,3,4}}),
    4.0 = eval({divide, 12,3}),

    % safe eval testing
    Pid = spawn(fun() -> evaluation_process() end),
    {error, dividing_by_zero} = rpc(Pid, {plus,12,{divide,12,0}}),
    {error, invalid_expression} = rpc(Pid, {plus,12,{divide,asd,0}}),
    {error, invalid_expression} = rpc(Pid, {plus,12,{divide,asd,0}}),
    {error, unknown_argument} = rpc(Pid, []),
    {error, unknown_argument} = rpc(Pid, {plus, [], #{}}),
    {ok, -5} = rpc(Pid, {times, {plus,2,3}, {minus,3,4}}),
    hooray.

% Sends Msg to Pid and returns anything returned.
rpc(Pid, Msg) ->
    Pid ! {self(), Msg},
    receive Any -> Any end.

% evaluate functions that catches different patterns of the format {OpAtom, Val1, val2}.
% starts by evaluating A and B separately. Returns invalid_expression if no match found.
eval({times, A, B}) -> 
    A_evaluated = eval(A),
    B_evaluated = eval(B),
    A_evaluated * B_evaluated;
eval({plus, A, B}) -> 
    A_evaluated = eval(A),
    B_evaluated = eval(B),
    A_evaluated + B_evaluated;
eval({minus, A, B}) -> 
    A_evaluated = eval(A),
    B_evaluated = eval(B),
    A_evaluated - B_evaluated;
eval({divide, A, B}) -> 
    A_evaluated = eval(A),
    B_evaluated = eval(B),
    A_evaluated / B_evaluated;
eval(N) when is_integer(N) -> N;
eval(N) when is_float(N) -> N;
eval(_) -> invalid_expression.

% Process that recieves msgs containing Sender and (hopefully) an Arithmetic expression,
% sends this expression to a function that evaluates the expression and returns the results 
% given from that function, coming in the format of {error, Why} or {ok, Value}.
% If the incoming msg does not follow the format {From, Expression} and unexpected_input atom is returned.
evaluation_process() -> 
    receive 
        {From, Arithmetic_expression} -> 
            From ! safe_eval(Arithmetic_expression),
            evaluation_process();
        _ -> 
            something_went_really_wrong,
            evaluation_process()
    end.

% Function that starts the actual work for safe_eval, recieves calls that gets 
% sent down the function chain to do the actual calculation and error checking, returns 
% either {error, Why} or {ok, Val} depending on outcome.
safe_eval(X) -> 
    Ret = safe_evaluate(X),
    case is_number(Ret) of 
        true -> {ok, Ret};
        false -> {error, Ret}
    end.

% Checks for obivous errors before doing calculations.
% Checks for if either A or B is an atom or if an divide by zero would occour.
safe_evaluate({_, A, B}) when is_atom(A) or is_atom(B) -> invalid_expression;
safe_evaluate({divide, _, B}) when B =:= 0 ->  dividing_by_zero;

% The 'actual' safe_evaluate functions, matches in the form of ({op_atom, Variable, SecondVariable}).
% Starts by doing an evaluation on A and B and then does the operation on the results from these.
safe_evaluate({times, A, B}) -> 
    A_evaluated = safe_evaluate(A),
    B_evaluated = safe_evaluate(B),
    calculate(times, A_evaluated, B_evaluated);
safe_evaluate({plus, A, B}) -> 
    A_evaluated = safe_evaluate(A),
    B_evaluated = safe_evaluate(B),
    calculate(plus, A_evaluated, B_evaluated);
safe_evaluate({minus, A, B}) -> 
    A_evaluated = safe_evaluate(A),
    B_evaluated = safe_evaluate(B),
    calculate(minus, A_evaluated, B_evaluated);
safe_evaluate({divide, A, B}) -> 
    A_evaluated = safe_evaluate(A),
    B_evaluated = safe_evaluate(B),
    calculate(divide, A_evaluated, B_evaluated);

% Catches if A or B evaluation is not in itself a operation tuple.
safe_evaluate(N) when is_integer(N) -> N;
safe_evaluate(N) when is_float(N) -> N;

% catches anything not already captured and returns an uknown_argument atom.
safe_evaluate(_) -> unknown_argument.

% Sends the parameters first to check if either A or B is an error atom,
% otherwise sends the information to an function that does the operation.
% Returns the returned value from either.
calculate(Op, A, B) ->
    Status = check_for_errors(A, B),
    case Status of
        no_errors -> 
            do_operation(Op, A,B);
        _ -> Status
    end.

% Converts the Op atom to a actual operation and does it on A and B, 
% returns the result. 
do_operation(Op, A, B) -> 
    case Op of 
        divide -> A / B;
        times -> A * B;
        minus -> A - B;
        plus -> A + B
    end.

% Tests if either A or B is an atom, otherwise return a no_errors atom.
check_for_errors(A, B) ->
    case is_atom(A) of 
        true -> A;
        false -> 
            case is_atom(B) of
                true -> B;
                false -> no_errors
            end
    end.

    
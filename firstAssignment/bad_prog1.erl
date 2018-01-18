-module(bad_prog1).
 
%  This program has a number of errors
%  find them and fix them.
%  Once you have fixed the code
%  run some text examples to see if the fuctions
%  behave as expected.
%  Running bad_prog1:test() should return the atom 'hooray'

-compile(export_all).

% TODO: unit tests & kommentarer
test_all() ->
    % geometry calculation tests
    10 = double(5),
    100 = area({square,10}),
    40 = perimeter({square,10}),

    % temperature testing
    % melting point of sulfur 
    {f,212} = temperature_convert({c,100}), 

    % math tests
    24 = factorial(4),

    %all tests done
    hooray.

factorial(0) -> 1;
factorial(N) -> N*factorial(N-1).

test1() ->
    io:format("double(2) is ~p~n",[double(2)]).

% gemoetry functions
double(X) ->
    2*X.
area({square,X}) ->
    X*X;
area({rectangle,X,Y}) ->
    X*Y;
area({circle,R}) ->
    math:pi() * math:pow(R,2).
	
% temperature conversion 
% using formula 5(F-32) = 9C
% and F = C * 9 / 5+32
temperature_convert({c,C}) -> 
    F = C*9 div 5+32, % alternative change line 20 to 212.0
    {f,F};
temperature_convert({f,F}) -> 
    C = F-32*5/9,
    {c,C}.

% calculates the perimeter
% supports {rectangle, Width, Height} and {square, Side}.
perimeter(Shape) ->
    case Shape of
        {rectangle, Width, Height} -> 2*(Width+Height);
        {square, Side} -> 4*Side;
        _ -> io:format("cannot compute the area of ~p~n", [Shape])
    end.



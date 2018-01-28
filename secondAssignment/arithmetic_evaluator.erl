-module(arithmetic_evaluator).
-compile(export_all).

% This code is written for the PARADIS course at Stockholms University vt18
% Module for evaulating arithmetic expressions.
% Supports + - / * in the form of nested tuples using the keywords 
% such as plus, times, divide, minus.
% Lasse Sjöblom - larseriksjoblom@gmail.com

test() ->
    3 = addition({plus,1,2}),
    %7 = eval({plus,3,4}),
    %-5 = eval({times, {plus,2,3}, {minus,3,4}}),
    %4.0 = eval({divide, 12,3}),
    hooray.


eval(E) ->

    true.



addition(E) -> element(2, E) + element(3, E).
subtraction(E) -> element(2, E) - element(3, E).
multiplication(E) -> element(2, E) * element(3, E).
division(E) -> element(2, E) / element(3, E).

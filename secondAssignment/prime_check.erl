-module(prime_check).
-export([is_prime/1, seq/1, filter/2, all_primes/1, test/0]).
% This code is written for the PARADIS course at Stockholms University vt18
% module for calulating prime numbers 
% can check if a number is prime or get all primes from a list.
% Lasse Sjöblom - larseriksjoblom@gmail.com


test() -> 
    % seq test
    [1, 2, 3] = seq(3),
    {'EXIT', _} = (catch seq(-5)),

    % is prime test
    false = is_prime(-5),
    false = is_prime(0),
    false = is_prime(1),
    true = is_prime(2),
    true = is_prime(3),
    true = is_prime(1033),
    false = is_prime(2954),
    true = is_prime(9949),
    false = is_prime(9950),
    true = is_prime(9967),
    true = is_prime(9973),

    % all_primes tests
    [2,3] = all_primes(3),

    % rotate list test
    TestList = [1,2,3],
    {'EXIT', _} = (catch rotate(1, [])),    
    {'EXIT', _} = (catch rotate(-5, TestList)),    
    [3,1,2] = rotate(1, TestList),
    [2,3,1] = rotate(2, TestList),
    [1,2,3] = rotate(3, TestList),
    hooray.

% Checks if N is prime using the Primality test, trial division algorithm, 
% see https://en.wikipedia.org/wiki/Primality_test and https://en.wikipedia.org/wiki/Trial_division for more details 
is_prime(N) when N =< 1 -> false;
is_prime(N) when N =< 3 -> true;
is_prime(N) when N rem 2 == 0 -> false;
is_prime(N) when N rem 3 == 0 -> false;
is_prime(N) -> is_prime(N, 5).
is_prime(N, I) when I * I =< N -> 
    if 
        N rem I == 0 -> false;
        N rem (I + 2) == 0 -> false;
        true -> is_prime(N, I+6)
    end;
is_prime(N, I) when I * I > N -> true.


% Builds a list containing numbers from 1 to N.
seq(N) when N =< 1 -> erlang:error(badarg);
seq(N) -> seq(N, []).
seq(N, List) when N > 1 ->
    seq(N-1, [N] ++ List);
seq(N, List) ->
    [N] ++ List.

% Filter L using F.
% Assumes that using the already implemented filter function 
% is allowed given that this is not specified in assignment document
filter(F, L) -> lists:filter(F, L).

% Creates a list of every prime number between 1 and N.
all_primes(N) ->
    Is_prime = fun(X) -> is_prime(X) end,
    filter(Is_prime, seq(N)).

% rotates L N steps, example: [1,2,3] rotated once is [3,1,2].
rotate(_, []) -> erlang:error(badarg);
rotate(N, _) when N < 0 -> erlang:error(badarg);
rotate(N, L) when N > 0 -> 
    Last = lists:last(L),
    ListWithoutLast = lists:droplast(L),
    RotatedList = [Last] ++ ListWithoutLast,
    rotate(N-1, RotatedList);
rotate(N, L) when N == 0 -> L.
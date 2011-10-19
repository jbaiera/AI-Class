
sum([], 0).
sum([X|Xs], N) :- sum(Xs, N1), N is X + N1.

partition(_, [], [], []).
partition(P, [X|Xs], L, R) :-
    ( X > P     ->
        R = [X|R1],
        partition(P, Xs, L, R1)
    ; X =< P    ->
        L = [X|L1],
        partition(P, Xs, L1, R)
    ).

greater(P, [], []).
greater(P, [X|Xs], Acc) :- 
    ( X > P     ->
        greater(P, Xs, Acc2),
        Acc = [X|Acc2]
    ; X =< P    ->
        greater(P, Xs, Acc)
    ).

greater2(P, [], []).
greater2(P, [X|Xs], Acc) :- X > P, greater(P, Xs, Acc2), Acc = [X|Acc2].
greater2(P, [X|Xs], Acc) :- X =< P, greater(P, Xs, Acc).

factorial(0, 1).
factorial(N, M) :- N1 is N - 1, M = N * M1, factorial(N1, M1).

fac(N, M) :- N > 0, N1 is N-1, fac(N1, M1), M is M1*N.
fac(0, 1).


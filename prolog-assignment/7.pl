
partition(_, [], [], []).
partition(P, [X|Xs], L, R) :- X =< P, partition(P, Xs, NewL, NewR), L = [X|NewL], R = NewR.
partition(P, [X|Xs], L, R) :- X  > P, partition(P, Xs, NewL, NewR), L = NewL, R = [X|NewR].

quicksort([], []).
quicksort([P|Xs], Res) :- partition(P, Xs, L, R), quicksort(L, LRes), quicksort(R, RRes), Part = [P|RRes], append(LRes,Part,Res).


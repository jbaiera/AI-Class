
removeAll(A, [], []).
removeAll(A, [X|Xs], Result) :- X == A, removeAll(A, Xs, NewResult), Result = NewResult.
removeAll(A, [X|Xs], Result) :- X \= A, removeAll(A, Xs, NewResult), Result = [X|NewResult].

count(A, [], 0).
count(A, [X|Xs], Count) :- X == A, count(A, Xs, NewCount), Count is NewCount + 1.
count(A, [X|Xs], Count) :- X \= A, count(A, Xs, NewCount), Count is NewCount.

min(X, Y, Val) :- X < Y, Val is X.
min(X, Y, Val) :- X >= Y, Val is Y.

min([X], X).
min([X|Xs], Min) :- min(Xs, Min2), min(X, Min2, Min3), Min is Min3.

pair(_-_).

frequencies([], []).
frequencies(Xs, Counts) :- min(Xs, Min), count(Min, Xs, Count), removeAll(Min, Xs, Pruned), frequencies(Pruned, NewCounts), Counts = [Min - Count | NewCounts].


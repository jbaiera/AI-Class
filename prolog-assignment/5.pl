
min(X, Y, Val) :- X < Y, Val is X.
min(X, Y, Val) :- X >= Y, Val is Y.

min([X], X).
min([X|Xs], Min) :- min(Xs, Min2), min(X, Min2, Min3), Min is Min3.

max(X, Y, Val) :- X < Y, Val is Y.
max(X, Y, Val) :- X >= Y, Val is X.

max([X], X).
max([X|Xs], Max) :- max(Xs, Max2), max(X, Max2, Max3), Max is Max3.

remove(A, [], []).
remove(A, [X|Xs], Result) :- X == A, Result = Xs.
remove(A, [X|Xs], Result) :- X \= A, remove(A, Xs, NewResult), Result = [X|NewResult].

median([X], Result) :- Result is X.
median([X,Y], Result) :- Result is ((X+Y)/2).
median(Xs, Result) :- min(Xs, Min), max(Xs, Max), remove(Min, Xs, R1), remove(Max, R1, R2), median(R2, R3), Result is R3.


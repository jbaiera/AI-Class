
subs(_, _, [], []).
subs(A, B, [X|Rest], Result) :- A == X, subs(A, B, Rest, NewResult), Result = [B|NewResult].
subs(A, B, [X|Rest], Result) :- A \= X, subs(A, B, Rest, NewResult), Result = [X|NewResult].


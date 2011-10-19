
sibling(X, Y) :- parent(X, Z), parent(Y, Z), \+ (X == Y).
sibling(X, Y) :- fraternity(X, Z), fraternity(Y, Z), \+ (X == Y).
uncle(X, Y) :- parent(X, Z), sibling(Z, Y), \+ (X == Y).

fraternity(billy, katie).
parent(billy, bob).
parent(katie, bob).
parent(timmy, bob).
parent(bob, joey).
parent(tim, joey).
parent(russ, foo).



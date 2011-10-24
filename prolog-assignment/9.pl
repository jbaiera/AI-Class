
tail([], []).
tail([X|Xs], Xs).

substring(_, [], []).
substring(Length, _, Substr) :- Length == 0, Substr = [].
substring(Length, [S|Str], Substr) :- Length > 0, NewLength is Length-1, substring(NewLength, Str, NewSubstr), Substr = [S|NewSubstr].

find(_, [], []).
find([], _, []).
find(Str, Line, Pairs) :- find(Str, Line, Pairs, 0).
find(_, [], [], _).
find([], _, [], _).
find(Str, Line, Pairs, Pos) :- length(Str, Length), substring(Length, Line, Sub),
    tail(Line, Rest), length(Rest, RestLength),
    (RestLength < Length    -> NewPairs = []
    ;RestLength >= Length   -> NewPos is Pos+1, find(Str, Rest, NewPairs, NewPos)),
    (Str == Sub     -> Pairs = [Pos|NewPairs]
    ;Str \= Sub     -> Pairs = NewPairs).

linePairs(_, [], []).
linePairs(LineNo, [P|Ps], Pairs) :- linePairs(LineNo, Ps, NewPairs), Pairs = [LineNo-P | NewPairs].

search(_, [], []).
search(Str, [L|Ls], Pairs) :- search(Str, [L|Ls], Pairs, 0).
search(Str, [], [], _).
search(Str, [L|Ls], Pairs, LineNo) :- find(Str, L, R), linePairs(LineNo, R, LP),
    (Ls \= []   -> NewLine is LineNo+1, search(Str, Ls, NP, NewLine), Pairs = [LP | NP]
    ;Ls == []   -> Pairs = LP).

searchfile(Filename, Searchstring, Locations) :- file_str("FileIn", Filename), readdevice(Old), 
    openread(fin, "FileIn"), readdevice(fin), getlinelist(Lines), 
    readdevice(Old), closefile(fin), 
    search(Searchstring, Lines, Locations). 

getlinelist(Lines) :- readln(l), getlinelist(ls), Lines = [l|ls].

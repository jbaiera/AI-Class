
student(_ - _ - _ - _ - _ - _ - _).
studentName(Record, Name) :- Record = Name - _ - _ - _ - _ - _ - _.
studentAssignments(Record, [A1,A2,A3]) :- Record = _ - A1 - A2 - A3 - _ - _ - _.
studentExams(Record, [E1,E2]) :- Record = _ - _ - _ - _ - E1 - E2 - _.
studentFinal(Record,F) :- Record = _ - _ - _ - _ - _ - _ - F.

readName(Stream,Record) :-
    read(Stream,Name),
    studentName(Record,Name).

readAssignments(Stream,Record) :-
    read(Stream,A1),
    read(Stream,A2),
    read(Stream,A3),
    studentAssignments(Record,[A1,A2,A3]).

readExams(Stream,Record) :-
    read(Stream,E1),
    read(Stream,E2),
    studentExams(Record,[E1,E2]).

readFinal(Stream,Record) :-
    read(Stream,Final),
    studentFinal(Record,Final).

processStudent(InFile) :-
    openFileToRead(InFile, InStream),
    processStudent(InStream, Student),
    print('Finished processing.'),
    finalGrade(Student, FinalGrade),
    print(FinalGrade), nl,
    print(Student).

printer(X) :- print('Hi.').

processStudent(InStream, Student) :-
    student(Record),
    readName(InStream, Record),
    readAssignments(InStream, Record),
    readExams(InStream, Record),
    readFinal(InStream, Record).

openFileToRead(InFile, InStream) :- open(InFile, read, InStream).

finalGrade(Name - A1 - A2 - A3 - E1 - E2 - F, FinalGrade) :- FinalGrade = F.


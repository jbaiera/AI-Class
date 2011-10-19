
student(_ - _ - _ - _ - _ - _ - _ - _ - _ - _).
studentName(Record, Name) :- Record = Name - _ - _ - _ - _ - _ - _ - _ - _ - _.
studentAttendance(Record, Att) :- Record = _ - Att - _ - _ - _ - _ - _ - _ - _ - _.
studentAssignments(Record, [A1,A2,A3,A4]) :- Record = _ - _ - A1 - A2 - A3 - A4 - _ - _ - _ - _.
studentProjects(Record, [P1, P2]) :- Record = _ - _ - _ - _- _ - _ - P1 - P2 - _ - _.
studentExams(Record, E1) :- Record = _ - _ - _ - _ - _ - _ - _ - _ - E1 - _.
studentFinal(Record,F) :- Record = _ - _ - _ - _ - _ - _ - _ - _ - _ - F.

readName(Stream,Record) :-
    read(Stream,Name),
    studentName(Record,Name).

readAttendance(Stream,Record) :-
    read(Stream,Att),
    studentAttendance(Record,Att).

readAssignments(Stream,Record) :-
    read(Stream,A1),
    read(Stream,A2),
    read(Stream,A3),
    read(Stream,A4),
    studentAssignments(Record,[A1,A2,A3,A4]).

readProjects(Stream,Record) :-
    read(Stream,P1),
    read(Stream,P2),
    studentProjects(Record,[P1,P2]).

readExams(Stream,Record) :-
    read(Stream,E1),
    studentExams(Record,E1).

readFinal(Stream,Record) :-
    read(Stream,Final),
    studentFinal(Record,Final).

processStudent(InFile) :-
    openFileToRead(InFile, InStream),
    processStudent(InStream, Student),
    studentName(Student, Name),
    finalGrade(Student, FinalGrade),
    format('Final grade for ~a is ~4g', [Name, FinalGrade]),

processStudent(InStream, Student) :-
    student(Record),
    readName(InStream, Record),
    readAttendance(InStream, Record),
    readAssignments(InStream, Record),
    readProjects(InStream, Record),
    readExams(InStream, Record),
    readFinal(InStream, Record),
    Student = Record.

openFileToRead(InFile, InStream) :- open(InFile, read, InStream).

finalGrade(Name - Att - A1 - A2 - A3 - A4 - P1 - P2 - E1 - F, FinalGrade) :- FinalGrade is 0.1 * Att + 0.2 * (A1 + A2 + A3 + A4) / 4 + 0.1 * P1 + 0.25 * P2 + 0.15 * E1 + 0.2 * F.


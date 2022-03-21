%student_info(Id,First_Name,Last_Name,BirthYear)
student_info(9,"John","Mike",1990).
student_info(10,"Andrew","Jackson",1990).
student_info(11,"Itachi","Carter",1991).
student_info(12,"Luna","Ethan",1989).
student_info(13,"Everly","James",1988).
 
%student(Id,AdmissionYear)
student(9,2004).
student(10,2005).
student(11,2006).
student(12,2003).
student(13,2007).
 
%studies(Id,Lecture)
studies(9,aa).
studies(10,pp).
studies(11,lfa).
 
%grade(Id,Lecture,Grade)
grade(10,pp,4).
grade(9,pp,6).
grade(10,aa,4).
grade(11,lfa,10).

%1
ninetiesKids(X, Y) :- student_info(_, X, Y, 1990).

%2
beforeTheNineties(X, Y) :- student_info(_, X, Y, Z), Z < 1990.

%3
beforeOFive(X, Y) :- student(Z,Y), Y < 2005, student_info(Z,X,Y,_).

%4
under18(X) :- student(X, Y), student_info(X,_,_,Z), Z - Y >= 18.

%5
atLeastTwoFailed(X) :- grade(X, L1, Y), grade(X, L2, Z), L1 /= L2, Y < 5, Z < 5.

%6
atLeastOneGraduate(X, L) :- grade(X, L, G) , G >= 5.


nat(zero).
nat(X) :- X = succ(Y), nat(Y).

%7
add(X, zero, X). 
add(zero, X, X).
add(X, succ(Y), R) :- add(succ(X), Y, R).   

%8
minus(X, zero, X).
minus(zero, X, X).
minus(succ(X), succ(Y), R) :- minus(X, Y, R).


toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

%9
min(_, zero, zero).
min(zero, _, zero).
min(succ(X), succ(Y), succ(R)) :- min(X, Y, R).

%10
max(X, zero, X).
max(zero, X, X).
max(succ(X), succ(Y), succ(R)) :- max(X, Y, R).

%11
gt_(X, zero) :- X /= zero.
gt_(succ(X), succ(Y)) :- gt_(X,Y).

%12
leq(X, _).
leq(succ(X), succ(Y)) :- leq(X,Y).

%13
div(X, X, succ(zero)) :- !.
div(X, Y, zero) :- leq(X, Y), !.
div(X, Y, succ(R) ) :- minus(X, Y, S),div(S, Y, R).

%14
multiply(X, _, succ(zero), X).
multiply(X, Y, succ(Z), R) :- add(X, Y, A), multiply(A, Y, Z, R).


mod(X, Y, R) :- div(X, Y, Z), multiply(Y, Y, Z, U), minus(X, U, R), !.

%15
gcd(M, M, M) :- !.
gcd(N, M, R) :- gtt(N, M), minus(N, M, Z), gcd(Z, M, R).
gcd(N, M, R) :- leq(N, M), minus(M, N, Z), gcd(N, Z, R).
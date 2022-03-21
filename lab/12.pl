%test
hello_world :- write('Hello, World!'), nl.


%1
endsWith(X,[X]).
endsWith(X, [_|Y]) :- endsWith(X,Y).

%2

startsWith(X,[X|_]).

sameFirstLast(L) :- startsWith(E,L), endsWith(E,L).

%3

inBetween(X, [X|_]).

inner(L1,L2).
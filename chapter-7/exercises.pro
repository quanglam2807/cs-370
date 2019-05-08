/* Quang */

/* START - http://knuth.luther.edu/~leekent/ProgrammingLanguages/family.prolog */
parent(fred, sophusw). parent(fred, lawrence). 
parent(fred, kenny). parent(fred, esther). 
parent(inger,sophusw). parent(johnhs, fred). 
parent(mads,johnhs). parent(lars, johan). 
parent(johan,sophus). parent(lars,mads).
parent(sophusw,gary). parent(sophusw,john). 
parent(sophusw,bruce). parent(gary, kent). 
parent(gary, stephen). parent(gary,anne). 
parent(john,michael). parent(john,michelle).
parent(addie,gary). parent(gerry, kent). 
male(gary). 
male(fred). 
male(sophus). 
male(lawrence).
male(kenny). 
male(johnhs).
male(mads).
male(lars). 
male(john). 
male(bruce). 
male(johan). 
male(sophusw). 
male(kent). 
male(michael).
male(stephen). 
female(esther). 
female(inger). 
female(anne). 
female(michelle). 
female(gerry). 
female(addie). 
father(X,Y):-parent(X,Y),male(X).
mother(X,Y):-parent(X,Y), female(X).
/* END http://knuth.luther.edu/~leekent/ProgrammingLanguages/family.prolog */

/* 1. Write a rule (i.e. predicate) that describes the relationship of a sibling. 
Then write a query to find out if Anne and Stephen are siblings. 
Then ask if Stephen and Michael are siblings. What is Prolog’s response? */
sibling(X, Y) :- parent(P, X), parent(P, Y).
/* 
?- sibling(anne, stephen).
true
?- sibling(stephen, michael).
false
*/

/* 2. Write a rule that describes the relationship of a brother. 
Then write a query to find the brothers of sophusw. What is Prolog’s response? */
brother(X, Y) :- sibling(X, Y), male(X), not(X = Y).
/*
?- brother(sophusw, B).
B = lawrence ;
B = kenny ;
*/

/* 3. Write a rule that describes the relationship of a niece.
Then write a query to find all nieces in the database. What is Prolog’s response? */
niece(X, Y) :- parent(P, X), sibling(P, Y), female(X).
/*
?- niece(X, Y).
X = esther,
Y = fred ;
X = anne,
Y = gary ;
X = anne,
Y = john ;
X = anne,
Y = bruce ;
X = anne,
Y = gary ;
X = michelle,
Y = gary ;
X = michelle,
Y = john ;
X = michelle,
Y = bruce ;
*/

/* 4. Write a predicate that describes the relationship of cousins. */
cousin(X, Y) :- parent(PX, X), parent(PY, Y), sibling(PX, PY).

/* 5. Write a predicate that describes the ancestor relationship. */
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

/* 6. Write a predicate called odd that returns true 
if a list has an odd number of elements. */
odd([_|T]) :- even(T).
even([]).
even([_|T]) :- odd(T).

/* 7. Write a predicate that checks to see if a list is a palindrome. */
palindrome([]).
palindrome([_]).
palindrome(Pal) :-
    append([H|T], [H], Pal),
    palindrome(T).

/* 8. Show the substitution required to prove 
that sublist([a, b], [c, a, b]) is true. Use
the definition in Fig. 7.3 and use the same method of proving it’s true.
*/
sublist(X, Y) :- append(_, X, L), append(L, _, Y).

/* 9. Write a predicate that computes the factorial of a number. */
factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.

/* 10. Write a predicate that computes the nth fibonacci number
 in exponential time complexity. */
fibexp(0, 0).
fibexp(1, 1).
fibexp(N, F) :- N > 1, N1 is N - 1, N2 is N -2, fibexp(N1, F1), fibexp(N2, F2), F is F1 + F2. 

/* 11. Write a predicate that computes the 
nth fibonacci number in linear time com- plexity. */
fiblin(1, 1, 0).
fiblin(N, F, F1) :- N > 1, N1 is N - 1, fiblin(N1, F1, F2), F is F1 + F2. 

/* Write a predicate that returns true if a third list is the result of 
zipping two others together. For instance, 
zipped([1,2,3],[a,b,c],[pair(1,a),pair(2,b),pair(3,c)])
should return true since zipping [1, 2, 3] and [a, b, c] would yield the list of pairs
given above. */
zipped([], [], []).
zipped([A|TA], [B|TB], [Z|TZ]) :- zipped(TA, TB, TZ), Z = pair(A,B).

/* 13. Write a predicate that counts the number of 
times a specific atom appears in a list. */
count(_, [], 0).
count(X, [X|T], N) :- count(X, T, N1), N is N1 + 1.
count(X, [Y|T], N) :- not(X = Y), count(X, T, N).

/* 14. Write a predicate that returns true if a list is 
three copies of the same sublist. For
instance, the predicate should return true if called as
threecopies([a, b, c, a, b, c, a, b, c]).
It should also return true if it were called like
threecopies([a,b,c,d,a,b,c,d,a,b,c,d]).
*/
threecopies(LST) :- append(X, X, X1), append(X1, X, LST).
append([], L, L)
append([H|T], L, [H|TL]) :- append(T, L, TL).

reverse([], []).
reverse([H|T], L) :- reverse(T, TR), append(TR, [H], L).

sum([], 0)
sum([H|T], S) :- D is H+S, sum(T, D).

palindrome(L) :- reverse(L, L).
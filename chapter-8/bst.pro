/* node(val,left,right) */
/* node(val,nil,nil) */
/* lookup(nil,V) */

lookup(node(V,_,_), V). 
lookup(node(V,L,_), N) :- N < V, lookup(L,N).
lookup(node(V,_,R), N) :- N > V, lookup(R,N).

lookup(node(10,node(5,node(5,node,node(3, nil, nil)),node(7,nil,nil)),node(15,node(12,nil,nil)))).

insert(V,nil,node(V,nil,nil)).
insert(V,node(V,L,R),node(V,L,R)).
insert(V,node(N,L,R),node(N,NL,R)) :- V < N, insert(V,L,NL).
insert(V,node(N,L,R),node(N,L,NR)) :- V > N, insert(V,R,NR).

getLeftMost(node(V,nil,_),V).
getLeftMost(node(_,L,_),NV) :- L \= nil, getLeftMost(L,NV).

delete(_,nil,nil).
delete(V,node(V,nil,nil),nil).
delete(V,node(V,L,nil),L).
delete(V,node(V,R,nil),R).
delete(V,node(V,L,R),node(NV,L,NR)) :- L \= nil, R \= nil, getLeftMost(R,NV), delete(NV,R,NR).
delete(V,node(N,L,R),node(N,NL,R)) :- V < N, delete(V,L,NL).
delete(V,node(N,L,R),node(N,L,NR)) :- V > N, delete(V,R,NR).

processlist([],Tree,Tree).
processlist([H|T],Tree,NewTree) :- insert(H,Tree,Three), processlist(T,Three,NewTree).

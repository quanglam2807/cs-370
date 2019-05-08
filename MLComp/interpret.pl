/* Quang Lam */

readAST(AST) :- open('a.term',read,Stream), read(Stream,AST).

smlEval(letdec(bindval(Id,Val),[H|_]),R,Bindings) :- append(Bindings,[(Id,Val)],NewBindings), smlEval(H,R,NewBindings).
smlEval([H|T],R,Bindings) :- smlEval(H,_,Bindings), smlEval(T,R,Bindings).
smlEval(apply(id(println),E),unit,Bindings) :- smlEval(E,R,Bindings), print(R) , nl.
smlEval(apply(id(+),tuplecon([E1,E2])),R,Bindings) :- smlEval(E1,R1,Bindings), smlEval(E2,R2,Bindings), R is R1 + R2.
smlEval(int(I),R,_) :- atom_number(I,R).

smlEval(id(I),R,LST) :- binding(I,LST,E), smlEval(E,R,_).
error_process(notFound) :- write('Not found').
binding(_,[]) :- throw(notFound).
binding(I,[(idpat(I),R)|_], R). 
binding(I,[(idpat(X),_)|T], R) :- I\=X, binding(I,T,R).

interpret :- readAST(AST), smlEval(AST,R,[]), print(R), !.
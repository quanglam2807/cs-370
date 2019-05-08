/* calc :- readln(L, _, _, _, lowercase), preprocess(L, preL) */

interpret(add(AST1, AST2), MIN, MOUT, VAL) :- 
    interpret(AST1, MIN, MOUT1, VAL1), 
    interpret(AST2, MOUT1, MOUT, VAL2), 
    VAL is VAL1 + VAL2.

interpret(mul(AST1, AST2), MIN, MOUT, VAL) :- 
    interpret(AST1, MIN, MOUT1, VAL1), 
    interpret(AST2, MOUT1, MOUT, VAL2), 
    VAL is VAL1 * VAL2.

interpret(sub(AST1, AST2), MIN, MOUT, VAL) :- 
    interpret(AST1, MIN, MOUT1, VAL1), 
    interpret(AST2, MOUT1, MOUT, VAL2), 
    VAL is VAL1 - VAL2.

interpret(div(AST1, AST2), MIN, MOUT, VAL) :- 
    interpret(AST1, MIN, MOUT1, VAL1), 
    interpret(AST2, MOUT1, MOUT, VAL2), 
    VAL is VAL1 / VAL2.

interpret(store(AST1), MIN, MOUT1, MOUT) :-
    interpret(AST1, MIN, MOUT1, MOUT).

interpret(recall, MIN, MOUT, MIN).

interpret(negate(AST1), MIN, MOUT, VAL) :-
    interpret(AST1, MIN, MOUT, NVAL),
    VAL is NVAL * -1.

interpret(num(N), MIN, MIN, N).
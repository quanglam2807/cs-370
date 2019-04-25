codegen(caseof (exp, matchlist), ....)
(codegen(exp, .......);

let
fun f 0 1 = 0
  | f 0 0 = 1
  | f 1 0 = 2
in
  f 1 0;
  f 1
end

fun f v1 = fn v2 => (fn (0, 1) = 0
                      | (0, 0) = 0
                      | (1, 0) = 2) (v1, v2)
f:int -> int -> int

(* tuplepat -> listpat (for exercise 7 & 8) *)

(* Exercise 6 *)
let val x = 4
in
  println
    case x of
      1 => "hello"
    | 2 => "how"
    | 3 => "are"
    | 4 => "you"
end
(* Become this *)
(* caseof(id("x"), [ match(intpat("1"), str("hello")), ... ]) *)
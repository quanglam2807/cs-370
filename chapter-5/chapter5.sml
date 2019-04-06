(* Quang Lam *)

(* 10 *)
fun gcd(x, 0) = x
  | gcd(x, y) = gcd(y, x mod y);

(* 11 *)
fun allCaps "" = ""
  | allCaps str = String.concat([String.str(Char.toUpper(String.sub(str, 0))), allCaps(String.extract(str, 1, NONE))]);

(* 12 *)
fun firstCaps [] = []
  | firstCaps lst = String.concat([String.str(Char.toUpper(String.sub(List.hd(lst), 0))), String.extract(List.hd(lst), 1, NONE)])::firstCaps(List.tl(lst));

(* 13 *)
fun swap (a::b::lst) = b::a::swap(lst)
  | swap [a] = [a]
  | swap [] = [];

(* 14 *)
fun rotate(0, lst) = lst
  | rotate(n, []) = []
  | rotate(n, h::t) = rotate(n - 1, t@[h]);

(* 15 *)
fun delete(n, str) =
  let fun deleteHelper(n, lst) = implode(List.take(lst, n)@List.drop(lst, n + 1))
  in
    deleteHelper(n, String.explode(str))
  end;

(* 16 *)
fun intpow(x, 0) = 1
  | intpow(x, n) = 
    if n mod 2 = 1 then x * intpow(x, n - 1)
    else
      let val r = intpow(x, n div 2)
      in
        r * r
      end;

(* 17 *)
fun rotate2(n, lst) = 
  let fun helper(m, [], acc) = []
        | helper(0, xs::ys, acc) = xs::ys@acc
        | helper(m, xs::ys, acc) = helper(m - 1, ys, acc@[xs])
  in
    if n = 0 then lst
    else helper(n, lst, [])
  end;

(* 18 *)
(* l = len(lst). if l > n then after l steps, it goes back to the original *)
fun rotate3(n, lst) = rotate2(n mod List.length(lst), lst);

(* 19 *)
fun delete2 n str = delete(n, str);

(* 20 *)
fun delete5 str = delete2 5 str;

(* Shared Higher-order function *)
fun applyFun(f, []) = []
    | applyFun(f, x::lst) = f(x)@applyFun(f, lst);

(* 21 *)
fun isEven(x) = if x mod 2 = 0 then [x] else [];
fun evens(lst) = applyFun(isEven, lst);

(* 22 *)
fun isLowerFirst(str) = if Char.isLower(String.sub(str, 0)) then [str] else [];
fun lowerFirsts(lst) = applyFun(isLowerFirst, lst);

(* 23 *)
fun toUpper(char) = [Char.toUpper(char)];
fun allCaps2(str) = String.implode(applyFun(toUpper, String.explode(str)));

(* 24 *)
fun find(s, file) = 
let val strm = TextIO.openIn(file)
in
  while true do
    let val line = valOf(TextIO.inputLine(strm))
    in
      if (String.isSubstring s line) then 
        TextIO.output(TextIO.stdOut, line)
      else 
        ()
    end
end handle Option => ();

(* 25 *)
fun runFunctionWhileHandlingException (f) b = [f b handle except => b];
fun transform (f) [] = []
    | transform (f) lst = applyFun(runFunctionWhileHandlingException f, lst);

(* 26 *)
datatype Natural = O | succ of Natural;
fun pred(n:Natural):Natural = 
  case n of
    O => raise Fail "predecessor on O"
  | succ(m) => m;

(* 27 *)
fun convert(n:Natural) = 
  case n of O => 0
    | succ(m) => 1 + convert(m);

(* 28 *)
fun add(a:Natural, b:Natural):Natural =
    case a of O => b
  | a => add(pred(a), succ(b));

(* 29 *)
fun mul(a:Natural, b:Natural):Natural =
    case a of O => O
  | a => add(b, mul(pred(a), b));

(* 30 *)
fun hadd(lst)= List.foldr add O lst;
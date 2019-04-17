functor mlcompLexFun(structure Tokens : mlcomp_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

(* mlcomp.lex -- lexer spec *)

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
val pos = ref 1
val error = fn x => TextIO.output(TextIO.stdErr, x ^ "\n")
val eof = fn () => Tokens.EOF(!pos, !pos)

fun countnewlines s = 
    let val lst = explode s
        fun count (c:char) nil = 0
          | count c (h::t) = 
            let val tcount = count c t
            in
              if c = h then 1+tcount else tcount
            end
    in
      pos:= (!pos) + (count #"\n" lst)
    end



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (countnewlines yytext; lex())
      end
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := (!pos) + 1; lex()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Plus(!pos,!pos)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Times(!pos,!pos)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Minus(!pos,!pos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Append(!pos,!pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Equals(!pos,!pos)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LParen(!pos,!pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RParen(!pos,!pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBracket(!pos,!pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBracket(!pos,!pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ListCons(!pos,!pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Comma(!pos,!pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Semicolon(!pos,!pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Underscore(!pos,!pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Arrow(!pos,!pos)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VerticalBar(!pos,!pos)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Greater(!pos,!pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Less(!pos,!pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GreaterEqual(!pos,!pos)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LessEqual(!pos,!pos)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NotEqual(!pos,!pos)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Exclaim(!pos,!pos)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SetEqual(!pos,!pos)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.Tilde(!pos, !pos)))
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.Int(yytext,!pos,!pos))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.Char(yytext,!pos,!pos))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.String(yytext,!pos,!pos))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val tok = String.implode (List.map (Char.toLower) 
             (String.explode yytext))
    in
      if      tok="let" then Tokens.Let(!pos,!pos)
      else if tok="val" then Tokens.Val(!pos,!pos)
      else if tok="in" then Tokens.In(!pos,!pos)
      else if tok="end" then Tokens.End(!pos,!pos)
      else if tok="if" then Tokens.If(!pos,!pos)
      else if tok="then" then Tokens.Then(!pos,!pos)
      else if tok="else" then Tokens.Else(!pos,!pos)
      else if tok="div" then Tokens.Div(!pos,!pos)
      else if tok="mod" then Tokens.Mod(!pos,!pos)
      else if tok="fn" then Tokens.Fn(!pos,!pos)
      else if tok="while" then Tokens.While(!pos,!pos)
      else if tok="do" then Tokens.Do(!pos,!pos)
      else if tok="and" then Tokens.And(!pos,!pos)
      else if tok="rec" then Tokens.Rec(!pos,!pos)
      else if tok="fun" then Tokens.Fun(!pos,!pos)
      else if tok="as" then Tokens.As(!pos,!pos)
      else if tok="handle" then Tokens.Handle(!pos,!pos)
      else if tok="raise" then Tokens.Raise(!pos,!pos)
      else if tok="true" then Tokens.True(!pos,!pos)
      else if tok="false" then Tokens.False(!pos,!pos)
      else Tokens.Id(yytext,!pos,!pos) 
    end)
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (error ("\nerror: bad token "^yytext^"\n"); lex())
      end
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"/"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"/"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ26(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ27(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ28(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ30(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ29(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #":"
                  then yyQ31(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ32(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ33(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"0"
              then yyAction26(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ33(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ33(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"0"
              then yyAction26(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ33(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ35(strm', lastMatch)
              else yyQ34(strm', lastMatch)
      (* end case *))
and yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ35(strm', lastMatch)
            else if inp < #"*"
              then if inp = #")"
                  then yyQ36(strm', lastMatch)
                  else yyQ34(strm', lastMatch)
              else yyQ34(strm', lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ34(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ37(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ41(strm', lastMatch)
              else yyQ40(strm', lastMatch)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ41(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyQ40(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ42(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ42(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ42(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ42(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyQ15(strm', lastMatch)
            else if inp < #";"
              then if inp = #"("
                  then yyQ7(strm', lastMatch)
                else if inp < #"("
                  then if inp = #" "
                      then yyQ2(strm', lastMatch)
                    else if inp < #" "
                      then if inp = #"\n"
                          then yyQ3(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ2(strm', lastMatch)
                              else yyQ1(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #"#"
                      then yyQ6(strm', lastMatch)
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ4(strm', lastMatch)
                          else yyQ5(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = #"-"
                  then yyQ12(strm', lastMatch)
                else if inp < #"-"
                  then if inp = #"+"
                      then yyQ10(strm', lastMatch)
                    else if inp < #"+"
                      then if inp = #")"
                          then yyQ8(strm', lastMatch)
                          else yyQ9(strm', lastMatch)
                      else yyQ11(strm', lastMatch)
                else if inp = #"0"
                  then yyQ13(strm', lastMatch)
                else if inp < #"0"
                  then yyQ1(strm', lastMatch)
                else if inp = #":"
                  then yyQ14(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = #"^"
              then yyQ1(strm', lastMatch)
            else if inp < #"^"
              then if inp = #"@"
                  then yyQ19(strm', lastMatch)
                else if inp < #"@"
                  then if inp = #">"
                      then yyQ18(strm', lastMatch)
                    else if inp < #">"
                      then if inp = #"<"
                          then yyQ16(strm', lastMatch)
                          else yyQ17(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = #"\\"
                  then yyQ1(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ21(strm', lastMatch)
                      else yyQ20(strm', lastMatch)
                  else yyQ22(strm', lastMatch)
            else if inp = #"|"
              then yyQ24(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"a"
                  then yyQ20(strm', lastMatch)
                else if inp < #"a"
                  then if inp = #"_"
                      then yyQ23(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = #"{"
                  then yyQ1(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = #"~"
              then yyQ25(strm', lastMatch)
              else yyQ1(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end

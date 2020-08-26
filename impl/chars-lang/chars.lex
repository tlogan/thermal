open Token

type pos = int
type lexresult = (svalue, int) token 
type lexarg = string


val linenum = ref 1
val colnum = ref 1

val incline = fn () =>
  (linenum := (!linenum + 1); colnum := 1)

val inccol = fn x => let
  val old = !colnum
  val () = colnum := old + x
in 
  old
end

val error = fn x =>
  print (x ^ "\n")

val eof = fn () =>
  (EOF (!linenum, !colnum))

%%

%header (structure Chars);


%s COMMENT;
alpha=[a-zA-Z];
digit=[0-9];
symbol=[\-\^\\=+!@$%&*<>/{}|?~:']|"["|"]";

%%

<INITIAL>"/*"  => (YYBEGIN COMMENT; (inccol 2); lex());
<COMMENT>"*/"  => (YYBEGIN INITIAL; (inccol 2); lex());
<COMMENT>\n => (incline (); lex ());
<COMMENT>\r\n => (incline (); lex ());
<COMMENT>. => ((inccol 1); lex ());

<INITIAL>\n => (incline (); lex ());
<INITIAL>\r\n => (incline (); lex ());
<INITIAL>\t => (ignore (inccol 4); lex());
<INITIAL>(\ )+ => (ignore (inccol (size yytext)); lex());
<INITIAL>"//"[^\n]*\n  => (incline (); lex());



<INITIAL>"with" => (WITH (!linenum, inccol 1));
<INITIAL>"pipe" => (PIPE (!linenum, inccol 1));
<INITIAL>":" => (COLON (!linenum, inccol 1));
<INITIAL>"|" => (BAR (!linenum, inccol 1));
<INITIAL>"=>" => (FATARROW (!linenum, inccol 1));
<INITIAL>"[" => (LSQ (!linenum, inccol 1));
<INITIAL>"]" => (RSQ (!linenum, inccol 1));

<INITIAL>"." => (DOT (!linenum, inccol 1));

<INITIAL>"case" => (CASE (!linenum, inccol 1));

<INITIAL>"reflect" => (REFLECT (!linenum, inccol 1));
<INITIAL>"log" => (LOG (!linenum, inccol 1));
<INITIAL>"sym" => (SYM (!linenum, inccol 1));

<INITIAL>"def" => (DEF (!linenum, inccol 1));
<INITIAL>"select" => (SELECT (!linenum, inccol 1));

<INITIAL>"add" => (ADD (!linenum, inccol 1));
<INITIAL>"sub" => (SUB (!linenum, inccol 1));
<INITIAL>"mul" => (MUL (!linenum, inccol 1));
<INITIAL>"div" => (DIV (!linenum, inccol 1));


<INITIAL>"addw" => (ADDW (!linenum, inccol 1));
<INITIAL>"subw" => (SUBW (!linenum, inccol 1));
<INITIAL>"mulw" => (MULW (!linenum, inccol 1));
<INITIAL>"divsw" => (DIVSW (!linenum, inccol 1));
<INITIAL>"divuw" => (DIVUW (!linenum, inccol 1));
<INITIAL>"remsw" => (REMSW (!linenum, inccol 1));
<INITIAL>"remuw" => (REMUW (!linenum, inccol 1));

<INITIAL>"addf" => (ADDF (!linenum, inccol 1));
<INITIAL>"subf" => (SUBF (!linenum, inccol 1));
<INITIAL>"mulf" => (MULF (!linenum, inccol 1));
<INITIAL>"divf" => (DIVF (!linenum, inccol 1));

<INITIAL>"equal" => (EQUAL (!linenum, inccol 1));

<INITIAL>"alloc_mem" => (ALLOC_MEM (!linenum, inccol 1));
<INITIAL>"size" => (SIZE (!linenum, inccol 1));
<INITIAL>"slice" => (SLICE (!linenum, inccol 1));
<INITIAL>"set" => (SET (!linenum, inccol 1));
<INITIAL>"get" => (GET (!linenum, inccol 1));

<INITIAL>"alloc_chan" => (ALLOC_CHAN (!linenum, inccol 1));
<INITIAL>"send" => (SEND (!linenum, inccol 1));
<INITIAL>"recv" => (RECV (!linenum, inccol 1));

<INITIAL>"latch" => (LATCH (!linenum, inccol 1));
<INITIAL>"choose" => (CHOOSE (!linenum, inccol 1));
<INITIAL>"offer" => (OFFER (!linenum, inccol 1));
<INITIAL>"abort" => (ABORT (!linenum, inccol 1));


<INITIAL>"sync" => (SYNC (!linenum, inccol 1));
<INITIAL>"spawn" => (SPAWN (!linenum, inccol 1));
<INITIAL>"bind" => (BIND (!linenum, inccol 1));
<INITIAL>"return" => (RETURN (!linenum, inccol 1));


<INITIAL>"alloc_loc" => (ALLOC_LOC (!linenum, inccol 1));
<INITIAL>"propagate" => (PROPAGATE (!linenum, inccol 1));


<INITIAL>"(" => (LPAREN (!linenum, inccol 1));
<INITIAL>")" => (RPAREN (!linenum, inccol 1));
<INITIAL>"<|" => (LANG (!linenum, inccol 1));
<INITIAL>"|>" => (RANG (!linenum, inccol 1));

<INITIAL>"infixl" => (INFIXL (!linenum, inccol 1));
<INITIAL>"infixr" => (INFIXR (!linenum, inccol 1));
<INITIAL>"d"{digit} => (DIGIT ((valOf o Int.fromString) (substring (yytext, 1, 1)), !linenum, inccol 1));

<INITIAL>"()" => (LRPAREN (!linenum, inccol 1));

<INITIAL>"-"?{digit}+("."{digit}+)? =>
  (NUM (yytext, !linenum, inccol (size yytext)));


<INITIAL>("-"|"+")?{digit}+"w"("8"|"16"|"32"|"64"|"128")=>
  (WORD (yytext, !linenum, inccol (size yytext)));

<INITIAL>("-"|"+")?{digit}+("."|","){digit}+"f"("8"|"16"|"32"|"64"|"128") =>
  (FLOAT (yytext, !linenum, inccol (size yytext)));

<INITIAL>"`"([^`]|"\\`")*"`" => (STRING (yytext, !linenum, inccol (size yytext)));

<INITIAL>({symbol}|{digit}|{alpha})({symbol}|{digit}|{alpha}|_)* => (ID (yytext, !linenum, inccol (size yytext)));


<INITIAL>. => (BAD (!linenum, inccol (print ("BAD: " ^ yytext ^ "\n"); size yytext))); 

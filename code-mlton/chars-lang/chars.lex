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

alpha=[A-Za-z];
digit=[0-9];

%%

<INITIAL>"(*"  => (YYBEGIN COMMENT; (inccol 2); lex());
<COMMENT>"*)"  => (YYBEGIN INITIAL; (inccol 2); lex());
<COMMENT>\n => (incline (); lex ());
<COMMENT>\r\n => (incline (); lex ());
<COMMENT>. => ((inccol 1); lex ());

<INITIAL>\n => (incline (); lex ());
<INITIAL>\r\n => (incline (); lex ());
<INITIAL>\t => (ignore (inccol 4); lex());
<INITIAL>(\ )+ => (ignore (inccol (size yytext)); lex());
<INITIAL>"//"[^\n]*\n  => (incline (); lex());

<INITIAL>";" => (SEMICOLON (!linenum, inccol 1));
<INITIAL>":" => (COLON (!linenum, inccol 1));
<INITIAL>"," => (COMMA (!linenum, inccol 1));
<INITIAL>"\\" => (BSLASH (!linenum, inccol 1));
<INITIAL>"." => (DOT (!linenum, inccol 1));
<INITIAL>"|" => (BAR (!linenum, inccol 1));
<INITIAL>"->" => (ARROW (!linenum, inccol 1));
<INITIAL>"::" => (DCOLON (!linenum, inccol 1));
<INITIAL>"=" => (EQ (!linenum, inccol 1));
<INITIAL>"/\\" => (WEDGE (!linenum, inccol 1));
<INITIAL>"\\/" => (VEE (!linenum, inccol 1));
<INITIAL>"-->" => (LONGARROW (!linenum, inccol 1));
<INITIAL>"<->" => (DARROW (!linenum, inccol 1));

<INITIAL>"+" => (CROSS (!linenum, inccol 1));
<INITIAL>"-" => (DASH (!linenum, inccol 1));
<INITIAL>"*" => (STAR (!linenum, inccol 1));
<INITIAL>"/" => (SLASH (!linenum, inccol 1));
<INITIAL>"%" => (CIRSLASH (!linenum, inccol 1));

<INITIAL>"alloc_chan" => (ALLOC_CHAN (!linenum, inccol 1));
<INITIAL>"send" => (SEND (!linenum, inccol 1));
<INITIAL>"recv" => (RECV (!linenum, inccol 1));
<INITIAL>"wrap" => (WRAP (!linenum, inccol 1));
<INITIAL>"chse" => (CHSE (!linenum, inccol 1));
<INITIAL>"spawn" => (SPAWN (!linenum, inccol 1));
<INITIAL>"sync" => (SYNC (!linenum, inccol 1));

<INITIAL>"~" => (TILDE (!linenum, inccol 1));
<INITIAL>"reduced" => (REDUCED (!linenum, inccol 1));
<INITIAL>"called" => (CALLED (!linenum, inccol 1));
<INITIAL>"returned" => (RETURNED (!linenum, inccol 1));
<INITIAL>"spawned" => (SPAWNED (!linenum, inccol 1));
<INITIAL>"blocked" => (BLOCKED (!linenum, inccol 1));
<INITIAL>"synced" => (SYNCED (!linenum, inccol 1));
<INITIAL>"stuck" => (STUCK (!linenum, inccol 1));
<INITIAL>"done" => (DONE (!linenum, inccol 1));

<INITIAL>"solve" => (SOLVE (!linenum, inccol 1));


<INITIAL>"[" => (LSQ (!linenum, inccol 1));
<INITIAL>"]" => (RSQ (!linenum, inccol 1));
<INITIAL>"{" => (LCUR (!linenum, inccol 1));
<INITIAL>"}" => (RCUR (!linenum, inccol 1));
<INITIAL>"(" => (LPAREN (!linenum, inccol 1));
<INITIAL>")" => (RPAREN (!linenum, inccol 1));

<INITIAL>"_" => (LODASH (!linenum, inccol 1));
<INITIAL>"that" => (THAT (!linenum, inccol 1));
<INITIAL>"true" => (TRUE (!linenum, inccol 1));
<INITIAL>"false" => (FALSE (!linenum, inccol 1));

<INITIAL>("-"?){digit}+(("."|","){digit}+)? =>
  (NUMLIT (yytext, !linenum, inccol (size yytext)));

<INITIAL>"`"([^"`"]|"\`")*"`" => (STRINGLIT (yytext, !linenum, inccol (size yytext)));

<INITIAL>{alpha}({alpha}|{digit}|"_")* =>
  (ID (yytext, !linenum, inccol (size yytext)));

<INITIAL>. => (BAD (!linenum, inccol (print ("BAD: " ^ yytext ^ "\n"); size yytext))); 

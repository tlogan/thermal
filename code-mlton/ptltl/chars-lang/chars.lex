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

<INITIAL>"true" => (TRUE (!linenum, inccol 1));
<INITIAL>"false" => (FALSE (!linenum, inccol 1));

<INITIAL>"-->" => (LONGARROW (!linenum, inccol 1));
<INITIAL>"<->" => (DARROW (!linenum, inccol 1));
<INITIAL>"\\/" => (VEE (!linenum, inccol 1));
<INITIAL>"++" => (DPLUS (!linenum, inccol 1));
<INITIAL>"/\\" => (WEDGE (!linenum, inccol 1));
<INITIAL>"since" => (SINCE (!linenum, inccol 1));

<INITIAL>"always" => (ALWAYS (!linenum, inccol 1));
<INITIAL>"once" => (ONCE (!linenum, inccol 1));
<INITIAL>"prev" => (PREV (!linenum, inccol 1));
<INITIAL>"start" => (START (!linenum, inccol 1));
<INITIAL>"end" => (END (!linenum, inccol 1));
<INITIAL>"~" => (TILDE (!linenum, inccol 1));

<INITIAL>"(" => (LPAREN (!linenum, inccol 1));
<INITIAL>")" => (RPAREN (!linenum, inccol 1));

<INITIAL>{alpha}({alpha}|{digit}|"_")* =>
  (ID (yytext, !linenum, inccol (size yytext)));

<INITIAL>. => (BAD (!linenum, inccol (print ("BAD: " ^ yytext ^ "\n"); size yytext))); 

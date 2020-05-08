open Tree

%%

%name Spec 

%term

  SEMI | COMPO | SELECT |
  DEF |
  COMMA |
  FATARROW | COLON | DOT | LSQ | RSQ | CASE |
  ADD | SUB | MUL | DIV | REM | 
  ADDW | SUBW | MULW | DIVSW | DIVUW | REMSW | REMUW | 
  ADDF | SUBF | MULF | DIVF | 
  EQUAL | 
  ALLOC_MEM | SIZE | SLICE | SET | GET |
  ALLOC_CHAN | SEND | RECV | 
  WRAP | CHSE | SYNC | SPAWN | 
  LPAREN | RPAREN | LANG | RANG | LRPAREN | 
  SYM | INFIXL | INFIXR | DIGIT of int |

  NUM of string | WORD of string | FLOAT of string |
  STRING of string | ID of string |

  BAD | EOF



%nonterm
  tree_nt of term |
  term_nt of term |
  terms_nt of term list |
  fields_nt of (string * (infix_option * term)) list |
  field_nt of (string * (infix_option * term)) |
  lams_nt of (term * term) list |
  lam_nt of (term * term)
  

%pos int

%eop EOF
%noshift EOF


%right SEMI
%right FATARROW CASE COLON DEF

%right COMMA
%left COMPO

%right SYM 

%left ALLOC_CHAN SEND RECV WRAP CHSE SPAWN SYNC ADD SUB MUL DIV REM SELECT EQUAL
%nonassoc INFIXL INFIXR DIGIT

%left LPAREN LANG
%right RPAREN RANG

%left LRPAREN

%left NUM STRING ID

%left DOT
%left LSQ 
%left RSQ 

%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)


term_nt:

  LPAREN term_nt RPAREN (Assoc (term_nt, LPARENleft)) |

  term_nt COMMA term_nt (Cns (Lst ([term_nt1, term_nt2], COMMAleft), COMMAleft)) |
  term_nt COMMA (Lst ([term_nt], COMMAleft)) |

  lam_nt (Fnc ([lam_nt], [], [], lam_ntleft)) |
  LPAREN lams_nt (Fnc (lams_nt, [], [], lams_ntleft)) |

  term_nt term_nt %prec COMPO (Compo (term_nt1, term_nt2, term_nt1left)) |
  term_nt SEMI term_nt (Seq (term_nt1, term_nt2, SEMIleft)) |

  field_nt (Rec ([field_nt], false, field_ntleft)) |
  LPAREN fields_nt (Rec (fields_nt, false, LPARENleft)) |


  term_nt LSQ term_nt RSQ (Select (Lst ([term_nt1, term_nt2], LSQleft), LSQleft)) |
  term_nt DOT ID (Select (Lst ([term_nt, Str (ID, DOTleft)], DOTleft), DOTleft)) |
  SELECT (Fnc ([(Id ("_param", ~1), Select (Id ("_param", SELECTleft), SELECTleft))], [], [], SELECTleft)) |

  EQUAL (Fnc ([(Id ("_param", ~1), Select (Id ("_param", EQUALleft), EQUALleft))], [], [], EQUALleft)) |

  ALLOC_CHAN (Fnc ([(Id ("_param", ~1), Alloc_Chan (Id ("_param", ALLOC_CHANleft), ALLOC_CHANleft))], [], [], ALLOC_CHANleft)) |

  SEND (Fnc ([(Id ("_param", ~1), Send (Id ("_param", ~1), SENDleft))], [], [], SENDleft)) |
  RECV (Fnc ([(Id ("_param", ~1), Recv (Id ("_param", ~1), RECVleft))], [], [], RECVleft)) |

  WRAP (Fnc ([(Id ("_param", ~1), Wrap (Id ("_param", ~1), WRAPleft))], [], [], WRAPleft)) |
  CHSE (Fnc ([(Id ("_param", ~1), Chse (Id ("_param", ~1), CHSEleft))], [], [], CHSEleft)) |

  SYNC (Fnc ([(Id ("_param", ~1), Sync (Id ("_param", ~1), SYNCleft))], [], [], SYNCleft)) |

  SPAWN (Fnc ([(Id ("_param", ~1), Spawn (Id ("_param", ~1), SPAWNleft))], [], [], SPAWNleft)) |

  LANG term_nt RANG (Par (term_nt, LANGleft)) |

  SYM term_nt (Sym (term_nt, SYMleft)) | 

  LRPAREN (Blank LRPARENleft) | 

  ID (Id (ID, IDleft)) |

  NUM (Num (NUM, NUMleft)) |

  ADD (Fnc ([(Id ("_param", ~1), Add (Id ("_param", ~1), ADDleft))], [], [], ADDleft)) |
  SUB (Fnc ([(Id ("_param", ~1), Sub (Id ("_param", ~1), SUBleft))], [], [], SUBleft)) |
  MUL (Fnc ([(Id ("_param", ~1), Mul (Id ("_param", ~1), MULleft))], [], [], MULleft)) |
  DIV (Fnc ([(Id ("_param", ~1), Div (Id ("_param", ~1), DIVleft))], [], [], DIVleft)) |
  REM (Fnc ([(Id ("_param", ~1), Rem (Id ("_param", ~1), REMleft))], [], [], REMleft)) |

  STRING (Str (STRING, STRINGleft))


lams_nt:
  CASE lam_nt lams_nt (lam_nt :: lams_nt) |
  CASE lam_nt RPAREN ([lam_nt])

lam_nt:
  term_nt FATARROW term_nt ((term_nt1, term_nt2))

fields_nt:
  DEF field_nt fields_nt (field_nt :: fields_nt) |
  DEF field_nt RPAREN ([field_nt])

field_nt:
  ID INFIXL DIGIT COLON term_nt
    (ID, (SOME (Left, DIGIT), term_nt)) |
  ID INFIXR DIGIT COLON term_nt
    (ID, (SOME (Right, DIGIT), term_nt)) |
  ID COLON term_nt (ID, (NONE, term_nt))

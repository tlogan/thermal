open Tree

%%

%name Spec 

%term


  SEMI | APP | SELECT |
  SEMIEQ | COLON | COMMA | DCOLON |
  FATARROW | DOT | BAR | 
  ADD | SUB | MUL | DIV | REM | 
  ADDW | SUBW | MULW | DIVSW | DIVUW | REMSW | REMUW | 
  ADDF | SUBF | MULF | DIVF | 
  EQUAL | 
  ALLOC_MEM | SIZE | SLICE | SET | GET |
  ALLOC_CHAN | SEND | RECV | 
  WRAP | CHSE | SYNC | SPAWN | 
  LPAREN | RPAREN | LANG | RANG | LODASH | 
  SYM | INFIXL | INFIXR |

  NUM of string | WORD of string | FLOAT of string |
  STRING of string | HASHSTRING of string | ID of string |

  BAD | EOF



%nonterm
  tree_nt of term |
  term_nt of term |
  terms_nt of term list |
  fields_nt of (string * string * term) list |
  field_nt of (string * string * term) |
  lams_nt of (term * term) list |
  lams_ext_nt of (term * term) list
  

%pos int

%eop EOF
%noshift EOF

%right SEMI 
%right BAR
%right SEMIEQ FATARROW
%right COMMA
%right SQ CURL DIAM

%left DOT
%left APP 

%right SYM 

%left ALLOC_CHAN SEND RECV WRAP CHSE SPAWN SYNC ADD SUB MUL DIV REM SELECT

%left LPAREN LANG
%right RPAREN RANG

%left LODASH

%left NUM STRING ID

%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)


term_nt:

  term_nt COMMA term_nt (LstCns (term_nt1, term_nt2, COMMAleft)) |
  SQ (LstNil (SQleft)) |

  lams_nt (Fnc (lams_nt, [], [], lams_ntleft)) |

  term_nt term_nt %prec APP (App (term_nt1, term_nt2, term_nt1left)) |
  term_nt SEMI term_nt (Seq (term_nt1, term_nt2, SEMIleft)) |

  fields_nt (Rec (fields_nt, fields_ntleft)) |

(* term_nt DOT ID (Select (term_nt, ID, DOTleft)) | *)
  SELECT (Fnc ([(Id "_param", Select (Id "_param", SELECTleft))], [], [], SELECTleft)) |
  term_nt ID term_nt (Infix (ID, term_nt1, term_nt2, IDleft)) |

  ALLOC_CHAN (Fnc ([(Id "_param", AllocChan (Id "_param", ALLOC_CHANleft))], [], [], ALLOC_CHANleft)) |

  SEND (Fnc ([(Id "_param", Send (Id "_param", SENDleft))], [], [], SENDleft)) |
  RECV (Fnc ([(Id "_param", Recv (Id "_param", RECVleft))], [], [], RECVleft)) |

  WRAP (Fnc ([(Id "_param", Wrap (Id "_param", WRAPleft))], [], [], WRAPleft)) |
  CHSE (Fnc ([(Id "_param", Chse (Id "_param", CHSEleft))], [], [], CHSEleft)) |

  SYNC (Fnc ([(Id "_param", Sync (Id "_param", SYNCleft))], [], [], SYNCleft)) |

  SPAWN (Fnc ([(Id "_param", Spawn (Id "_param", SPAWNleft))], [], [], SPAWNleft)) |

  LANG term_nt RANG (Par (term_nt, LANGleft)) |

  SYM term_nt (Fnc ([(CatchAll, term_nt)], [], [], SYMleft)) | 

  LODASH (CatchAll LODASHleft) | 

  ID (Id (ID, IDleft)) |

  NUM (Num (NUM, NUMleft)) |

  ADD (Fnc ([(Id "_param", Add (Id "_param", ADDleft))], [], [], ADDleft)) |
  SUB (Fnc ([(Id "_param", Sub (Id "_param", SUBleft))], [], [], SUBleft)) |
  MUL (Fnc ([(Id "_param", Mul (Id "_param", MULleft))], [], [], MULleft)) |
  DIV (Fnc ([(Id "_param", Div (Id "_param", DIVleft))], [], [], DIVleft)) |
  REM (Fnc ([(Id "_param", Rem (Id "_param", REMleft))], [], [], REMleft)) |



  STRING (Str (STRING, STRINGleft)) |

  LPAREN term_nt RPAREN (term_nt)

lams_nt:
  term_nt FATARROW term_nt lams_nt BAR ((term_nt1, term_nt2) :: lams_nt) |
  DIAM ([])
  
terms_nt:
  term_nt terms_nt %prec LSQ (term_nt :: terms_nt) |
  term_nt %prec LSQ ([term_nt])

fields_nt:
  field_nt COMMA fields_nt (field_nt :: fields_nt) |
  CUR ([])

field_nt:
  INFIXL ID COLON term_nt ((InfixLeft, ID, term_nt)) |
  INFIXR ID COLON term_nt ((InfixRight, ID, term_nt)) |
  ID COLON term_nt ((InfixNone, ID, term_nt))

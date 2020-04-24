open Tree

%%

%name Spec 

%term


  SEMI |
  SEMIEQ | COLON | COMMA |
  DOT | BAR | 
  CROSS | DASH | STAR | SLASH | CIRSLASH | 
  ADDW | SUBW | MULW | DIVSW | DIVUW | REMSW | REMUW | 
  ADDF | SUBF | MULF | DIVF | 
  EQUAL | 
  ALLOC_MEM | SIZE | SLICE | SET | GET
  ALLOC_CHAN | SEND | RECV | 
  WRAP | CHSE | SYNC | SPAWN | 
  LSQ | RSQ | LCUR | RCUR | LPAREN | RPAREN | LANG | RANG | LODASH | 
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


%left DOT
%left APP 


%nonassoc ALLOC_CHAN SEND RECV WRAP CHSE SPAWN SYNC TILDE

%nonassoc LSQ RSQ LCUR RCUR LPAREN RPAREN LANG RANG

%nonassoc SYM 

%nonassoc LODASH

%nonassoc NUM STRING ID

%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)


term_nt:
  term_nt SEMI term_nt (Seq (term_nt1, term_nt2, SEMIleft)) |
  term_nt term_nt %prec APP (App (term_nt1, term_nt2, term_nt1left)) |
  term_nt COMMA term_nt (Cns (term_nt1, term_nt2, COMMAleft)) |
  term_nt ID term_nt (Infix (ID, term_nt1, term_nt2, IDleft)) |

  term_nt DOT ID (Select (term_nt, ID, DOTleft) |
  SELECT (Fnc ([(Id "_param", Select (Id "_param", SELECTleft))], [], [], SELECTleft)) |
  ADD (Fnc ([(Id "_param", Add (Id "_param", ADDleft))], [], [], ADDleft)) |
  SUB (Fnc ([(Id "_param", Sub (Id "_param", SUBleft))], [], [], SUBleft)) |
  MUL (Fnc ([(Id "_param", Mul (Id "_param", MULleft))], [], [], MULleft)) |
  DIV (Fnc ([(Id "_param", Div (Id "_param", DIVleft))], [], [], DIVleft)) |
  REMV (Fnc ([(Id "_param", Rem (Id "_param", REMVleft))], [], [], REMVleft)) |

  ALLOC_CHAN (Fnc ([(Id "_param", AllocChan (Id "_param", ALLOC_CHANleft))], [], [], ALLOC_CHANleft)) |

  SEND (Fnc ([(Id "_param", Send (Id "_param", SENDleft))], [], [], SENDleft)) |
  RECV (Fnc ([(Id "_param", Recv (Id "_param", RECVleft))], [], [], RECVleft)) |

  WRAP (Fnc ([(Id "_param", Wrap (Id "_param", WRAPleft))], [], [], WRAPleft)) |
  CHSE (Fnc ([(Id "_param", Chse (Id "_param", CHSEleft))], [], [], CHSEleft)) |

  SYNC (Fnc ([(Id "_param", Sync (Id "_param", SYNCleft))], [], [], SYNCleft)) |

  SPAWN (Fnc ([(Id "_param", Spawn (Id "_param", SPAWNleft))], [], [], SPAWNleft)) |

  term_nt FATARROW term_nt (Fnc ([(term_nt1, term_nt2)], [], [], FATARROWleft)) |
  lams_nt (Fnc (lams_nt, [], [], lams_ntleft)) |
  LSQ terms_nt RSQ (Lst (terms_nt, LSQleft)) |
  LSQ RSQ (Lst ([], LSQleft)) |
  LCUR fields_nt RCUR (Rec (fields_nt, LCURleft)) |
  LCUR RCUR (Rec ([], LCURleft)) |
  LANG term_nt RANG (Par (term_nt, LANGleft)) |

  SYM term_nt (Fnc ([(CatchAll, term_nt)], [], [], SYMleft)) | 

  LODASH (CatchAll LODASHleft) | 

  ID (Id (ID, IDleft)) |
  NUM (Num (NUM, NUMleft)) |
  STRING (Str (STRING, STRINGleft)) |

  LPAREN term_nt RPAREN (term_nt)

lams_nt:
  LPAREN term_nt DOT term_nt lams_ext_nt ((term_nt1, term_nt2) :: lams_ext_nt)

lams_ext_nt:
  BAR term_nt DOT term_nt lams_ext_nt ((term_nt1, term_nt2) :: lams_ext_nt) |
  RPAREN ([])
  
terms_nt:
  term_nt terms_nt (term_nt :: terms_nt) |
  term_nt ([term_nt])

fields_nt:
  field_nt fields_nt (field_nt :: fields_nt) |
  field_nt ([field_nt])

field_nt:
  INFIX ID COLON term_nt ((INFIX, ID, term_nt)) |
  INFIXL ID COLON term_nt ((INFIXL, ID, term_nt)) |
  INFIXR ID COLON term_nt ((INFIXR, ID, term_nt)) |
  ID COLON term_nt (("", ID, term_nt)) |

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
  fields_nt of (string * term) list |
  field_nt of (string * term) |
  lams_nt of (term * term) list |
  lams_ext_nt of (term * term) list
  

%pos int

%eop EOF
%noshift EOF

%right SEMI 
%right BAR
%right SEMIEQ DOT
%right COMMA

%left CROSS DASH 
%left STAR SLASH CIRSLASH 
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

  SELECT (Fnc ([(ID "_param", Select (ID "_param", SELECTleft))], [], [], SELECTleft)) |
  ADD (Fnc ([(ID "_param", Add (ID "_param", ADDleft))], [], [], ADDleft)) |
  SUB (Fnc ([(ID "_param", Sub (ID "_param", SUBleft))], [], [], SUBleft)) |
  MUL (Fnc ([(ID "_param", Mul (ID "_param", MULleft))], [], [], MULleft)) |
  DIV (Fnc ([(ID "_param", Div (ID "_param", DIVleft))], [], [], DIVleft)) |
  REMV (Fnc ([(ID "_param", Rem (ID "_param", REMVleft))], [], [], REMVleft)) |

  ALLOC_CHAN (Fnc ([(ID "_param", AllocChan (ID "_param", ALLOC_CHANleft))], [], [], ALLOC_CHANleft)) |

  SEND (Fnc ([(ID "_param", Send (ID "_param", SENDleft))], [], [], SENDleft)) |
  RECV (Fnc ([(ID "_param", Recv (ID "_param", RECVleft))], [], [], RECVleft)) |

  WRAP (Fnc ([(ID "_param", Wrap (ID "_param", WRAPleft))], [], [], WRAPleft)) |
  CHSE (Fnc ([(ID "_param", Chse (ID "_param", CHSEleft))], [], [], CHSEleft)) |

  SYNC (Fnc ([(ID "_param", Sync (ID "_param", SYNCleft))], [], [], SYNCleft)) |

  SPAWN (Fnc ([(ID "_param", Spawn (ID "_param", SPAWNleft))], [], [], SPAWNleft)) |

  term_nt DOT term_nt (Fnc ([(term_nt1, term_nt2)], [], [], DOTleft)) |
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
  ID COLON term_nt ((ID, term_nt))

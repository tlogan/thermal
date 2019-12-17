open Tree

%%

%name Spec 

%term
  SEMICOLON | COLON |
  COMMA | DOT |
  BSLASH | BAR | ARROW | 
  DCOLON |
  EQ | WEDGE | VEE |
  LONGARROW | DARROW |

  CROSS | DASH | STAR | SLASH | CIRSLASH |

  APP |

  ALLOC_CHAN |
  SEND | RECV | WRAP |
  CHSE | SPAWN | SYNC |

  TILDE |

  REDUCED | CALLED | RETURNED |
  SPAWNED | BLOCKED | SYNCED |
  STUCK | DONE |
  SOLVE | SAT |

  LSQ | RSQ | LCUR | RCUR | LPAREN | RPAREN |

  LODASH |
  THAT |
  TRUE | FALSE | 
  NUMLIT of string | STRINGLIT of string |
  ID of string |

  EOF | BAD

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



%right BAR
%right ARROW DOT
%right COMMA
%right SEMICOLON
%left LONGARROW DARROW
%left VEE 
%left WEDGE
%nonassoc EQ
%left BSLASH
%left APP
%right DCOLON 

%left CROSS DASH 
%left STAR SLASH CIRSLASH 

%nonassoc ALLOC_CHAN SEND RECV WRAP CHSE SPAWN SYNC TILDE REDUCED CALLED RETURNED SPAWNED BLOCKED SYNCED STUCK DONE SOLVE SAT

%nonassoc LSQ RSQ LCUR RCUR LPAREN RPAREN 

%nonassoc LODASH THAT TRUE FALSE 

%nonassoc NUMLIT STRINGLIT ID


%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)


term_nt:
  term_nt SEMICOLON term_nt (Seq (term_nt1, term_nt2, SEMICOLONleft)) |
  term_nt BSLASH ID (Select (term_nt, ID, BSLASHleft)) |
  term_nt ARROW term_nt (Pipe (term_nt1, term_nt2, ARROWleft)) |
  term_nt DCOLON term_nt (Cns (term_nt1, term_nt2, DCOLONleft)) |
  term_nt DARROW term_nt (Equiv (term_nt1, term_nt2, DARROWleft)) |
  term_nt LONGARROW term_nt (Implies (term_nt1, term_nt2, LONGARROWleft)) |
  term_nt VEE term_nt (Or (term_nt1, term_nt2, VEEleft)) |
  term_nt WEDGE term_nt (And (term_nt1, term_nt2, WEDGEleft)) |
  term_nt EQ term_nt (Equal (term_nt1, term_nt2, EQleft)) |

  term_nt CROSS term_nt (Add (term_nt1, term_nt2, CROSSleft)) |
  term_nt DASH term_nt (Sub (term_nt1, term_nt2, DASHleft)) |
  term_nt STAR term_nt (Mult (term_nt1, term_nt2, STARleft)) |
  term_nt SLASH term_nt (Div (term_nt1, term_nt2, SLASHleft)) |
  term_nt CIRSLASH term_nt (Mod (term_nt1, term_nt2, CIRSLASHleft)) |

  ALLOC_CHAN (AllocChan ALLOC_CHANleft) |
  SEND term_nt (Send (term_nt, SENDleft)) |
  RECV term_nt (Recv (term_nt, RECVleft)) |
  WRAP term_nt (Wrap (term_nt, WRAPleft)) |
  CHSE term_nt (Chse (term_nt, CHSEleft)) |
  SPAWN term_nt (Spawn (term_nt, SPAWNleft)) |
  SYNC term_nt (Spawn (term_nt, SYNCleft)) |
  SOLVE term_nt (Solve (term_nt, SOLVEleft)) |
  SAT term_nt (Sat (term_nt, SATleft)) |

  TILDE term_nt (Not (term_nt, TILDEleft)) |

  REDUCED term_nt (Reduced (term_nt, REDUCEDleft)) |
  BLOCKED term_nt (Blocked (term_nt, BLOCKEDleft)) |
  SYNCED term_nt (Synced (term_nt, SYNCEDleft)) |
  STUCK term_nt (Stuck (term_nt, STUCKleft)) |
  DONE term_nt (Done (term_nt, DONEleft)) |
  term_nt term_nt %prec APP (App (term_nt1, term_nt2, term_nt1left)) |
  term_nt DOT term_nt (Fnc ([(term_nt1, term_nt2)], (fn id => NONE), [], DOTleft)) |
  lams_nt (Fnc (lams_nt, (fn id => NONE), [], lams_ntleft)) |
  LSQ terms_nt RSQ (Lst (terms_nt, LSQleft)) |
  LSQ RSQ (Lst ([], LSQleft)) |
  LCUR fields_nt RCUR (Rec (fields_nt, LCURleft)) |
  LCUR RCUR (Rec ([], LCURleft)) |

  LODASH (CatchAll LODASHleft) | 
  THAT (That THATleft) | 
  TRUE (Bool (true, TRUEleft)) | 
  FALSE (Bool (false, FALSEleft)) |

  ID (Id (ID, IDleft)) |
  NUMLIT (Num (NUMLIT, NUMLITleft)) |
  STRINGLIT (Str (STRINGLIT, STRINGLITleft)) |

  LPAREN term_nt RPAREN (term_nt)

lams_nt:
  LPAREN term_nt DOT term_nt lams_ext_nt ((term_nt1, term_nt2) :: lams_ext_nt)

lams_ext_nt:
  BAR term_nt DOT term_nt lams_ext_nt ((term_nt1, term_nt2) :: lams_ext_nt) |
  RPAREN ([])
  
terms_nt:
  term_nt COMMA terms_nt (term_nt :: terms_nt) |
  term_nt ([term_nt])

fields_nt:
  field_nt COMMA fields_nt (field_nt :: fields_nt) |
  field_nt ([field_nt])

field_nt:
  ID COLON term_nt ((ID, term_nt))

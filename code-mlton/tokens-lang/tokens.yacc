open Tree

%%

%name Spec 

%term
  SEMICOLON | LARROW |
  COMMA | DOT |
  BSLASH | BAR | ARROW | 
  HASH | DCOLON | COLON | 
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
  FOR | SOLVE | 

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
  lams_ext_nt of (term * term) list |
  ids_nt of string list
  

%pos int

%eop EOF
%noshift EOF



%right BAR
%right ARROW DOT
%right COMMA
%right SEMICOLON
%nonassoc HASH
%left LONGARROW DARROW
%left VEE 
%left WEDGE
%nonassoc EQ COLON
%left BSLASH
%left APP
%right DCOLON 

%left CROSS DASH 
%left STAR SLASH CIRSLASH 

%nonassoc ALLOC_CHAN SEND RECV WRAP CHSE SPAWN SYNC TILDE REDUCED CALLED RETURNED SPAWNED BLOCKED SYNCED STUCK DONE FOR SOLVE 

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
  term_nt BSLASH ID (Selec (term_nt, ID, BSLASHleft)) |
  term_nt ARROW term_nt (Pipe (term_nt1, term_nt2, ARROWleft)) |
  term_nt HASH term_nt (Pred (term_nt1, term_nt2, HASHleft)) |
  term_nt DCOLON term_nt (Cns (term_nt1, term_nt2, DCOLONleft)) |
  term_nt COLON term_nt (Rep (term_nt1, term_nt2, COLONleft)) |
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

  TILDE term_nt (Not (term_nt, TILDEleft)) |

  REDUCED term_nt (Reduced (term_nt, REDUCEDleft)) |
  BLOCKED term_nt (Blocked (term_nt, BLOCKEDleft)) |
  SYNCED term_nt (Synced (term_nt, SYNCEDleft)) |
  STUCK term_nt (Stuck (term_nt, STUCKleft)) |
  DONE term_nt (Done (term_nt, DONEleft)) |
  FOR ids_nt BSLASH term_nt (AbsProp (ids_nt, term_nt, FORleft)) |
  term_nt term_nt %prec APP (App (term_nt1, term_nt2, term_nt1left)) |
  term_nt DOT term_nt (Fnc ([(term_nt1, term_nt2)], DOTleft)) |
  lams_nt (Fnc (lams_nt, lams_ntleft)) |
  LSQ terms_nt RSQ (Lst (terms_nt, LSQleft)) |
  LSQ RSQ (Lst ([], LSQleft)) |
  LCUR fields_nt RCUR (Rec (fields_nt, LCURleft)) |
  LCUR RCUR (Rec ([], LCURleft)) |

  LODASH (CatchAll LODASHleft) | 
  THAT (That THATleft) | 
  TRUE (BoolLit (true, TRUEleft)) | 
  FALSE (BoolLit (false, FALSEleft)) |

  ID (Id (ID, IDleft)) |
  NUMLIT (NumLit (NUMLIT, NUMLITleft)) |
  STRINGLIT (StringLit (STRINGLIT, STRINGLITleft)) |

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
  ID LARROW term_nt ((ID, term_nt))

ids_nt:
  ID ids_nt (ID :: ids_nt) |
  ID ([ID])

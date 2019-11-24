open Tree

%%

%name Spec 

%term
  SEMICOLON | LARROW |
  COMMA | DOT | 
  FATARROW | BAR | BARARROW |
  HASH | DCOLON | COLON | 
  EQ | WEDGE | VEE |
  LONGARROW | DARROW |

  SEND | RECV | WRAP |
  CHSE | SPAWN | SYNC |

  TILDE |

  REDUCED | CALLED | RETURNED |
  SPAWNED | BLOCKED | SYNCED |
  STUCK | DONE |
  FOR | SOLVE | 

  LSQ | RSQ | LCUR | RCUR | LPAREN | RPAREN |

  THIS |
  TRUE | FALSE | 
  NUMLIT of string | STRINGLIT of string |
  ID of string |
  EOF | BAD

%nonterm
  tree_nt of term |
  term_nt of term |
  base_nt of term |
  terms_nt of term list |
  fields_nt of (string * term) list |
  field_nt of (string * term) |
  lams_nt of (term * term) list |
  lam_nt of (term * term)
  

%pos int

%eop EOF
%noshift EOF

%right BAR
%right FATARROW
%right COMMA
%nonassoc SEMICOLON LARROW DOT BARARROW HASH DCOLON COLON EQ WEDGE VEE LONGARROW DARROW

%nonassoc SEND RECV WRAP CHSE SPAWN SYNC TILDE REDUCED CALLED RETURNED SPAWNED BLOCKED SYNCED STUCK DONE FOR SOLVE 

%nonassoc LSQ RSQ LCUR RCUR LPAREN RPAREN 

%nonassoc THIS TRUE FALSE 

%nonassoc NUMLIT STRINGLIT ID


%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)

base_nt:
  LPAREN LPAREN (Unt) |
  LPAREN lams_nt RPAREN (Fnc (lams_nt)) |
  LSQ terms_nt RSQ (Lst terms_nt) |
  LCUR fields_nt RCUR (Rec fields_nt) |
  ID (Id ID) |
  TRUE (BoolLit true) | 
  FALSE (BoolLit false) |
  LPAREN term_nt RPAREN (term_nt)

term_nt:
  term_nt SEMICOLON term_nt (Seq (term_nt1, term_nt2)) |
  term_nt LARROW term_nt (Def (term_nt1, term_nt2)) |
  term_nt DOT ID (Selec (term_nt, ID)) |
  term_nt BARARROW term_nt (Pipe (term_nt1, term_nt2)) |
  term_nt HASH term_nt (Pred (term_nt1, term_nt2)) |
  term_nt DCOLON term_nt (Cons (term_nt1, term_nt2)) |
  term_nt COLON term_nt (Rep (term_nt1, term_nt2)) |
  term_nt EQ term_nt (Rep (term_nt1, term_nt2)) |
  term_nt WEDGE term_nt (And (term_nt1, term_nt2)) |
  term_nt VEE term_nt (Or (term_nt1, term_nt2)) |
  term_nt LONGARROW term_nt (Implies (term_nt1, term_nt2)) |
  term_nt DARROW term_nt (Equiv (term_nt1, term_nt2)) |

  SEND term_nt (Send term_nt) |
  RECV term_nt (Recv term_nt) |
  WRAP term_nt (Wrap term_nt) |
  CHSE term_nt (Chse term_nt) |
  SPAWN term_nt (Spawn term_nt) |
  BLOCKED term_nt (Blocked term_nt) |
  SYNCED term_nt (Synced term_nt) |
  STUCK term_nt (Stuck term_nt) |
  DONE term_nt (Done term_nt) |
  FOR term_nt (Done term_nt) |
  base_nt base_nt (App (term_nt1, term_nt2)) |
  base_nt (base_nt)


lams_nt:
  lam_nt BAR lams_nt (lam_nt :: lams_nt) | 
  lam_nt ([lam_nt]) 
  
lam_nt:
  term_nt FATARROW term_nt ((term_nt1, term_nt2))

terms_nt:
  term_nt COMMA terms_nt (term_nt :: terms_nt) |
  ([])
  
fields_nt:
  field_nt COMMA fields_nt (field_nt :: fields_nt) |
  ([])

field_nt:
  ID LARROW term_nt ((ID, term_nt))

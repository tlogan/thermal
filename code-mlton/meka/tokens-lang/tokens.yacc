open Tree

%%

%name Spec 

%term
  SEMICOLON | LARROW |
  COMMA | DOT | 
  FATARROW | BAR | BARARROW |
  HASH | COLON | 
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

  TRUE | FALSE | 
  NUMLIT of string | STRINGLIT of string |
  ID of string |
  EOF | BAD

%nonterm
  tree_nt of term |
  term_nt of term 
  

%pos int


%eop EOF
%noshift EOF

%nonassoc SEMICOLON LARROW COMMA DOT FATARROW BAR BARARROW HASH COLON EQ WEDGE VEE LONGARROW DARROW

%nonassoc SEND RECV WRAP CHSE SPAWN SYNC TILDE REDUCED CALLED RETURNED SPAWNED BLOCKED SYNCED STUCK DONE FOR SOLVE 

%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)

term_nt:
(*
  form_nt LONGARROW form_nt (Imp (form_nt1, form_nt2)) |
  form_nt DARROW form_nt (Equiv (form_nt1, form_nt2)) |
  form_nt VEE form_nt (Or (form_nt1, form_nt2)) |
  form_nt DPLUS form_nt (Xor (form_nt1, form_nt2)) |
  form_nt WEDGE form_nt (And (form_nt1, form_nt2)) |
  form_nt SINCE form_nt (Since (form_nt1, form_nt2)) |

  ALWAYS form_nt (Always form_nt) |
  ONCE form_nt (Once form_nt) |
  PREV form_nt (Prev form_nt) |
  START form_nt (Start form_nt) |
  END form_nt (End form_nt) |
  TILDE form_nt (Not form_nt) |

  LPAREN form_nt RPAREN (form_nt)
 *)
  ID (Id ID) |
  TRUE (BoolLit true) | 
  FALSE (BoolLit false)
  
  

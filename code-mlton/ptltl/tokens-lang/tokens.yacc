open Tree

%%

%name Spec 

%term
  TRUE | FALSE | 
  LONGARROW | DARROW | VEE | DPLUS | WEDGE | SINCE |
  ALWAYS | ONCE | PREV | START | END | TILDE |
  LPAREN | RPAREN |
  ID of string |
  EOF | BAD

%nonterm
  tree_nt of formula |
  form_nt of formula
  

%pos int


%eop EOF
%noshift EOF

%nonassoc LONGARROW DARROW VEE DPLUS WEDGE SINCE 
%nonassoc ALWAYS ONCE PREV START END TILDE 

%start tree_nt

%verbose

%%

tree_nt:
  form_nt (form_nt)

form_nt:
  ID (Id ID) |
  TRUE (Prim true) | 
  FALSE (Prim false) | 

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
  
  

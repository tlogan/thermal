open Tree

%%

%name Spec 

%term


  SEMI | COMPO | SELECT |
  COLON |
  DOT |
  COMMA | BSLASH | LSQ | RSQ | BAR | HASH |
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
  STRING of string | ID of string |

  BAD | EOF



%nonterm
  tree_nt of term |
  term_nt of term |
  terms_nt of term list |
  fields_nt of (infix_option * string * term) list |
  field_nt of (infix_option * string * term) |
  lams_nt of (term * term) list |
  lam_nt of (term * term)
  

%pos int

%eop EOF
%noshift EOF

%right SEMI 
%right COLON
%right BAR HASH COMMA
%left COMPO 
%right DOT


%right SYM 

%left ALLOC_CHAN SEND RECV WRAP CHSE SPAWN SYNC ADD SUB MUL DIV REM SELECT EQUAL
%nonassoc INFIXL INFIXR

%left LPAREN LANG
%right RPAREN RANG

%left LODASH

%left NUM STRING ID

%left BSLASH
%left LSQ 
%left RSQ 

%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)


term_nt:

  term_nt DOT term_nt (Cns (term_nt1, term_nt2, DOTleft)) |
  term_nt DOT (Cns (term_nt1, Blank ~1, DOTleft)) |

  LPAREN lams_nt (Fnc (lams_nt, [], [], lams_ntleft)) |
  lam_nt (Fnc ([lam_nt], [], [], lam_ntleft)) |

  term_nt term_nt %prec COMPO (Compo (term_nt1, term_nt2, term_nt1left)) |
  term_nt SEMI term_nt (Seq (term_nt1, term_nt2, SEMIleft)) |

  LPAREN fields_nt (Rec (fields_nt, fields_ntleft)) |

  term_nt LSQ term_nt RSQ (Select (Lst ([term_nt1, term_nt2], ~1), LSQleft)) |
  term_nt BSLASH ID (Select (Lst ([term_nt, Str (ID, ~1)], ~1), BSLASHleft)) |
  SELECT (Fnc ([(Id ("_param", ~1), Select (Id ("_param", ~1), SELECTleft))], [], [], SELECTleft)) |

  EQUAL (Fnc ([(Id ("_param", ~1), Select (Id ("_param", ~1), EQUALleft))], [], [], EQUALleft)) |

  ALLOC_CHAN (Fnc ([(Id ("_param", ~1), AllocChan (Id ("_param", ~1), ALLOC_CHANleft))], [], [], ALLOC_CHANleft)) |

  SEND (Fnc ([(Id ("_param", ~1), Send (Id ("_param", ~1), SENDleft))], [], [], SENDleft)) |
  RECV (Fnc ([(Id ("_param", ~1), Recv (Id ("_param", ~1), RECVleft))], [], [], RECVleft)) |

  WRAP (Fnc ([(Id ("_param", ~1), Wrap (Id ("_param", ~1), WRAPleft))], [], [], WRAPleft)) |
  CHSE (Fnc ([(Id ("_param", ~1), Chse (Id ("_param", ~1), CHSEleft))], [], [], CHSEleft)) |

  SYNC (Fnc ([(Id ("_param", ~1), Sync (Id ("_param", ~1), SYNCleft))], [], [], SYNCleft)) |

  SPAWN (Fnc ([(Id ("_param", ~1), Spawn (Id ("_param", ~1), SPAWNleft))], [], [], SPAWNleft)) |

  LANG term_nt RANG (Par (term_nt, LANGleft)) |

  SYM term_nt (Sym (term_nt, SYMleft)) | 

  LODASH (Blank LODASHleft) | 

  ID (Id (ID, IDleft)) |

  NUM (Num (NUM, NUMleft)) |

  ADD (Fnc ([(Id ("_param", ~1), Add (Id ("_param", ~1), ADDleft))], [], [], ADDleft)) |
  SUB (Fnc ([(Id ("_param", ~1), Sub (Id ("_param", ~1), SUBleft))], [], [], SUBleft)) |
  MUL (Fnc ([(Id ("_param", ~1), Mul (Id ("_param", ~1), MULleft))], [], [], MULleft)) |
  DIV (Fnc ([(Id ("_param", ~1), Div (Id ("_param", ~1), DIVleft))], [], [], DIVleft)) |
  REM (Fnc ([(Id ("_param", ~1), Rem (Id ("_param", ~1), REMleft))], [], [], REMleft)) |



  STRING (Str (STRING, STRINGleft)) |

  LPAREN term_nt RPAREN (term_nt)

lams_nt:
  BAR lam_nt lams_nt (lam_nt :: lams_nt) |
  BAR lam_nt RPAREN ([lam_nt])

lam_nt:
  term_nt COMMA term_nt ((term_nt1, term_nt2))

fields_nt:
  COLON field_nt fields_nt %prec COLON (field_nt :: fields_nt) |
  COLON field_nt RPAREN %prec COLON ([field_nt])

field_nt:
  INFIXL ID term_nt ((InfixLeft, ID, term_nt)) |
  INFIXR ID term_nt ((InfixRight, ID, term_nt)) |
  ID term_nt ((InfixNone, ID, term_nt))

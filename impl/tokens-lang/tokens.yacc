open Tree

%%

%name Spec 

%term

  WITH | PIPE | COMPO | SELECT |
  DEF |
  HASH |
  FATARROW | COLON | DOT | LSQ | RSQ | CASE |
  LOG | SYM |
  ADD | SUB | MUL | DIV |
  ADDW | SUBW | MULW | DIVSW | DIVUW | REMSW | REMUW | 
  ADDF | SUBF | MULF | DIVF | 
  EQUAL | 
  ALLOC_MEM | SIZE | SLICE | SET | GET |
  ALLOC_CHAN | SEND | RECV | 
  LATCH | CHSE | SYNC | SPAWN | 
  LPAREN | RPAREN | LANG | RANG | LRPAREN | 
  INFIXL | INFIXR | DIGIT of int |

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

%right HASH WITH PIPE

%right LOG SYM

%right FATARROW CASE COLON DEF

%left ALLOC_CHAN SEND RECV LATCH CHSE SPAWN SYNC ADD SUB MUL DIV SELECT EQUAL
%nonassoc INFIXL INFIXR DIGIT

%left LPAREN LANG
%right RPAREN RANG

%left LRPAREN

%left NUM STRING ID

%left DOT
%left LSQ 
%left RSQ 

%left COMPO




%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)


term_nt:

  LPAREN term_nt RPAREN (Assoc (term_nt, LPARENleft)) |

  HASH term_nt term_nt (List_Intro (term_nt1, term_nt2, HASHleft)) |
  HASH term_nt (List_Intro (term_nt, Blank HASHright, HASHleft)) |

  lam_nt (Func_Intro ([lam_nt], lam_ntleft)) |
  LPAREN lams_nt (Func_Intro (lams_nt, lams_ntleft)) |

  WITH term_nt term_nt (With (term_nt1, term_nt2, WITHleft)) |
  WITH term_nt (With (term_nt1, Blank WITHright, WITHleft)) |

  PIPE term_nt term_nt (App (term_nt2, term_nt1, PIPEleft)) |

  LOG term_nt (Log (term_nt, LOGleft)) |
  SYM term_nt (Sym (term_nt, SYMleft)) | 

  field_nt (Rec_Intro ([field_nt], field_ntleft)) |
  LPAREN fields_nt (Rec_Intro (fields_nt, LPARENleft)) |

  term_nt LSQ term_nt RSQ
    (Select (List_Intro (
      term_nt1,
      List_Intro (term_nt2, Blank RSQright, RSQleft),
      LSQleft
    ), LSQleft)) |

  term_nt DOT ID
    (Select (List_Intro (
      term_nt,
      List_Intro (String_Val (ID, IDleft), Blank IDright, IDleft),
      DOTleft
  ), DOTleft)) |

  SELECT (Func_Intro ([(Id ("_param", ~1), Select (Id ("_param", SELECTleft), SELECTleft))], SELECTleft)) |

  EQUAL (Func_Intro ([(Id ("_param", ~1), Select (Id ("_param", EQUALleft), EQUALleft))], EQUALleft)) |

  ALLOC_CHAN (Func_Intro ([(Id ("_param", ~1), Event_Intro (Alloc_Chan, Id ("_param", ~1), ALLOC_CHANleft))], ALLOC_CHANleft)) |

  SEND (Func_Intro ([(Id ("_param", ~1), Event_Intro (Send, Id ("_param", ~1), SENDleft))], SENDleft)) |

  RECV (Func_Intro ([(Id ("_param", ~1), Event_Intro (Recv, Id ("_param", ~1), RECVleft))], RECVleft)) |

  LATCH (Func_Intro ([(Id ("_param", ~1), Event_Intro (Latch, Id ("_param", ~1), LATCHleft))], LATCHleft)) |

  CHSE (Func_Intro ([(Id ("_param", ~1), Event_Intro (Choose, Id ("_param", ~1), CHSEleft))], CHSEleft)) |

  SYNC (Func_Intro ([(Id ("_param", ~1), Effect_Intro (Sync, Id ("_param", ~1), SYNCleft))], SYNCleft)) |

  SPAWN (Func_Intro ([(Id ("_param", ~1), Effect_Intro (Spawn, Id ("_param", ~1), SPAWNleft))], SPAWNleft)) |

  LANG term_nt RANG (Effect_Intro (Par, term_nt, LANGleft)) |

  LRPAREN (Blank LRPARENleft) | 

  ID (Id (ID, IDleft)) |

  NUM (Num_Val (NUM, NUMleft)) |

  ADD (Func_Intro ([(Id ("_param", ~1), Num_Add (Id ("_param", ~1), ADDleft))], ADDleft)) |

  SUB (Func_Intro ([(Id ("_param", ~1), Num_Sub (Id ("_param", ~1), SUBleft))], SUBleft)) |

  MUL (Func_Intro ([(Id ("_param", ~1), Num_Mul (Id ("_param", ~1), MULleft))], MULleft)) |

  DIV (Func_Intro ([(Id ("_param", ~1), Num_Div (Id ("_param", ~1), DIVleft))], DIVleft)) |

  STRING (String_Val (STRING, STRINGleft)) |

  term_nt term_nt %prec COMPO (Compo (term_nt1, term_nt2, term_nt1left))

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

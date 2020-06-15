open Tree

%%

%name Spec 

%term

  SEMI | COMPO | SELECT |
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
  WRAP | CHSE | SYNC | SPAWN | 
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


%right SEMI
%right FATARROW CASE COLON DEF

%right HASH


%left ALLOC_CHAN SEND RECV WRAP CHSE SPAWN SYNC ADD SUB MUL DIV SELECT EQUAL
%nonassoc INFIXL INFIXR DIGIT

%left LPAREN LANG
%right RPAREN RANG

%left LRPAREN

%left NUM STRING ID

%left DOT
%left LSQ 
%left RSQ 

%right LOG SYM

%left COMPO


%start tree_nt

%verbose

%%

tree_nt:
  term_nt (term_nt)


term_nt:

  LPAREN term_nt RPAREN (Assoc (term_nt, LPARENleft)) |
  LOG term_nt (Log (term_nt, LOGleft)) |
  SYM term_nt (Sym (term_nt, SYMleft)) | 

  HASH term_nt term_nt (List_Intro (term_nt1, term_nt2, HASHleft)) |
  HASH term_nt (List_Intro (term_nt, Blank HASHright, HASHleft)) |

  lam_nt (Func_Intro ([lam_nt], lam_ntleft)) |
  LPAREN lams_nt (Func_Intro (lams_nt, lams_ntleft)) |

  term_nt term_nt %prec COMPO (Compo (term_nt1, term_nt2, term_nt1left)) |
  term_nt SEMI term_nt (Seq (term_nt1, term_nt2, SEMIleft)) |

  field_nt (Rec_Intro ([field_nt], field_ntleft)) |
  LPAREN fields_nt (Rec_Intro (fields_nt, LPARENleft)) |


  term_nt LSQ term_nt RSQ
    (Rec_Elim (List_Intro (
      term_nt1,
      List_Intro (term_nt2, Blank RSQright, RSQleft),
      LSQleft
  ), LSQleft)) |

  term_nt DOT ID
    (Rec_Elim (List_Intro (
      term_nt,
      List_Intro (String_Val (ID, IDleft), Blank IDright, IDleft),
      DOTleft
  ), DOTleft)) |

  SELECT (Func_Intro ([(Id ("_param", ~1), Rec_Elim (Id ("_param", SELECTleft), SELECTleft))], SELECTleft)) |

  EQUAL (Func_Intro ([(Id ("_param", ~1), Rec_Elim (Id ("_param", EQUALleft), EQUALleft))], EQUALleft)) |

  ALLOC_CHAN (Func_Intro ([(Id ("_param", ~1), Chan_Alloc (Id ("_param", ALLOC_CHANleft), ALLOC_CHANleft))], ALLOC_CHANleft)) |

  SEND (Func_Intro ([(Id ("_param", ~1), Evt_Send_Intro (Id ("_param", ~1), SENDleft))], SENDleft)) |

  RECV (Func_Intro ([(Id ("_param", ~1), Evt_Recv_Intro (Id ("_param", ~1), RECVleft))], RECVleft)) |

  WRAP (Func_Intro ([(Id ("_param", ~1), Evt_Wrap_Intro (Id ("_param", ~1), WRAPleft))], WRAPleft)) |

  CHSE (Func_Intro ([(Id ("_param", ~1), Evt_Choose_Intro (Id ("_param", ~1), CHSEleft))], CHSEleft)) |

  SYNC (Func_Intro ([(Id ("_param", ~1), Evt_Elim (Id ("_param", ~1), SYNCleft))], SYNCleft)) |

  SPAWN (Func_Intro ([(Id ("_param", ~1), Spawn (Id ("_param", ~1), SPAWNleft))], SPAWNleft)) |

  LANG term_nt RANG (Par (term_nt, LANGleft)) |

  LRPAREN (Blank LRPARENleft) | 

  ID (Id (ID, IDleft)) |

  NUM (Num_Val (NUM, NUMleft)) |

  ADD (Func_Intro ([(Id ("_param", ~1), Num_Add (Id ("_param", ~1), ADDleft))], ADDleft)) |

  SUB (Func_Intro ([(Id ("_param", ~1), Num_Sub (Id ("_param", ~1), SUBleft))], SUBleft)) |

  MUL (Func_Intro ([(Id ("_param", ~1), Num_Mul (Id ("_param", ~1), MULleft))], MULleft)) |

  DIV (Func_Intro ([(Id ("_param", ~1), Num_Div (Id ("_param", ~1), DIVleft))], DIVleft)) |

  STRING (String_Val (STRING, STRINGleft))


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

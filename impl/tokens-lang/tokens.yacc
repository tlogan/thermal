open Tree

%%

%name Spec 

%term

  WITH | PIPE | COMPO | SELECT |
  DEF |
  BAR |
  FATARROW | COLON | DOT | LSQ | RSQ | CASE |
  LOG | SYM | REFLECT |
  ADD | SUB | MUL | DIV |
  ADDW | SUBW | MULW | DIVSW | DIVUW | REMSW | REMUW | 
  ADDF | SUBF | MULF | DIVF | 
  EQUAL | 
  ALLOC_MEM | SIZE | SLICE | SET | GET |
  ALLOC_CHAN | SEND | RECV | LATCH | CHOOSE | OFFER | ABORT |

  CHANGE | COMBINE |

  SYNC | BIND | RETURN | SPAWN | 
  INSTALL_DOMINO | KNOCK |
  CHILL | REACT |
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

%right BAR WITH PIPE

%right LOG SYM REFLECT

%right FATARROW CASE COLON DEF

%left ALLOC_CHAN SEND RECV LATCH CHOOSE OFFER ABORT SPAWN INSTALL_DOMINO KNOCK SYNC ADD SUB MUL DIV SELECT EQUAL CHANGE COMBINE CHILL REACT

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

  BAR term_nt term_nt (Intro_List (term_nt1, term_nt2, BARleft)) |
  BAR term_nt (Intro_List (term_nt, Value (Blank, BARright), BARleft)) |

  lam_nt (Intro_Func ([lam_nt], lam_ntleft)) |
  LPAREN lams_nt (Intro_Func (lams_nt, lams_ntleft)) |

  WITH term_nt term_nt (With (term_nt1, term_nt2, WITHleft)) |
  WITH term_nt (With (term_nt1, Value (Blank, WITHright), WITHleft)) |

  PIPE term_nt term_nt (App (term_nt2, term_nt1, PIPEleft)) |

  LOG term_nt (Log (term_nt, LOGleft)) |
  SYM ID (Sym (ID, SYMleft)) | 
  REFLECT term_nt LPAREN lams_nt (Reflect (term_nt, lams_nt, REFLECTleft)) | 
  REFLECT term_nt lam_nt (Reflect (term_nt, [lam_nt], REFLECTleft)) | 

  field_nt (Intro_Rec ([field_nt], false, field_ntleft)) |
  LPAREN fields_nt (Intro_Rec (fields_nt, false, LPARENleft)) |

  term_nt LSQ term_nt RSQ
    (Select (Intro_List (
      term_nt1,
      Intro_List (term_nt2, Value (Blank, RSQright), RSQleft),
      LSQleft
    ), LSQleft)) |

  term_nt DOT ID
    (Select (Intro_List (
      term_nt,
      Intro_List (Value (String ID, IDleft), Value (Blank, IDright), IDleft),
      DOTleft
  ), DOTleft)) |

  SELECT (Intro_Func ([(Id ("_param", ~1), Select (Id ("_param", SELECTleft), SELECTleft))], SELECTleft)) |

  EQUAL (Intro_Func ([(Id ("_param", ~1), Select (Id ("_param", EQUALleft), EQUALleft))], EQUALleft)) |

  ALLOC_CHAN (Intro_Func ([(Id ("_param", ~1), Value (Event Alloc_Chan, ALLOC_CHANleft))], ALLOC_CHANleft)) |

  SEND (Intro_Func ([(Id ("_param", ~1), Intro_Send (Id ("_param", ~1), SENDleft))], SENDleft)) |

  RECV (Intro_Func ([(Id ("_param", ~1), Intro_Recv (Id ("_param", ~1), RECVleft))], RECVleft)) |

  LATCH (Intro_Func ([(Id ("_param", ~1), Intro_Latch (Id ("_param", ~1), LATCHleft))], LATCHleft)) |

  CHOOSE (Intro_Func ([(Id ("_param", ~1), Intro_Choose (Id ("_param", ~1), CHOOSEleft))], CHOOSEleft)) |

  OFFER (Intro_Func ([(Id ("_param", ~1), Intro_Offer (Id ("_param", ~1), OFFERleft))], OFFERleft)) |

  ABORT (Value (Event Abort, ABORTleft)) |

  SYNC (Intro_Func ([(Id ("_param", ~1), Intro_Sync (Id ("_param", ~1), SYNCleft))], SYNCleft)) |

  SPAWN (Intro_Func ([(Id ("_param", ~1), Intro_Spawn (Id ("_param", ~1), SPAWNleft))], SPAWNleft)) |

  CHANGE (Intro_Func ([(Id ("_param", ~1), Intro_Change (Id ("_param", ~1), CHANGEleft))], CHANGEleft)) |

  COMBINE (Intro_Func ([(Id ("_param", ~1), Intro_Combine (Id ("_param", ~1), COMBINEleft))], COMBINEleft)) |


  INSTALL_DOMINO (Intro_Func (
    [(Id ("_param", ~1), Intro_Install_Domino (Id ("_param", ~1), INSTALL_DOMINOleft))], INSTALL_DOMINOleft
  )) |


  KNOCK (Intro_Func (
    [(Id ("_param", ~1), Intro_Knock (Id ("_param", ~1), KNOCKleft))], KNOCKleft
  )) |


  CHILL (Intro_Func ([(Id ("_param", ~1), Intro_Chill (Id ("_param", ~1), CHILLleft))], CHILLleft)) |

  REACT (Intro_Func ([(Id ("_param", ~1), Intro_React (Id ("_param", ~1), REACTleft))], REACTleft)) |


  LRPAREN (Value (Blank, LRPARENleft)) | 

  ID (Id (ID, IDleft)) |

  NUM (Value (Num NUM, NUMleft)) |

  ADD (Intro_Func ([(Id ("_param", ~1), Add_Num (Id ("_param", ~1), ADDleft))], ADDleft)) |

  SUB (Intro_Func ([(Id ("_param", ~1), Sub_Num (Id ("_param", ~1), SUBleft))], SUBleft)) |

  MUL (Intro_Func ([(Id ("_param", ~1), Mul_Num (Id ("_param", ~1), MULleft))], MULleft)) |

  DIV (Intro_Func ([(Id ("_param", ~1), Div_Num (Id ("_param", ~1), DIVleft))], DIVleft)) |

  STRING (Value (String STRING, STRINGleft)) |

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

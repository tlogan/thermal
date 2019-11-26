structure Tree = struct


  datatype term = 
    Seq of (term * term * int) |
    Def of (term * term * int) |
    Selec of (term * string * int) |
    Pipe of (term * term * int) |
    Pred of (term * term * int) |
    Cns of (term * term * int) |
    Rep of (term * term * int) |
    Equiv of (term * term * int) |
    Implies of (term * term * int) |
    Or of (term * term * int) |
    And of (term * term * int) |
    Equal of (term * term * int) |
  
    Send of (term * int) |
    Recv of (term * int) |
    Wrap of (term * int) |
    Chse of (term * int) |
    Spawn of (term * int) |
    Blocked of (term * int) |
    Synced of (term * int) |
    Stuck of (term * int) |
    Done of (term * int) |
  
    AbsProp of ((string list) * term * int) |
    App of (term * term * int) |
    Unt of int |
    Fnc of (((term * term) list) * int) |
    Lst of ((term list) * int) |
    Rec of (((string * term) list) * int) |
  
    This of int |
    BoolLit of (bool * int) |
  
    Id of (string * int) |
    NumLit of (string * int) |
    StringLit of (string * int)


  fun to_string t = (case t of
    Seq (t1, t2, pos) => String.surround ("Seq@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Def (t1, t2, pos) => String.surround ("Def@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Selec (t1, name, pos) => String.surround ("Selec@" ^ (Int.toString pos)) (
      (to_string t1) ^ ", " ^ name 
    ) |

    Pipe (t1, t2, pos) => String.surround ("Pipe@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Pred (t1, t2, pos) => String.surround ("Pred@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Cns (t1, t2, pos) => String.surround ("Cns@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Rep (t1, t2, pos) => String.surround ("Rep@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Equiv (t1, t2, pos) => String.surround ("Equiv" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Implies (t1, t2, pos) => String.surround ("Implies@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Or (t1, t2, pos) => String.surround ("Or@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    And (t1, t2, pos) => String.surround ("And@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Equal (t1, t2, pos) => String.surround ("Equal@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |
  
    Send (t, pos) => String.surround ("Send@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Recv (t, pos) => String.surround ("Recv@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Wrap (t, pos) => String.surround ("Wrap@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Chse (t, pos) => String.surround ("Chse@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Spawn (t, pos) => String.surround ("Spawn@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Blocked (t, pos) => String.surround ("Blocked@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Synced (t, pos) => String.surround ("Synced@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Stuck (t, pos) => String.surround ("Stuck@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Done (t, pos) => String.surround ("Done@" ^ (Int.toString pos)) (
      (to_string t)
    ) |
  
    AbsProp (names, t, pos) => String.surround ("AbsProp@" ^ (Int.toString pos)) (
      "(" ^ (String.concatWith "," names) ^ "),\n" ^
      (to_string t)
    ) |

    App (t1, t2, pos) => String.surround ("App@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^
      (to_string t2)
    ) |

    Unt pos =>
      "Unt@" ^ (Int.toString pos) |

    Fnc (lams, pos) => String.surround ("Fnc@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string_from_lam lams)
    ) |

    Lst (ts, pos) => String.surround ("Lst@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string ts)
    ) |

    Rec (fs, pos) => String.surround ("Rec@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string_from_field fs)
    ) |
  
    This pos =>
      "This@" ^ (Int.toString pos) |

    BoolLit (true, pos) =>
      "True@" ^ (Int.toString pos) |

    BoolLit (false, pos) =>
      "False@" ^ (Int.toString pos) |
  
    Id (name, pos) => String.surround ("Id@" ^ (Int.toString pos)) (
      name 
    ) |

    NumLit (num, pos) => String.surround ("NumLit@" ^ (Int.toString pos)) (
      num 
    ) |

    StringLit (str, pos) => String.surround ("StringLit@" ^ (Int.toString pos)) (
      str 
    )

  )

  and to_string_from_lam (t1, t2) = String.surround "Lam" (
    (to_string t1) ^ ",\n" ^
    (to_string t2)
  )

  and to_string_from_field (name, t) = String.surround name (
    (to_string t)
  )



end
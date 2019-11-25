structure Tree = struct
end


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
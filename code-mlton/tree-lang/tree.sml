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

    Add of (term * term * int) |
    Sub of (term * term * int) |
    Mult of (term * term * int) |
    Div of (term * term * int) |
    Mod of (term * term * int) |
  
    AllocChan of int |
    Send of (term * int) |
    Recv of (term * int) |
    Wrap of (term * int) |
    Chse of (term * int) |
    Spawn of (term * int) |
    Sync of (term * int) |
    Solve of (term * int) |

    Not of (term * int) |
    Reduced of (term * int) |
    Blocked of (term * int) |
    Synced of (term * int) |
    Stuck of (term * int) |
    Done of (term * int) |
  
    AbsProp of ((string list) * term * int) |
    App of (term * term * int) |
    Fnc of (((term * term) list) * int) |
    Lst of ((term list) * int) |
    Rec of (((string * term) list) * int) |
  
    CatchAll of int |
    That of int |
    BoolLit of (bool * int) |
  
    Id of (string * int) |
    NumLit of (string * int) |
    StringLit of (string * int)


  datatype norm_list =
    NCons of (term * term * norm_list) |
    NEnd of term

  type contin = (
    term *
    norm_list *
    (string -> (term option))
  )
  
  type contin_stack = (contin list)
  
  type chan_id = int

  datatype base_event =
    Base_Send of (chan_id * term * contin_stack) |
    Base_Recv of (chan_id * contin_stack)


  datatype transition_mode = 
    Mode_Alloc of int |
    Mode_Reduce of term |
    Mode_Spawn of term |
    Mode_Block of (base_event list) |
    Mode_Sync of (int * term) |
    Mode_Stuck of string |
    Mode_Done of term

  val empty_table = (fn key => NONE)

  fun insert (table, key, item) = (
    (fn key' => if key' = key then SOME item else (table key')) 
  )
  
  
  
  fun insert_table (val_store_base, val_store_top) = (
    (fn key => (case (val_store_top key) of
      SOME value => SOME value |
      NONE => (val_store_base key)
    ))
  )
  
  fun find (table, key) = (table key)
  
  fun remove (table, key) = (fn key' =>
    if key' = key then NONE else (table key')
  ) 



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
      (to_string t1) ^ ",\n" ^ (to_string t2)) |

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

    Add (t1, t2, pos) => String.surround ("Add@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Sub (t1, t2, pos) => String.surround ("Sub@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Mult (t1, t2, pos) => String.surround ("Mult@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Div (t1, t2, pos) => String.surround ("Div@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Mod (t1, t2, pos) => String.surround ("Mod@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    AllocChan pos =>
      "AllocChan@" ^ (Int.toString pos) |

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

    Sync (t, pos) => String.surround ("Sync@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Solve (t, pos) => String.surround ("Solve@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Not (t, pos) => String.surround ("Not@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Reduced (t, pos) => String.surround ("Reduced@" ^ (Int.toString pos)) (
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

    Fnc (lams, pos) => String.surround ("Fnc@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string_from_lam lams)) |

    Lst (ts, pos) => String.surround ("Lst@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string ts)
    ) |

    Rec (fs, pos) => String.surround ("Rec@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string_from_field fs)
    ) |
  
    CatchAll pos =>
      "CatchAll@" ^ (Int.toString pos) |

    That pos =>
      "That@" ^ (Int.toString pos) |

    BoolLit (true, pos) =>
      "True@" ^ (Int.toString pos) |

    BoolLit (false, pos) =>
      "False@" ^ (Int.toString pos) |
  
    Id (name, pos) =>
      "(Id@" ^ (Int.toString pos) ^ " " ^ name ^ ")" |

    NumLit (num, pos) =>
      "(NumLit@" ^ (Int.toString pos) ^ " " ^ num ^ ")" |

    StringLit (str, pos) =>
      "(Stringit@" ^ (Int.toString pos) ^ " " ^ str ^ ")"
  )

  and to_string_from_lam (t1, t2) = String.surround "Lam" (
    (to_string t1) ^ ",\n" ^
    (to_string t2)
  )

  and to_string_from_field (name, t) = String.surround name (
    (to_string t))


  fun store_left (val_store, pat, value) = (case (pat, value) of
    (* **TODO** *)
    _ => NONE 
  )
  

  fun store_functions (val_store, def) = (case def of
    _ => val_store 
  
    (* **TODO**
    (Id str, Fnc (lams, pos)) => (
      insert (val_store, str, Fnc (lams, pos))
    ) |
  
    (pat, Id (str, _)) => 
      (case (find (val_store, str)) of
        NONE => val_store |
        SOME (Fnc (lams, pos)) => insert (val_store, str, Fnc (lams, pos))
      ) |
  
    (Rec fs1, Rec fs2) => (let
      val pairs = List.mapPartial
        (fn (name, pat) =>
          Option.map
            (fn v => (pat, v))
            (List.find
              (fn (n', v) => name = n')
              fs2)
        )

      fun acc_step val_store' (pat, value) = (
        store_functions (val_store', (pat, value))
      )
    in
      List.foldl acc_step val_store pairs
    end) |
  
    (Lst pats, Lst values) => (
      if (List.length pats = List.length values) then
        let
          val pairs = List.zip (pats, values)
          fun acc_step val_store' (pat, value) = (
            store_functions (val_store', (pat, value))
          )
        in
          List.foldl acc_step val_store pairs
        end
      else
        val_store 
    ) |

    (Evt (ec1, t1), Evt (ec2, t2)) =>
      if (ec1 = ec2) then
        store_functions (val_store, (t1, t2))
      else
        val_store |
    *)
  
  
  )
  
  
  fun store_functions_norm_list (val_store, ts) = (case ts of
  
    NCons (pat, def, ts') => store_functions_norm_list (
      (store_functions (val_store, (pat, def))),
      ts'
    ) |
  
    _ => val_store 

  )


  fun normalize t = (let
  
    fun sym i = Id ("_g_" ^ (Int.toString i), ~1)
  
    fun result_of_norm ts = (case ts of
      NEnd t => t |
      NCons (a, b, ts') => result_of_norm ts' 
    )
  
    fun normalize_cont (t, vc, k) = (case t of
      _ => (NEnd (sym vc), vc + 1)
      (*
      TODO
      *)
  
  
    )
  
    and normalize_base (result_term, vc, k) = (
      let
        val (tk, vc') = (k (sym vc, vc + 1)) 
        val norm = 
          NCons (
            sym vc, result_term, 
            tk
          )
      in
        (norm, vc')
      end
    )
  
    and normalize_single (t, result_fun, vc, k) = (
      normalize_cont (t, vc, fn (var, vc') =>
        let
          val (tk, vc'') = (k (sym vc', vc' + 1)) 
          val norm = 
            NCons (sym vc', result_fun var, tk)
        in
          (norm, vc'')
        end
      )
    )
  
    and normalize_pair ((t1, t2), result_fun, vc, k) = (
      normalize_cont (t1, vc, fn (var1, vc') =>
        normalize_cont (t2, vc', fn (var2, vc'') =>
          let
            val (tk, vc''') = (k (sym vc'', vc'' + 1)) 
            val norm = 
              NCons (
                sym vc'',
                result_fun (var1, var2), 
                tk
              )
          in
            (norm, vc''')
          end
        )
      )
    )
  
    and normalize_term_list (ts, vc) = (case ts of
      [] => [] |
      t :: ts' => (
        let
          val (n, vc') =
            (normalize_cont (t, vc, fn (var, vc) => (NEnd var, vc)))
        in
          (n, vc') :: (normalize_term_list (ts', vc'))
        end
      )
    )
  
    and combine_norm_pairs (norm_pairs, result_fun, k) = (let
  
      val norms = (List.map (fn (n, vc) => n) norm_pairs)
  
      val vars = List.map result_of_norm norms
  
  
      val ls = (List.length norm_pairs)
      val list_size = Int.toString ls
  
      val (n, vc) = (List.last norm_pairs)
  
      val (tk, vc') = (k (sym vc, vc + 1))
  
      val base_norm = 
        NCons (
          sym vc, result_fun vars, 
          tk
        )
  
  
      fun combine_pair (n1, n2) = (case n1 of
        NCons (a, b, n') => NCons (a, b, combine_pair (n', n2)) |
        NEnd _ => n2 )
  
      fun combine ns = (case ns of
        [] => base_norm |
        n :: ns' => combine_pair (n, combine ns') 
      )
  
    in
      (combine norms, vc')
    end)
  
  
    val (norm, vc') = (normalize_cont (
      t, 100,
      (fn (var, vc) => (NEnd var, vc))
    )) 
  
  
  in
    norm
  end)


  fun initial_thread t = (let
    val ts = normalize t
    val val_store = store_functions_norm_list (empty_table, ts) 
    val cont_stack = []
  in
    (ts, val_store, [])
  end)


  fun seq_step (
    (ts, val_store, cont_stack),
    (chan_store, block_store, cnt)
  ) = (case ts of
    (* **TODO** *)
    _ => (
      Mode_Stuck "TODO",
      [], (chan_store, block_store, cnt)
    )
    (*
    NEnd result_term => (let
  
      val result_md = Return result_term 
  
      val reso = subst_term (val_store, t)
  
      val (threads, md) = (case cont_stack of
        [] => ([], Done result_term) |
        (pat, ts', val_store') :: cont_stack' => 
          (case store_left (val_store', pat, reso) of
            NONE => ([], Stuck "seq_step result") |
            SOME val_store'' => (
              [(ts', val_store'', cont_stack')],
              result_md
            )
          )
      ) 
    in
      (
        md,
        threads,
        (chan_store, block_store, cnt)
      ) 
    end) |

    NCons (pat, StringLit str, ts') => reduce_step (
      (pat, StringLit str, ts', val_store, cont_stack),
      (chan_store, block_store, cnt)) |

    *)

  )

  fun string_from_mode md = (case md of
    (* **TODO **)
    _ => ""
    (*
    *)
  )


  fun concur_step (
    threads, env 
  
  ) = (case threads of
    [] => (print "all done!\n"; NONE) |
    thread :: threads' => (let
      val (md, seq_threads, env') = (seq_step (thread, env)) 
      val _ = print ((string_from_mode md) ^ "\n")
    in
      SOME (md, (threads' @ seq_threads, env'))
    end)
  )



  fun run t = (let

    val thread = initial_thread t

    val chan_store = empty_table
    val block_store = empty_table
    val cnt = 0

    fun loop cfg = (case (concur_step cfg) of
      NONE => () |
      SOME (md, cfg') =>
        loop cfg' 
    )
  
  in
    loop (
      [thread],
      (chan_store, block_store, cnt)
    )
  end)




end
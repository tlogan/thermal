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


(*
  fun normalize t = (let
  
    fun sym i = Id ("_g_" ^ (Int.toString i), -1)
  
    fun result_of_norm norm = (case norm of
      NEnd t => t |
      NCons (a, b) => result_of_norm b
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
            (normalize_cont (t, vc, fn (var, vc) => (var, vc)))
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
        NCons (a, b) => NCons (a, combine_pair (b, n2)) |
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

  fun extract_free_vars t = (case t of
    (* TODO *)
  )


  fun subst_term (store, t) = (case t of
    (* TODO *)
  )

  fun mk_base_events (evt, cont_stack, cnt) = (case evt of
  
    Evt (Send, Lst [ChanId i, msg]) =>
      ([Base_Send (i, msg, [])], cnt) |
  
    Evt (Recv, ChanId i) =>
      ([Base_Recv (i, [])], cnt) |
  
    Evt (Chse, Lst values) =>
      mk_base_events_from_list (values, cont_stack, cnt) |
  
    Evt (Wrap, Lst [evt', Fnc (lams, fnc_store)]) =>
      let
        val (bases, cnt') = mk_base_events (evt', cont_stack, cnt)
      in
        List.foldl (fn (acc_events, acc_cnt) => (fn base => let
  
          val wrap_arg = Var ("_wrap_arg_" ^ (Int.toString acc_cnt))
          val t = AppLeft (Fnc (lams, fnc_store), wrap_arg)
          val cont = (wrap_arg, t, empty_table)
  
          val base' = (case base of
            Base_Send (i, msg, wrap_stack) => 
              Base_Send (i, msg, cont :: wrap_stack) |
            Base_Recv (i, wrap_stack) => 
              Base_Recv (i, cont :: wrap_stack)
          ) 
          val acc_cnt' = acc_cnt + 1
          val acc_events' = acc_events @ [base']
        in
          (acc_events', acc_cnt') 
        end)) ([], cnt') bases 
      end |
  
    _ => ([], cnt)
  )
  
  and mk_base_events_from_list (evts, cont_stack, cnt) = (case evts of
    [] => ([], cnt) |
    evt :: evts' => 
      let
        val (base_events, cnt') = mk_base_events (evt, cont_stack, cnt)
      in
        if (List.null base_events) then
          ([], cnt')
        else (let
          val (base_events', cnt'') = mk_base_events_from_list (evts', cont_stack, cnt')
        in
          (base_events @ base_events', cnt'') 
        end)
      end
  )
  
  
  fun store_functions (val_store, binding) = (case binding of
    (Var str, Fnc (name, lams)) => (
      if (name = "") then
        insert (val_store, str, Fnc (str, lams))
      else 
        insert (val_store, str, Fnc (name, lams))
    ) |
  
    (pat, Var str) => 
      (case (find (val_store, str)) of
        None => val_store |
        Some (Fnc (name, lams)) => insert (val_store, str, Fnc (name, lams))
      ) |
  
    (Rec fs1, Rec fs2) =>
      let
        fun just_term (f, t) = t
      in
        collect_lists (
          val_store,
          List.map just_term fs1,
          List.map just_term fs2
        )
      end |
  
    (Lst ts1, Lst ts2) =>
      collect_lists (val_store, ts1, ts2) |
  
    (Evt (ec1, t1), Evt (ec2, t2)) =>
      if (ec1 = ec2) then
        store_functions (val_store, (t1, t2))
      else
        val_store |
  
    _ => val_store 
  
  )
  
  
  and collect_lists (val_store, pats, values) = (
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
  )
  
  fun collect_bindings (val_store, t) = (case t of
    Bind (pat, value) =>
      store_functions (val_store, (pat, value)) | 
  
    Seq (Bind binding, t') =>
      collect_bindings (
        (store_functions (val_store, binding)),
        t'
      ) |
  
    Seq (_, t') =>
      collect_bindings (val_store, t') |
  
    _ => val_store 
  )

  fun block (
    base_events, cont_stack,
    (chan_store, block_store, cnt)
  ) = (let
    val chan_store' = (List.foldl  
      (fn chan_store => fn base => block_one (base, cont_stack, chan_store, cnt))
      chan_store
      base_events
    )
    val block_store' = insert (block_store, cnt, ())
    val cnt' = cnt + 1
  in
    (Block base_events, [], (chan_store', block_store', cnt'))
  end)




  fun reduce_step (
    (pat, t, ts', val_store, cont_stack),
    (chan_store, block_store, cnt)
  
  ) = (let
    val reso = subst_term (val_store, t)
    val (threads, md) = (case store_left (val_store, pat, reso) of
      Some val_store' =>
        ([(ts', val_store', cont_stack)], Mode_Reduced (Bind (pat, reso))) |
      None => ([], Stuck "reduce_step")
    )
  in
    (
      md, 
      threads,
      (chan_store, block_store, cnt)
    )
  end)
  
  fun resolve (val_store, t) = (let
    val free_vars = extract_free_vars t
    val is_resolved = ([] = free_vars)
  in
    if is_resolved then
      t 
    else
      resolve (val_store, subst_term (val_store, t))
  end)




  fun seq_step (
    (ts, val_store, cont_stack),
    (chan_store, block_store, cnt)
  ) = (case ts of
    NEnd result_term => (let
  
      val result_md = Return result_term 
  
      val reso = subst_term (val_store, t)
  
      val (threads, md) = (case cont_stack of
        [] => ([], Done result_term) |
        (pat, ts', val_store') :: cont_stack' => 
          (case store_left (val_store', pat, reso) of
            None => ([], Stuck "seq_step result") |
            Some val_store'' => (
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


    _ => (
      Stuck "TODO",
      [], (chan_store, block_store, cnt)
    )

    (*
    TODO
    *)

  )

  fun concur_step (
    threads, env 
  
  ) = (case threads of
    [] => (print "all done!\n"; None) |
    thread :: threads' => (let
      val (md, seq_threads, env') = (seq_step (thread, env)) 
      val _ = print ((string_from_mode md) ^ "\n")
    in
      Some (md, (threads' @ seq_threads, env'))
    end)
  )

  fun initial_thread t = (let
  
    val ts = normalize t
    val val_store = collect_bindings (empty_table, ts) 
    val cont_stack = []
  in
    (ts, val_store, [])
  end)

  fun run t = (let

    val thread = initial_thread t

    val chan_store = empty_table
    val block_store = empty_table
    val cnt = 0

    fun loop cfg = (case (concur_step cfg) of
      None => () |
      Some (md, cfg') =>
        loop cfg' 
    )
  
  in
    loop (
      [thread],
      (chan_store, block_store, cnt)
    )
  end)

*)



end
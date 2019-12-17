structure Tree = struct

  type chan_id = int
  type thread_id = int

  datatype term = 
    Seq of (term * term * int) |
    Select of (term * string * int) |
    Pipe of (term * term * int) |
    Cns of (term * term * int) |
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
    Sat of (term * int) |

    Not of (term * int) |
    Reduced of (term * int) |
    Blocked of (term * int) |
    Synced of (term * int) |
    Stuck of (term * int) |
    Done of (term * int) |
  
    App of (term * term * int) |
    Fnc of (((term * term) list) * (string -> term option) * (string list) * int) |
    (* Fnc (lams, val_store, mutual_ids, pos)
    **
    ** TODO: update function closure,
    ** to contain free-var value-mappings and
    ** mutually recursve function mappings
    *)
    Lst of ((term list) * int) |
    Rec of (((string * term) list) * int) |
  
    CatchAll of int |
    That of int |
    Bool of (bool * int) |
  
    Id of (string * int) |
    Num of (string * int) |
    Str of (string * int) |

    (* internal reps *)
    ChanId of int |
    ThreadId of int |
    Backchain of (
      string * (term list) (* result string and proposition list *) *
      chan_id * thread_id *
      (string -> (term option)) (* env - evolving through query *)
    ) |
    Solution of solution

  and solution =
    Sol_Empty | Sol_Val of term | Sol_Abs of term



  type contin = (
    ((term * term) list) *
    (string -> (term option))
  )
  
  type contin_stack = (contin list)

  datatype base_event =
    Base_Send of (chan_id * term * contin_stack) |
    Base_Recv of (chan_id * contin_stack)

  datatype transition_mode = 
    Mode_Start |
    Mode_Hidden |
    Mode_Alloc of int |
    Mode_Reduce of term |
    Mode_Spawn of term |
    Mode_Block of (base_event list) |
    Mode_Sync of (int * term * int * int) |
    Mode_Stick of string |
    Mode_Finish of term

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

    Select (t1, name, pos) => String.surround ("Selec@" ^ (Int.toString pos)) (
      (to_string t1) ^ ", " ^ name
    ) |

    Pipe (t1, t2, pos) => String.surround ("Pipe@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Cns (t1, t2, pos) => String.surround ("Cns@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)) |

    Equiv (t1, t2, pos) => String.surround ("Equiv@_" ^ (Int.toString pos)) (
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

    Sat (t, pos) => String.surround ("Sat@" ^ (Int.toString pos)) (
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
  
    App (t1, t2, pos) => String.surround ("App@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^
      (to_string t2)
    ) |


    Fnc (lams, fnc_store, mutual_ids, pos) => String.surround ("Fnc@" ^ (Int.toString pos)) (
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

    Bool (true, pos) =>
      "true@" ^ (Int.toString pos) |

    Bool (false, pos) =>
      "false@" ^ (Int.toString pos) |
  
    Id (name, pos) =>
      "(Id@" ^ (Int.toString pos) ^ " " ^ name ^ ")" |

    Num (num, pos) =>
      "(Num@" ^ (Int.toString pos) ^ " " ^ num ^ ")" |

    Str (str, pos) =>
      "(Stringit@" ^ (Int.toString pos) ^ " " ^ str ^ ")" |

    ChanId i =>
      "(ChanId " ^ (Int.toString i) ^ ")" |

    ThreadId i =>
      "(ThreadId " ^ (Int.toString i) ^ ")" |

    Backchain _ =>
      "Backchain"
  )

  and to_string_from_lam (t1, t2) = String.surround "Lam" (
    (to_string t1) ^ ",\n" ^
    (to_string t2)
  )

  and to_string_from_field (name, t) = String.surround name (
    (to_string t))


  fun store_insert (val_store, pat, value) = (case (pat, value) of
    (* **TODO** *)
    _ => NONE 
  )

  fun push (
    (t_arg, lams),
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt
  ) = (let
    val cont = (lams, val_store)
    val cont_stack' = cont :: cont_stack
  in
    (
      Mode_Hidden,
      [(t_arg, val_store, cont_stack', thread_id)],
      (chan_store, block_store, cnt)
    )
  end)
  
  
  fun sym i = "_g_" ^ (Int.toString i)

  fun normalize (
    t, term_fn, val_store, cont_stack, thread_id,
    chan_store, block_store, cnt
  ) = (let
    val hole = Id (sym cnt, ~1)
    val hole_lam = (hole, term_fn hole)
  in
    push (
      (t, [hole_lam]),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    )
  end)


  fun pop (
    result,
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt
  ) = (let
    val (threads, md) = (case cont_stack of
      [] => ([], Mode_Finish result) |
      (lams, val_store') :: cont_stack' => (let


        fun match_first lams = (case lams of
          [] => NONE |
          (p, t) :: lams' =>
            (case (store_insert (val_store', p, result)) of
              NONE => match_first lams' |
              SOME val_store'' => SOME (t, val_store'')
            )
        )

      in
        (case (match_first lams) of

          NONE => (
            [], Mode_Stick "result does not match continuation hole pattern"
          ) |

          SOME (t_body, val_store'') => (
            [(t_body, val_store'', cont_stack', thread_id)],
            Mode_Reduce t_body  
          )

        )
      end)
    )
  in
    (
      md,
      threads,
      (chan_store, block_store, cnt)
    ) 
  end)

  fun resolve (val_store, t) = (case t of
    _ => NONE
    (* **TODO** *)
  )

  fun normalize_single_reduce (
    t, f,  
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt,
    reduce_f
  ) = (case (resolve (val_store, t)) of
    NONE => normalize (
      t, fn v => (f v),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    SOME v => (reduce_f v)
  )


  fun normalize_single_pop (
    t, f, 
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt
  ) = normalize_single_reduce (
    t, f, 
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt,
    (fn v => pop (
      (f v),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ))
  )

  fun normalize_pair_reduce (
    (t1, t2), f,  
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt,
    reduce_f
  ) = (case (
    resolve (val_store, t1),
    resolve (val_store, t2)
  ) of
    (NONE, _) => normalize (
      t1, fn v1 => f (v1, t2),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    (_, NONE) => normalize (
      t2, fn v2 => f (t1, v2),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    (SOME v1, SOME v2) => reduce_f (v1, v2)

  )


  fun normalize_pair_pop (
    (t1, t2), f, 
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt
  ) = normalize_pair_reduce (
    (t1, t2), f, 
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt,
    (fn (v1, v2) => pop (
      (f (v1, v2)),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ))
  )

  fun normalize_list_reduce (
    ts, f,  
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt,
    reduce_f
  ) = (let

    fun loop (prefix, postfix) = (case postfix of
      [] => reduce_f prefix |
      x :: xs => (case (resolve (val_store, x)) of

        NONE => normalize (
          x, fn v => f (prefix @ (v :: xs)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        SOME v => loop (prefix @ [v], xs)

      )
    )
  in
    loop ([], ts)
  end)

  fun normalize_list_pop (
    ts, f, 
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt
  ) = normalize_list_reduce (
    ts, f, 
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt,
    (fn vs => pop (
      (f vs),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ))
  )

  fun fnc_equal (f1, f2) = (let
    (* **TODO** *)  
  in
    false
  end)

  fun term_equal (t1, t2) = (let
    (* **TODO** *)  
  in
    false
  end)

  fun num_add (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)

  fun num_sub (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)

  fun num_mult (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)


  fun num_div (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)

  fun num_mod (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)


  fun mk_base_events (evt, cont_stack, cnt) = (case evt of
  
    Send (Lst ([ChanId i, msg], _), pos) =>
      ([Base_Send (i, msg, [])], cnt) |
  
    Recv (ChanId i, pos) =>
      ([Base_Recv (i, [])], cnt) |

    _ => ([], cnt)

    (*
    ** TODO **
  
    Chse (Lst (values, _), pos) =>
      mk_base_events_from_list (values, cont_stack, cnt) |
  

    Wrap (Lst ([evt', Fnc (lams, fnc_store, mutual_ids, pos)], _), pos) =>
      let
        val (bases, cnt') = mk_base_events (evt', cont_stack, cnt)
      in
        List.foldl (fn (acc_events, acc_cnt) => (fn base => let
  
          val wrap_arg = Var ("_wrap_arg_" ^ (Int.toString acc_cnt))
          val t = AppLeft (Fnc (lams, fnc_store, mutual_ids, pos), wrap_arg)
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

    *)
  
  )

  fun poll (base, chan_store, block_store) = (case base of
    Base_Send (i, msg, _) =>
      (let
        val chan_op = find (chan_store, i)
        fun poll_recv (send_q, recv_q) = (case recv_q of
          [] => (false, chan_store) |
          (block_id, cont_stack, _) :: recv_q' =>
            (case (find (block_store, block_id)) of
                SOME () => (true, chan_store) |
                NONE => poll (
                  base,
                  insert (chan_store, i, (send_q, recv_q')),
                  block_store
                )
            )
        )
      in
        (case chan_op of
          NONE =>
            (false, chan_store) |
          SOME chan =>
            (poll_recv chan)
        )
      end) |
  
     Base_Recv (i, _) =>
      (let
        val chan_op = find (chan_store, i)
        fun poll_send (send_q, recv_q) = (case send_q of
          [] => (false, chan_store) |
          (block_id, cont_stack, msg, _) :: send_q' =>
            (case (find (block_store, block_id)) of
              SOME () => (true, chan_store) |
              NONE => poll (
                base,
                insert (chan_store, i, (send_q', recv_q)),
                block_store
              )
            )
        )
      in
        (case chan_op of
          NONE =>
          (false, chan_store) |
          SOME chan =>
          (poll_send chan)
        )
      end)
  
  )


  fun find_active_base (
    bases, chan_store, block_store
  ) = (case bases of

    [] =>
      (NONE, chan_store) |

    base :: bases' => (let
      val (is_active, chan_store') = poll (base, chan_store, block_store)
    in
      if is_active then
        (SOME base, chan_store')
      else 
        find_active_base (bases', chan_store', block_store)
    end)
      
  )

  
  fun transact (
    base, cont_stack, thread_id,
    (chan_store, block_store, cnt)
  ) = (case base of

    Base_Send (i, msg, wrap_stack) =>
      (let
        val chan_op = find (chan_store, i)
        val recv_op = (case chan_op of
          SOME (_, (block_id, recv_stack, recv_thread_id) :: recvs) =>
            SOME (recv_stack, recv_thread_id) | 
          SOME (_, []) => NONE |
          NONE => NONE
        )
        val (threads, md') = (case recv_op of
          NONE => ([], Mode_Stick "transact Base_Send") |
          SOME (recv_stack, recv_thread_id) => (
            [
              (Lst ([], 0), empty_table, wrap_stack @ cont_stack, thread_id),
              (msg, empty_table, recv_stack, recv_thread_id)
            ],
            Mode_Sync (i, msg, thread_id, recv_thread_id)
          )
        ) 
      in
        (
          md', 
          threads,
          (chan_store, block_store, cnt)
        ) 
      end) |
  
    Base_Recv (i, wrap_stack) =>
      (let
        val chan_op = find (chan_store, i)
        val send_op = (case chan_op of
          SOME ((block_id, send_stack, msg, send_thread_id) :: sends, _) =>
            SOME (send_stack, msg, send_thread_id) | 
          SOME ([], _) => NONE |
          NONE => NONE
        )
  
        val (threads, md') = (case send_op of
          NONE => ([], Mode_Stick "transact Base_Recv") |
          SOME (send_stack, msg, send_thread_id) => (
            [
              (Lst ([], 0), empty_table, send_stack, send_thread_id),
              (msg, empty_table, wrap_stack @ cont_stack, thread_id)
            ],
            Mode_Sync (i, msg, send_thread_id, thread_id)
          )
        )
      in
        (
          md',
          threads,
          (chan_store, block_store, cnt)
        )
      end)
  
  )
  
  fun block_one (base, cont_stack, chan_store, block_id, thread_id) = (case base of
    Base_Send (i, msg, wrap_stack) =>
      (let
        val cont_stack' = wrap_stack @ cont_stack
        val chan_op = find (chan_store, i)
        val chan' = (case chan_op of
          NONE =>
            ([(block_id, cont_stack', msg, thread_id)], []) | 
          SOME (send_q, recv_q) =>
            (send_q @ [(block_id, cont_stack', msg, thread_id)], recv_q)
        )
        val chan_store' = insert (chan_store, i, chan')
      in
        chan_store'
      end) |
  
    Base_Recv (i, wrap_stack) =>
      (let
        val cont_stack' = wrap_stack @ cont_stack
        val chan_op = find (chan_store, i)
        val chan' = (case chan_op of
          NONE =>
            ([], [(block_id, cont_stack', thread_id)]) | 
          SOME (send_q, recv_q) =>
            (send_q, recv_q @ [(block_id, cont_stack', thread_id)])
        )
        val chan_store' = insert (chan_store, i, chan')
      in
        chan_store'
      end)
  
  )
  
  fun block (
    base_events, cont_stack, thread_id,
    (chan_store, block_store, cnt)
  ) = (let
    val chan_store' = (List.foldl  
      (fn (base, chan_store) =>
        block_one (base, cont_stack, chan_store, cnt, thread_id)
      )
      chan_store
      base_events
    )
    val block_store' = insert (block_store, cnt, ())
    val cnt' = cnt + 1
  in
    (Mode_Block base_events, [], (chan_store', block_store', cnt'))
  end)

  fun is_event t = (case t of

    Send (Lst ([ChanId _, _], _), pos) =>
      true |

    Recv (ChanId _, _) =>
      true |

    Wrap (Lst ([t', Fnc _], _), _) =>
      is_event t' |

    Chse (Lst (ts, _), _) =>
      List.all (fn t => is_event t) ts |

    _ =>
      false
  
  )


  fun mk_prop (result_id, lams) = (let
    (** TODO **)
  in
    (Lst ([], ~1))
  end)
  



  fun backchain (
    result_id, goals, return_chan_id, that_thread_id, env,
    val_store, cont_stack, thread_id,
    chan_store, block_store, cnt
  ) = (case goals of
    (*
    [] => (let
      (* simp = resolve as much as possible *)
      val simp_term = simp (result_id, env)

      val free_vars = extract_free_vars simp_term
      val result_msg = if null free_vars then
        Sol_Val result_msg
      else
        Sol_Abs (Fnc ([
          (Lst (free_vars, ~1), result_msg)
        ], (fn id => NONE), [], ~1), ~1)

      val sync_send = Sync (Send (Lst [
        ChanId return_chan_id,
        result_msg 
      ], ~1, ~1))
    in
      (
        Mode_Hidden,
        [(sync_send, val_store, cont_stack, thread_id)],
        (chan_store, block_store, cnt)
      )
    end)
    *)
    (* **TODO** *)
    _ => (
      Mode_Stick "TODO",
      [], (chan_store, block_store, cnt)
    )
  )
  

  fun seq_step (
    md,
    (t, val_store, cont_stack, thread_id),
    (chan_store, block_store, cnt)
  ) = (case t of
    Seq (t1, t2, _) => normalize (
      t1, fn _ => t2,
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    Select (t, name, pos) => (case (resolve (val_store, t)) of

      NONE => normalize (
        t, fn v => Select (v, name, pos),
        val_store, cont_stack, thread_id,
        chan_store, block_store, cnt
      ) |

      SOME (Rec (fields, _)) => (let
        val field_op = (List.find
          (fn (key, v) => key = name)
          fields
        )
      in
        (case field_op of
          SOME (_, v) => (
            Mode_Reduce v,
            [(v, val_store, cont_stack, thread_id)],
            (chan_store, block_store, cnt)
          ) |

          NONE => (
            Mode_Stick "selection from non-record",
            [], (chan_store, block_store, cnt)
          )
        )
      end) |

      _ => (
        Mode_Stick "selection from non-record",
        [], (chan_store, block_store, cnt)
      )

    ) |

    Pipe (t_arg, t_fn, pos) => normalize_single_reduce (
      t_fn, fn v_fn => Pipe (t_arg, v_fn, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (t_arg, Fnc (lams, fnc_store, mutual_ids, _)) => push (
          (t_arg, lams),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "pipe into non-function",
          [], (chan_store, block_store, cnt)
        )
      )
    ) |

    Cns (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Cns (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (v, Lst (ts, _)) => pop (
          Lst (v :: ts, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "cons with non-list",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Equiv (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Equiv (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (b1 = b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "<-> with non-boolean",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Implies (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Implies (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (not b1 orelse b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "--> with non-boolean",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Or (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Or (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (b1 orelse b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "\\/ with non-boolean",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    And (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => And (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (b1 andalso b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "/\\ with non-boolean",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Equal (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Equal (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn

        (v1, v2) => pop (
          Bool (term_equal (v1, v2), pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        )

      )

    ) |

    Add (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Add (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_add (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "+ with non-number",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Sub (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Sub (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_sub (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "- with non-number",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Mult (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Mult (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_mult (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "* with non-number",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |


    Div (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Div (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_div (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "/ with non-number",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Mod (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Mod (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_mod (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "% with non-number",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    AllocChan i => (let
      val chan_store' = insert (chan_store, cnt, ([], []))
      val cnt' = cnt + 1
    in
      pop (
        ChanId cnt,
        val_store, cont_stack, thread_id,
        chan_store', block_store, cnt'
      )
    end) |

    Send (t, pos) => normalize_single_pop (
      t, fn v => Send (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) | 

    Recv (t, pos) => normalize_single_pop (
      t, fn v => Recv (v, pos) ,
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) | 

    Wrap (t, pos) => normalize_single_pop (
      t, fn v => Wrap (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) | 

    Chse (t, pos) => normalize_single_pop (
      t, fn v => Chse (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) | 

    Spawn (t, pos) => normalize_single_reduce (
      t, fn v => Spawn (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn

        (Fnc ([(Lst ([], _), t_body)], fnc_store, mutual_ids, _)) => (let
          val spawn_id = cnt
          val cnt' = cnt + 1
        in
          (
            Mode_Spawn t_body,
            [
              (Lst ([], pos), val_store, cont_stack, thread_id),
              (t_body, val_store, [], spawn_id)
            ],
            (chan_store, block_store, cnt')
          )
        end) |

        _ => (
          Mode_Stick "spawn with non-function",
          [], (chan_store, block_store, cnt)
        )
      )
    ) |

    Sync (t, pos) => normalize_single_reduce (
      t, fn v => Sync (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn v => if (is_event v) then
        (let

          val (bases, cnt') = mk_base_events (v, [], cnt) 
          
          val (active_base_op, chan_store') = (
            find_active_base (bases, chan_store, block_store)
          )
        in
          (case active_base_op of
            SOME base =>
              transact (
                base, cont_stack, thread_id,
                (chan_store', block_store, cnt')
              ) |
            NONE =>
              block (
                bases, cont_stack, thread_id,
                (chan_store', block_store, cnt')
              )
          )
        end)
      else
        (
          Mode_Stick "sync with non-event",
          [], (chan_store, block_store, cnt)
        )
      )
    ) |

    Solve (t, pos) => normalize_single_reduce (
      t, fn v => Solve (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_ids, pos_f) => (let
          val chan_id = cnt
          val chan_store' = insert (chan_store, chan_id, ([], []))
          val thread_id' = cnt + 1
          val cnt' = cnt + 2 

          val env = empty_table
          val result_id = sym cnt' 
          val cnt'' = cnt' + 1

          val prop = mk_prop (result_id, lams)

        in
          (
            Mode_Reduce (ChanId cnt),
            [
              (ChanId chan_id, val_store, cont_stack, thread_id), 
              (
                Backchain (result_id, [prop], chan_id, thread_id, env),
                val_store, [], thread_id'
              )
            ],
            (chan_store, block_store, cnt'')
          )
        end) |

        _ => (
          Mode_Stick "solve with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      ) 
    ) |

    Sat (t, pos) => normalize_single_reduce (
      t, fn v => Sat (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_ids, pos_f) => (let
          val chan_id = cnt
          val chan_store' = insert (chan_store, chan_id, ([], []))
          val thread_id' = cnt + 1
          val cnt' = cnt + 2 

          val env = empty_table
          val result_id = sym cnt' 
          val cnt'' = cnt' + 1

          val prop = mk_prop (result_id, lams)

        in
          (
            Mode_Reduce (ChanId cnt),
            [
              (ChanId chan_id, val_store, cont_stack, thread_id), 
              (
                Backchain (result_id, [prop], chan_id, thread_id, env),
                val_store, [], thread_id'
              )
            ],
            (chan_store, block_store, cnt'')
          )
        end) |

        _ => (
          Mode_Stick "solve with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      ) 
    ) |

    Not (t, pos) => normalize_single_reduce (
      t, fn v => Not (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Bool (b, _) => pop (
          Bool (not b, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "~ with non-boolean",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Reduced (t, pos) => normalize_single_reduce (
      t, fn v => Reduced (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_ids, _) => (case md of
          Mode_Reduce v_arg => push (
            (v_arg, lams),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          ) |

          _ => pop (
            Bool (false, pos),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          )
        ) | 

        _ => (
          Mode_Stick "reduced with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      )
    ) | 

    Blocked (t, pos) => normalize_single_reduce (
      t, fn v => Blocked (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_ids, _) => (case md of
          Mode_Block i => push (
            (ThreadId thread_id, lams),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          ) |

          _ => pop (
            Bool (false, pos),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          )
        ) | 

        _ => (
          Mode_Stick "blocked with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      )
    ) | 

    Synced (t, pos) => normalize_single_reduce (
      t, fn v => Synced (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_ids, _) => (case md of
          Mode_Sync (chan_id, msg, send_id, recv_id) => push (
            (Lst ([
              ChanId chan_id, msg,
              ThreadId send_id, ThreadId recv_id
            ], ~1), lams),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          ) |

          _ => pop (
            Bool (false, pos),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          )
        ) | 

        _ => (
          Mode_Stick "synced with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      )
    ) | 

    Stuck (t, pos) => normalize_single_reduce (
      t, fn v => Stuck (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_ids, _) => (case md of
          Mode_Stick stuck_str => push (
            (Str (stuck_str, ~1), lams),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          ) |

          _ => pop (
            Bool (false, pos),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          )
        ) | 

        _ => (
          Mode_Stick "synced with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      )
    ) | 

    Done (t, pos) => normalize_single_reduce (
      t, fn v => Done (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_ids, _) => (case md of Mode_Finish v_arg => push (
            (v_arg, lams),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          ) |

          _ => pop (
            Bool (false, pos),
            val_store, cont_stack, thread_id,
            chan_store, block_store, cnt
          )
        ) | 

        _ => (
          Mode_Stick "reduced with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      )
    ) | 

    App (t_fn, t_arg, pos) => normalize_single_reduce (
      t_fn, fn v_fn => App (t_arg, v_fn, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (Fnc (lams, fnc_store, mutual_ids, _), t_arg) => push (
          (t_arg, lams),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stick "application of non-function",
          [], (chan_store, block_store, cnt)
        )
      )
    ) |

    Fnc (lams, fnc_store, mutual_ids, pos) => pop (
      Fnc (lams, fnc_store, mutual_ids, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    Lst (ts, pos) => normalize_list_pop (
      ts, fn ts => Lst (ts, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) | 

    Rec (fields, pos) => (let
      val keys = (map (fn (k, t) => k) fields)
      val ts = (map (fn (k, t) => t) fields)
    in
      normalize_list_pop (
        ts, fn ts => Rec (ListPair.zip (keys, ts), pos), 
        val_store, cont_stack, thread_id,
        chan_store, block_store, cnt
      )
    end) |

    CatchAll pos => (
      Mode_Stick "_ in non-pattern",
      [], (chan_store, block_store, cnt)
    ) |

    That pos => (
      Mode_Stick "'that' thread reference outside of query",
      [], (chan_store, block_store, cnt)
    ) |

    Bool (b, pos) => pop (
      Bool (b, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    Id (str, pos) => pop (
      Id (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    Num (str, pos) => pop (
      Num (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    Str (str, pos) => pop (
      Str (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    ChanId i => pop (
      ChanId i,
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    ThreadId i => pop (
      ThreadId i,
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt
    ) |

    Backchain (result_id, goals, return_chan_id, that_thread_id, env) => (
      backchain (
        result_id, goals, return_chan_id, that_thread_id, env,
        val_store, cont_stack, thread_id,
        chan_store, block_store, cnt
      )
    ) |

    _ => (
      Mode_Stick "TODO",
      [], (chan_store, block_store, cnt)
    )

    (* **TODO**
    Solution of solution
    Sol_Empty | Sol_Val of term | Sol_Abs of term
    *)

  )

  fun string_from_mode md = (case md of
    (* **TODO **)
    _ => ""
    (*
    *)
  )


  fun concur_step (
    md, threads, env 
  
  ) = (case threads of
    [] => (print "all done!\n"; NONE) |
    thread :: threads' => (let
      val (md', seq_threads, env') = (seq_step (md, thread, env)) 
      val _ = print ((string_from_mode md) ^ "\n")
    in
      SOME (md', threads' @ seq_threads, env')
    end)
  )



  fun run t = (let

    val val_store = empty_table 
    val cont_stack = []
    val thread_id = 0
    val thread = (t, val_store, cont_stack, thread_id)


    val chan_store = empty_table
    val block_store = empty_table
    val cnt = 1


    fun loop cfg = (case (concur_step cfg) of
      NONE => () |
      SOME (cfg') =>
        loop cfg' 
    )
  
  in
    loop (
      Mode_Start,
      [thread],
      (chan_store, block_store, cnt)
    )
  end)


end
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
    Bool of (bool * int) |
  
    Id of (string * int) |
    Num of (string * int) |
    Str of (string * int) |

    (* internal reps *)
    ChanId of int |
    ThreadId of int |
    Query of (((term * term) list) * chan_id * thread_id)


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
    Mode_Push |
    Mode_Alloc of int |
    Mode_Reduce of term |
    Mode_Spawn of term |
    Mode_Block of (base_event list) |
    Mode_Sync of (int * term * int * int) |
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

    Query _ =>
      "Query"
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
      Mode_Push,
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
      [] => ([], Mode_Done result) |
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
            [], Mode_Stuck "result does not match continuation hole pattern"
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

  fun fnc_equal (f1, f2) = (let
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
  
    Wrap (Lst ([evt', Fnc (lams, _)], _), pos) =>
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
          NONE => ([], Mode_Stuck "transact Base_Send") |
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
          NONE => ([], Mode_Stuck "transact Base_Recv") |
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
            Mode_Stuck "selection from non-record",
            [], (chan_store, block_store, cnt)
          )
        )
      end) |

      _ => (
        Mode_Stuck "selection from non-record",
        [], (chan_store, block_store, cnt)
      )

    ) |

    Pipe (t_arg, t_fn, pos) => normalize_single_reduce (
      t_fn, fn v_fn => Pipe (t_arg, v_fn, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        (t_arg, Fnc (lams, _)) => push (
          (t_arg, lams),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        _ => (
          Mode_Stuck "pipe into non-function",
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
          Mode_Stuck "cons with non-list",
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
          Mode_Stuck "<-> with non-boolean",
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
          Mode_Stuck "--> with non-boolean",
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
          Mode_Stuck "\\/ with non-boolean",
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
          Mode_Stuck "/\\ with non-boolean",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Equal (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Equal (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn

        (Fnc f1, Fnc f2) => pop (
          Bool (fnc_equal (f1, f2), pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, cnt
        ) |

        (v1, v2) => pop (
          Bool (v1 = v2, pos),
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
          Mode_Stuck "+ with non-number",
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
          Mode_Stuck "- with non-number",
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
          Mode_Stuck "* with non-number",
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
          Mode_Stuck "/ with non-number",
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
          Mode_Stuck "% with non-number",
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

        (Fnc ([(Lst ([], _), t_body)], _)) => (let
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
          Mode_Stuck "spawn with non-function",
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
          Mode_Stuck "sync with non-event",
          [], (chan_store, block_store, cnt)
        )
      )
    ) |

    Solve (t, pos) => normalize_single_reduce (
      t, fn v => Solve (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, pos_f) => (let
          val chan_id = cnt
          val chan_store' = insert (chan_store, chan_id, ([], []))
          val thread_id' = cnt + 1
          val cnt' = cnt + 2 
        in
          (
            Mode_Reduce (ChanId cnt),
            [
              (ChanId cnt, val_store, cont_stack, thread_id), 
              (Query (lams, chan_id, thread_id), val_store, [], thread_id')
            ],
            (chan_store, block_store, cnt')
          )
        end) |

        _ => (
          Mode_Stuck "solve with non-predicate",
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
          Mode_Stuck "~ with non-boolean",
          [], (chan_store, block_store, cnt)
        )
      )

    ) |

    Reduced (t, pos) => normalize_single_reduce (
      t, fn v => Reduced (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, _) => (case md of
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
          Mode_Stuck "reduced with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      )
    ) | 

    Blocked (t, pos) => normalize_single_reduce (
      t, fn v => Blocked (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, cnt,
      (fn
        Fnc (lams, _) => (case md of
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
          Mode_Stuck "reduced with non-predicate",
          [], (chan_store, block_store, cnt)
        )
      )
    ) | 

    _ => (
      Mode_Stuck "TODO",
      [], (chan_store, block_store, cnt)
    )

    (* **TODO** *)
    (*
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
    Bool of (bool * int) |
  
    Id of (string * int) |
    Num of (string * int) |
    Str of (string * int) |
    ChanId i =>
    ThreadId i =>
    Query (lams, chan_id) =>
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
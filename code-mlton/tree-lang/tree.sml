structure Tree = struct

  type chan_id = int
  type thread_id = int


  type ('a, 'b) store = ('a * 'b) list

  datatype left_right = Left | Right

  type infix_option = (left_right * int) option

  datatype term = 
    Blank of int |
    Id of (string * int) |
    Assoc of (term * int) |

    Cns of (term * int) |
    Lst of ((term list) * int) |

    Fnc of (
      ((term * term) list) *
      ((string, infix_option * term) store) *
      ((string, infix_option * ((term * term) list)) store) *
      int
    ) (* Fnc (lams, val_store, mutual_store, pos) *) |

    Compo of (term * term * int) |

    App of (term * term * int) |

    Seq of (term * term * int) |

    Rec of (
      ((string * (infix_option * term)) list) *
      bool *
      int
    ) (* Rec (fields, mutual_calls_active, pos) *) |

    Select of (term * int) |
  
    Alloc_Chan of (term * int) |

    Send of (term * int) |
    Recv of (term * int) |

    Wrap of (term * int) |
    Chse of (term * int) |
    Sync of (term * int) |

    Spawn of (term * int) |
    Par of (term * int) |
  
    Sym of (term * int) |

    Str of (string * int) |

    Num of (string * int) |

    Add of (term * int) |
    Sub of (term * int) |
    Mul of (term * int) |
    Div of (term * int) |
    Rem of (term * int) |

    (* internal reps *)
    ChanId of int |
    ThreadId of int |
    Error of string

  datatype contin_mode = Contin_Seq | Contin_Norm | Contin_App | Contin_Sync

  type contin = (
    contin_mode * 
    ((term * term) list) *
    ((string, infix_option * term) store) *
    ((string, infix_option * (term * term) list) store)
  )
  
  type contin_stack = (contin list)

  datatype base_event =
    Base_Send of (chan_id * term * contin_stack) |
    Base_Recv of (chan_id * contin_stack)

  datatype transition_mode = 
    Mode_Start |
    Mode_Upkeep |
    Mode_Reduce of term |
    Mode_Spawn of term |
    Mode_Block of (base_event list) |
    Mode_Sync of (int * term * int * int)
      (* Mode_Sync (thread_id, msg, send_id, recv_id) *) |
    Mode_Stick of string |
    Mode_Finish of term



  fun to_string t = (case t of
    Cns (t, pos) => String.surround ("Cns@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Lst (ts, pos) => String.surround ("Lst@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string ts)
    ) |


    Fnc (lams, fnc_store, mutual_store, pos) => String.surround ("Fnc@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string_from_lam lams)) |

    Compo (t1, t2, pos) => String.surround ("Compo@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^
      (to_string t2)
    ) |

    App (t1, t2, pos) => String.surround ("App@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^
      (to_string t2)
    ) |

    Seq (t1, t2, pos) => String.surround ("Seq@" ^ (Int.toString pos)) (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Rec (fs, _, pos) => String.surround ("Rec@" ^ (Int.toString pos)) (
      String.concatWith ",\n" (List.map to_string_from_field fs)
    ) |

    Select (t, pos) => String.surround ("Select@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Alloc_Chan (t, pos) => String.surround ("Alloc_Chan@" ^ (Int.toString pos)) (
      to_string t
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

    Sync (t, pos) => String.surround ("Sync@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Spawn (t, pos) => String.surround ("Spawn@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Par (t, pos) => String.surround ("Par@" ^ (Int.toString pos)) (
      (to_string t)
    ) |

    Sym (t, pos) => String.surround ("Sym@" ^ (Int.toString pos)) (
      (to_string t)
    ) |
  
    Blank pos =>
      "Blank@" ^ (Int.toString pos) |

    Id (name, pos) =>
      "(Id@" ^ (Int.toString pos) ^ " " ^ name ^ ")" |

    Num (num, pos) =>
      "(Num@" ^ (Int.toString pos) ^ " " ^ num ^ ")" |

    Str (str, pos) =>
      "(Str@" ^ (Int.toString pos) ^ " " ^ str ^ ")" |

    ChanId i =>
      "(ChanId " ^ (Int.toString i) ^ ")" |

    ThreadId i =>
      "(ThreadId " ^ (Int.toString i) ^ ")" |

    _ =>
      "(NOT YET IMPLEMENTED)"

  )

  and to_string_from_lam (t1, t2) = String.surround "Lam" (
    (to_string t1) ^ ",\n" ^
    (to_string t2)
  )

  and to_string_from_field (name, (fix_op, t)) = String.surround name (
    (to_string_from_infix_option fix_op) ^ (to_string t)
  )

  and to_string_from_infix_option fix_op = (case fix_op of
    SOME (Left, _) => "INFIXL " |
    SOME (Right, _) => "INFIXR " |
    NONE => ""
  )


  val empty_table = [] 

  fun insert (table, key, item) = (
    (key, item) :: table
  )
  
  
  fun insert_table (val_store_base, val_store_top) = (
    val_store_top @ val_store_base
  )
  
  fun find (table, key) =
  (Option.map
    (fn (k, v) => v)
    (List.find (fn (k, v) => k = key) table)
  )
  
  fun remove (table, key) =
  (List.filter
    (fn k => k <> key)
    table
  ) 

  fun add_num (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)

  fun sub_num (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)

  fun mul_num (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)


  fun div_num (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)

  fun rem_num (n1, n2) = (let
    (* **TODO** *)  
  in
    n1 
  end)


  fun mk_base_events (evt, cont_stack) = (case evt of
  
    Send (Lst ([ChanId i, msg], _), pos) =>
      [Base_Send (i, msg, [])] |
  
    Recv (ChanId i, pos) =>
      [Base_Recv (i, [])] |

    Chse (Lst (values, _), pos) =>
      mk_base_events_from_list (values, cont_stack) |

    Wrap (Lst ([evt', Fnc (lams, fnc_store, mutual_store, _)], _), pos) =>
      let
        val bevts = mk_base_events (evt', cont_stack)
      in
        (List.foldl
          (fn (bevt, bevts_acc) => let
  
            val cont = (Contin_Sync, lams, fnc_store, mutual_store)
  
            val bevt' = (case bevt of
              Base_Send (i, msg, wrap_stack) => 
                Base_Send (i, msg, cont :: wrap_stack) |
              Base_Recv (i, wrap_stack) => 
                Base_Recv (i, cont :: wrap_stack)
            ) 
          in
            bevts_acc @ [bevt']
          end)
          []
          bevts 
        )
      end |

    _ => []

  
  )

  and mk_base_events_from_list (evts, cont_stack) = (case evts of
    [] => [] |
    evt :: evts' => 
      let
        val base_events = mk_base_events (evt, cont_stack)
      in
        if (List.null base_events) then
          [] 
        else (let
          val base_events' = mk_base_events_from_list (evts', cont_stack)
        in
          (base_events @ base_events') 
        end)
      end
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


  fun find_active_base_event (
    bevts, chan_store, block_store
  ) = (case bevts of

    [] =>
      (NONE, chan_store) |

    bevt :: bevts' => (let
      val (is_active, chan_store') = poll (bevt, chan_store, block_store)
    in
      if is_active then
        (SOME bevt, chan_store')
      else 
        find_active_base_event (bevts', chan_store', block_store)
    end)
      
  )

  
  fun transact (
    bevt, cont_stack, thread_id,
    (chan_store, block_store, sync_store, cnt)
  ) = (case bevt of

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
        (chan_store, block_store, sync_store, cnt)
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
        (chan_store, block_store, sync_store, cnt)
      )
    end)
  
  )
  
  fun block_one (bevt, cont_stack, chan_store, block_id, thread_id) = (case bevt of
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
    (chan_store, block_store, sync_store, cnt)
  ) = (let
    val chan_store' = (List.foldl  
      (fn (bevt, chan_store) =>
        block_one (bevt, cont_stack, chan_store, cnt, thread_id)
      )
      chan_store
      base_events
    )
    val block_store' = insert (block_store, cnt, ())
    val cnt' = cnt + 1
  in
    (Mode_Block base_events, [], (chan_store', block_store', sync_store, cnt'))
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


  fun match_value_insert (val_store, pat, value) = (case (pat, value) of

    (Lst ([], _), Lst ([], _)) => SOME val_store | 

    (Lst (t :: ts, _), Lst (v :: vs, _))  =>
      (Option.mapPartial
        (fn val_store' =>
          match_value_insert (val_store', t, v)
        )
        (match_value_insert (val_store, Lst (ts, ~1), Lst (vs, ~1)))
      ) |

    _ => NONE

    (* **TODO**

    (Send (t, _), Send (v, _)) =>
      match_value_insert (val_store, t, v) |

    (Recv (t, _), Recv (v, _)) =>
      match_value_insert (val_store, t, v) |

    (Fnc p_fnc, Fnc v_fnc) => (
      if fnc_equal (p_fnc, v_fnc) then
        SOME val_store
      else
        NONE
    ) |

    (Lst (ts, _), Lst (vs, _)) => (case (ts, vs) of
      ([], []) =>
        SOME val_store |

      (t :: ts', v :: vs') => (Option.mapPartial
        (fn val_store' =>
          match_value_insert (val_store', t, v)
        )
        (match_value_insert (val_store, Lst (ts', ~1), Lst (vs', ~1)))
      ) |

      _ =>
        NONE
    ) |

    (Rec (p_fields, _), Rec (v_fields, _)) => (case (p_fields, v_fields) of
      ([], []) =>
        SOME val_store |

      ((pk, t) :: ps, _ :: _) => (let
        val (match, remainder) = (List.partition  
          (fn (k, v) => k = pk)
          v_fields
        )
      in
        (case match of
          [(k, v)] => (Option.mapPartial
            (fn val_store' => match_value_insert (val_store', t, v))
            (match_value_insert (val_store, Rec (ps, ~1), Rec (remainder, ~1)))
          ) |

          _ => NONE
        )
      end) |

      _ =>
        NONE
      
    ) |

    (Blank _, _) =>
      SOME val_store |

    (Bool (b, _), Bool (bv, _)) => (
      if b = bv then
        SOME val_store
      else
        NONE
    ) |

    (Id (str, _), v) =>
      SOME (insert (val_store, str, v)) |

    (Num (n, _), Num (nv, _)) => (
      if n = nv then
        SOME val_store
      else
        NONE
    ) |

    (Str (str, _), Str (strv, _)) => (
      if str = strv then
        SOME val_store
      else
        NONE
    ) |

    *)
  )

  fun sym i = "_g_" ^ (Int.toString i)


  fun push (
    (t_arg, cont),
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (let
    val cont_stack' = cont :: cont_stack
  in
    (
      Mode_Upkeep,
      [(t_arg, val_store, cont_stack', thread_id)],
      (chan_store, block_store, sync_store, cnt)
    )
  end)

  fun pop (
    result,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (let
    val (threads, md) = (case cont_stack of
      [] => (let
        val _ = print (
          "Result:\n" ^
          (to_string result) ^
          "\n*********\n\n"
        )
      in
        ([], Mode_Finish result)
      end) |
      (cmode, lams, val_store', mutual_store) :: cont_stack' => (let

        val val_store'' = (case result of
          Rec (fields, _, _) => (if cmode = Contin_Seq then
            insert_table (val_store', fields)
          else
            val_store'
          ) |
          _ => val_store'
        )

        (* embed mutual_store within self's functions *)
        val fnc_store = (map 
          (fn (k, (fix_op, lams)) =>
            (k, (fix_op, Fnc (lams, val_store'', mutual_store, ~1)))
          )
          mutual_store
        )

        val val_store''' = insert_table (val_store'', fnc_store)

        fun match_first lams = (case lams of
          [] => NONE |
          (p, t) :: lams' =>
            (case (match_value_insert (val_store''', p, result)) of
              NONE => match_first lams' |
              SOME val_store'' => SOME (t, val_store'')
            )
        )

      in
        (case (match_first lams) of

          NONE => (
            [], Mode_Stick "result does not match continuation hole pattern"
          ) |

          SOME (t_body, val_store''') => (
            [(t_body, val_store''', cont_stack', thread_id)],
            Mode_Reduce t_body  
          )

        )
      end)
    )
  in
    (
      md,
      threads,
      (chan_store, block_store, sync_store, cnt)
    ) 
  end)



  fun mk_hole_lam (
    term_fn, cnt
  ) = (let
    val hole = Id (sym cnt, ~1)
    val hole_lam = (hole, term_fn hole)
  in
    hole_lam
  end)

  fun hole i = Id (sym i, ~1)

  fun normalize (
    t, term_fn, val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (
    push (
      (t, (Contin_Norm, [(hole cnt, term_fn (hole cnt))], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    )
  )

  fun reduce_list (
    ts, push_f, pop_f,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (let

    fun loop (prefix, postfix) = (case postfix of
      [] => (case (pop_f prefix) of 
        Error msg => (
          Mode_Stick msg,
          [], (chan_store, block_store, sync_store, cnt)
        ) |

        result => pop (
          result,
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        )

      ) |

      x :: xs => (case x of
        (Id (id, _)) => (case (find (val_store, id)) of
          SOME (NONE, v) => loop (prefix @ [v], xs) |
          _ => (
            Mode_Stick "ID in list cannot be resolved",
            [], (chan_store, block_store, sync_store, cnt)
          )
        ) |

        _ => push (
          (
            x,
            (
              Contin_Norm,
              [( hole cnt, push_f (prefix @ (hole cnt :: xs)) )],
              val_store,
              []
            )
          ),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt + 1
        )
      )
    )

  in
    loop ([], ts)
  end)
  
  fun reduce_single (
    t, push_f, pop_f,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (case t of
    (Id (id, _)) => (case (find (val_store, id)) of
      SOME (NONE, v) => (case (pop_f v) of
        Error msg => (
          Mode_Stick msg,
          [], (chan_store, block_store, sync_store, cnt)
        ) |

        result => pop (
          result,
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        )
      ) |

      _  => (
        Mode_Stick "ID cannot be resolved",
        [], (chan_store, block_store, sync_store, cnt)
      )

    ) |

    _ => push (
      (t, (Contin_Norm, [( hole cnt, push_f (hole cnt) )], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    )
  )


  fun apply (
    t_fn, t_arg, pos,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (case t_fn of
    (Id (id, _)) => (case (find (val_store, id)) of
      SOME (NONE, Fnc (lams, fnc_store, mutual_store, _)) => push (
        (t_arg, (Contin_App, lams, fnc_store, mutual_store)),
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      ) |

      SOME _ => (
        Mode_Stick "application of non-function",
        [], (chan_store, block_store, sync_store, cnt)
      ) |

      _  => (
        Mode_Stick "ID cannot be resolved",
        [], (chan_store, block_store, sync_store, cnt)
      )
    ) |

    _ => push (
      (t_fn, (Contin_Norm, [( hole cnt, App (hole cnt, t_arg, pos) )], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    )

  )


  fun associate_right val_store (t1, id, rator, direc, prec, pos, t2) = (case t1 of 
    Compo (Compo (t1', Id (id', pos'), p1'), t2', p2') =>
    (if (id' = id) then
      (if direc = Right then
        associate_right val_store (
          t1',
          id, rator, direc, prec, pos',
          App (rator, Lst ([t2', t2], pos), pos)
        )  
      else 
        App (rator, Lst ([t1, t2], pos), pos)
      )
    else (case (find (val_store, id')) of
      SOME (SOME (direc', prec'), rator') =>
      (if (prec > prec') then
        associate_right val_store (
          t1',
          id', rator', direc', prec', pos',
          App (rator, Lst ([t2', t2], pos), pos)
        )  
      else
        App (rator, Lst ([t1, t2], pos), pos)
      ) |

      _ => Compo (App (t1', Id (id', pos'), p1'), t2', p2')
    )) |

    _ =>
      App (rator, Lst ([t1, t2], pos), pos)
  )

  fun seq_step (
    md,
    (t, val_store, cont_stack, thread_id),
    (chan_store, block_store, sync_store, cnt)
  ) = (case t of

    Assoc (term, pos) => (
      Mode_Upkeep,
      [(term, val_store, cont_stack, thread_id)],
      (chan_store, block_store, sync_store, cnt)
    ) |

    Id (id, pos) => (case (find (val_store, id)) of
      SOME (NONE, v) => (
        Mode_Upkeep,
        [(v, val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      ) |

      _ => (
        Mode_Stick "ID cannot be resolved",
        [], (chan_store, block_store, sync_store, cnt)
      )
    ) |

    Cns (t, pos) => reduce_single (
      t,
      fn t => Cns (t, pos),
      (fn
        Lst ([v, Lst (ts, _)], _) => Lst (v :: ts, pos) |

        _ => Error "cons with non-list"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Lst (ts, pos) => reduce_list (
      ts, fn vs => Lst (vs, pos), fn vs => Lst (vs, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Fnc (lams, [], mutual_store, pos) => pop (
      Fnc (lams, val_store, mutual_store, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Fnc (lams, fnc_store, mutual_store, pos) => pop (
      Fnc (lams, fnc_store, mutual_store, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let

      val term = (case (find (val_store, id)) of
        SOME (SOME (direc, prec), rator) =>  (
          associate_right val_store (t1, id, rator, direc, prec, pos, t2)
        ) |

        _ => Compo (App (t1, Id (id, pos), p1), t2, p2)
      )

    in
      (
        Mode_Upkeep,
        [(term, val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      )
    end) |

    Compo (t1, t2, pos) => (
      Mode_Upkeep,
      [(App (t1, t2, pos), val_store, cont_stack, thread_id)],
      (chan_store, block_store, sync_store, cnt)
    ) |


    App (t_fn, t_arg, pos) => apply (
      t_fn, t_arg, pos,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Seq (t1, t2, _) => push (
      (t1, (Contin_Seq, [(hole cnt, t2)], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    ) |

    Rec (fields, false, pos) => (let
      val mutual_store = (List.mapPartial
        (fn
          (k, (fix_op,  Fnc (lams, [], [], _))) => 
            SOME (k, (fix_op, lams)) |
          _ => NONE
        )
        fields
      )
      
      (* embed mutual ids into ts' functions *)
      val fields' = (map
        (fn
          (k, (fix_op, Fnc (lams, [], [], pos))) =>
            (k, (fix_op, Fnc (lams, val_store, mutual_store, pos))) |
          field => field 
        )
       fields 
      )
    in
      (
        Mode_Upkeep,
        [(Rec (fields', true, pos), val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      )
    end) |
    
    Rec (fields, true, pos) => (let
      val ts = (map (fn (k, (fix_op, t)) => t) fields)

      fun f ts = (let
        val fields' = (List.map
          (fn ((key, (fix_op, _)), t) => (key, (fix_op, t)))
          (ListPair.zip (fields, ts))
        )
      in
        Rec (fields', true,  pos)
      end)

    in
      reduce_list (
        ts, f, f, 
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      )
    end) |

    Select (t, pos) => reduce_single (
      t,
      fn t => Select (t, pos),
      (fn
        Lst ([Rec (fields, _, _), Id (key, _)], _) =>
        (case find (fields, key) of
          SOME (_, v) => v |
          NONE => Error "selection not found"
        ) |

        _ => Error "selecting from non-record"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Alloc_Chan (_, i) => (let
      val chan_store' = insert (chan_store, cnt, ([], []))
      val cnt' = cnt + 1
    in
      pop (
        ChanId cnt,
        val_store, cont_stack, thread_id,
        chan_store', block_store, sync_store, cnt'
      )
    end) |

    (* internal rep *)
    ChanId i => pop (
      ChanId i,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Send (t, pos) => reduce_single (
      t, fn v => Send (v, pos), fn v => Send (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Recv (t, pos) => reduce_single (
      t, fn v => Recv (v, pos), fn v => Recv (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Wrap (t, pos) => reduce_single (
      t, fn v => Wrap (v, pos), fn v => Wrap (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Chse (t, pos) => reduce_single (
      t, fn v => Chse (v, pos), fn v => Chse (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Sync (t, pos) => (case t of
      (Id (id, _)) => (case (find (val_store, id)) of
        SOME (NONE, v) =>
        (if (is_event v) then
          (let

            val bevts = mk_base_events (v, []) 
            
            val (active_bevt_op, chan_store') = (
              find_active_base_event (bevts, chan_store, block_store)
            )
          in
            (case active_bevt_op of
              SOME bevt =>
                transact (
                  bevt, cont_stack, thread_id,
                  (chan_store', block_store, sync_store, cnt)
                ) |
              NONE =>
                block (
                  bevts, cont_stack, thread_id,
                  (chan_store', block_store, sync_store, cnt)
                )
            )
          end)
        else
          (
            Mode_Stick "sync with non-event",
            [], (chan_store, block_store, sync_store, cnt)
          )
        ) |

        _  => (
          Mode_Stick "ID cannot be resolved",
          [], (chan_store, block_store, sync_store, cnt)
        )

      ) |

      _ => push (
        (t, (Contin_Norm, [( hole cnt, Sync (hole cnt, pos) )], val_store, [])),
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt + 1
      )
    ) |

    (* internal rep *)
    ThreadId i => pop (
      ThreadId i,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Spawn (t, pos) =>(case t of
      (Id (id, _)) => (case (find (val_store, id)) of
        SOME (_, Fnc ([(Lst ([], _), t_body)], fnc_store, mutual_store, _)) =>
        (let
          val spawn_id = cnt
          val cnt' = cnt + 1
        in
          (
            Mode_Spawn t_body,
            [
              (Lst ([], pos), val_store, cont_stack, thread_id),
              (t_body, val_store, [], spawn_id)
            ],
            (chan_store, block_store, sync_store, cnt')
          )
        end) |

        SOME _ => (
          Mode_Stick "spawn with non-function",
          [], (chan_store, block_store, sync_store, cnt)
        ) |
      
        _  => (
          Mode_Stick "ID cannot be resolved",
          [], (chan_store, block_store, sync_store, cnt)
        )

      ) |

      _ => push (
        (t, (Contin_Norm, [( hole cnt, Spawn (hole cnt, pos) )], val_store, [])),
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt + 1
      )
    ) |

    Blank pos => pop (
      Blank pos,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Str (str, pos) => pop (
      Str (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Num (str, pos) => pop (
      Num (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Add (t, pos) => reduce_single (
      t, fn t => Add (t, pos),
      (fn
        Lst ([Num (n1, _), Num (n2, _)], _) => (
          Num (add_num (n1, n2), pos)
        ) |
        _ => Error "adding non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Sub (t, pos) => reduce_single (
      t, fn t => Sub (t, pos),
      (fn
        Lst ([Num (n1, _), Num (n2, _)], _) => (
          Num (sub_num (n1, n2), pos)
        ) |
        _ => Error "subtracting non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Mul (t, pos) => reduce_single (
      t, fn t => Mul (t, pos),
      (fn
        Lst ([Num (n1, _), Num (n2, _)], _) => (
          Num (mul_num (n1, n2), pos)
        ) |
        _ => Error "multplying non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Div (t, pos) => reduce_single (
      t, fn t => Div (t, pos),
      (fn
        Lst ([Num (n1, _), Num (n2, _)], _) => (
          Num (div_num (n1, n2), pos)
        ) |
        _ => Error "dividing non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Rem (t, pos) => reduce_single (
      t, fn t => Rem (t, pos),
      (fn
        Lst ([Num (n1, _), Num (n2, _)], _) => (
          Num (rem_num (n1, n2), pos)
        ) |
        _ => Error "remaindering non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |


    _ => (
      Mode_Stick "TODO",
      [], (chan_store, block_store, sync_store, cnt)
    )

    (* **TODO**
  

    Par of (term * int) |
  
    Sym of (term * int) |
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

  fun eval t = (let

    val val_store = empty_table 
    val cont_stack = []
    val thread_id = 0
    val thread = (t, val_store, cont_stack, thread_id)


    val chan_store = empty_table
    val block_store = empty_table
    val sync_store = empty_table (* of ((thread_id, query_id) -> event_list) *)
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
      (chan_store, block_store, sync_store, cnt)
    )
  end)

end

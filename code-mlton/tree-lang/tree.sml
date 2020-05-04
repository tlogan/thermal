structure Tree = struct

  type chan_id = int
  type thread_id = int


  type ('a, 'b) store = ('a * 'b) list

  datatype left_right = Left | Right

  datatype term = 
    Assoc of (term * int) |
    Cns of (term * int) |
    Lst of ((term list) * int) |

    Fnc of (
      ((term * term) list) *
      ((string, (left_right * int) option * term) store) *
      ((string, (left_right * int) option * ((term * term) list)) store) *
      int
    ) (* Fnc (lams, val_store, mutual_store, pos) *) |

    Compo of (term * term * int) |

    App of (term * term * int) |

    Seq of (term * term * int) |

    Rec of (
      (((left_right * int) option * string * term) list) *
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

    Blank of int |
  
    Id of (string * int) |

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

  type contin = (
    ((term * term) list) *
    ((string, term) store) *
    ((string, (term * term) list) store)
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

  and to_string_from_field (fix_op, name, t) = String.surround name (
    (to_string_from_left_right_option fix_op) ^ (to_string t)
  )

  and to_string_from_left_right_option fix_op = (case fix_op of
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

(*
**
**
**
**
**  fun fnc_equal (f1, f2) = (let
**    (* **TODO** *)  
**  in
**    f1 = f2
**  end)
**
**  fun term_equal (t1, t2) = (let
**    (* **TODO** *)  
**  in
**    t1 = t2 
**  end)
**
**  fun num_add (n1, n2) = (let
**    (* **TODO** *)  
**  in
**    n1 
**  end)
**
**  fun num_sub (n1, n2) = (let
**    (* **TODO** *)  
**  in
**    n1 
**  end)
**
**  fun num_mult (n1, n2) = (let
**    (* **TODO** *)  
**  in
**    n1 
**  end)
**
**
**  fun num_div (n1, n2) = (let
**    (* **TODO** *)  
**  in
**    n1 
**  end)
**
**  fun num_mod (n1, n2) = (let
**    (* **TODO** *)  
**  in
**    n1 
**  end)
**
**
**
**
**
**  
**  
**
**
**
**
**
**
**  fun normalize_single_pop (
**    t, f, 
**    val_store, cont_stack, thread_id,
**    chan_store, block_store, sync_store, cnt
**  ) = reduce_single (
**    t, f, 
**    val_store, cont_stack, thread_id,
**    chan_store, block_store, sync_store, cnt,
**    (fn v => pop (
**      (f v),
**      val_store, cont_stack, thread_id,
**      chan_store, block_store, sync_store, cnt
**    ))
**  )
**
**
**
**  fun normalize_pair_pop (
**    (t1, t2), f, 
**    val_store, cont_stack, thread_id,
**    chan_store, block_store, sync_store, cnt
**  ) = normalize_pair_reduce (
**    (t1, t2), f, 
**    val_store, cont_stack, thread_id,
**    chan_store, block_store, sync_store, cnt,
**    (fn (v1, v2) => pop (
**      (f (v1, v2)),
**      val_store, cont_stack, thread_id,
**      chan_store, block_store, sync_store, cnt
**    ))
**  )
**
**
**  fun mk_base_events (evt, cont_stack) = (case evt of
**  
**    Send (Lst ([ChanId i, msg], _), pos) =>
**      [Base_Send (i, msg, [])] |
**  
**    Recv (ChanId i, pos) =>
**      [Base_Recv (i, [])] |
**
**    Chse (Lst (values, _), pos) =>
**      mk_base_events_from_list (values, cont_stack) |
**
**    Wrap (Lst ([evt', Fnc (lams, fnc_store, mutual_store, _)], _), pos) =>
**      let
**        val bevts = mk_base_events (evt', cont_stack)
**      in
**        (List.foldl
**          (fn (bevt, bevts_acc) => let
**  
**            val cont = (lams, fnc_store, mutual_store)
**  
**            val bevt' = (case bevt of
**              Base_Send (i, msg, wrap_stack) => 
**                Base_Send (i, msg, cont :: wrap_stack) |
**              Base_Recv (i, wrap_stack) => 
**                Base_Recv (i, cont :: wrap_stack)
**            ) 
**          in
**            bevts_acc @ [bevt']
**          end)
**          []
**          bevts 
**        )
**      end |
**
**    _ => []
**
**  
**  )
**
**  and mk_base_events_from_list (evts, cont_stack) = (case evts of
**    [] => [] |
**    evt :: evts' => 
**      let
**        val base_events = mk_base_events (evt, cont_stack)
**      in
**        if (List.null base_events) then
**          [] 
**        else (let
**          val base_events' = mk_base_events_from_list (evts', cont_stack)
**        in
**          (base_events @ base_events') 
**        end)
**      end
**  )
**
**
**  fun poll (base, chan_store, block_store) = (case base of
**    Base_Send (i, msg, _) =>
**      (let
**        val chan_op = find (chan_store, i)
**        fun poll_recv (send_q, recv_q) = (case recv_q of
**          [] => (false, chan_store) |
**          (block_id, cont_stack, _) :: recv_q' =>
**            (case (find (block_store, block_id)) of
**                SOME () => (true, chan_store) |
**                NONE => poll (
**                  base,
**                  insert (chan_store, i, (send_q, recv_q')),
**                  block_store
**                )
**            )
**        )
**      in
**        (case chan_op of
**          NONE =>
**            (false, chan_store) |
**          SOME chan =>
**            (poll_recv chan)
**        )
**      end) |
**  
**     Base_Recv (i, _) =>
**      (let
**        val chan_op = find (chan_store, i)
**        fun poll_send (send_q, recv_q) = (case send_q of
**          [] => (false, chan_store) |
**          (block_id, cont_stack, msg, _) :: send_q' =>
**            (case (find (block_store, block_id)) of
**              SOME () => (true, chan_store) |
**              NONE => poll (
**                base,
**                insert (chan_store, i, (send_q', recv_q)),
**                block_store
**              )
**            )
**        )
**      in
**        (case chan_op of
**          NONE =>
**          (false, chan_store) |
**          SOME chan =>
**          (poll_send chan)
**        )
**      end)
**  
**  )
**
**
**  fun find_active_base_event (
**    bevts, chan_store, block_store
**  ) = (case bevts of
**
**    [] =>
**      (NONE, chan_store) |
**
**    bevt :: bevts' => (let
**      val (is_active, chan_store') = poll (bevt, chan_store, block_store)
**    in
**      if is_active then
**        (SOME bevt, chan_store')
**      else 
**        find_active_base_event (bevts', chan_store', block_store)
**    end)
**      
**  )
**
**  
**  fun transact (
**    bevt, cont_stack, thread_id,
**    (chan_store, block_store, sync_store, cnt)
**  ) = (case bevt of
**
**    Base_Send (i, msg, wrap_stack) =>
**      (let
**        val chan_op = find (chan_store, i)
**        val recv_op = (case chan_op of
**          SOME (_, (block_id, recv_stack, recv_thread_id) :: recvs) =>
**            SOME (recv_stack, recv_thread_id) | 
**          SOME (_, []) => NONE |
**          NONE => NONE
**        )
**        val (threads, md') = (case recv_op of
**          NONE => ([], Mode_Stick "transact Base_Send") |
**          SOME (recv_stack, recv_thread_id) => (
**            [
**              (Lst ([], 0), empty_table, wrap_stack @ cont_stack, thread_id),
**              (msg, empty_table, recv_stack, recv_thread_id)
**            ],
**            Mode_Sync (i, msg, thread_id, recv_thread_id)
**          )
**        ) 
**      in
**        (
**          md', 
**          threads,
**          (chan_store, block_store, sync_store, cnt)
**        ) 
**      end) |
**  
**    Base_Recv (i, wrap_stack) =>
**      (let
**        val chan_op = find (chan_store, i)
**        val send_op = (case chan_op of
**          SOME ((block_id, send_stack, msg, send_thread_id) :: sends, _) =>
**            SOME (send_stack, msg, send_thread_id) | 
**          SOME ([], _) => NONE |
**          NONE => NONE
**        )
**  
**        val (threads, md') = (case send_op of
**          NONE => ([], Mode_Stick "transact Base_Recv") |
**          SOME (send_stack, msg, send_thread_id) => (
**            [
**              (Lst ([], 0), empty_table, send_stack, send_thread_id),
**              (msg, empty_table, wrap_stack @ cont_stack, thread_id)
**            ],
**            Mode_Sync (i, msg, send_thread_id, thread_id)
**          )
**        )
**      in
**        (
**          md',
**          threads,
**          (chan_store, block_store, sync_store, cnt)
**        )
**      end)
**  
**  )
**  
**  fun block_one (bevt, cont_stack, chan_store, block_id, thread_id) = (case bevt of
**    Base_Send (i, msg, wrap_stack) =>
**      (let
**        val cont_stack' = wrap_stack @ cont_stack
**        val chan_op = find (chan_store, i)
**        val chan' = (case chan_op of
**          NONE =>
**            ([(block_id, cont_stack', msg, thread_id)], []) | 
**          SOME (send_q, recv_q) =>
**            (send_q @ [(block_id, cont_stack', msg, thread_id)], recv_q)
**        )
**        val chan_store' = insert (chan_store, i, chan')
**      in
**        chan_store'
**      end) |
**  
**    Base_Recv (i, wrap_stack) =>
**      (let
**        val cont_stack' = wrap_stack @ cont_stack
**        val chan_op = find (chan_store, i)
**        val chan' = (case chan_op of
**          NONE =>
**            ([], [(block_id, cont_stack', thread_id)]) | 
**          SOME (send_q, recv_q) =>
**            (send_q, recv_q @ [(block_id, cont_stack', thread_id)])
**        )
**        val chan_store' = insert (chan_store, i, chan')
**      in
**        chan_store'
**      end)
**  
**  )
**  
**  fun block (
**    base_events, cont_stack, thread_id,
**    (chan_store, block_store, sync_store, cnt)
**  ) = (let
**    val chan_store' = (List.foldl  
**      (fn (bevt, chan_store) =>
**        block_one (bevt, cont_stack, chan_store, cnt, thread_id)
**      )
**      chan_store
**      base_events
**    )
**    val block_store' = insert (block_store, cnt, ())
**    val cnt' = cnt + 1
**  in
**    (Mode_Block base_events, [], (chan_store', block_store', sync_store, cnt'))
**  end)
**
**  fun is_event t = (case t of
**
**    Send (Lst ([ChanId _, _], _), pos) =>
**      true |
**
**    Recv (ChanId _, _) =>
**      true |
**
**    Wrap (Lst ([t', Fnc _], _), _) =>
**      is_event t' |
**
**    Chse (Lst (ts, _), _) =>
**      List.all (fn t => is_event t) ts |
**
**    _ =>
**      false
**  
**  )
**
**
**  fun mk_prop (result_id, lams) = (let
**
**    val var = Id (result_id, ~1)
**    val prop_cases = (List.foldl
**      (fn ((t, b), prop_cases_acc) => (case prop_cases_acc of
**        [] => [(
**          Equal (var, t, ~1),
**          [],
**          b
**        )] |
**        (curr, prevs, _) :: _ => (
**          prop_cases_acc @ [(
**            Equal (var, t, ~1),
**            prevs @ [curr],
**            b
**          )]
**        )
**      ))
**      []
**      lams
**    )
**
**    val mk_or_clause = (List.foldl
**      (fn (cl, or_cl) =>
**        Or (or_cl, cl, ~1)
**      )
**      (Bool (false, ~1))
**    )
**    
**
**    val case_clauses = (List.map
**      (fn (curr_cl, prev_cls, b) =>
**        And (curr_cl, (Not (mk_or_clause prev_cls, ~1)), ~1)
**      )
**      prop_cases
**    )
**
**    val mk_and_clause = (List.foldl
**      (fn (cl, and_cl) =>
**        And (and_cl, cl, ~1)
**      )
**      (Bool (true, ~1))
**    )
**
**  in
**    mk_and_clause case_clauses
**  end)
**
**  fun backchain (
**    goals, monitee_thread_id, env,
**    val_store, cont_stack, thread_id,
**    chan_store, block_store, sync_store, cnt
**  ) = (case goals of
**    (*
**    [] => (let
**      (* simp = resolve as much as possible *)
**      val simp_term = simp (result_id, env)
**
**      val free_vars = extract_free_vars simp_term
**      val result_msg = if null free_vars then
**        Sol_Val result_msg
**      else
**        Sol_Abs (Fnc ([
**          (Lst (free_vars, ~1), result_msg)
**        ], (fn id => NONE), [], ~1), ~1)
**
**      val sync_send = Sync (Send (Lst [
**        ChanId return_chan_id,
**        result_msg 
**      ], ~1, ~1))
**    in
**      (
**        Mode_Upkeep,
**        [(sync_send, val_store, cont_stack, thread_id)],
**        (chan_store, block_store, sync_store, cnt)
**      )
**    end)
**    *)
**    (* **TODO** *)
**    _ => (
**      Mode_Stick "TODO",
**      [], (chan_store, block_store, sync_store, cnt)
**    )
**  )
**
**
**
**
**
**
*)

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

  fun resolve (val_store, t) = (case t of
    _ => NONE
    (* **TODO** *)
  )

  fun sym i = "_g_" ^ (Int.toString i)


  fun push (
    (t_arg, (lams, fnc_store, mutual_store)),
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (let
    val cont = (lams, fnc_store, mutual_store)
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
      [] => ([], Mode_Finish result) |
      (lams, val_store', mutual_store) :: cont_stack' => (let

        (* embed mutual_store within self's functions *)
        val fnc_store = (map 
          (fn (k, (fix_op, lams)) =>
            (k, (fix_op, Fnc (lams, val_store', mutual_store, ~1)))
          )
          mutual_store
        )

        val val_store'' = insert_table (val_store', fnc_store)

        fun match_first lams = (case lams of
          [] => NONE |
          (p, t) :: lams' =>
            (case (match_value_insert (val_store'', p, result)) of
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
      (t, ([(hole cnt, term_fn (hole cnt))], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    )
  )

  fun normalize_pair_reduce (
    (t1, t2), f,  
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt,
    reduce_f
  ) = (case (
    resolve (val_store, t1), resolve (val_store, t2)
  ) of
    (NONE, _) => push (
      (t1, ([(hole cnt, f (hole cnt, t2))], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    ) |

    (_, NONE) => push (
      (t2, ([(hole cnt, f (t1, hole cnt))], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    ) |

    (SOME v1, SOME v2) => reduce_f (v1, v2)

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

      x :: xs => (case (resolve (val_store, x)) of

        NONE => push (
          (x, ([( hole cnt, push_f (prefix @ (hole cnt :: xs)) )], val_store, [])),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt + 1
        ) |

        SOME v => loop (prefix @ [v], xs)

      )
    )

  in
    loop ([], ts)
  end)
  
  fun reduce_single (
    t, push_f, pop_f,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (case (resolve (val_store, t)) of
    NONE => push (
      (t, ([( hole cnt, push_f (hole cnt) )], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    ) |

    SOME v => (case (pop_f v) of
      Error msg => (
        Mode_Stick msg,
        [], (chan_store, block_store, sync_store, cnt)
      ) |

      result => pop (
        result,
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      )

    )
  )


  fun apply (
    t_fn, t_arg, pos,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (case (resolve (val_store, t_fn)) of
    NONE => push (
      (t_fn, ([( hole cnt, App (hole cnt, t_arg, pos) )], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    ) |

    SOME (Fnc (lams, fnc_store, mutual_store, _)) => push (
      (t_arg, (lams, fnc_store, mutual_store)),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |
    
    _ => (
      Mode_Stick "application of non-function",
      [], (chan_store, block_store, sync_store, cnt)
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

    Cns (t, pos) => reduce_single (
      t,
      fn t => Cns (t, pos),
      (fn
        Cns (Lst ([v, Lst (ts, _)], _), _) => Lst (v :: ts, pos) |

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


    (* TODO: debug infix precedence *)

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

    (* TODO: Seq Rec *)
    (*
    Seq (Rec (fields, _), t2, _) => 
    *)

    Seq (t1, t2, _) => push (
      (t1, ([(hole cnt, t2)], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    ) |

    Rec (fields, false, pos) => (let
      val mutual_store = (List.mapPartial
        (fn
          (fix_op, k, Fnc (lams, [], [], _)) => 
            SOME (k, (fix_op, lams)) |
          _ => NONE
        )
        fields
      )
      
      (* embed mutual ids into ts' functions *)
      val fields' = (map
        (fn
          (fix_op, k, Fnc (lams, [], [], pos)) =>
            (fix_op, k, Fnc (lams, val_store, mutual_store, pos)) |
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
      val ts = (map (fn (fix_op, k, t) => t) fields)

      fun f ts = (let
        val fields' = (List.map
          (fn ((fix_op, key, _), t) => (fix_op, key, t))
          (ListPair.zip (fields, ts))
        )
      in
        Rec (fields',true,  pos)
      end)

    in
      reduce_list (
        ts, f, f, 
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      )
    end) |


    _ => (
      Mode_Stick "TODO",
      [], (chan_store, block_store, sync_store, cnt)
    )
    (* **TODO**

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

    Blank of int |
  
    Id of (string * int) |

    Str of (string * int) |

    Num of (string * int) |

    Add of (term * int) |
    Sub of (term * int) |
    Mul of (term * int) |
    Div of (term * int) |
    Rem of (term * int) |

    (* internal reps *)
    ChanId of int |
    ThreadId of int



    Select (t, name, pos) => (case (resolve (val_store, t)) of

      NONE => normalize (
        t, fn v => Select (v, name, pos),
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
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
            (chan_store, block_store, sync_store, cnt)
          ) |

          NONE => (
            Mode_Stick "selection from non-record",
            [], (chan_store, block_store, sync_store, cnt)
          )
        )
      end) |

      _ => (
        Mode_Stick "selection from non-record",
        [], (chan_store, block_store, sync_store, cnt)
      )

    ) |


    Pipe (t_arg, t_fn, pos) => reduce_single (
      t_fn, fn v_fn => Pipe (t_arg, v_fn, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (t_arg, Fnc (lams, fnc_store, mutual_store, _)) => push (
          (t_arg, (lams, fnc_store, mutual_store)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "pipe into non-function",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )
    ) |


    Open (t_rec, t_body, pos) => reduce_single (
      t_rec, fn v_rec => Open (v_rec, t_body, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Rec (fields, _), t_body) => (let
          val val_store' = insert_table (val_store, fields)
        in
          (
            Mode_Upkeep,
            [(t_body, val_store', cont_stack, thread_id)],
            (chan_store, block_store, sync_store, cnt)
          )
        end) |

        _ => (
          Mode_Stick "open of non-record",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )
    ) |

    Equiv (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Equiv (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (b1 = b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "<-> with non-boolean",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Implies (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Implies (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (not b1 orelse b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "--> with non-boolean",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Or (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Or (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (b1 orelse b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "\\/ with non-boolean",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    And (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => And (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Bool (b1, _), Bool (b2, _)) => pop (
          Bool (b1 andalso b2, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "/\\ with non-boolean",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Equal (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Equal (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn

        (v1, v2) => pop (
          Bool (term_equal (v1, v2), pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        )

      )

    ) |

    Add (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Add (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_add (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "+ with non-number",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Sub (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Sub (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_sub (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "- with non-number",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Mul (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Mul (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_mult (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "* with non-number",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |


    Div (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Div (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_div (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "/ with non-number",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Rem (t1, t2, pos) => normalize_pair_reduce (
      (t1, t2), fn (t1, t2) => Rem (t1, t2, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        (Num n1, Num n2) => pop (
          Num (num_mod (n1, n2)),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "% with non-number",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Alloc_Chan i => (let
      val chan_store' = insert (chan_store, cnt, ([], []))
      val cnt' = cnt + 1
    in
      pop (
        ChanId cnt,
        val_store, cont_stack, thread_id,
        chan_store', block_store, sync_store, cnt'
      )
    end) |

    Send (t, pos) => normalize_single_pop (
      t, fn v => Send (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Recv (t, pos) => normalize_single_pop (
      t, fn v => Recv (v, pos) ,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Wrap (t, pos) => normalize_single_pop (
      t, fn v => Wrap (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Chse (t, pos) => normalize_single_pop (
      t, fn v => Chse (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Spawn (t, pos) => reduce_single (
      t, fn v => Spawn (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn

        (Fnc ([(Lst ([], _), t_body)], fnc_store, mutual_store, _)) => (let
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

        _ => (
          Mode_Stick "spawn with non-function",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )
    ) |

    Sync (t, pos) => reduce_single (
      t, fn v => Sync (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn v => if (is_event v) then
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
      )
    ) |

    Solve (t, pos) => reduce_single (
      t, fn v => Solve (v, pos),  
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_store, pos_f) => (let
          val chan_id = cnt
          val chan_store' = insert (chan_store, chan_id, ([], []))
          val thread_id' = cnt + 1
          val cnt' = cnt + 2 

          val env = empty_table
          val result_id = sym cnt' 
          val cnt'' = cnt' + 1

          val bkchn_id = cnt''
          val cnt''' = cnt'' + 1

          val sync_store' = insert (sync_store, (thread_id, bkchn_id), [])

          val prop = mk_prop (result_id, lams)

        in
          (
            Mode_Reduce (ChanId cnt),
            [(
              Backchain ([prop], thread_id, env),
              val_store, [], thread_id'
            )],
            (chan_store, block_store, sync_store', cnt''')
          )
        end) |

        _ => (
          Mode_Stick "solve with non-predicate",
          [], (chan_store, block_store, sync_store, cnt)
        )
      ) 
    ) |

    For (t, pos) => (let
      val empty_term = (
        Not (Equal (
          Solution Sol_Empty,
          Solve (t, pos),
          ~1
        ), ~1)
      )
    in
      (
        Mode_Upkeep,
        [(empty_term, val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      )
    end) |

    Not (t, pos) => reduce_single (
      t, fn v => Not (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        Bool (b, _) => pop (
          Bool (not b, pos),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt
        ) |

        _ => (
          Mode_Stick "~ with non-boolean",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )

    ) |

    Synced (t, pos) => reduce_single (
      t, fn v => Synced (v, pos), 
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt,
      (fn
        Fnc (lams, fnc_store, mutual_store, _) => (case md of
          Mode_Sync (chan_id, msg, send_id, recv_id) => (
            if (send_id = thread_id) then (
              push (
                (
                  Send (Lst ([ChanId chan_id, msg], ~1), ~1),
                  (lams, fnc_store, mutual_store)
                ),
                val_store, cont_stack, thread_id,
                chan_store, block_store, sync_store, cnt
              )
              

            ) else if (recv_id = thread_id) then (
              push (
                (
                  Recv (Lst ([ChanId chan_id, msg], ~1), ~1),
                  (lams, fnc_store, mutual_store)
                ),
                val_store, cont_stack, thread_id,
                chan_store, block_store, sync_store, cnt
              )

            ) else (
              pop (
                Bool (false, pos),
                val_store, cont_stack, thread_id,
                chan_store, block_store, sync_store, cnt
              )

            )
          ) |

          _ => pop (
            Bool (false, pos),
            val_store, cont_stack, thread_id,
            chan_store, block_store, sync_store, cnt
          )

        ) | 

        _ => (
          Mode_Stick "synced with non-predicate",
          [], (chan_store, block_store, sync_store, cnt)
        )
      )
    ) | 


    Blank pos => (
      Mode_Stick "_ in non-pattern",
      [], (chan_store, block_store, sync_store, cnt)
    ) |

    Bool (b, pos) => pop (
      Bool (b, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Id (str, pos) => pop (
      Id (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Num (str, pos) => pop (
      Num (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Str (str, pos) => pop (
      Str (str, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    ChanId i => pop (
      ChanId i,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    ThreadId i => pop (
      ThreadId i,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Backchain (goals, monitee_thread_id, env) => (
      backchain (
        goals, monitee_thread_id, env,
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      )
    ) |

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
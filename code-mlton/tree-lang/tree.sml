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

    List_Intro of (term * term * int) |
    List_Val of ((term list) * int) |

    Func_Intro of (
      ((term * term) list) *
      int
    ) (* Func_Val (lams, pos) *) |

    Func_Val of (
      ((term * term) list) *
      ((string, infix_option * term) store) *
      ((string, infix_option * ((term * term) list)) store) *
      int
    ) (* Func_Val (lams, val_store, mutual_store, pos) *) |
    Func_Elim of (term * term * int) |

    Compo of (term * term * int) |
    Seq of (term * term * int) |

    Rec_Intro of (
      ((string * (infix_option * term)) list) *
      int
    ) (* Rec_Intro (fields, pos) *) |

    Rec_Val of (
      ((string * (infix_option * term)) list) *
      int
    ) (* Rec_Intro (fields, pos) *) |

    Rec_Elim of (term * int) |
  
    Chan_Alloc of (term * int) |

    Evt_Send_Intro of (term * int) |
    Evt_Send_Val of (term * int) |
    Evt_Recv_Intro of (term * int) |
    Evt_Recv_Val of (term * int) |
    Evt_Wrap_Intro of (term * int) |
    Evt_Wrap_Val of (term * int) |
    Evt_Choose_Intro of (term * int) |
    Evt_Choose_Val of (term * int) |
    Evt_Elim of (term * int) |

    Spawn of (term * int) |
    Par of (term * int) |
  
    Sym of (term * int) |

    String_Val of (string * int) |

    Num_Val of (string * int) |

    Num_Add of (term * int) |
    Num_Sub of (term * int) |
    Num_Mul of (term * int) |
    Num_Div of (term * int) |
    Num_Rem of (term * int) |

    (* internal reps *)
    Chan_Loc of int |
    ThreadId of int |
    Error of string

  datatype contin_mode = Contin_Seq | Contin_Norm | Contin_Func_Elim | Contin_Evt_Elim

  type contin = (
    contin_mode * 
    ((term * term) list) *
    ((string, infix_option * term) store) *
    ((string, infix_option * (term * term) list) store)
  )
  
  type contin_stack = (contin list)

  datatype base_event =
    Base_Evt_Send_Intro of (chan_id * term * contin_stack) |
    Base_Evt_Recv_Intro of (chan_id * contin_stack)

  datatype transition_mode = 
    Mode_Start |
    Mode_Suspend |
    Mode_Reduce of term |
    Mode_Continue |
    Mode_Spawn of term |
    Mode_Block of (base_event list) |
    Mode_Sync of (int * term * int * int)
      (* Mode_Sync (thread_id, msg, send_id, recv_id) *) |
    Mode_Stick of string |
    Mode_Finish



  fun to_string t = (case t of


    Assoc (t, pos) => String.surround "Assoc" (
      (to_string t)
    ) |

    List_Intro (t1, t2, pos) => String.surround "List_Intro" (
      (to_string t1) ^ ",\n" ^
      (to_string t2)
    ) |

    List_Val (ts, pos) => String.surround "[" (
      (String.concatWith ",\n" (List.map to_string ts)) ^ "]"
    ) |

    Func_Intro (lams, pos) => String.surround "Func_Intro" (
      String.concatWith ",\n" (List.map to_string_from_lam lams)) |

    Func_Val (lams, fnc_store, mutual_store, pos) => String.surround "Func_Val" (
      String.concatWith ",\n" (List.map to_string_from_lam lams)) |

    Compo (t1, t2, pos) => String.surround "Compo" (
      (to_string t1) ^ ",\n" ^
      (to_string t2)
    ) |

    Func_Elim (t1, t2, pos) => String.surround "Func_Elim" (
      (to_string t1) ^ ",\n" ^
      (to_string t2)
    ) |

    Seq (t1, t2, pos) => String.surround "Seq" (
      (to_string t1) ^ ",\n" ^ (to_string t2)
    ) |

    Rec_Intro (fs, pos) => String.surround "Rec_Intro" (
      String.concatWith ",\n" (List.map to_string_from_field fs)
    ) |

    Rec_Val (fs, pos) => String.surround "Rec_Val" (
      String.concatWith ",\n" (List.map to_string_from_field fs)
    ) |

    Rec_Elim (t, pos) => String.surround "Rec_Elim" (
      (to_string t)
    ) |

    Chan_Alloc (t, pos) => String.surround "Chan_Alloc" (
      to_string t
    ) |

    Evt_Send_Intro (t, pos) => String.surround "Evt_Send_Intro" (
      (to_string t)
    ) |

    Evt_Recv_Intro (t, pos) => String.surround "Evt_Recv_Intro" (
      (to_string t)
    ) |

    Evt_Wrap_Intro (t, pos) => String.surround "Evt_Wrap_Intro" (
      (to_string t)
    ) |

    Evt_Choose_Intro (t, pos) => String.surround "Evt_Choose_Intro" (
      (to_string t)
    ) |

    Evt_Elim (t, pos) => String.surround ("Evt_Elim") (
      (to_string t)
    ) |

    Spawn (t, pos) => String.surround ("Spawn") (
      (to_string t)
    ) |

    Par (t, pos) => String.surround ("Par") (
      (to_string t)
    ) |

    Sym (t, pos) => String.surround ("Sym") (
      (to_string t)
    ) |
  
    Blank pos =>
      "Blank" |

    Id (name, pos) =>
      "(Id" ^ " " ^ name ^ ")" |

    String_Val (str, pos) =>
      "(String_Val" ^ " " ^ str ^ ")" |

    Chan_Loc i =>
      "(Chan_Loc " ^ (Int.toString i) ^ ")" |

    ThreadId i =>
      "(ThreadId " ^ (Int.toString i) ^ ")" |


    Num_Val (num, pos) =>
      "(Num_Val" ^ " " ^ num ^ ")" |

    Num_Add (t, pos) => String.surround ("Num_Add") (
      (to_string t)
    ) |

    Num_Sub (t, pos) => String.surround ("Num_Sub") (
      (to_string t)
    ) |

    Num_Mul (t, pos) => String.surround ("Num_Mul") (
      (to_string t)
    ) |

    Num_Div (t, pos) => String.surround ("Num_Div") (
      (to_string t)
    ) |

    Num_Rem (t, pos) => String.surround ("Num_Rem") (
      (to_string t)
    ) |

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
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 + i2

    val _ = print ("add_num n1: " ^ n1 ^ "\n") 
    val _ = print ("add_num n2: " ^ n2 ^ "\n") 
    val str = Int.toString i3
    val _ = print ("add_num result: " ^ str ^ "\n") 
    
  in
    str
  end)

  fun sub_num (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 - i2
  in
    Int.toString i3
  end)

  fun mul_num (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 * i2
  in
    Int.toString i3
  end)


  fun div_num (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 div i2
  in
    Int.toString i3
  end)

  fun rem_num (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = Int.rem (i1, i2)
  in
    Int.toString i3
  end)


  fun mk_base_events (evt, cont_stack) = (case evt of
  
    Evt_Send_Intro (List_Val ([Chan_Loc i, msg], _), pos) =>
      [Base_Evt_Send_Intro (i, msg, [])] |
  
    Evt_Recv_Intro (Chan_Loc i, pos) =>
      [Base_Evt_Recv_Intro (i, [])] |

    Evt_Choose_Intro (List_Val (values, _), pos) =>
      mk_base_events_from_list (values, cont_stack) |

    Evt_Wrap_Intro (List_Val ([evt', Func_Val (lams, fnc_store, mutual_store, _)], _), pos) =>
      let
        val bevts = mk_base_events (evt', cont_stack)
      in
        (List.foldl
          (fn (bevt, bevts_acc) => let
  
            val cont = (Contin_Evt_Elim, lams, fnc_store, mutual_store)
  
            val bevt' = (case bevt of
              Base_Evt_Send_Intro (i, msg, wrap_stack) => 
                Base_Evt_Send_Intro (i, msg, cont :: wrap_stack) |
              Base_Evt_Recv_Intro (i, wrap_stack) => 
                Base_Evt_Recv_Intro (i, cont :: wrap_stack)
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
    Base_Evt_Send_Intro (i, msg, _) =>
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
  
     Base_Evt_Recv_Intro (i, _) =>
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

    Base_Evt_Send_Intro (i, msg, wrap_stack) =>
    (let
      val chan_op = find (chan_store, i)
      val recv_op = (case chan_op of
        SOME (_, (block_id, recv_stack, recv_thread_id) :: recvs) =>
          SOME (recv_stack, recv_thread_id) | 
        SOME (_, []) => NONE |
        NONE => NONE
      )
      val (threads, md') = (case recv_op of
        NONE => ([], Mode_Stick "transact Base_Evt_Send_Intro") |
        SOME (recv_stack, recv_thread_id) => (
          [
            (Blank 0, empty_table, wrap_stack @ cont_stack, thread_id),
            (msg, empty_table, recv_stack, recv_thread_id)
          ],
          (
          print ("send sync msg: " ^ (to_string msg) ^ "\n");
          Mode_Sync (i, msg, thread_id, recv_thread_id)
          )
        )
      ) 
    in
      (
        md', 
        threads,
        (chan_store, block_store, sync_store, cnt)
      ) 
    end) |
  
    Base_Evt_Recv_Intro (i, wrap_stack) =>
    (let
      val chan_op = find (chan_store, i)
      val send_op = (case chan_op of
        SOME ((block_id, send_stack, msg, send_thread_id) :: sends, _) =>
          SOME (send_stack, msg, send_thread_id) | 
        SOME ([], _) => NONE |
        NONE => NONE
      )
  
      val (threads, md') = (case send_op of
        NONE => ([], Mode_Stick "transact Base_Evt_Recv_Intro") |
        SOME (send_stack, msg, send_thread_id) => (
          [
            (Blank 0, empty_table, send_stack, send_thread_id),
            (msg, empty_table, wrap_stack @ cont_stack, thread_id)
          ],
          ( (* TODO: LOOK HERE, what does the val_store of the send_stack look like? *)
          print ("recv sync msg: " ^ (to_string msg) ^ "\n");
          Mode_Sync (i, msg, send_thread_id, thread_id)
          )
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
    Base_Evt_Send_Intro (i, msg, wrap_stack) =>
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
  
    Base_Evt_Recv_Intro (i, wrap_stack) =>
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

  fun match_value_insert (val_store, pat, value) = (case (pat, value) of

    (Assoc (pat', _), _) =>
      match_value_insert (val_store, pat', value) |

    (Blank _, _) =>
      SOME val_store |

    (Id (str, _), v) =>
      SOME (insert (val_store, str, (NONE, v))) |

    (List_Intro (t, t', _), List_Val (v :: vs, _)) =>
      (Option.mapPartial
        (fn val_store' =>
          match_value_insert (val_store', t, v)
        )
        (match_value_insert (val_store, t', List_Val (vs, ~1)))
      ) |

    (List_Val ([], _), List_Val ([], _)) => SOME val_store | 

    (List_Val (t :: ts, _), List_Val (v :: vs, _))  =>
      (Option.mapPartial
        (fn val_store' =>
          match_value_insert (val_store', t, v)
        )
        (match_value_insert (val_store, List_Val (ts, ~1), List_Val (vs, ~1)))
      ) |


    (Num_Val (n, _), Num_Val (nv, _)) => (
      if n = nv then
        SOME val_store
      else
        NONE
    ) |

    _ => NONE

    (* **TODO**

    (Evt_Send_Intro (t, _), Evt_Send_Intro (v, _)) =>
      match_value_insert (val_store, t, v) |

    (Evt_Recv_Intro (t, _), Evt_Recv_Intro (v, _)) =>
      match_value_insert (val_store, t, v) |

    (Func_Val p_fnc, Func_Val v_fnc) => (
      if fnc_equal (p_fnc, v_fnc) then
        SOME val_store
      else
        NONE
    ) |

    (String_Val (str, _), String_Val (strv, _)) => (
      if str = strv then
        SOME val_store
      else
        NONE
    ) |

    (Rec_Intro (p_fields, _), Rec_Intro (v_fields, _)) => (case (p_fields, v_fields) of
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
            (match_value_insert (val_store, Rec_Intro (ps, ~1), Rec_Intro (remainder, ~1)))
          ) |

          _ => NONE
        )
      end) |

      _ =>
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
      Mode_Suspend,
      [(t_arg, val_store, cont_stack', thread_id)],
      (chan_store, block_store, sync_store, cnt)
    )
  end)

  fun pop (
    result,
    cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (let
    val (threads, md) = (case cont_stack of
      [] => ([], Mode_Finish) |
      (cmode, lams, val_store', mutual_store) :: cont_stack' => (let

        val val_store'' = (case result of
          Rec_Val (fields, _) => (if cmode = Contin_Seq then
            insert_table (val_store', fields)
          else
            val_store'
          ) |
          _ => val_store'
        )

        (* embed mutual_store within self's functions *)
        val fnc_store = (map 
          (fn (k, (fix_op, lams)) =>
            (k, (fix_op, Func_Val (lams, val_store'', mutual_store, ~1)))
          )
          mutual_store
        )

        val val_store''' = insert_table (val_store'', fnc_store)

        fun match_first lams = (case lams of
          [] => NONE |
          (p, t) :: lams' =>
            (print ("match pattern: " ^ (to_string p) ^ " with\n" ^ (to_string result) ^ "\n");
            case (match_value_insert (val_store''', p, result)) of
              NONE => match_first lams' |
              SOME val_store'''' => (
                SOME (t, val_store'''')
              )
            )
        )

      in
        (case (match_first lams) of

          NONE => (
            [], Mode_Stick ("result - " ^ (to_string result) ^ " - does not match continuation hole pattern")
          ) |

          SOME (t_body, val_store'''') => (
            [(t_body, val_store'''', cont_stack', thread_id)],
            Mode_Continue  
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





  fun is_event_value t = (case t of
    Evt_Send_Val _ => true |
    Evt_Recv_Val _ => true |
    Evt_Wrap_Val _ => true |
    Evt_Choose_Val _ => true |
    _ => false
  )

  fun is_value t = (case t of
    Blank _ => true |
    List_Val _ => true |
    Func_Val _ => true | 
    Rec_Val _ => true |
    String_Val _ => true |
    Num_Val _ => true |
    Chan_Loc _ => true |
    Error _ => true |
    _ => is_event_value t 
  )



  fun hole i = Id (sym i, ~1)

  fun reduce_list (
    ts, push_f, reduce_f,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (let

    fun loop (prefix, postfix) = (case postfix of
      [] => (case (reduce_f prefix) of 
        Error msg => (
          Mode_Stick msg,
          [], (chan_store, block_store, sync_store, cnt)
        ) |

        result => (
          Mode_Reduce result,
          [(result, val_store, cont_stack, thread_id)],
          (chan_store, block_store, sync_store, cnt)
        )

      ) |

      x :: xs => (case x of
        (Id (id, _)) =>
          (case (find (val_store, id)) of
            SOME (NONE, v) => loop (prefix @ [v], xs) |
            _ => (
              Mode_Stick ("reduce list variable " ^ id ^ " cannot be resolved"),
              [], (chan_store, block_store, sync_store, cnt)
            )
          ) |

        _ =>
          (if is_value x then 
            loop (prefix @ [x], xs)
          else
            push (
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
    )

  in
    loop ([], ts)
  end)
  
  fun reduce_single (
    t, push_f, reduce_f,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (case t of
    (Id (id, _)) =>
      (case (find (val_store, id)) of
        SOME (NONE, v) => (
          Mode_Suspend,
          [(push_f v, val_store, cont_stack, thread_id)],
          (chan_store, block_store, sync_store, cnt)
        ) |

        _  =>
          (
            Mode_Stick ("reduce single variable " ^ id ^ " cannot be resolved")
            ,
            [], (chan_store, block_store, sync_store, cnt)
          )

      ) |

    _ =>
      (if is_value t then
        (case (reduce_f t) of
          Error msg => (
            Mode_Stick msg,
            [], (chan_store, block_store, sync_store, cnt)
          ) |

          result => (
            Mode_Reduce result,
            [(result, val_store, cont_stack, thread_id)],
            (chan_store, block_store, sync_store, cnt)
          )
        )
      else
        push (
            (t, (Contin_Norm, [( hole cnt, push_f (hole cnt) )], val_store, [])),
            val_store, cont_stack, thread_id,
            chan_store, block_store, sync_store, cnt + 1
          )
        )
      )


  fun apply (
    t_fn, t_arg, pos,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (case t_fn of
    (Id (id, _)) =>
      (case (find (val_store, id)) of
        SOME (NONE, v_fn) => (
          Mode_Suspend,
          [(Func_Elim (v_fn, t_arg, pos), val_store, cont_stack, thread_id)],
          (chan_store, block_store, sync_store, cnt)
        ) |
        _  => (
          Mode_Stick ("apply arg variable " ^ id ^ " cannot be resolved"),
          [], (chan_store, block_store, sync_store, cnt)
        )
      ) |

    Func_Val (lams, fnc_store, mutual_store, _) =>
      push (
        (t_arg, (Contin_Func_Elim, lams, fnc_store, mutual_store)),
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      ) |

    v =>
      (if is_value t_fn then
        (
          Mode_Stick ("application of non-function: " ^ (to_string v)),
          [], (chan_store, block_store, sync_store, cnt)
        )
      else
        push (
          (t_fn, (Contin_Norm, [( hole cnt, Func_Elim (hole cnt, t_arg, pos) )], val_store, [])),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt + 1
        )
      )
    

  )


  fun associate_right val_store (
    t1, id, rator, direc, prec, pos, t2
  ) = (case t1 of 
    Compo (Compo (t1', Id (id', pos'), p1'), t2', p2') =>
    (case (find (val_store, id')) of
      SOME (SOME (direc', prec'), rator') =>
      (if (prec' = prec) then
        (if direc = Right then
          associate_right val_store (
            t1',
            id, rator, direc, prec, pos',
            Func_Elim (rator, List_Val ([t2', t2], pos), pos)
          )  
        else 
          Func_Elim (rator, List_Val ([t1, t2], pos), pos)
        )
      else if (prec > prec') then
        associate_right val_store (
          t1',
          id', rator', direc', prec', pos',
          Func_Elim (rator, List_Val ([t2', t2], pos), pos)
        )  
      else
        Func_Elim (rator, List_Val ([t1, t2], pos), pos)
      ) |

      _ => Compo (Func_Elim (t1', Id (id', pos'), p1'), t2', p2')
    ) |

    _ =>
      Func_Elim (rator, List_Val ([t1, t2], pos), pos)
  )

  fun seq_step (
    md,
    (t, val_store, cont_stack, thread_id),
    (chan_store, block_store, sync_store, cnt)
  ) = (
    print ("seq_step@" ^ (Int.toString thread_id) ^ "\n" ^ (to_string t) ^ "\n");
    case t of

    Assoc (term, pos) => (
      Mode_Suspend,
      [(term, val_store, cont_stack, thread_id)],
      (chan_store, block_store, sync_store, cnt)
    ) |

    Id (id, pos) => (case (find (val_store, id)) of
      SOME (NONE, v) => (
        Mode_Suspend,
        [(v, val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      ) |

      _ => (
        Mode_Stick ("variable " ^ id ^ " cannot be resolved"),
        [], (chan_store, block_store, sync_store, cnt)
      )
    ) |

    List_Intro (t, t', pos) => reduce_list (
      [t, t'],
      (fn
        [t, t'] => List_Intro (t, t', pos) |
        _ => raise (Fail "Internal: List_Intro")
      ),
      (fn
        [v, Blank _] => List_Val ([v], pos) |
        [v, List_Val (ts, _)] => List_Val (v :: ts, pos) |
        _ => Error "cons with non-list"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    List_Val (ts, pos) => pop (
      List_Val (ts, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Func_Intro (lams, pos) => pop (
      Func_Val (lams, val_store, [], pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Func_Val (lams, [], mutual_store, pos) => pop (
      Func_Val (lams, val_store, mutual_store, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Func_Val (lams, fnc_store, mutual_store, pos) => pop (
      Func_Val (lams, fnc_store, mutual_store, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let

      val term = (case (find (val_store, id)) of
        SOME (SOME (direc, prec), rator) =>  (let
          val x = associate_right val_store (t1, id, rator, direc, prec, pos, t2)
        in
          x
        end ) |

        _ => (let
          val x = Compo (Func_Elim (t1, Id (id, pos), p1), t2, p2)
        in
          x
        end)
      )

    in
      (
        Mode_Suspend,
        [(term, val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      )
    end) |

    Compo (t1, t2, pos) => (
      Mode_Suspend,
      [(Func_Elim (t1, t2, pos), val_store, cont_stack, thread_id)],
      (chan_store, block_store, sync_store, cnt)
    ) |


    Func_Elim (t_fn, t_arg, pos) => apply (
      t_fn, t_arg, pos,
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Seq (t1, t2, _) => push (
      (t1, (Contin_Seq, [(hole cnt, t2)], val_store, [])),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt + 1
    ) |

    Rec_Intro (fields, pos) => (let
      val mutual_store = (List.mapPartial
        (fn
          (k, (fix_op,  Func_Intro (lams, _))) => 
            SOME (k, (fix_op, lams)) |
          _ => NONE
        )
        fields
      )
      
      (* embed mutual ids into ts' functions *)
      val fields' = (map
        (fn
          (k, (fix_op, Func_Intro (lams, pos))) =>
            (k, (fix_op, Func_Val (lams, val_store, mutual_store, pos))) |
          field => field 
        )
       fields 
      )
    in
      (
        Mode_Suspend,
        [(Rec_Val (fields', pos), val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      )
    end) |
    
    Rec_Val (fields, pos) => (let
      val ts = (map (fn (k, (fix_op, t)) => t) fields)

      fun f ts = (let
        val fields' = (List.map
          (fn ((key, (fix_op, _)), t) => (key, (fix_op, t)))
          (ListPair.zip (fields, ts))
        )
      in
        Rec_Val (fields',  pos)
      end)

    in
      reduce_list (
        ts, f, f, 
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      )
    end) |

    Rec_Elim (t, pos) => reduce_single (
      t,
      fn t => Rec_Elim (t, pos),
      (fn
        List_Val ([Rec_Val (fields, _), Id (key, _)], _) =>
        (case find (fields, key) of
          SOME (_, v) => v |
          NONE => Error "selection not found"
        ) |

        _ => Error "selecting from non-record"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Chan_Alloc (_, i) => (let
      val chan_store' = insert (chan_store, cnt, ([], []))
      val cnt' = cnt + 1
    in
      pop (
        Chan_Loc cnt,
        cont_stack, thread_id,
        chan_store', block_store, sync_store, cnt'
      )
    end) |

    (* internal rep *)
    Chan_Loc i => pop (
      Chan_Loc i,
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Evt_Send_Intro (t, pos) => reduce_single (
      t, fn t => Evt_Send_Intro (t, pos), fn v => Evt_Send_Val (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Evt_Send_Val (t, pos) => pop (
      Evt_Send_Val (t, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Evt_Recv_Intro (t, pos) => reduce_single (
      t, fn t => Evt_Recv_Intro (t, pos), fn v => Evt_Recv_Val (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Evt_Recv_Val (t, pos) => pop (
      Evt_Recv_Val (t, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Evt_Wrap_Intro (t, pos) => reduce_single (
      t, fn t => Evt_Wrap_Intro (t, pos), fn v => Evt_Wrap_Val (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Evt_Wrap_Val (t, pos) => pop (
      Evt_Wrap_Val (t, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Evt_Choose_Intro (t, pos) => reduce_single (
      t, fn t => Evt_Choose_Intro (t, pos), fn v => Evt_Choose_Val (v, pos),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) | 

    Evt_Choose_Val (t, pos) => pop (
      Evt_Choose_Val (t, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Evt_Elim (t, pos) => (case t of
      (Id (id, _)) => (case (find (val_store, id)) of
        SOME (NONE, v) => (
          Mode_Suspend,
          [(Evt_Elim (v, pos), val_store, cont_stack, thread_id)],
          (chan_store, block_store, sync_store, cnt)
        ) |

        _  => (
          Mode_Stick ("Evt_Elim argument variable " ^ id ^ " cannot be resolved"),
          [], (chan_store, block_store, sync_store, cnt)
        )

      ) |

      v => (if (is_event_value v) then
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
      else if (is_value v) then
        (
          Mode_Stick "sync with non-event",
          [], (chan_store, block_store, sync_store, cnt)
        )
      else 
        push (
          (t, (Contin_Norm, [( hole cnt, Evt_Elim (hole cnt, pos) )], val_store, [])),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt + 1
        )
      )
    ) |

    (* internal rep *)
    ThreadId i => pop (
      ThreadId i,
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |


    Spawn (t, pos) =>(case t of
      (Id (id, _)) => (case (find (val_store, id)) of
        SOME (_, v) => (
          Mode_Suspend,
          [(Spawn (v, pos), val_store, cont_stack, thread_id)],
          (chan_store, block_store, sync_store, cnt)
        ) |

        _  => (
          Mode_Stick ("Spawn argument variable " ^ id ^ " cannot be resolved"),
          [], (chan_store, block_store, sync_store, cnt)
        )


      ) |

      Func_Val ([(Blank _, t_body)], fnc_store, mutual_store, _) => (let
        val spawn_id = cnt
        val cnt' = cnt + 1
      in
        (
          Mode_Spawn t_body,
          [
            (List_Val ([], pos), val_store, cont_stack, thread_id),
            (t_body, val_store, [], spawn_id)
          ],
          (chan_store, block_store, sync_store, cnt')
        )
      end) |
      
      v => (if is_value v then
        (
          Mode_Stick "spawn with non-function",
          [], (chan_store, block_store, sync_store, cnt)
        )
      else
        push (
          (t, (Contin_Norm, [( hole cnt, Spawn (hole cnt, pos) )], val_store, [])),
          val_store, cont_stack, thread_id,
          chan_store, block_store, sync_store, cnt + 1
        )
      )
    ) |

    Blank pos => pop (
      Blank pos,
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    String_Val (str, pos) => pop (
      String_Val (str, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Num_Val (str, pos) => pop (
      Num_Val (str, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

    Num_Add (t, pos) => reduce_single (
      t, fn t => Num_Add (t, pos),
      (fn
        List_Val ([Num_Val (n1, _), Num_Val (n2, _)], _) => (
          print ("adding: " ^ n1 ^ " and " ^ n2 ^ "\n"); 
          Num_Val (add_num (n1, n2), pos)
        ) |
        _ => Error "adding non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Num_Sub (t, pos) => reduce_single (
      t, fn t => Num_Sub (t, pos),
      (fn
        List_Val ([Num_Val (n1, _), Num_Val (n2, _)], _) => (
          Num_Val (sub_num (n1, n2), pos)
        ) |
        _ => Error "subtracting non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Num_Mul (t, pos) => reduce_single (
      t, fn t => Num_Mul (t, pos),
      (fn
        List_Val ([Num_Val (n1, _), Num_Val (n2, _)], _) => (
          Num_Val (mul_num (n1, n2), pos)
        ) |
        _ => Error "multplying non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Num_Div (t, pos) => reduce_single (
      t, fn t => Num_Div (t, pos),
      (fn
        List_Val ([Num_Val (n1, _), Num_Val (n2, _)], _) => (
          Num_Val (div_num (n1, n2), pos)
        ) |
        _ => Error "dividing non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Num_Rem (t, pos) => reduce_single (
      t, fn t => Num_Rem (t, pos),
      (fn
        List_Val ([Num_Val (n1, _), Num_Val (n2, _)], _) => (
          Num_Val (rem_num (n1, n2), pos)
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

  fun to_string_from_mode md = (case md of
    Mode_Start => "Start" |
    Mode_Suspend => "Push/Suspend" |
    Mode_Reduce t => "Reduce" |
    Mode_Continue => "Pop/Continue" |
    Mode_Spawn t => "Spawn" |
    Mode_Block bevts => "Block" |
    Mode_Sync (thread_id, msg, send_id, recv_id) => "Sync" |
    Mode_Stick msg => "Stick: " ^ msg  |
    Mode_Finish => "Finish"
  )

  fun concur_step (
    md, threads, env 
  
  ) = (case threads of
    [] => (print "all done!\n"; NONE) |
    thread :: threads' => (let
      val (md', seq_threads, env') = (seq_step (md, thread, env)) 

      val _ = print ((to_string_from_mode md') ^ "\n")
      (*
      val _ = print (
        "# seq_threads: " ^
        (Int.toString (length seq_threads)) ^
        "\n"
      )
      *)
      val _ = print "\n" 
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

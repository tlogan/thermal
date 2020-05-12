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
    Log of (term * int) |

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

    Rec_Intro_Mutual of (
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
    Base_Evt_Send of (chan_id * term * contin_stack) |
    Base_Evt_Recv of (chan_id * contin_stack)

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
    Mode_Finish of term


  val surround_with = String.surround_with
  val surround = String.surround
(*
  fun surround tag body = (let
    val abc = "(" ^ tag
    val bodyLines = String.tokens (fn c => c = #"\n") body
    val indentedLines = map (fn l => "  " ^ l) bodyLines
    val indentedBody = String.concatWith "\n" indentedLines 
    val xyz = if body = "" then ")" else "\n" ^ indentedBody ^ ")"
  in
    abc ^ xyz 
  end)
*)

  fun from_infix_option_to_string fix_op = (case fix_op of
    SOME (Left, d) => " infixl d" ^ (Int.toString d) |
    SOME (Right, d) => " infixr d" ^ (Int.toString d) |
    NONE => ""
  )


  fun to_string t = (case t of


    Assoc (t, pos) => "(" ^ (to_string t) ^ ")" |

    Log (t, pos) => "log " ^  (to_string t) |

    List_Intro (t1, t2, pos) => (
      (to_string t1) ^ ", " ^ (to_string t2)
    ) |

    List_Val (ts, pos) => surround "" ( 
      String.concatWith "\n" (List.map (fn t => "# " ^ (to_string t)) ts)
    ) |

    Func_Intro (lams, pos) => String.surround "" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Func_Val (lams, fnc_store, mutual_store, pos) => String.surround "val" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Compo (t1, t2, pos) => (to_string t1) ^ " " ^ (to_string t2) |

    Func_Elim (t1, t2, pos) => surround "apply" (
      (to_string t1) ^ " " ^ (to_string t2)
    ) |

    Seq (t1, t2, pos) => (to_string t1) ^ ";\n" ^ (to_string t2) |

    Rec_Intro (fs, pos) => String.surround "" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Rec_Intro_Mutual (fs, pos) => String.surround "mutual" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Rec_Val (fs, pos) => String.surround "val" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Rec_Elim (t, pos) => "select " ^ (to_string t) |

    Chan_Alloc (t, pos) => "alloc_chan " ^ (to_string t) |

    Evt_Send_Intro (t, pos) => "send" ^ (to_string t) |

    Evt_Send_Val (t, pos) => "send_val" ^ (to_string t) |

    Evt_Recv_Intro (t, pos) => "recv" ^ (to_string t) |
    
    Evt_Recv_Val (t, pos) => "recv_val" ^ (to_string t) |

    Evt_Wrap_Intro (t, pos) => "wrap " ^ (to_string t) |

    Evt_Wrap_Val (t, pos) => "wrap_val " ^ (to_string t) |

    Evt_Choose_Intro (t, pos) => "choose " ^ (to_string t) |

    Evt_Choose_Val (t, pos) => "choose_val " ^ (to_string t) |

    Evt_Elim (t, pos) => "sync " ^ (to_string t) |

    Spawn (t, pos) => "spawn " ^ (to_string t) |

    Par (t, pos) =>  surround_with "<|" "" (to_string t) "|>" |

    Sym (t, pos) => "sym " ^ (to_string t) |
  
    Blank pos => "()" |

    Id (name, pos) => name |

    String_Val (str, pos) => str |

    Chan_Loc i => "chan_loc_" ^ (Int.toString i) |

    ThreadId i => "thread_" ^ (Int.toString i) |


    Num_Val (num, pos) => num |

    Num_Add (t, pos) => "add " ^ (to_string t) |

    Num_Sub (t, pos) => "sub " ^ (to_string t) |

    Num_Mul (t, pos) => "mul " ^ (to_string t) |

    Num_Div (t, pos) => "div " ^ (to_string t) |

    _ =>
      "(NOT YET IMPLEMENTED)"

  )

  and from_lam_to_string (t1, t2) = String.surround "" (
    "case "  ^ (to_string t1) ^ " => " ^ (to_string t2)
  )

  and from_field_to_string (name, (fix_op, t)) = String.surround "" (
    "def "  ^ name ^ (from_infix_option_to_string fix_op) ^ " : " ^ (to_string t)
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

  fun num_add (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 + i2
    val str = Int.toString i3
  in
    str
  end)

  fun num_sub (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 - i2
  in
    Int.toString i3
  end)

  fun num_mul (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 * i2
  in
    Int.toString i3
  end)


  fun num_div (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = i1 div i2
  in
    Int.toString i3
  end)

  fun num_rem (n1, n2) = (let
    val i1 = (valOf o Int.fromString) n1
    val i2 = (valOf o Int.fromString) n2
    val i3 = Int.rem (i1, i2)
  in
    Int.toString i3
  end)


  fun mk_base_events (evt, cont_stack) = (case evt of
  
    Evt_Send_Val (List_Val ([Chan_Loc i, msg], _), pos) =>
      [Base_Evt_Send (i, msg, [])] |
  
    Evt_Recv_Val (Chan_Loc i, pos) =>
      [Base_Evt_Recv (i, [])] |

    Evt_Choose_Val (List_Val (values, _), pos) =>
      mk_base_events_from_list (values, cont_stack) |

    Evt_Wrap_Val (List_Val ([evt', Func_Val (lams, fnc_store, mutual_store, _)], _), pos) =>
      let
        val bevts = mk_base_events (evt', cont_stack)
      in
        (List.foldl
          (fn (bevt, bevts_acc) => let
  
            val cont = (Contin_Evt_Elim, lams, fnc_store, mutual_store)
  
            val bevt' = (case bevt of
              Base_Evt_Send (i, msg, wrap_stack) => 
                Base_Evt_Send (i, msg, cont :: wrap_stack) |
              Base_Evt_Recv (i, wrap_stack) => 
                Base_Evt_Recv (i, cont :: wrap_stack)
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
    Base_Evt_Send (i, msg, _) =>
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
  
     Base_Evt_Recv (i, _) =>
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

    Base_Evt_Send (i, msg, wrap_stack) =>
    (let
      val chan_op = find (chan_store, i)
      val recv_op = (case chan_op of
        SOME (_, (block_id, recv_stack, recv_thread_id) :: recvs) =>
          SOME (recv_stack, recv_thread_id) | 
        SOME (_, []) => NONE |
        NONE => NONE
      )
      val (threads, md') = (case recv_op of
        NONE => ([], Mode_Stick "transact Base_Evt_Send") |
        SOME (recv_stack, recv_thread_id) => (
          [
            (Blank 0, empty_table, wrap_stack @ cont_stack, thread_id),
            (msg, empty_table, recv_stack, recv_thread_id)
          ],
          Mode_Sync (i, msg, thread_id, recv_thread_id)
        )
      ) 

      val chan_store' = (case chan_op of
        SOME (sends, []) => insert (chan_store, i, (sends, [])) |
        SOME (sends, recv :: recvs) => insert (chan_store, i, (sends, recvs)) |
        NONE => chan_store 
      )

    in
      (
        md', 
        threads,
        (chan_store', block_store, sync_store, cnt)
      ) 
    end) |
  
    Base_Evt_Recv (i, wrap_stack) =>
    (let
      val chan_op = find (chan_store, i)
      val send_op = (case chan_op of
        SOME ((block_id, send_stack, msg, send_thread_id) :: sends, _) =>
          SOME (send_stack, msg, send_thread_id) | 
        SOME ([], _) => NONE |
        NONE => NONE
      )
  
      val (threads, md') = (case send_op of
        NONE => ([], Mode_Stick "transact Base_Evt_Recv") |
        SOME (send_stack, msg, send_thread_id) => (
          [
            (Blank 0, empty_table, send_stack, send_thread_id),
            (msg, empty_table, wrap_stack @ cont_stack, thread_id)
          ],
          Mode_Sync (i, msg, send_thread_id, thread_id)
        )
      )


      val chan_store' = (case chan_op of
        SOME ([], recvs) => insert (chan_store, i, ([], recvs)) |
        SOME (send :: sends, recvs) => insert (chan_store, i, (sends, recvs)) |
        NONE => chan_store 
      )
    in
      (
        md',
        threads,
        (chan_store', block_store, sync_store, cnt)
      )
    end)
  
  )
  
  fun block_one (bevt, cont_stack, chan_store, block_id, thread_id) = (case bevt of
    Base_Evt_Send (i, msg, wrap_stack) =>
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
  
    Base_Evt_Recv (i, wrap_stack) =>
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
      [] => ([], Mode_Finish result)|
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
            (case (match_value_insert (val_store''', p, result)) of
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
    ts, norm_f, reduce_f,
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
            (push (
              (
                x,
                (
                  Contin_Norm,
                  [( hole cnt, norm_f (prefix @ (hole cnt :: xs)) )],
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
    )

  in
    loop ([], ts)
  end)
  
  fun reduce_single (
    t, norm_f, reduce_f,
    val_store, cont_stack, thread_id,
    chan_store, block_store, sync_store, cnt
  ) = (case t of
    (Id (id, _)) =>
      (case (find (val_store, id)) of
        SOME (NONE, v) => (
          Mode_Suspend,
          [(reduce_f v, val_store, cont_stack, thread_id)],
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
          (t, (Contin_Norm, [( hole cnt, norm_f (hole cnt) )], val_store, [])),
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
        SOME (_, v_fn) => (
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


(*


  (* TODO: rewrite associate_infix to take only one term as input, check for infix inside*)
  fun associate_infix val_store (
    t1, id, rator, direc, prec, pos, t2
  ) = (case t1 of 
    Compo (Compo (t1', Id (id', pos'), p1'), t2', p2') =>
    (case (find (val_store, id')) of
      SOME (SOME (direc', prec'), rator') =>
      (if (prec' = prec) then
        (if direc = Right then
          associate_infix val_store (
            t1',
            id, rator, direc, prec, pos',
            Compo (Compo (t2', Id (id, pos), pos), t2, pos)
          )  
        else 

          (* TODO: recurse on sub term and compose with other *)
          Func_Elim (rator, List_Intro (t1, List_Intro (t2, Blank 0, pos), pos), pos)
        )
      else if (prec > prec') then
        associate_infix val_store (
          t1',
          id', rator', direc', prec', pos',
          Func_Elim (rator, List_Intro (t2', List_Intro (t2, Blank 0, pos), pos), pos)
        )  
      else
        Func_Elim (rator, List_Intro (t1, List_Intro (t2, Blank 0, pos), pos), pos)
      ) |

      _ => Compo (Func_Elim (t1', Id (id', pos'), p1'), t2', p2')
    ) |

    _ =>
      Func_Elim (rator, List_Intro (t1, List_Intro (t2, Blank 0, pos), pos), pos)
  )

*)

      (*

  fun associate_infix val_store t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) =>
      (case (find (val_store, id)) of
        SOME (SOME (direc, prec), rator) => (* do previous funcition work *)
        (case t1 of 
          Compo (Compo (t1', Id (id', pos'), p1'), t2', p2') =>
          (case (find (val_store, id')) of
            SOME (SOME (direc', prec'), rator') =>
            (if (prec' = prec) then
              (if direc = Right then
                associate_infix val_store (
                  Compo (t1', Compo (Id (id', pos'),
                    Compo (Compo (t2', Id (id, pos), p2'), t2, p2),
                    p1' 
                  ), p1)
                )  
              else 
                associate_infix val_store (
                  Compo (Compo ((associate_infix val_store t1), Id (id, pos), p1), t2, p2)
                )
                (* TODO: recurse on sub term and compose with other *)
                Func_Elim (rator, List_Intro (t1, List_Intro (t2, Blank 0, pos), pos), pos)
              )
            else if (prec > prec') then
              associate_infix val_store (
                t1',
                id', rator', direc', prec', pos',
                Func_Elim (rator, List_Intro (t2', List_Intro (t2, Blank 0, pos), pos), pos)
              )  
            else
              Func_Elim (rator, List_Intro (t1, List_Intro (t2, Blank 0, pos), pos), pos)
            ) |

            _ => Compo (Func_Elim (t1', Id (id', pos'), p1'), t2', p2')
          ) |

          _ =>
            Func_Elim (rator, List_Intro (t1, List_Intro (t2, Blank 0, pos), pos), pos)
        ) |

        _ => (
          Compo (Func_Elim (t1, Id (id, pos), p1), t2, p2)
        )
      )
    _ => t
  )
      *)



  fun associate_infix val_store t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t1' = associate_infix val_store t1
    in
      (case (find (val_store, id)) of
        SOME (SOME (direc, prec), rator) => (case t1' of 
          Compo (Compo (t1a, Id (id1, pos1), p1a), t1b, p1b) =>
          (case (find (val_store, id1)) of
            SOME (SOME (direc', prec'), rator') =>
            (if (prec' = prec andalso direc = Right) orelse (prec > prec') then
              Compo (
                Compo (t1a, Id (id1, pos1), p1a),
                associate_infix val_store (Compo (Compo (t1b, Id (id, pos), p1b), t2, p2)),
                p1
              )
            else 
              Compo (Compo (t1', Id (id, pos), p1), t2, p2)
            ) |

            _ => (let
              val t1'' = Compo (Func_Elim (t1a, Id (id1, pos1), p1a), t1b, p1b)
            in
              Compo (Compo (t1'', Id (id, pos), p1), t2, p2)
            end)
          ) |

          _ => Compo (Compo (t1', Id (id, pos), p1), t2, p2)
        ) |

        _ => (
          Compo (Func_Elim (t1', Id (id, pos), p1), t2, p2)
        )
      )
    end) |

    _ => t
  )

  fun to_func_elim val_store t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (
      (case (find (val_store, id)) of
        SOME (SOME (direc, prec), rator) => (
          Func_Elim (
            Id (id, pos),
            List_Intro (
              to_func_elim val_store t1,
              List_Intro (to_func_elim val_store t2, Blank 0, pos),
              pos
            ),
            pos
          )
        ) |

        _ => (
          Func_Elim (
            Func_Elim (to_func_elim val_store t1, Id (id, pos), p1),
            to_func_elim val_store t2,
            p2
          )
        )
      )
    ) |
    _ => t
  )

  fun seq_step (
    md,
    (t, val_store, cont_stack, thread_id),
    (chan_store, block_store, sync_store, cnt)
  ) = (
    (*print ("\n(*** thread " ^ (Int.toString thread_id) ^ " ***)\n" ^ (to_string t) ^ "\n\n");*)
    case t of

    Assoc (term, pos) => (
      Mode_Reduce term,
      [(term, val_store, cont_stack, thread_id)],
      (chan_store, block_store, sync_store, cnt)
    ) |

    Log (t, pos) => reduce_single (
      t,
      fn t => Log (t, pos),
      fn v => (
        print ((to_string v) ^ "\n");
        v
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
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
      val t_m = associate_infix val_store t
      val t' = to_func_elim val_store t_m 
    in
      (
        Mode_Reduce t',
        [(t', val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      )
    end) |

    Compo (t1, t2, pos) => (
      Mode_Reduce (Func_Elim (t1, t2, pos)),
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
        [(Rec_Intro_Mutual (fields', pos), val_store, cont_stack, thread_id)],
        (chan_store, block_store, sync_store, cnt)
      )
    end) |
    
    Rec_Intro_Mutual (fields, pos) => (let
      val ts = (map (fn (k, (fix_op, t)) => t) fields)

      fun f con ts = (let
        val fields' = (List.map
          (fn ((key, (fix_op, _)), t) => (key, (fix_op, t)))
          (ListPair.zip (fields, ts))
        )
      in
        con (fields',  pos)
      end)


    in
      reduce_list (
        ts, f Rec_Intro_Mutual, f Rec_Val, 
        val_store, cont_stack, thread_id,
        chan_store, block_store, sync_store, cnt
      )
    end) |

    Rec_Val (fields, pos) => pop (
      Rec_Val (fields, pos),
      cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt
    ) |

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
        List_Val ([Num_Val (n1, _), Num_Val (n2, _)], _) =>
          Num_Val (num_add (n1, n2), pos) |
        _ => Error "adding non-numbers"
      ),
      val_store, cont_stack, thread_id,
      chan_store, block_store, sync_store, cnt

    ) |

    Num_Sub (t, pos) => reduce_single (
      t, fn t => Num_Sub (t, pos),
      (fn
        List_Val ([Num_Val (n1, _), Num_Val (n2, _)], _) => (
          Num_Val (num_sub (n1, n2), pos)
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
          Num_Val (num_mul (n1, n2), pos)
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
          Num_Val (num_div (n1, n2), pos)
        ) |
        _ => Error "dividing non-numbers"
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

  fun to_string_from_mode md = "----" ^ (case md of
    Mode_Start => "Start" |
    Mode_Suspend => "Push/Suspend" |
    Mode_Reduce t => "Reduce" |
    Mode_Continue => "Pop/Continue" |
    Mode_Spawn t => "Spawn" |
    Mode_Block bevts => "Block" |
    Mode_Sync (thread_id, msg, send_id, recv_id) => "Sync" |
    Mode_Stick msg => "Stick: " ^ msg  |
    Mode_Finish t => "Finish: " ^ (to_string t)
  ) ^ "----"

  fun concur_step (
    md, threads, env 
  
  ) = (case threads of
    [] => ( (*print "all done!\n";*) NONE) |
    thread :: threads' => (let
      val (md', seq_threads, env') = (seq_step (md, thread, env)) 

      
      (*
      val _ = print ((to_string_from_mode md') ^ "\n")
      *)
      (*
      val _ = print (
        "# seq_threads: " ^
        (Int.toString (length seq_threads)) ^
        "\n"
      )
      *)
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

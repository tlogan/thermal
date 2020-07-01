structure Tree = struct

  type chan_id = int
  type thread_id = int

  type ('a, 'b) store = ('a * 'b) list

  datatype left_right = Left | Right

  type infix_option = (left_right * int) option

  datatype event_Intro = 
    Alloc_Chan_Intro |
    Send_Intro |
    Recv_Intro |
    Latch_Intro |
    Choose_Intro |
    Offer_Intro |
    Block_Intro

  datatype effect_intro =
    Return_Intro |
    Sync_Intro |
    Bind_Intro |
    Exec_Intro

  datatype contin_mode = Contin_With | Contin_Norm | Contin_App | Contin_Sync

  datatype term = 
    Blank_Intro of int |
    Sym of (term * int) |
    Id of (string * int) |
    Assoc of (term * int) |
    Log of (term * int) |

    List_Intro of (term * term * int) |

    Func_Intro of (
      ((term * term) list) *
      int
    ) (* Func_Intro (lams, pos) *) |


    App of (term * term * int) |

    Compo of (term * term * int) |
    With of (term * term * int) |

    Rec_Intro of (
      ((string * (infix_option * term)) list) *
      int
    ) (* Rec_Intro (fields, pos) *) |

    Rec_Intro_Mutual of (
      ((string * (infix_option * term)) list) *
      int
    ) (* Rec_Intro (fields, pos) *) |


    Select of (term * int) |
  

    Event_Intro of (event_intro * term * int) |


    Effect_Intro of (effect_intro * term * int) |

    Num_Intro of (string * int) |

    Num_Add of (term * int) |
    Num_Sub of (term * int) |
    Num_Mul of (term * int) |
    Num_Div of (term * int) |

    String_Intro of (string * int) |

    Value of value 

  and value =
    Blank_Value |
    List_Value of (value list * int) |

    Func_Value of (
      ((term * term) list) *
      ((string, infix_option * value) store) *
      ((string, infix_option * ((term * term) list)) store) *
      int
    ) (* Func_Value (lams, value_store, mutual_store, pos) *) |

    Rec_Value of (
      ((string * (infix_option * value)) list) *
      int
    ) (* Rec_Intro (fields, pos) *) |


    Event of transaction list |

    Effect of effect |
  
    String_Value of (string * int) |

    Num_Value of (string * int) |
    Chan_Loc of int |
    ThreadId of int |
    Error of string


  and event =
    Alloc_Chan | 
    Send of chan_id * value |
    Recv of chan_id |
    Offer of value |
    Block

  and effect =
    Return of value |
    Sync of transaction list |
    Bind of effect_value * contin list |
    Exec of effect_value

  and transaction = Tx of event_value * contin list 

  and contin = Contin of (
    contin_mode * 
    ((term * term) list) *
    ((string, infix_option * term) store) *
    ((string, infix_option * (term * term) list) store)
  )


  type thread_config = (
    (string, infix_option * value) store *
    contin list *
    thread_id 
  )

  type sender = (int * contin list * value * thread_id)
    (* (block_id, contin_stack, msg, thread_id) *)
  type receiver = (int * contin list * thread_id)

  type channel = sender * receiver 

  type global_config = (
    (thread_id, channel) store *
    (int, unit) *
    int
  )
  (* (chan_store, block_store, cnt) *)

  type term_step = (
    term *
    thread_config *
    global_config
  )
  (*
    t,
    (value_store, contin_stack, thread_id),
    (chan_store, block_store, cnt)
  *)


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

  fun event_to_string evt = (case evt of
    Alloc_Chan => "alloc_chan" |
    Send => "send" |
    Recv => "recv" |
    Latch => "latch" |
    Choose => "choose" |
    Offer => "offer" | 
    Block => "block"
  )

  fun to_string t = (case t of

    Assoc (t, pos) => "(" ^ (to_string t) ^ ")" |

    Log (t, pos) => "log " ^  (to_string t) |
    Sym (t, pos) => "sym " ^ (to_string t) |

    List_Intro (t1, t2, pos) => (
      (to_string t1) ^ ", " ^ (to_string t2)
    ) |

    Func_Intro (lams, pos) => String.surround "" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Compo (t1, t2, pos) => "(compo " ^ (to_string t1) ^ " " ^ (to_string t2) ^")"|

    App (t1, t2, pos) => surround "apply" (
      (to_string t1) ^ " " ^ (to_string t2)
    ) |

    With (t1, t2, pos) => "with " ^ (to_string t1) ^ "\n" ^ (to_string t2) |

    Rec_Intro (fs, pos) => String.surround "" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Rec_Intro_Mutual (fs, pos) => String.surround "mutual" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Select (t, pos) => "select " ^ (to_string t) |

    Event_Intro (evt, t, pos) => "evt " ^ (event_to_string evt) ^ (to_string t) |

    Value v => value_to_string v |

    _ => "(NOT YET IMPLEMENTED)"

    (*
    Sync (t, pos) => "sync " ^ (to_string t) |

    Exec (t, pos) => "exec " ^ (to_string t) |

    Blank_Intro pos => "()" |

    Id (name, pos) => name |

    String_Value (str, pos) => str |

    Chan_Loc i => "chan_loc_" ^ (Int.toString i) |

    ThreadId i => "thread_" ^ (Int.toString i) |

    Num_Add (t, pos) => "add " ^ (to_string t) |

    Num_Sub (t, pos) => "sub " ^ (to_string t) |

    Num_Mul (t, pos) => "mul " ^ (to_string t) |

    Num_Div (t, pos) => "div " ^ (to_string t) |

    Error msg => "(ERROR: " ^ msg ^ ")"
    *)

  )

  and value_to_string v = (case v of

    List_Value (vs, pos) => surround "" ( 
      String.concatWith "\n" (List.map (fn v => "# " ^ (value_to_string v)) vs)
    ) |

    Func_Value (lams, fnc_store, mutual_store, pos) => String.surround "val" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Rec_Value (fs, pos) => String.surround "val" (
      String.concatWith ",\n" (List.map from_field_value_to_string fs)
    ) |

    Event_Value transactions => String.surround "evt" (
      String.concatWith "\n" (List.map transaction_to_string transactions)
    ) |

    _ => "(NOT YET IMPLEMENTED)"
  )

  and from_field_value_to_string (name, (fix_op, v)) = String.surround "" (
    "def "  ^ name ^ (from_infix_option_to_string fix_op) ^ " : " ^ (value_to_string v)
  )


  and from_lam_to_string (t1, t2) = String.surround "" (
    "case "  ^ (to_string t1) ^ " => " ^ (to_string t2)
  )

  and from_field_to_string (name, (fix_op, t)) = String.surround "" (
    "def "  ^ name ^ (from_infix_option_to_string fix_op) ^ " : " ^ (to_string t)
  )

  and transaction_to_string (Tx (evt, wrap_stack)) =
    String.surround "transaction" (
      (event_value_to_string evt) ^ "\n" ^ (stack_to_string wrap_stack)
    )

  and event_value_to_string evt = (case evt of  

    Alloc_Chan_Value => "alloc_chan" |

    Send_Value (i, msg) => String.surround "send_value " (
      (Int.toString i) ^ (value_to_string msg)
    ) |

    Recv_Value i => String.surround "recv_value " (
      (Int.toString i)
    ) |

    _ => "(NOT IMPLE: event_value_to_string)"

  )


  and stack_to_string stack = (String.surround "stack" (
    String.concatWith "\n" (map contin_to_string stack)
  ))

  and contin_to_string cont = "CONTIN TODO"


  val empty_table = [] 

  fun insert (table, key, item) = (
    (key, item) :: table
  )
  
  
  fun insert_table (value_store_base, value_store_top) = (
    value_store_top @ value_store_base
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



  fun match_symbolic_term_insert value_store (pattern, symbolic_term) = (case (pattern, symbolic_term) of
    (Blank_Intro _, _) => SOME value_store |

    (Sym (Id (id, _), _), _) => (let
      val thunk = Func_Value ([(Blank_Intro ~1, symbolic_term)], value_store, [], ~1)
    in
      SOME (insert (value_store, id, (NONE, thunk)))
    end) |

    (Id (p_id, _), Id (st_id, _)) =>
    (if p_id = st_id then
      SOME value_store
    else
      NONE
    ) |

    (Assoc (p, _), Assoc (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Log (p, _), Log (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (List_Intro (p1, p2, _), List_Intro (st1, st2, _)) => (
      (Option.mapPartial
        (fn value_store' =>
          match_symbolic_term_insert value_store' (p2, st2)
        )
        (match_symbolic_term_insert value_store (p1, st1))
      )
    ) |

    (Func_Intro (p_lams, _), Func_Intro (st_lams, _)) =>
      from_lams_match_symbolic_term_insert value_store (p_lams, st_lams) |

    (App (p1, p2, _), App (st1, st2, _)) =>
    (Option.mapPartial
      (fn value_store' =>
        match_symbolic_term_insert value_store' (p2, st2)
      )
      (match_symbolic_term_insert value_store (p1, st1))
    ) |

    (Compo (p1, p2, _), Compo (st1, st2, _)) => (
      (Option.mapPartial
        (fn value_store' =>
          match_symbolic_term_insert value_store' (p2, st2)
        )
        (match_symbolic_term_insert value_store (p1, st1))
      )
    ) |

    (With (p1, p2, _), With (st1, st2, _)) =>
    (Option.mapPartial
      (fn value_store' =>
        match_symbolic_term_insert value_store' (p2, st2)
      )
      (match_symbolic_term_insert value_store (p1, st1))
    ) |


    (Rec_Intro_Mutual (p_fields, _), Rec_Intro_Mutual (st_fields, _)) =>
      from_fields_match_symbolic_term_insert value_store (p_fields, st_fields) |


    (Select (p, _), Select (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Event_Intro (p_evt, p, _), Event_Intro (st_evt, st, _)) =>
      if p_evt = st_evt then match_symbolic_term_insert value_store (p, st)
      else NONE |

(*
TODO:
    (Event_Value p_transactions, Event_Value st_transactions) =>
      match_symbolic_transactions_insert value_store (p_transactions, st_transactions) |
*)

(*
**    (List_Value (ps, _), Value (List_Value (sts, _))) =>
**    (if (List.length ps = List.length sts) then
**      (List.foldl
**        (fn ((p, st), value_store_op) => 
**          (Option.mapPartial
**            (fn value_store' =>
**              match_symbolic_term_insert value_store' (p, st)
**            )
**            value_store_op
**          )
**        )
**        (SOME value_store)
**        (ListPair.zip (ps, sts))
**      )
**    else
**      NONE
**    ) |
*)


    (Value p_v, Value st_v) =>
    (if p_v = st_v then
      SOME value_store
    else
      NONE
    ) |

    (Effect_Intro (p_effect, p, _), Effect_Intro (st_effect, st, _)) =>
      if p_effect = st_effect then match_symbolic_term_insert value_store (p, st)
      else NONE |

    (Num_Add (p, _), Num_Add (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Num_Sub (p, _), Num_Sub (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Num_Mul (p, _), Num_Mul (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Num_Div (p, _), Num_Div (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    _ => (
      NONE
    )


  )

  and from_lams_match_symbolic_term_insert value_store (p_lams, st_lams) = 
  (if (List.length p_lams = List.length st_lams) then
    (List.foldl
      (fn (((p1, p2), (st1, st2)), value_store_op) =>
        (Option.mapPartial
          (fn value_store' =>
            (Option.mapPartial
              (fn value_store' =>
                match_symbolic_term_insert value_store' (p2, st2)
              )
              (match_symbolic_term_insert value_store (p1, st1))
            )
          )
          value_store_op
        )
      )
      (SOME value_store)
      (ListPair.zip (p_lams, st_lams))
    )
  else
    NONE
  )

  and from_fields_match_symbolic_term_insert value_store (p_fields, st_fields) =
  (if (List.length p_fields = List.length st_fields) then
    (List.foldl
      (fn (((p_key, (p_fop, p)), (st_key, (st_fop, st))), value_store_op) =>
        (if p_key = st_key andalso p_fop = st_fop then
          (Option.mapPartial
            (fn value_store' =>
              match_symbolic_term_insert value_store (p, st)
            )
            value_store_op
          )
        else 
          NONE
        )
      )
      (SOME value_store)
      (ListPair.zip (p_fields, st_fields))
    )
  else
    NONE
  )

  fun match_value_insert (value_store, pat, value) = (case (pat, value) of

    (Assoc (pat', _), _) =>
      match_value_insert (value_store, pat', value) |

    (Blank_Intro _, _) =>
      SOME value_store |

    (Id (str, _), v) =>
      SOME (insert (value_store, str, (NONE, v))) |

    (List_Intro (t, t', _), List_Value (v :: vs, _)) =>
      (Option.mapPartial
        (fn value_store' =>
          match_value_insert (value_store', t, v)
        )
        (match_value_insert (value_store, t', List_Value (vs, ~1)))
      ) |

    (Rec_Intro (p_fields, _), Rec_Value (v_fields, _)) => (
      from_fields_match_value_insert value_store (p_fields, v_fields)
    ) |

    (Func_Intro ([(Blank_Intro _, p_body)], _), Func_Value ([(Blank_Intro _, st_body)], _, _, _)) => (
      (* function value's local stores are ignored; only syntax is matched; *)
      (* it's up to the user to determine if syntax can actually be evaluated in alternate context *)
      (* variables in pattern are specified by pattern_var syntax (sym f); *)
      (* it may then be used in new context and evaluated with f () *) 
      match_symbolic_term_insert value_store (p_body, st_body)
    ) |


    (Num_Intro (n, _), Num_Value (nv, _)) => (
      if n = nv then
        SOME value_store
      else
        NONE
    ) |

    _ => NONE

    (* **TODO**

    (List_Value ([], _), List_Value ([], _)) => SOME value_store | 

    (List_Value (t :: ts, _), List_Value (v :: vs, _)) =>
      (Option.mapPartial
        (fn value_store' =>
          match_value_insert (value_store', t, v)
        )
        (match_value_insert (value_store, List_Value (ts, ~1), List_Value (vs, ~1)))
      ) |


    (Event_Send_Intro (t, _), Event_Send_Intro (v, _)) =>
      match_value_insert (value_store, t, v) |

    (Event_Recv_Intro (t, _), Event_Recv_Intro (v, _)) =>
      match_value_insert (value_store, t, v) |

    (Func_Value p_fnc, Func_Value v_fnc) => (
      if fnc_equal (p_fnc, v_fnc) then
        SOME value_store
      else
        NONE
    ) |

    (String_Value (str, _), String_Value (strv, _)) => (
      if str = strv then
        SOME value_store
      else
        NONE
    ) |

    (Rec_Intro (p_fields, _), Rec_Intro (v_fields, _)) => (case (p_fields, v_fields) of
      ([], []) =>
        SOME value_store |

      ((pk, t) :: ps, _ :: _) => (let
        val (match, remainder) = (List.partition  
          (fn (k, v) => k = pk)
          v_fields
        )
      in
        (case match of
          [(k, v)] => (Option.mapPartial
            (fn value_store' => match_value_insert (value_store', t, v))
            (match_value_insert (value_store, Rec_Intro (ps, ~1), Rec_Intro (remainder, ~1)))
          ) |

          _ => NONE
        )
      end) |

      _ =>
        NONE
      
    ) |

    *)
  )

  and from_fields_match_value_insert value_store (p_fields, v_fields) = (case p_fields of
    [] => SOME value_store |
    (pname, (pfix_op, p)) :: pfs => (let
      val (key_matches, vfs) = (List.partition
        (fn (vname, (vfix_op, _)) =>
          pname = vname andalso
          (pfix_op = vfix_op orelse pfix_op = NONE)
        )
        v_fields
      )
      fun match_term key_matches = (case key_matches of
        [] => NONE |
        [(vname,(vfix_op, v))] => (Option.mapPartial
          (fn value_store' =>
            SOME (insert (value_store', vname, (vfix_op, v)))
          )
          (match_value_insert (value_store, p, v))
        ) |
        _ :: key_matches' => match_term key_matches'
      )
      val value_store_op = match_term key_matches
    in
      (Option.mapPartial
        (fn value_store' =>
          from_fields_match_value_insert value_store' (pfs, vfs)
        )
        value_store_op
      )
    end)
  )








  fun sym i = "_g_" ^ (Int.toString i)

  fun hole i = Id (sym i, ~1)

  fun push (
    (t_arg, cont),
    value_store, contin_stack,
    cnt
  ) = (let
    val contin_stack' = cont :: contin_stack

  in
    (
      t_arg, value_store, contin_stack',
      cnt
    )
  end)

  fun pop (result, contin_stack, cnt) = (case contin_stack of
    [] => (
      raise (Fail "Internal Error: pop called with value and empty stack")
    ) |
    (cmode, lams, value_store', mutual_store) :: contin_stack' => (let

      val value_store'' = (case result of
        Rec_Value (fields, _) => (if cmode = Contin_With then
          insert_table (value_store', fields)
        else
          value_store'
        ) |
        _ => value_store'
      )

      (* embed mutual_store within self's functions *)
      val fnc_store = (map 
        (fn (k, (fix_op, lams)) =>
          (k, (fix_op, Func_Value (lams, value_store'', mutual_store, ~1)))
        )
        mutual_store
      )

      val value_store''' = insert_table (value_store'', fnc_store)

      fun match_first lams = (case lams of
        [] => NONE |
        (p, t) :: lams' =>
          (case (match_value_insert (value_store''', p, result)) of
            NONE => match_first lams' |
            SOME value_store'''' => (
              SOME (t, value_store'''')
            )
          )
      )

      val next_term = (case (match_first lams) of

        NONE => Error (
          "result - " ^
          (value_to_string result) ^
          " - does not match continuation hole pattern"
        ) |

        SOME (t_body, value_store'''') => (
          t_body, 
          value_store'''', contin_stack'
          cnt
        )
      )

    in
      (
        next_term, 
        value_store'''', contin_stack', cnt
      )
    end)
  )




  fun apply (
    t_fn, t_arg, pos,
    value_store, contin_stack,
    cnt
  ) = (case t_fn of
    (Id (id, _)) =>
      (case (find (value_store, id)) of
        SOME (_, v_fn) => (
          App (Value v_fn, t_arg, pos), 
          value_store, contin_stack
          cnt
        ) |

        _  => SOME (_, v_fn) => (
          Error ("apply arg variable " ^ id ^ " cannot be resolved"),
          value_store, contin_stack,
          cnt
        ) |

      ) |

    Value (Func_Value (lams, fnc_store, mutual_store, _)) =>
      push (
        (t_arg, (Contin_App, lams, fnc_store, mutual_store)),
        value_store, contin_stack,
        cnt
      ) |

    Value v => (
      Error ("application of non-function: " ^ (value_to_string v)) |
      value_store, contin_stack,
      cnt
    ) |

    _ =>
      push (
        (t_fn, (Contin_Norm, [( hole cnt, App (hole cnt, t_arg, pos) )], value_store, [])),
        value_store, contin_stack,
        cnt + 1
      )
  )


  fun associate_infix value_store t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t1' = associate_infix value_store t1
    in
      (case (find (value_store, id)) of
        SOME (SOME (direc, prec), rator) => (case t1' of 
          Compo (Compo (t1a, Id (id1, pos1), p1a), t1b, p1b) =>
          (case (find (value_store, id1)) of
            SOME (SOME (direc', prec'), rator') =>
            (if (prec' = prec andalso direc = Right) orelse (prec > prec') then
              Compo (
                Compo (t1a, Id (id1, pos1), p1a),
                associate_infix value_store (Compo (Compo (t1b, Id (id, pos), p1b), t2, p2)),
                p1
              )
            else 
              Compo (Compo (t1', Id (id, pos), p1), t2, p2)
            ) |

            _ => (let
              val t1'' = Compo (App (t1a, Id (id1, pos1), p1a), t1b, p1b)
            in
              Compo (Compo (t1'', Id (id, pos), p1), t2, p2)
            end)
          ) |

          _ => Compo (Compo (t1', Id (id, pos), p1), t2, p2)
        ) |

        _ => (
          Compo (App (t1', Id (id, pos), p1), t2, p2)
        )
      )
    end) |

    _ => t
  )

  fun to_func_elim value_store t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (
      (case (find (value_store, id)) of
        SOME (SOME (direc, prec), rator) => (
          App (
            Id (id, pos),
            List_Intro (
              to_func_elim value_store t1,
              List_Intro (to_func_elim value_store t2, Blank_Intro 0, pos),
              pos
            ),
            pos
          )
        ) |

        _ => (
          App (
            App (to_func_elim value_store t1, Id (id, pos), p1),
            to_func_elim value_store t2,
            p2
          )
        )
      )
    ) |
    _ => t
  )




  fun reduce_single (
    t, norm_f, reduce_f,
    value_store, contin_stack,
    cnt
  ) = (case t of
    (Id (id, _)) =>
      (case (find (value_store, id)) of
        SOME (NONE, v) =>
          pop (reduce_f v, contin_stack, cnt) |

        _  => (
          Error ("reduce single variable " ^ id ^ " cannot be resolved")
          value_store, contin_stack,
          cnt
        ) |

      ) |

    Value v =>
      (case (reduce_f v) of
        Error msg =>
          (
            Error msg, 
            value_store, contin_stack,
            cnt
          ) |

        result =>
          pop (result, contin_stack, cnt)

      ) |
    _ => 
      push (
        (t, (Contin_Norm, [( hole cnt, norm_f (hole cnt) )], value_store, [])),
        value_store, contin_stack,
        cnt + 1
      )
  )


  fun reduce_list (
    ts, norm_f, reduce_f,
    value_store, contin_stack,
    cnt
  ) = (let

    fun loop (prefix, postfix) = (case postfix of
      [] => (case (reduce_f prefix) of 
        Error msg =>
          (
            Error msg, 
            value_store, contin_stack,
            cnt
          )

        v =>
          pop (v, contin_stack, cnt)
      ) |

      x :: xs => (case x of
        (Id (id, _)) =>
          (case (find (value_store, id)) of
            SOME (NONE, v) => loop (prefix @ [v], xs) |
            _ => (
              Error ("reduce list variable " ^ id ^ " cannot be resolved"),
              value_store, contin_stack,
              cnt
            )

          ) |

        Value v => loop (prefix @ [v], xs) |
        _ =>
          (push (
            (
              x,
              (
                Contin_Norm,
                [( hole cnt, norm_f ((map (fn v => Value v) prefix) @ (hole cnt :: xs)) )],
                value_store,
                []
              )
            ),
            value_store, contin_stack,
            cnt + 1
          ))
      )
    )

  in
    loop ([], ts)
  end)

  

  fun term_step (t, value_store, contin_stack, cnt) = (case t of

    Value _ =>
      raise (Fail "internal error: term_step: value as input to term_step") |

    Assoc (term, pos) => (
      term, value_store, contin_stack, cnt
    ) |

    Log (t, pos) => reduce_single (
      t,
      fn t => Log (t, pos),
      fn v => (
        print ((value_to_string v) ^ "\n");
        v
      ),
      value_store, contin_stack,
      cnt
    ) | 

    Id (id, pos) => (case (find (value_store, id)) of
      SOME (NONE, v) => pop (v, contin_stack, cnt) |

      _ => (
        Error ("variable " ^ id ^ " cannot be resolved"),
        value_store, contin_stack,
        cnt
      )

    ) |


    List_Intro (t, t', pos) => reduce_list (
      [t, t'],
      (fn
        [t, t'] => List_Intro (t, t', pos) |
        _ => raise (Fail "Internal: List_Intro")
      ),
      (fn
        [v, Blank_Val] => List_Value ([v], pos) |
        [v, List_Value (ts, _)] => List_Value (v :: ts, pos) |
        _ => Error "cons with non-list"
      ),
      value_store, contin_stack,
      cnt

    ) |


    Func_Intro (lams, pos) => pop (
      Func_Value (lams, value_store, [], pos),
      contin_stack,
      cnt
    ) |

    (*
    Func_Mutual (lams, [], mutual_store, pos) => pop (
      Func_Value (lams, value_store, mutual_store, pos),
      contin_stack,
      cnt
    ) |
    *)


    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t_m = associate_infix value_store t
      val t' = to_func_elim value_store t_m 
    in
      (t', value_store, contin_stack, cnt)
    end) |

    Compo (t1, t2, pos) => (
      App (t1, t2, pos), value_store, contin_stack, cnt
    ) |


    App (t_fn, t_arg, pos) => apply (
      t_fn, t_arg, pos,
      value_store, contin_stack,
      cnt
    ) |


    With (t1, t2, _) => push (
      (t1, (Contin_With, [(hole cnt, t2)], value_store, [])),
      value_store, contin_stack,
      cnt + 1
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
            (k, (fix_op, Value (Func_Value (lams, value_store, mutual_store, pos)))) |
          field => field 
        )
       fields 
      )
    in
      (
        Rec_Intro_Mutual (fields', pos), 
        (value_store, contin_stack, thread_id),
        cnt
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
        value_store, contin_stack,
        cnt
      )
    end) |

    Select (t, pos) => reduce_single (
      t,
      fn t => Select (t, pos),
      (fn
        List_Value ([Rec_Value (fields, _), String_Value (key, _)], _) =>
        (case find (fields, key) of
          SOME (_, v) => v |
          NONE => Error "selection not found"
        ) |

        _ => Error "selecting from non-record"
      ),
      value_store, contin_stack,
      cnt

    ) |

    Event_Intro (evt, t, pos) => reduce_single (
      t,
      fn t => Event_Intro (evt, t, pos),
      fn v => Event_Value (mk_transactions (evt, v)),
      value_store, contin_stack,
      cnt
    ) | 

    Blank_Intro pos => pop (
      Blank_Val, contin_stack, cnt
    ) |

    String_Intro (str, pos) => pop (
      String_Value (str, pos), contin_stack, cnt
    ) |

    Num_Intro (str, pos) => pop (
      Num_Value (str, pos),
      contin_stack, cnt
    ) |

    Num_Add (t, pos) => reduce_single (
      t, fn t => Num_Add (t, pos),
      (fn
        List_Value ([Num_Value (n1, _), Num_Value (n2, _)], _) =>
          Num_Value (num_add (n1, n2), pos) |
        _ => Error "adding non-numbers"
      ),
      value_store, contin_stack, cnt

    ) |

    Num_Sub (t, pos) => reduce_single (
      t, fn t => Num_Sub (t, pos),
      (fn
        List_Value ([Num_Value (n1, _), Num_Value (n2, _)], _) => (
          Num_Value (num_sub (n1, n2), pos)
        ) |
        _ => Error "subtracting non-numbers"
      ),
      value_store, contin_stack, cnt
    ) |

    Num_Mul (t, pos) => reduce_single (
      t, fn t => Num_Mul (t, pos),
      (fn
        List_Value ([Num_Value (n1, _), Num_Value (n2, _)], _) => (
          Num_Value (num_mul (n1, n2), pos)
        ) |
        _ => Error "multplying non-numbers"
      ),
      value_store, contin_stack, cnt

    ) |

    Num_Div (t, pos) => reduce_single (
      t, fn t => Num_Div (t, pos),
      (fn
        List_Value ([Num_Value (n1, _), Num_Value (n2, _)], _) => (
          Num_Value (num_div (n1, n2), pos)
        ) |
        _ => Error "dividing non-numbers"
      ),
      value_store, contin_stack, cnt

    ) |

    _ => raise (Fail "TODO")

    )
  )



  fun effect_step (effect, thread_config, global_config) = (let
    val (value_store, contin_stack, thread_id) = thread_config
    val (chan_store, block_store, cnt) = global_config
  in
    (case effect of
      Return value => raise (Fail "internal error: step applied to effect result")
      Exec effect' =>
        (effect')
      (*
      ** TODO:
      ** Sync of transaction list |
      ** Bind of effect * contin list
      *)
    )
  end)



  fun term_lift (t, thread_config, gloabl_config) =
    (Term t, thread_config, gloabl_config)

  fun effect_lift (effect, thread_config, gloabl_config) =
    (Effect effect, thread_config, gloabl_config)


  fun concur_step (
    md, threads, env 
  
  ) = (case threads of
    [] => ( (*print "all done!\n";*) NONE) |
    thread :: threads' => (**
      TODO: check if thread is waiting is
      - ready,
      - waiting on transaction, or
      - waiting on communication 
      ** without transactions, waiting on communication was stored on channel queue
    **)
    in
      (* TODO:
      ** - if result thread has error, abort program
      ** - if resut thread is complete, remove from thread pool
      *)
    end)
    (let
      (* TODO: check thread nature: Reduce term, Exec effect, or Sync event, then call apropo thread_step *)
      val (md', seq_threads, env') = (term_step (md, thread, env)) 
      (* TODO: change seq thread to return just one thread, check if effect_val with empty: switch to alternate term_step or finish*)

      (*
      val _ = print ((from_mode_to_string md') ^ "\n")
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


(*
** outline for small step hierarchy **

sync_store: thread_id -> Effect continuation
chan_store: chan_id -> set of waiting sender transaction continuations, and set of waiting receiver transaction continuations  

datatype term = Send_Event_Intro | ...
datatype value = Sync of value * history: represents term for synchronizing event and its transaction history
Effect of effect: represents effect


take one thread, invariant: thread is value <-> contin stack is empty 
  case Value (Sync (Send_Evt k msg, hist)) =>
    find commnication partner in chan_store
  case Value (Sync (Offer v, hist)) =>
    check if commitable
    step to suspended thread found in sync_store
  .
  .
  .
  case Value (Bind (Return v, f)) =>
    step to f body, update val_store
    turn Sync effect into Action
  .
  .
  .
  case Value v => done
  case t => term_step t




*)


  fun eval t = (let

    val thread_id = 0 
    val value_store = empty_table 
    val contin_stack = []
    val cnt = 0
    val thread = (thread_id, t, value_store, contin_stack, 0)

    val cnt = 1


    fun loop cfg = (case (concur_step cfg) of
      NONE => () |
      SOME (cfg') =>
        loop cfg' 
    )
  
  in
    loop (
      [thread],
      (chan_store, block_store, cnt)
    )
  end)

end




(**** OLD SCRATCH *******)


(*
  fun poll (base, chan_store, block_store) = (case base of
    Send_Value (i, msg, _) =>
      (let
        val chan_op = find (chan_store, i)
        fun poll_recv (send_q, recv_q) = (case recv_q of
          [] => (false, chan_store) |
          (block_id, contin_stack, _) :: recv_q' =>
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
  
     Recv_Value (i, _) =>
      (let
        val chan_op = find (chan_store, i)
        fun poll_send (send_q, recv_q) = (case send_q of
          [] => (false, chan_store) |
          (block_id, contin_stack, msg, _) :: send_q' =>
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


  fun find_active_transaction (
    transactions, chan_store, block_store
  ) = (case transactions of

    [] =>
      (NONE, chan_store) |

    transaction :: transactions' => (let
      val (base, wrap_stack) = transaction
      val (is_active, chan_store') = poll (base, chan_store, block_store)
    in
      if is_active then
        (SOME transaction, chan_store')
      else 
        find_active_transaction (transactions', chan_store', block_store)
    end)
      
  )

  
  fun proceed (
    (evt, wrap_stack), contin_stack, thread_id,
    (chan_store, block_store, sync_store, cnt)
  ) = (case evt of

    Send_Value (i, msg) =>
    (let
      val chan_op = find (chan_store, i)
      val recv_op = (case chan_op of
        SOME (_, (block_id, recv_stack, recv_thread_id) :: recvs) =>
          SOME (recv_stack, recv_thread_id) | 
        SOME (_, []) => NONE |
        NONE => NONE
      )
      val (threads, md') = (case recv_op of
        NONE => Stick "proceed Send"_Value |
        SOME (recv_stack, recv_thread_id) => (
          [
            (Blank 0, empty_table, wrap_stack @ contin_stack, thread_id),
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
  
    Recv_Value i =>
    (let
      val chan_op = find (chan_store, i)
      val send_op = (case chan_op of
        SOME ((block_id, send_stack, msg, send_thread_id) :: sends, _) =>
          SOME (send_stack, msg, send_thread_id) | 
        SOME ([], _) => NONE |
        NONE => NONE
      )
  
      val (threads, md') = (case send_op of
        NONE => ([], Mode_Stick "proceed Recv")_Value |
        SOME (send_stack, msg, send_thread_id) => (
          [
            (Value Blank_Val, empty_table, send_stack, send_thread_id),
            (Value msg, empty_table, wrap_stack @ contin_stack, thread_id)
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
  
  fun block_one ((evt, wrap_stack), contin_stack, chan_store, block_id, thread_id) = (case evt of
    Send_Value (i, msg) =>
      (let
        val contin_stack' = wrap_stack @ contin_stack
        val chan_op = find (chan_store, i)
        val chan' = (case chan_op of
          NONE =>
            ([(block_id, contin_stack', msg, thread_id)], []) |
          SOME (send_q, recv_q) =>
            (send_q @ [(block_id, contin_stack', msg, thread_id)], recv_q)
        )
        val chan_store' = insert (chan_store, i, chan')
      in
        chan_store'
      end) |
  
    Recv_Value i =>
      (let
        val contin_stack' = wrap_stack @ contin_stack
        val chan_op = find (chan_store, i)
        val chan' = (case chan_op of
          NONE =>
            ([], [(block_id, contin_stack', thread_id)]) | 
          SOME (send_q, recv_q) =>
            (send_q, recv_q @ [(block_id, contin_stack', thread_id)])
        )
        val chan_store' = insert (chan_store, i, chan')
      in
        chan_store'
      end)
  
  )
  
  fun block (
    event_values, contin_stack, thread_id,
    (chan_store, block_store, sync_store, cnt)
  ) = (let
    val chan_store' = (List.foldl  
      (fn (evt, chan_store) =>
        block_one (evt, contin_stack, chan_store, cnt, thread_id)
      )
      chan_store
      event_values
    )
    val block_store' = insert (block_store, cnt, ())
    val cnt' = cnt + 1
  in
    (Mode_Block event_values, [], (chan_store', block_store', sync_store, cnt'))
  end)

  *)


(*

  fun mk_transactions (evt, v) = (case (evt, v) of
  
    (Send, List_Value ([Chan_Loc i, msg], _)) =>
      [Tx (Send_Value (i, msg), [])] |
  
    (Recv, Chan_Loc i) =>
      [Tx (Recv_Value i, [])] |

    (Choose, List_Value (values, _)) =>
      mk_transactions_from_list values |

    _ => []

    (* TODO: modify to handle choose and other event results *)
    (*
    (Latch, List_Value ([Event_Value transactions, Func_Value (lams, fnc_store, mutual_store, _)], _)) =>
      (List.foldl
        (fn ((evt, wrap_stack), transactions_acc) => let
          val cont = (Contin_Sync, lams, fnc_store, mutual_store)
        in
          transactions_acc @ [(evt, cont :: wrap_stack)]
        end)
        []
        transactions 
      ) |
    *)

  
  )

  and mk_transactions_from_list (evts) = (case evts of
    [] => [] |
    (Event_Value event_values) :: evts' => 
      event_values @ (mk_transactions_from_list evts') |
    _ => raise (Fail "Internal: mk_transactions_from_list")
  )

*)




(* **TODO**

    Sync (t, pos) => (case t of

(*
** TODO: allocate chan during sync **
**    Alloc_Chan (_, i) => (let
**      val chan_store' = insert (chan_store, cnt, ([], []))
**      val cnt' = cnt + 1
**    in
**      pop (
**        Chan_Loc cnt,
**        contin_stack,
**        cnt'
**      )
**    end) |
**
*)
      (Id (id, _)) => (case (find (value_store, id)) of
        SOME (NONE, v) => (
          Mode_Hidden,
          [(Sync (v, pos), value_store, contin_stack, thread_id)],
          (chan_store, block_store, sync_store, cnt)
        ) |

        _  => (
          Mode_Stick ("Sync argument variable " ^ id ^ " cannot be resolved"),
          [], (chan_store, block_store, sync_store, cnt)
        )

      ) |

      Event_Value v =>
        (let

          val transactions = mk_transactions (v, []) 
          
          val (active_transaction_op, chan_store') = (
            find_active_transaction (transactions, chan_store, block_store)
          )

        in
          (case active_transaction_op of
            SOME transaction =>
              proceed (
                transaction, contin_stack, thread_id,
                (chan_store', block_store, sync_store, cnt)
              ) |
            NONE =>
              block (
                transactions, contin_stack, thread_id,
                (chan_store', block_store, sync_store, cnt)
              )
          )
        end)

      Value _ =>
        (
          Mode_Stick "sync with non-event",
          [], (chan_store, block_store, sync_store, cnt)
        ) |

      _ =>
        push (
          (t, (Contin_Norm, [( hole cnt, Sync (hole cnt, pos) )], value_store, [])),
          value_store, contin_stack,
          chan_store, block_store, sync_store, cnt + 1
        )

    ) |


    Exec (t, pos) =>(case t of
      (Id (id, _)) => (case (find (value_store, id)) of
        SOME (_, v) => (
          Mode_Hidden,
          [(Exec (v, pos), value_store, contin_stack, thread_id)],
          (chan_store, block_store, sync_store, cnt)
        ) |

        _  => (
          Mode_Stick ("Exec argument variable " ^ id ^ " cannot be resolved"),
          [], (chan_store, block_store, sync_store, cnt)
        )


      ) |

      Func_Value ([(Blank_Intro _, t_body)], fnc_store, mutual_store, _) => (let
        val exec_id = cnt
        val cnt' = cnt + 1
      in
        (
          Mode_Exec t_body,
          [
            (List_Value ([], pos), value_store, contin_stack, thread_id),
            (t_body, value_store, [], exec_id)
          ],
          (chan_store, block_store, sync_store, cnt')
        )
      end) |
      
      v => (if is_value v then
        (
          Mode_Stick "exec with non-function",
          [], (chan_store, block_store, sync_store, cnt)
        )
      else
        push (
          (t, (Contin_Norm, [( hole cnt, Exec (hole cnt, pos) )], value_store, [])),
          value_store, contin_stack,
          chan_store, block_store, sync_store, cnt + 1
        )
      )
    )

  
  )
*)


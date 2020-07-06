structure Tree = struct


  structure Thread_Key = Key_Fn (val tag = "thread")

  structure Chan_Key = Key_Fn (val tag = "chan")

  structure History_Key = Key_Fn (val tag = "history")

  structure Hole_Key = Key_Fn (val tag = "_g")

  datatype left_right = Left | Right

  type infix_option = (left_right * int) option


  datatype contin_mode = Contin_With | Contin_Norm | Contin_App | Contin_Sync

  datatype term = 
    Sym of (term * int) |
    Id of (string * int) |
    Assoc of (term * int) |
    Log of (term * int) |

    Intro_List of (term * term * int) |

    Intro_Func of (
      ((term * term) list) *
      int
    ) (* Intro_Func (lams, pos) *) |


    App of (term * term * int) |

    Compo of (term * term * int) |
    With of (term * term * int) |

    Intro_Rec of (
      ((string * (infix_option * term)) list) *
      int
    ) (* Intro_Rec (fields, pos) *) |

    Intro_Mutual_Rec of (
      ((string * (infix_option * term)) list) *
      int
    ) (* Intro_Rec (fields, pos) *) |

    Select of (term * int) |


    (* event *)
    (*
    ** TODO **
    Intro_Send of (term * int) |
    Intro_Recv of (term * int) |
    Intro_Latch of (term * int) |
    Intro_Choose of (term * int) |
    Intro_Offer of (term * int) |
    Intro_Block of (term * int)
    *)

    (* effect *)
    (*
    ** TODO **
    Intro_Return |
    Intro_Sync |
    Intro_Bind |
    Intro_Exec
    *)

    (* number *)
    Add_Num of (term * int) |
    Sub_Num of (term * int) |
    Mul_Num of (term * int) |
    Div_Num of (term * int) |

    (* string *)

    (* value *)
    Value of (value * int)

  and value =
    Blank |
    List of (value list * int) |

    Func of (
      ((term * term) list) *
      ((string, infix_option * value) list) *
      ((string, infix_option * ((term * term) list)) list) *
      int
    ) (* Func (lams, value_store, mutual_store, pos) *) |

    Rec of (
      ((string * (infix_option * value)) list) *
      int
    ) (* Intro_Rec (fields, pos) *) |


    Event of event |

    Effect of effect |
  
    String of (string * int) |

    Num of (string * int) |

    Chan of Chan_Key.t |

    Thread of Thread_Key.t |

    Error of string

  and effect =
    Return of value |
    Bind of effect * term |
    Exec of effect |
    Sync of event |
    Search of event * past_event list

  and event =
    Offer of value |
    Block |
    Alloc_Chan | 
    Send of Chan_Key.t * value |
    Recv of Chan_Key.t |

  and past_event =  
    Choose_Left |
    Choose_Right |
    Commun_Send of Thread_Key.t * History_Key.t * History_Key.t |
    Commun_Recv of Thread_Key.t * History_Key.t * History_Key.t

  and contin = Contin of (
    contin_mode * 
    ((term * term) list) *
    ((string, infix_option * term) store) *
    ((string, infix_option * (term * term) list) store)
  )

(*

  type sender = (Thread_Store.key * Active_Event_Key.t * past_event list * contin list * value)

  (* thread_id, active_event_key, trace of synched events, transaction continuation, message *)

  type receiver = (Thread_Store.key * Active_Event_Key.t * past_event list * contin list)

  type channel = sender list * receiver list

  type value_store = (string * value) list 
  type history = (thread_key * past_event list)

  type thread = term * value_store * contin list  

  type config =
  {
    thread_key : Thread_Key.t,

    thread_list : (Thread_Key.t * thread) list,

    suspension_map : (Thread_Key.t, contin list) map,

    active_key : Active_Event_Key.t,
    active_event_set : Active_Event_Key.t set, 

    chan_key : Chan_Key.t,
    chan_store : (Chan_Key.t * channel) list,

    history_key : History_Key.t,
    history_store : (History_Key.t * history list) list,

    hole_key : Hole_Key.t
  }
*)


  val surround_with = String.surround_with

  val surround = String.surround

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

    Intro_List (t1, t2, pos) => (
      (to_string t1) ^ ", " ^ (to_string t2)
    ) |

    Intro_Func (lams, pos) => String.surround "" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Compo (t1, t2, pos) => "(compo " ^ (to_string t1) ^ " " ^ (to_string t2) ^")"|

    App (t1, t2, pos) => surround "apply" (
      (to_string t1) ^ " " ^ (to_string t2)
    ) |

    With (t1, t2, pos) => "with " ^ (to_string t1) ^ "\n" ^ (to_string t2) |

    Intro_Rec (fs, pos) => String.surround "" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Intro_Mutual_Rec (fs, pos) => String.surround "mutual" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Select (t, pos) => "select " ^ (to_string t) |

    Intro_Event (evt, t, pos) => "evt " ^ (event_to_string evt) ^ (to_string t) |

    Value v => value_to_string v |

    _ => "(NOT YET IMPLEMENTED)"

    (*
    Sync (t, pos) => "sync " ^ (to_string t) |

    Exec (t, pos) => "exec " ^ (to_string t) |

    Id (name, pos) => name |

    String (str, pos) => str |

    Chan k => "chan_" ^ (Chan_Key.to_string k) |

    Thread k => "thread_" ^ (Chan_Key.to_string k) |

    Add_Num (t, pos) => "add " ^ (to_string t) |

    Sub_Num (t, pos) => "sub " ^ (to_string t) |

    Mul_Num (t, pos) => "mul " ^ (to_string t) |

    Div_Num (t, pos) => "div " ^ (to_string t) |

    Error msg => "(ERROR: " ^ msg ^ ")"
    *)

  )

  and value_to_string v = (case v of

    List (vs, pos) => surround "" ( 
      String.concatWith "\n" (List.map (fn v => "# " ^ (value_to_string v)) vs)
    ) |

    Func (lams, fnc_store, mutual_store, pos) => String.surround "val" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Rec (fs, pos) => String.surround "val" (
      String.concatWith ",\n" (List.map from_field_value_to_string fs)
    ) |

    Event transactions => String.surround "evt" (
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

    Alloc_Chan => "alloc_chan" |

    Send (i, msg) => String.surround "send_value " (
      (Int.toString i) ^ (value_to_string msg)
    ) |

    Recv i => String.surround "recv_value " (
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
    (Intro_Blank _, _) => SOME value_store |

    (Sym (Id (id, _), _), _) => (let
      val thunk = Func ([(Intro_Blank ~1, symbolic_term)], value_store, [], ~1)
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

    (Intro_List (p1, p2, _), List_Intro (st1, st2, _)) => (
      (Option.mapPartial
        (fn value_store' =>
          match_symbolic_term_insert value_store' (p2, st2)
        )
        (match_symbolic_term_insert value_store (p1, st1))
      )
    ) |

    (Intro_Func (p_lams, _), Func_Intro (st_lams, _)) =>
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


    (Intro_Mutual_Rec (p_fields, _), Rec_Intro_Mutual (st_fields, _)) =>
      from_fields_match_symbolic_term_insert value_store (p_fields, st_fields) |


    (Select (p, _), Select (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Intro_Event (p_evt, p, _), Event_Intro (st_evt, st, _)) =>
      if p_evt = st_evt then match_symbolic_term_insert value_store (p, st)
      else NONE |

(*
TODO:
    (Event p_transactions, Event st_transactions) =>
      match_symbolic_transactions_insert value_store (p_transactions, st_transactions) |
*)

(*
**    (List (ps, _), Value (List (sts, _))) =>
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

    (Intro_Effect (p_effect, p, _), Effect_Intro (st_effect, st, _)) =>
      if p_effect = st_effect then match_symbolic_term_insert value_store (p, st)
      else NONE |

    (Add_Num (p, _), Add_Num (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Sub_Num (p, _), Sub_Num (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Mul_Num (p, _), Mul_Num (st, _)) =>
      match_symbolic_term_insert value_store (p, st) |

    (Div_Num (p, _), Div_Num (st, _)) =>
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

    (Intro_Blank _, _) =>
      SOME value_store |

    (Id (str, _), v) =>
      SOME (insert (value_store, str, (NONE, v))) |

    (Intro_List (t, t', _), List (v :: vs, _)) =>
      (Option.mapPartial
        (fn value_store' =>
          match_value_insert (value_store', t, v)
        )
        (match_value_insert (value_store, t', List (vs, ~1)))
      ) |

    (Intro_Rec (p_fields, _), Rec (v_fields, _)) => (
      from_fields_match_value_insert value_store (p_fields, v_fields)
    ) |

    (Intro_Func ([(Blank_Intro _, p_body)], _), Func ([(Blank_Intro _, st_body)], _, _, _)) => (
      (* function value's local stores are ignored; only syntax is matched; *)
      (* it's up to the user to determine if syntax can actually be evaluated in alternate context *)
      (* variables in pattern are specified by pattern_var syntax (sym f); *)
      (* it may then be used in new context and evaluated with f () *) 
      match_symbolic_term_insert value_store (p_body, st_body)
    ) |


    (Value (Num (n, _)), Num (nv, _)) => (
      if n = nv then
        SOME value_store
      else
        NONE
    ) |

    _ => NONE

    (* **TODO**

    (List ([], _), List ([], _)) => SOME value_store | 

    (List (t :: ts, _), List (v :: vs, _)) =>
      (Option.mapPartial
        (fn value_store' =>
          match_value_insert (value_store', t, v)
        )
        (match_value_insert (value_store, List (ts, ~1), List (vs, ~1)))
      ) |


    (Intro_Event_Send (t, _), Event_Send_Intro (v, _)) =>
      match_value_insert (value_store, t, v) |

    (Intro_Event_Recv (t, _), Event_Recv_Intro (v, _)) =>
      match_value_insert (value_store, t, v) |

    (Func p_fnc, Func v_fnc) => (
      if fnc_equal (p_fnc, v_fnc) then
        SOME value_store
      else
        NONE
    ) |

    (String (str, _), String (strv, _)) => (
      if str = strv then
        SOME value_store
      else
        NONE
    ) |

    (Intro_Rec (p_fields, _), Rec_Intro (v_fields, _)) => (case (p_fields, v_fields) of
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
            (match_value_insert (value_store, Intro_Rec (ps, ~1), Rec_Intro (remainder, ~1)))
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


  fun hole k = Id (Hole_Key.to_string k, ~1)

  fun push (
    (t_arg, cont),
    value_store, contin_stack,
    hole_key
  ) = (let
    val contin_stack' = cont :: contin_stack

  in
    (
      t_arg, value_store, contin_stack',
      hole_key
    )
  end)

  fun pop (result, contin, contin_stack', hole_key) = (let
    val (cmode, lams, value_store', mutual_store) = contin
  
    val value_store'' = (case result of
      Rec (fields, _) => (if cmode = Contin_With then
        insert_table (value_store', fields)
      else
        value_store'
      ) |
      _ => value_store'
    )

    (* embed mutual_store within self's functions *)
    val fnc_store = (map 
      (fn (k, (fix_op, lams)) =>
        (k, (fix_op, Func (lams, value_store'', mutual_store, ~1)))
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
        hole_key
      )
    )

  in
    (next_term, value_store'''', contin_stack', hole_key)
  end)


  fun apply (
    t_fn, t_arg, pos,
    value_store, contin_stack,
    hole_key
  ) = (case t_fn of
    (Id (id, _)) =>
      (case (find (value_store, id)) of
        SOME (_, v_fn) => (
          App (Value v_fn, t_arg, pos), 
          value_store, contin_stack
          hole_key
        ) |

        _  => SOME (_, v_fn) => (
          Error ("apply arg variable " ^ id ^ " cannot be resolved"),
          value_store, contin_stack,
          hole_key
        ) |

      ) |

    Value (Func (lams, fnc_store, mutual_store, _)) =>
      push (
        (t_arg, (Contin_App, lams, fnc_store, mutual_store)),
        value_store, contin_stack,
        hole_key
      ) |

    Value v => (
      Error ("application of non-function: " ^ (value_to_string v)) |
      value_store, contin_stack,
      hole_key
    ) |

    _ =>
      push (
        (t_fn, (Contin_Norm, [( hole hole_key, App (hole hole_key, t_arg, pos) )], value_store, [])),
        value_store, contin_stack,
        Hole_Key.inc hole_key
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

  fun to_app value_store t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (
      (case (find (value_store, id)) of
        SOME (SOME (direc, prec), rator) => (
          App (
            Id (id, pos),
            Intro_List (
              to_app value_store t1,
              Intro_List (to_app value_store t2, Blank_Intro 0, pos),
              pos
            ),
            pos
          )
        ) |

        _ => (
          App (
            App (to_app value_store t1, Id (id, pos), p1),
            to_app value_store t2,
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
    hole_key
  ) = (case t of
    (Id (id, _)) =>
      (case (find (value_store, id)) of
        SOME (NONE, v) =>
          (Value (reduce_f v), value_store, contin_stack, hole_key) |

        _  => (
          Error ("reduce single variable " ^ id ^ " cannot be resolved")
          value_store, contin_stack,
          hole_key
        ) |

      ) |

    Value v =>
      (case (reduce_f v) of
        Error msg =>
          (
            Error msg, 
            value_store, contin_stack,
            hole_key
          ) |

        result =>
          (Value result, value_store, contin_stack, hole_key)

      ) |
    _ => 
      push (
        (t, (Contin_Norm, [( hole hole_key, norm_f (hole hole_key) )], value_store, [])),
        value_store, contin_stack,
        Hole_Key.inc hole_key
      )
  )


  fun reduce_list (
    ts, norm_f, reduce_f,
    value_store, contin_stack,
    hole_key
  ) = (let

    fun loop (prefix, postfix) = (case postfix of
      [] => (case (reduce_f prefix) of 
        Error msg =>
          (
            Error msg, 
            value_store, contin_stack,
            hole_key
          )

        v =>
          (Value v, value_store, contin_stack, hole_key)
      ) |

      x :: xs => (case x of
        (Id (id, _)) =>
          (case (find (value_store, id)) of
            SOME (NONE, v) => loop (prefix @ [v], xs) |
            _ => (
              Error ("reduce list variable " ^ id ^ " cannot be resolved"),
              value_store, contin_stack,
              hole_key
            )

          ) |

        Value v => loop (prefix @ [v], xs) |
        _ =>
          (push (
            (
              x,
              (
                Contin_Norm,
                [( hole hole_key, norm_f ((map (fn v => Value v) prefix) @ (hole hole_key :: xs)) )],
                value_store,
                []
              )
            ),
            value_store, contin_stack,
            Hole_Key.inc hole_key
          ))
      )
    )

  in
    loop ([], ts)
  end)

  

  fun term_step (t, value_store, contin_stack, hole_key) = (case t of

    Value v =>
      raise (Fail "internal error: term_step: value as input") |

    Assoc (term, pos) => (
      term, value_store, contin_stack, hole_key
    ) |

    Log (t, pos) => reduce_single (
      t,
      fn t => Log (t, pos),
      fn v => (
        print ((value_to_string v) ^ "\n");
        v
      ),
      value_store, contin_stack,
      hole_key
    ) | 

    Id (id, pos) => (case (find (value_store, id)) of
      SOME (NONE, v) =>
        (Value v, value_store, contin_stack, hole_key) |

      _ => (
        Error ("variable " ^ id ^ " cannot be resolved"),
        value_store, contin_stack,
        hole_key
      )

    ) |


    Intro_List (t, t', pos) => reduce_list (
      [t, t'],
      (fn
        [t, t'] => Intro_List (t, t', pos) |
        _ => raise (Fail "Internal: Intro_List")
      ),
      (fn
        [v, Blank_Val] => List ([v], pos) |
        [v, List (ts, _)] => List (v :: ts, pos) |
        _ => Error "cons with non-list"
      ),
      value_store, contin_stack,
      hole_key

    ) |


    Intro_Func (lams, pos) =>
        (Value (Func (lams, value_store, [], pos)),
          value_store,
          contin_stack,
          hole_key
        ) |

    (*
    Func_Mutual (lams, [], mutual_store, pos) =>
        (Value (Func (lams, value_store, mutual_store, pos)),
          value_store,
          contin_stack,
          hole_key
        ) |
    *)


    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t_m = associate_infix value_store t
      val t' = to_app value_store t_m 
    in
      (t', value_store, contin_stack, hole_key)
    end) |

    Compo (t1, t2, pos) => (
      App (t1, t2, pos), value_store, contin_stack, hole_key
    ) |


    App (t_fn, t_arg, pos) => apply (
      t_fn, t_arg, pos,
      value_store, contin_stack,
      hole_key
    ) |


    With (t1, t2, _) => push (
      (t1, (Contin_With, [(hole hole_key, t2)], value_store, [])),
      value_store, contin_stack,
      Hole_Key.inc hole_key
    ) |

    Intro_Rec (fields, pos) => (let
      val mutual_store = (List.mapPartial
        (fn
          (k, (fix_op,  Intro_Func (lams, _))) => 
            SOME (k, (fix_op, lams)) |
          _ => NONE
        )
        fields
      )
      
      (* embed mutual ids into ts' functions *)
      val fields' = (map
        (fn
          (k, (fix_op, Intro_Func (lams, pos))) =>
            (k, (fix_op, Value (Func (lams, value_store, mutual_store, pos)))) |
          field => field 
        )
       fields 
      )
    in
      (
        Intro_Mutual_Rec (fields', pos), 
        value_store, contin_stack, hole_key
      )
    end) |
    
    Intro_Mutual_Rec (fields, pos) => (let
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
        ts, f Intro_Mutual_Rec, f Rec_Val, 
        value_store, contin_stack,
        hole_key
      )
    end) |

    Select (t, pos) => reduce_single (
      t,
      fn t => Select (t, pos),
      (fn
        List ([Rec (fields, _), String (key, _)], _) =>
        (case find (fields, key) of
          SOME (_, v) => v |
          NONE => Error "selection not found"
        ) |

        _ => Error "selecting from non-record"
      ),
      value_store, contin_stack,
      hole_key

    ) |

    Intro_Event (evt, t, pos) => reduce_single (
      t,
      fn t => Intro_Event (evt, t, pos),
      fn v => Event (mk_transactions (evt, v)),
      value_store, contin_stack,
      hole_key
    ) | 

    Add_Num (t, pos) => reduce_single (
      t, fn t => Add_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) =>
          Num (num_add (n1, n2), pos) |
        _ => Error "adding non-numbers"
      ),
      value_store, contin_stack, hole_key

    ) |

    Sub_Num (t, pos) => reduce_single (
      t, fn t => Sub_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_sub (n1, n2), pos)
        ) |
        _ => Error "subtracting non-numbers"
      ),
      value_store, contin_stack, hole_key
    ) |

    Mul_Num (t, pos) => reduce_single (
      t, fn t => Mul_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_mul (n1, n2), pos)
        ) |
        _ => Error "multplying non-numbers"
      ),
      value_store, contin_stack, hole_key

    ) |

    Div_Num (t, pos) => reduce_single (
      t, fn t => Div_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_div (n1, n2), pos)
        ) |
        _ => Error "dividing non-numbers"
      ),
      value_store, contin_stack, hole_key

    ) |

    _ => raise (Fail "TODO")

    )
  )


  fun concur_step {
    thread_store,
    sync_store,
    thread_key,
    chan_store,
    chan_key,
    history_store,
    hole_key
  } = (case (thread_store) of
    [] => ( (*print "all done!\n";*) NONE) |
    (thread_key, (t, value_store, contin_stack)) :: thread_store' => (case (t, contin_stack) of

      (Value (Effect effect), []) => (let
        val (new_thread_store, chan_store', sync_store') = (
          execute (effect, chan_store, sync_store) 
        )
      in
        (
          thread_store' @ new_thread_store,
          sync_store', 
          thread_key,

          chan_store',
          chan_key,

          transaction_store,
          transactin_key, (* tx_key -> (thread_key * past_event list) list *) 

          hole_key,
        )
      end) |

      (Value v, []) => (thread_store', hole_key) | 

      (Value v, contin :: contin_stack') => (let
        val new_thread = pop (v, contin, contin_stack')
      in
        (thread_store' @ [new_thread], hole_key)
      end) |

      _ => (let
        val (new_thread, hole_key') = term_step ((t, value_store, contin_stack), hole_key)
      in
        (thread_store' @ [new_thread], hole_key')
      end) 
    )
  )


(*
** outline for small step hierarchy **

sync_store: thread_id -> Effect continuation
chan_store: chan_id -> set of waiting sender transaction continuations, and set of waiting receiver transaction continuations  

datatype term = Intro_Send_Event | ...
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

    val thread_id = Thread_Key.zero 
    val value_store = [] 
    val contin_stack = []
    val hole_key = Hole_Key.zero
    val thread = (t, value_store, contin_stack)

    fun loop cfg = (case (concur_step cfg) of
      NONE => () |
      SOME (cfg') =>
        loop cfg' 
    )

    val thread_store = [(thread_id, thread)]
  
  in
    loop (thread_store, hole_key)
  end)

end




(**** OLD SCRATCH *******)


(*
  fun poll (base, chan_store, block_store) = (case base of
    Send (i, msg, _) =>
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
  
     Recv (i, _) =>
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
    (chan_store, block_store, sync_store, hole_key)
  ) = (case evt of

    Send (i, msg) =>
    (let
      val chan_op = find (chan_store, i)
      val recv_op = (case chan_op of
        SOME (_, (block_id, recv_stack, recv_thread_id) :: recvs) =>
          SOME (recv_stack, recv_thread_id) | 
        SOME (_, []) => NONE |
        NONE => NONE
      )
      val (threads, md') = (case recv_op of
        NONE => Stick "proceed Send" |
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
        (chan_store', block_store, sync_store, hole_key)
      ) 
    end) |
  
    Recv i =>
    (let
      val chan_op = find (chan_store, i)
      val send_op = (case chan_op of
        SOME ((block_id, send_stack, msg, send_thread_id) :: sends, _) =>
          SOME (send_stack, msg, send_thread_id) | 
        SOME ([], _) => NONE |
        NONE => NONE
      )
  
      val (threads, md') = (case send_op of
        NONE => ([], Mode_Stick "proceed Recv") |
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
        (chan_store', block_store, sync_store, hole_key)
      )
    end)
  
  )
  
  fun block_one ((evt, wrap_stack), contin_stack, chan_store, block_id, thread_id) = (case evt of
    Send (i, msg) =>
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
  
    Recv i =>
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
    (chan_store, block_store, sync_store, hole_key)
  ) = (let
    val chan_store' = (List.foldl  
      (fn (evt, chan_store) =>
        block_one (evt, contin_stack, chan_store, hole_key, thread_id)
      )
      chan_store
      event_values
    )
    val block_store' = insert (block_store, hole_key, ())
    val hole_key' = Hole_Key.inc hole_key
  in
    (Mode_Block event_values, [], (chan_store', block_store', sync_store, hole_key'))
  end)

  *)


(*

  fun mk_transactions (evt, v) = (case (evt, v) of
  
    (Send, List ([Chan i, msg], _)) =>
      [Tx (Send (i, msg), [])] |
  
    (Recv, Chan i) =>
      [Tx (Recv i, [])] |

    (Choose, List (values, _)) =>
      mk_transactions_from_list values |

    _ => []

    (* TODO: modify to handle choose and other event results *)
    (*
    (Latch, List ([Event transactions, Func (lams, fnc_store, mutual_store, _)], _)) =>
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
    (Event event_values) :: evts' => 
      event_values @ (mk_transactions_from_list evts') |
    _ => raise (Fail "Internal: mk_transactions_from_list")
  )

*)




(* **TODO**

    Sync (t, pos) => (case t of

(*
** TODO: allocate chan during sync **
    Alloc_Chan (_, i) => (let
      val chan_store' = insert (chan_store, hole_key, ([], []))
      val hole_key' = Hole_Key.inc hole_key
    in
      (Value (Chan hole_key), value_store, contin_stack, hole_key) |
    end) |

*)
      (Id (id, _)) => (case (find (value_store, id)) of
        SOME (NONE, v) => (
          Mode_Hidden,
          [(Sync (v, pos), value_store, contin_stack, thread_id)],
          (chan_store, block_store, sync_store, hole_key)
        ) |

        _  => (
          Mode_Stick ("Sync argument variable " ^ id ^ " cannot be resolved"),
          [], (chan_store, block_store, sync_store, hole_key)
        )

      ) |

      Event v =>
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
                (chan_store', block_store, sync_store, hole_key)
              ) |
            NONE =>
              block (
                transactions, contin_stack, thread_id,
                (chan_store', block_store, sync_store, hole_key)
              )
          )
        end)

      Value _ =>
        (
          Mode_Stick "sync with non-event",
          [], (chan_store, block_store, sync_store, hole_key)
        ) |

      _ =>
        push (
          (t, (Contin_Norm, [( hole hole_key, Sync (hole hole_key, pos) )], value_store, [])),
          value_store, contin_stack,
          chan_store, block_store, sync_store, Hole_Key.inc hole_key
        )

    ) |


    Exec (t, pos) =>(case t of
      (Id (id, _)) => (case (find (value_store, id)) of
        SOME (_, v) => (
          Mode_Hidden,
          [(Exec (v, pos), value_store, contin_stack, thread_id)],
          (chan_store, block_store, sync_store, hole_key)
        ) |

        _  => (
          Mode_Stick ("Exec argument variable " ^ id ^ " cannot be resolved"),
          [], (chan_store, block_store, sync_store, hole_key)
        )


      ) |

      Func ([(Value (Blank, _), t_body)], fnc_store, mutual_store, _) => (let
        val exec_id = hole_key
        val hole_key' = Hole_Key.inc hole_key
      in
        (
          Mode_Exec t_body,
          [
            (List ([], pos), value_store, contin_stack, thread_id),
            (t_body, value_store, [], exec_id)
          ],
          (chan_store, block_store, sync_store, hole_key')
        )
      end) |
      
      v => (if is_value v then
        (
          Mode_Stick "exec with non-function",
          [], (chan_store, block_store, sync_store, hole_key)
        )
      else
        push (
          (t, (Contin_Norm, [( hole hole_key, Exec (hole hole_key, pos) )], value_store, [])),
          value_store, contin_stack,
          chan_store, block_store, sync_store, Hole_Key.inc hole_key
        )
      )
    )

  
  )
*)


structure Tree = struct


  structure Thread_Key = Key_Fn (val tag = "thread")
  structure Thread_Map = RedBlackMapFn (Thread_Key)

  structure Running_Key = Key_Fn (val tag = "thread")
  structure Running_Set = RedBlackSetFn (Running_Key)

  structure Chan_Key = Key_Fn (val tag = "chan")
  structure Chan_Map = RedBlackMapFn (Chan_Key)

  structure Sync_Send_Key = Key_Fn (val tag = "sync_send")
  structure Sync_Send_Map = RedBlackMapFn (Sync_Send_Key)

  structure Sync_Recv_Key = Key_Fn (val tag = "sync_recv")
  structure Sync_Recv_Map = RedBlackMapFn (Sync_Recv_Key)

  structure Hole = Key_Fn (val tag = "_g")

  structure String_Map = RedBlackMapFn (struct
    type ord_key = string
    val compare = String.compare
  end)


  datatype left_right = Left | Right

  type infix_option = (left_right * int) option


  datatype term = 
    Sym of (term * int) |
    Id of (string * int) |
    Assoc of (term * int) |
    Log of (term * int) |

    Intro_List of (term * term * int) |

    Intro_Func of (
      ((term * term) list) *
      int
    ) |

    App of (term * term * int) |

    Compo of (term * term * int) |
    With of (term * term * int) |

    Intro_Rec of (
      ((infix_option * term) String_Map.map) *
      int
    ) |

    Intro_Mutual_Rec of (
      ((infix_option * term) String_Map.map) *
      int
    ) |

    Select of (term * int) |

    (* event *)
    Intro_Send of (term * int) |
    Intro_Recv of (term * int) |
    Intro_Latch of (term * int) |
    Intro_Choose of (term * int) |
    Intro_Offer of (term * int) |
    Intro_Abort of (term * int) |

    (* effect *)
    Intro_Return (term * int) |
    Intro_Run (term * int) |
    Intro_Bind (term * int) |
    Intro_Exec (term * int) |

    (* number *)
    Add_Num of (term * int) |
    Sub_Num of (term * int) |
    Mul_Num of (term * int) |
    Div_Num of (term * int) |

    (* value *)
    Value of (value * int)

  and value =
    Blank |
    List of (value list * int) |

    Func of (
      ((term * term) list) *
      ((infix_option * value) String_Map.map) *
      ((infix_option * ((term * term) list)) String_Map.map) *
      int
    ) (* Func (lams, string_fix_value_map, mutual_map, pos) *) |

    Rec of (
      ((infix_option * value) String_Map.map) *
      int
    ) |


    Event of event |

    Effect of effect |
  
    String of (string * int) |

    Num of (string * int) |

    Chan of Chan_Key.ord_key |

    Thread of Thread_Key.ord_key |

    Error of string

  and effect =
    Return of value |
    Bind of effect * value |
    Exec of effect |
    Run of event |

  and event =
    Offer of value |
    Abort |
    Alloc_Chan | 
    Send of value |
    Recv of value |
    Latch of value |
    Choose of value |

  and past_event =  
    Choose_Left |
    Choose_Right |
    Sync_Send of (
      Thread_Key.ord_key *
      past_event list *
      Sync_Send_Key.ord_key *
      Sync_Recv_Key.ord_key
    ) |
    Sync_Recv of (
      Thread_Key.ord_key *
      past_event list *
      Sync_Recv_Key.ord_key *
      Sync_Send_Key.ord_key
    )

  datatype contin_mode =
    Contin_With | Contin_Norm | Contin_App |
    Contin_Bind

  type contin = (
    effect_contin_mode * 
    ((term * term) list) *
    ((infix_option * term) String_Map.map) *
    ((infix_option * (term * term) list) String_Map.map)
  )

  datatype thread_mode =
    Exec_Effect of contin list | 
    Run_Event of past_event list * contin list

  type thread = (
    Thread_Key.ord_key *
    term *
    (infix_option * value) String_Map.map *
    contin list * (* term continuation *)
    thread_mode
  )


  type waiting_send = (
    Running_Key.ord_key *
    Thread_Key.ord_key *
    past_event list *
    contin list *
    value
  )

  type waiting_recv = (
    Running_Key.ord_key *
    Thread_Key.ord_key *
    past_event list *
    contin list
  )

  type channel = waiting_send list * waiting_recv list
  
  type history = (thread_key * past_event list)

  type thread_config =
  {
    thread_list : thread list,
    thread_suspension_map : (contin list) Thread_Map.map,
    new_thread_key : Thread_Key.ord_key
  }
  
  type running_config =
  {
    running_set : Running_Set.set, 
    new_running_key : Running_Key.ord_key 
  }

  type chan_config =
  {
    chan_map : channel Chan_Map.map,
    chan_key : Chan_Key.ord_key
  }

  type sync_config =
  {
    send_completion_map : (history list) Sync_Send_Map.map,
    new_sync_send_key : Sync_Send_Key.ord_key,
    recv_completion_map : (history list) Sync_Recv_Map.map,
    new_sync_recv_key : Sync_Recv_Key.ord_key
  }

  type config = (
    thread_config *
    chan_config *
    sync_config *
    Hole.key
  )

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
    Abort => "abort"
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
    Run (t, pos) => "run " ^ (to_string t) |

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

    Func (lams, fnc_store, mutual_map, pos) => String.surround "val" (
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



  fun match_symbolic_term_insert string_fix_value_map (pattern, symbolic_term) =
  (case (pattern, symbolic_term) of
    (Intro_Blank _, _) => SOME string_fix_value_map |

    (Sym (Id (id, _), _), _) => (let
      val thunk = Func ([(Intro_Blank ~1, symbolic_term)], string_fix_value_map, [], ~1)
    in
      SOME (String_Map.insert (string_fix_value_map, id, (NONE, thunk)))
    end) |

    (Id (p_id, _), Id (st_id, _)) =>
    (if p_id = st_id then
      SOME string_fix_value_map
    else
      NONE
    ) |

    (Assoc (p, _), Assoc (st, _)) =>
      match_symbolic_term_insert string_fix_value_map (p, st) |

    (Log (p, _), Log (st, _)) =>
      match_symbolic_term_insert string_fix_value_map (p, st) |

    (Intro_List (p1, p2, _), List_Intro (st1, st2, _)) => (
      (Option.mapPartial
        (fn string_fix_value_map' =>
          match_symbolic_term_insert string_fix_value_map' (p2, st2)
        )
        (match_symbolic_term_insert string_fix_value_map (p1, st1))
      )
    ) |

    (Intro_Func (p_lams, _), Func_Intro (st_lams, _)) =>
      from_lams_match_symbolic_term_insert string_fix_value_map (p_lams, st_lams) |

    (App (p1, p2, _), App (st1, st2, _)) =>
    (Option.mapPartial
      (fn string_fix_value_map' =>
        match_symbolic_term_insert string_fix_value_map' (p2, st2)
      )
      (match_symbolic_term_insert string_fix_value_map (p1, st1))
    ) |

    (Compo (p1, p2, _), Compo (st1, st2, _)) => (
      (Option.mapPartial
        (fn string_fix_value_map' =>
          match_symbolic_term_insert string_fix_value_map' (p2, st2)
        )
        (match_symbolic_term_insert string_fix_value_map (p1, st1))
      )
    ) |

    (With (p1, p2, _), With (st1, st2, _)) =>
    (Option.mapPartial
      (fn string_fix_value_map' =>
        match_symbolic_term_insert string_fix_value_map' (p2, st2)
      )
      (match_symbolic_term_insert string_fix_value_map (p1, st1))
    ) |


    (Intro_Mutual_Rec (p_fields, _), Rec_Intro_Mutual (st_fields, _)) =>
      from_fields_match_symbolic_term_insert string_fix_value_map (p_fields, st_fields) |


    (Select (p, _), Select (st, _)) =>
      match_symbolic_term_insert string_fix_value_map (p, st) |

    (Intro_Event (p_evt, p, _), Event_Intro (st_evt, st, _)) =>
      if p_evt = st_evt then match_symbolic_term_insert string_fix_value_map (p, st)
      else NONE |

(*
TODO:
    (Event p_transactions, Event st_transactions) =>
      match_symbolic_transactions_insert string_fix_value_map (p_transactions, st_transactions) |
*)

(*
**    (List (ps, _), Value (List (sts, _))) =>
**    (if (List.length ps = List.length sts) then
**      (List.foldl
**        (fn ((p, st), string_fix_value_map_op) => 
**          (Option.mapPartial
**            (fn string_fix_value_map' =>
**              match_symbolic_term_insert string_fix_value_map' (p, st)
**            )
**            string_fix_value_map_op
**          )
**        )
**        (SOME string_fix_value_map)
**        (ListPair.zip (ps, sts))
**      )
**    else
**      NONE
**    ) |
*)


    (Value p_v, Value st_v) =>
    (if p_v = st_v then
      SOME string_fix_value_map
    else
      NONE
    ) |

    (Intro_Effect (p_effect, p, _), Effect_Intro (st_effect, st, _)) =>
      if p_effect = st_effect then match_symbolic_term_insert string_fix_value_map (p, st)
      else NONE |

    (Add_Num (p, _), Add_Num (st, _)) =>
      match_symbolic_term_insert string_fix_value_map (p, st) |

    (Sub_Num (p, _), Sub_Num (st, _)) =>
      match_symbolic_term_insert string_fix_value_map (p, st) |

    (Mul_Num (p, _), Mul_Num (st, _)) =>
      match_symbolic_term_insert string_fix_value_map (p, st) |

    (Div_Num (p, _), Div_Num (st, _)) =>
      match_symbolic_term_insert string_fix_value_map (p, st) |

    _ => (
      NONE
    )


  )

  and from_lams_match_symbolic_term_insert string_fix_value_map (p_lams, st_lams) = 
  (if (List.length p_lams = List.length st_lams) then
    (List.foldl
      (fn (((p1, p2), (st1, st2)), string_fix_value_map_op) =>
        (Option.mapPartial
          (fn string_fix_value_map' =>
            (Option.mapPartial
              (fn string_fix_value_map' =>
                match_symbolic_term_insert string_fix_value_map' (p2, st2)
              )
              (match_symbolic_term_insert string_fix_value_map (p1, st1))
            )
          )
          string_fix_value_map_op
        )
      )
      (SOME string_fix_value_map)
      (ListPair.zip (p_lams, st_lams))
    )
  else
    NONE
  )

  and from_fields_match_symbolic_term_insert string_fix_value_map (p_fields, st_fields) =
  (if (List.length p_fields = List.length st_fields) then
    (List.foldl
      (fn (((p_key, (p_fop, p)), (st_key, (st_fop, st))), string_fix_value_map_op) =>
        (if p_key = st_key andalso p_fop = st_fop then
          (Option.mapPartial
            (fn string_fix_value_map' =>
              match_symbolic_term_insert string_fix_value_map (p, st)
            )
            string_fix_value_map_op
          )
        else 
          NONE
        )
      )
      (SOME string_fix_value_map)
      (ListPair.zip (p_fields, st_fields))
    )
  else
    NONE
  )

  fun match_value_insert (string_fix_value_map, pat, value) = (case (pat, value) of

    (Assoc (pat', _), _) =>
      match_value_insert (string_fix_value_map, pat', value) |

    (Intro_Blank _, _) =>
      SOME string_fix_value_map |

    (Id (str, _), v) =>
      SOME (String_Map.insert (string_fix_value_map, str, (NONE, v))) |

    (Intro_List (t, t', _), List (v :: vs, _)) =>
      (Option.mapPartial
        (fn string_fix_value_map' =>
          match_value_insert (string_fix_value_map', t, v)
        )
        (match_value_insert (string_fix_value_map, t', List (vs, ~1)))
      ) |

    (Intro_Rec (p_fields, _), Rec (v_fields, _)) => (
      from_fields_match_value_insert string_fix_value_map (p_fields, v_fields)
    ) |

    (Intro_Func ([(Blank_Intro _, p_body)], _), Func ([(Blank_Intro _, st_body)], _, _, _)) => (
      (* function value's local stores are ignored; only syntax is matched; *)
      (* it's up to the user to determine if syntax can actually be evaluated in alternate context *)
      (* variables in pattern are specified by pattern_var syntax (sym f); *)
      (* it may then be used in new context and evaluated with f () *) 
      match_symbolic_term_insert string_fix_value_map (p_body, st_body)
    ) |


    (Value (Num (n, _)), Num (nv, _)) => (
      if n = nv then
        SOME string_fix_value_map
      else
        NONE
    ) |

    _ => NONE

    (* **TODO**

    (List ([], _), List ([], _)) => SOME string_fix_value_map | 

    (List (t :: ts, _), List (v :: vs, _)) =>
      (Option.mapPartial
        (fn string_fix_value_map' =>
          match_value_insert (string_fix_value_map', t, v)
        )
        (match_value_insert (string_fix_value_map, List (ts, ~1), List (vs, ~1)))
      ) |


    (Intro_Event_Send (t, _), Event_Send_Intro (v, _)) =>
      match_value_insert (string_fix_value_map, t, v) |

    (Intro_Event_Recv (t, _), Event_Recv_Intro (v, _)) =>
      match_value_insert (string_fix_value_map, t, v) |

    (Func p_fnc, Func v_fnc) => (
      if fnc_equal (p_fnc, v_fnc) then
        SOME string_fix_value_map
      else
        NONE
    ) |

    (String (str, _), String (strv, _)) => (
      if str = strv then
        SOME string_fix_value_map
      else
        NONE
    ) |

    (Intro_Rec (p_fields, _), Rec_Intro (v_fields, _)) => (case (p_fields, v_fields) of
      ([], []) =>
        SOME string_fix_value_map |

      ((pk, t) :: ps, _ :: _) => (let
        val (match, remainder) = (List.partition  
          (fn (k, v) => k = pk)
          v_fields
        )
      in
        (case match of
          [(k, v)] => (Option.mapPartial
            (fn string_fix_value_map' => match_value_insert (string_fix_value_map', t, v))
            (match_value_insert (string_fix_value_map, Intro_Rec (ps, ~1), Rec_Intro (remainder, ~1)))
          ) |

          _ => NONE
        )
      end) |

      _ =>
        NONE
      
    ) |

    *)
  )

  and from_fields_match_value_insert string_fix_value_map (p_fields, v_fields) =
  (case p_fields of
    [] => SOME string_fix_value_map |
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
          (fn string_fix_value_map' =>
            SOME (String_Map.insert (string_fix_value_map', vname, (vfix_op, v)))
          )
          (match_value_insert (string_fix_value_map, p, v))
        ) |
        _ :: key_matches' => match_term key_matches'
      )
      val string_fix_value_map_op = match_term key_matches
    in
      (Option.mapPartial
        (fn string_fix_value_map' =>
          from_fields_match_value_insert string_fix_value_map' (pfs, vfs)
        )
        string_fix_value_map_op
      )
    end)
  )


  fun hole k = Id (Hole_Key.to_string k, ~1)


  fun continue (result, contin) = (let
    val (cmode, lams, string_fix_value_map', mutual_map) = contin
  
    val string_fix_value_map'' = (case result of
      Rec (fields, _) => (if cmode = Contin_With then
        String_Map.mapi (fn (k, v) =>
          String_Map.insert (string_fix_value_map', k, v)
        ) fields
      else
        string_fix_value_map'
      ) |
      _ => string_fix_value_map'
    )

    (* embed mutual_map within self's functions *)
    val fnc_store = (map 
      (fn (k, (fix_op, lams)) =>
        (k, (fix_op, Func (lams, string_fix_value_map'', mutual_map, ~1)))
      )
      mutual_map
    )

    val string_fix_value_map''' = (String_Map.mapi
      (fn (k, v) => String_Map.insert (string_fix_value_map'', k, v))
      fnc_store
    )

    fun match_first lams = (case lams of
      [] => NONE |
      (p, t) :: lams' =>
        (case (match_value_insert (string_fix_value_map''', p, result)) of
          NONE => match_first lams' |
          SOME string_fix_value_map'''' => (
            SOME (t, string_fix_value_map'''')
          )
        )
    )

    val next_term = (case (match_first lams) of

      NONE => Error (
        "result - " ^
        (value_to_string result) ^
        " - does not match continuation hole pattern"
      ) |

      SOME (t_body, string_fix_value_map'''') => (
        t_body, 
        string_fix_value_map'''', contin_stack'
        hole_key
      )
    )

  in
    (next_term, string_fix_value_map'''')
  end)


  fun apply (
    t_fn, t_arg, pos,
    string_fix_value_map, contin_stack,
    hole_key
  ) = (case t_fn of
    (Id (id, _)) =>
      (case (find (string_fix_value_map, id)) of
        SOME (_, v_fn) => (
          App (Value v_fn, t_arg, pos), 
          string_fix_value_map, contin_stack
          hole_key
        ) |

        _  => SOME (_, v_fn) => (
          Error ("apply arg variable " ^ id ^ " cannot be resolved"),
          string_fix_value_map, contin_stack,
          hole_key
        ) |

      ) |

    Value (Func (lams, fnc_store, mutual_map, _)) =>
      (t_arg, string_fix_value_map, Some (Contin_App, lams, fnc_store, mutual_map), hole_key)

    Value v => (
      Error ("application of non-function: " ^ (value_to_string v)) |
      string_fix_value_map, contin_stack,
      hole_key
    ) |

    _ =>
      (
        t_fn,
        string_fix_value_map, 
        SOME (
          Contin_Norm,
          [( hole hole_key, App (hole hole_key, t_arg, pos) )],
          string_fix_value_map,
          []
        ),
        Hole.inc hole_key
      )
  )


  fun associate_infix string_fix_value_map t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t1' = associate_infix string_fix_value_map t1
    in
      (case (find (string_fix_value_map, id)) of
        SOME (SOME (direc, prec), rator) => (case t1' of 
          Compo (Compo (t1a, Id (id1, pos1), p1a), t1b, p1b) =>
          (case (find (string_fix_value_map, id1)) of
            SOME (SOME (direc', prec'), rator') =>
            (if (prec' = prec andalso direc = Right) orelse (prec > prec') then
              Compo (
                Compo (t1a, Id (id1, pos1), p1a),
                associate_infix string_fix_value_map (Compo (Compo (t1b, Id (id, pos), p1b), t2, p2)),
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

  fun to_app string_fix_value_map t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (
      (case (find (string_fix_value_map, id)) of
        SOME (SOME (direc, prec), rator) => (
          App (
            Id (id, pos),
            Intro_List (
              to_app string_fix_value_map t1,
              Intro_List (to_app string_fix_value_map t2, Blank_Intro 0, pos),
              pos
            ),
            pos
          )
        ) |

        _ => (
          App (
            App (to_app string_fix_value_map t1, Id (id, pos), p1),
            to_app string_fix_value_map t2,
            p2
          )
        )
      )
    ) |
    _ => t
  )




  fun reduce_single (
    t, norm_f, reduce_f,
    string_fix_value_map,
    contin_stack,
    hole_key
  ) = (case t of
    (Id (id, _)) =>
      (case (find (string_fix_value_map, id)) of
        SOME (NONE, v) =>
          (Value (reduce_f v), string_fix_value_map, contin_stack, hole_key) |

        _  => (
          Error ("reduce single variable " ^ id ^ " cannot be resolved")
          string_fix_value_map,
          contin_stack,
          hole_key
        ) |

      ) |

    Value v =>
      (case (reduce_f v) of
        Error msg =>
          (
            Error msg, 
            string_fix_value_map,
            contin_stack,
            hole_key
          ) |

        result =>
          (Value result, string_fix_value_map, contin_stack, hole_key)

      ) |
    _ => (let
      val contin = (
        Contin_Norm,
        [( hole hole_key, norm_f (hole hole_key) )],
        string_fix_value_map,
        []
      )
    in
      (
        t,
        string_fix_value_map,
        contin :: contin_stack,
        Hole.inc hole_key
      )
    end)
  )


  fun reduce_list (
    ts, norm_f, reduce_f,
    string_fix_value_map, contin_stack,
    hole_key
  ) = (let

    fun loop (prefix, postfix) = (case postfix of
      [] => (case (reduce_f prefix) of 
        Error msg =>
          (
            Error msg, 
            string_fix_value_map, contin_stack,
            hole_key
          )

        v =>
          (Value v, string_fix_value_map, contin_stack, hole_key)
      ) |

      x :: xs => (case x of
        (Id (id, _)) =>
          (case (find (string_fix_value_map, id)) of
            SOME (NONE, v) => loop (prefix @ [v], xs) |
            _ => (
              Error ("reduce list variable " ^ id ^ " cannot be resolved"),
              string_fix_value_map, contin_stack,
              hole_key
            )

          ) |

        Value v => loop (prefix @ [v], xs) |
        _ => (let
          val contin =
          (
            Contin_Norm,
            [( hole hole_key, norm_f ((map (fn v => Value v) prefix) @ (hole hole_key :: xs)) )],
            string_fix_value_map,
            []
          )
        in
          (
            x,
            string_fix_value_map,
            contin :: contin_stack,
            Hole.inc hole_key
          )
        end)
      )
    )

  in
    loop ([], ts)
  end)

  

  fun eval_term_step (t, string_fix_value_map, contin_stack, hole_key) = (case t of

    Value v => (case contin_stack of 
      [] => NONE |
      contin :: contin_stack' => (let
        val (t', string_fix_value_map') = continue (v, contin)
      in
        SOME (t', string_fix_value_map', contin_stack', hole_key)
      end)
    ) |

    Assoc (term, pos) => SOME (
      term, string_fix_value_map, contin_stack, hole_key
    ) |

    Log (t, pos) => SOME (reduce_single (
      t,
      fn t => Log (t, pos),
      fn v => (
        print ((value_to_string v) ^ "\n");
        v
      ),
      string_fix_value_map,
      contin_stack,
      hole_key
    )) | 

    Id (id, pos) => (case (find (string_fix_value_map, id)) of
      SOME (NONE, v) =>
        (Value v, string_fix_value_map, contin_stack, hole_key) |

      _ => (
        Error ("variable " ^ id ^ " cannot be resolved"),
        string_fix_value_map,
        contin_stack,
        hole_key
      )

    ) |


    Intro_List (t, t', pos) => SOME (reduce_list (
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
      string_fix_value_map,
      contin_stack,
      hole_key

    )) |


    Intro_Func (lams, pos) =>
      (
        Value (Func (lams, string_fix_value_map, [], pos)),
        string_fix_value_map,
        contin_stack,
        hole_key
      ) |

    (*
    Func_Mutual (lams, [], mutual_map, pos) =>
        (
          Value (Func (lams, string_fix_value_map, mutual_map, pos)),
          string_fix_value_map,
          contin_stack,
          hole_key
        ) |
    *)


    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t_m = associate_infix string_fix_value_map t
      val t' = to_app string_fix_value_map t_m 
    in
      (t', string_fix_value_map, contin_stack, hole_key)
    end) |

    Compo (t1, t2, pos) => (
      App (t1, t2, pos), string_fix_value_map, contin_stack, hole_key
    ) |


    App (t_fn, t_arg, pos) => apply (
      t_fn, t_arg, pos,
      string_fix_value_map, contin_stack,
      hole_key
    ) |


    With (t1, t2, _) =>
    (
      t1,
      string_fix_value_map,
      SOME (Contin_With, [(hole hole_key, t2)], string_fix_value_map, []),
      Hole_Key.inc hole_key
    ) |

    Intro_Rec (fields, pos) => (let
      val mutual_map = (List.mapPartial
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
            (k, (fix_op, Value (Func (lams, string_fix_value_map, mutual_map, pos)))) |
          field => field 
        )
       fields 
      )
    in
      (
        Intro_Mutual_Rec (fields', pos), 
        string_fix_value_map, contin_stack, hole_key
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
      SOME (reduce_list (
        ts, f Intro_Mutual_Rec, f Rec_Val, 
        string_fix_value_map, contin_stack,
        hole_key
      ))
    end) |

    Select (t, pos) => SOME (reduce_single (
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
      string_fix_value_map,
      contin_stack,
      hole_key

    )) |

    Intro_Event (evt, t, pos) => SOME (reduce_single (
      t,
      fn t => Intro_Event (evt, t, pos),
      fn v => Event (mk_transactions (evt, v)),
      string_fix_value_map,
      contin_stack,
      hole_key
    )) | 

    Add_Num (t, pos) => SOME (reduce_single (
      t, fn t => Add_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) =>
          Num (num_add (n1, n2), pos) |
        _ => Error "adding non-numbers"
      ),
      string_fix_value_map,
      contin_stack,
      hole_key
    )) |

    Sub_Num (t, pos) => SOME (reduce_single (
      t, fn t => Sub_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_sub (n1, n2), pos)
        ) |
        _ => Error "subtracting non-numbers"
      ),
      string_fix_value_map,
      contin_stack,
      hole_key
    )) |

    Mul_Num (t, pos) => SOME (reduce_single (
      t, fn t => Mul_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_mul (n1, n2), pos)
        ) |
        _ => Error "multplying non-numbers"
      ),
      string_fix_value_map,
      contin_stack,
      hole_key
    )) |

    Div_Num (t, pos) => SOME (reduce_single (
      t, fn t => Div_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_div (n1, n2), pos)
        ) |
        _ => Error "dividing non-numbers"
      ),
      string_fix_value_map,
      contin_stack,
      hole_key
    )) |

  )



  fun exec_effect_step (thread_id, effect, string_fix_value_map, effect_stack, new_thread_key) =
  (case effect of
    Return v => (case effect_stack of
      [] => [(Value v, string_fix_value_map, [])] |  
      contin :: contin_stack => (let
        val (t', string_fix_value_map') = continue (v, contin)

        val new_threads = [(thread_id, t, string_fix_value_map', [], Exec_Effect [])]
      in
        (new_threads, new_thread_key)
      end)
    ) |

    Bind (effect', v) => (case v of
      Func (lams, fnc_store, mutual_map, _) =>
      (
        [(
          thread_id,
          Value (Effect effect'),
          string_fix_value_map,
          [],
          Exec_Effect (Contin_Bind, lams, fnc_store, mutual_map), hole_key) :: effect_stack
        )],
        new_thread_key
      ) |

      _ =>
      [(
        thread_id,
        Value (Error "bind with non-function"),
        string_fix_value_map,
        [],
        Exec_Effect effect_stack
      )]
    ) |

    Exec effect' => (let
      val parent_thread = 
      (
        thread_id,
        Value (Effect (Return Blank)),
        string_fix_value_map,
        [],
        Exec_Effect effect_stack
      )

      val new_thread = 
      (
        new_thread_key,
        Value (Effect effect'),
        string_fix_value_map,
        [],
        Exec_Effect [] 
      )
    in
      ([parent_thread, new_thread_id], Thread_Key.inc new_thread_key)
    end) |

    Run evt => (let
      val new_threads =
      [(
        thread_id,
        evt,
        [],
        Run_Event ([], [])
      )]
    in
      (new_threads, new_thread_key)
    end)
  )

  fun clean_chan running_set chan (new_sends, new_recvs) = (let
    val (waiting_sends, waiting_recvs) = chan
    val waiting_sends' = (List.foldl
      (fn ((run_key, _, _, _) as send, waiting_sends') =>
        if Running_Set.member (running_set, run_key) then
          send :: waiting_sends'
        else
          waiting_sends
      )
      new_sends
      waiting_sends
    )

    val waiting_recvs' = (List.foldl
      (fn ((run_key, _, _, _) as recv, waiting_recvs') =>
        if Running_Set.member (running_set, run_key) then
          recv :: waiting_recvs'
        else
          waiting_recvs
      )
      new_recvs
      waiting_recvs
    )
  in
    (waiting_sends', waiting_recvs')
  end)

  fun sync_send_recv sync_config (waiting_send, waiting_recv)  = (let

    val (
      send_running_key,
      send_thread_key,
      send_trail,
      send_contin,
      send_msg 
    ) = waiting_send 

    val (
      recv_running_key,
      recv_thread_key,
      recv_trail,
      recv_contin
    ) = waiting_recv 

    val send_completion_map = #send_completion_map sync_config
    val send_sync_key = #send_sync_key sync_config
    val send_completion_map' = Send_Sync_Map.insert (
      send_completion_map, send_sync_key, []
    ) 
    val send_sync_key' = Send_Sync_Key.inc send_sync_key

    val recv_completion_map = #recv_completion_map sync_config
    val recv_sync_key = #recv_sync_key sync_config
    val recv_completion_map' = Recv_Sync_Map.insert (
      recv_completion_map, recv_sync_key, []
    ) 
    val recv_sync_key' = Recv_Sync_Key.inc recv_sync_key

    val sync_config' = {
      send_completion_map = send_completion_map',
      send_sync_key = send_sync_key',

      recv_completion_map = recv_completion_map',
      recv_sync_key = recv_sync_key'
    }


    val send_head = Sync_Recv (
      recv_thread_key,
      recv_trail,
      recv_completion_key,
      send_completion_key
    ) 
    
    val recv_head = Sync_Send (
      send_thread_key,
      send_trail,
      send_completion_key,
      recv_completion_key
    ) 


    val send_thread = (
      send_thread_key,
      Value (Event (Offer Blank)),
      send_infix_value_map,
      [],
      Run_Effect (
        send_running_key, send_head :: send_trail, send_contin
      )
    )

    val recv_thread = (
      recv_thread_key,
      Value (Event (Offer msg)),
      recv_infix_value_map,
      [],
      Run_Effect (
        recv_running_key, recv_head :: recv_trail, recv_contin
      )
    )

    val new_threads = [send_thread, recv_thread]

  in
    (new_threads, sync_config')
  end)

  (* result:
  ** (
  **   new_threads, thread_suspension_map',
  **   running_config', chan_config', sync_config'
  ** )
  *)

  fun run_event_step (
    thread_key, event, trail, event_stack,
    thread_suspension_map,
    running_config, chan_config, sync_config
  ) =
  (case event of
    Offer v =>  (case event_stack of
      [] => (* TODO: Try to commit or just leave around *) |
      contin :: contin_stack' => (let
        val (t', string_fix_value_map') = continue (v, contin)
        val new_threads = [(
          thread_key,
          t',
          string_fix_value_map,
          [],
          Run_Effect (trail, effect_stack) 
        )]
      in
        (
          new_threads,
          thread_suspension_map,
          running_config,
          chan_config,
          sync_config
        )
      end)
    ) |

    Abort =>
    (
      [],
      thread_suspension_map,
      running_config,
      chan_config,
      sync_config
    ) |

    Alloc_Chan => (let
      val new_chan = ([], [])    
      val {new_chan_key, chan_map} = chan_config
      val chan_map' = Chan_Map.insert (chan_map, new_chan_key, new_chan)
      val new_chan_key' = Chan_Key.inc new_chan_key

      val chan_config' = {
        chan_map = chan_map',
        new_chan_key = new_chan_key'
      }

      val new_threads = [(
        thread_key,
        Value (Event (Offer (Chan new_chan_key))),
        string_fix_value_map,
        [],
        Run_Effect (trail, effect_stack) 
      )]

    in
      (
        new_threads,
        thread_suspension_map,
        running_config,
        chan_config',
        sync_config
      )
    end) |
      
    Latch (event', v) => (let
      val (t', effect_stack') = (case v of
        Func (lams, fnc_store, mutual_map, _) =>
        (
          Val (Event event'),
          (Contin_Latch, lams, fnc_store, mutual_map) :: contin_stack
        ) | 

        _  =>
        (
          Val (Error "latch with non-function")
        )
      )

      val new_threads = [(
        thread_key,
        t',
        string_fix_value_map,
        [],
        Run_Effect (trail, effect_stack') 
      )]
    in
      (
        new_threads,
        thread_suspension_map,
        running_config,
        chan_config,
        sync_config
      )
    end) |

    Choose (evt_l, evt_r) => (let
      val new_threads = [
        (
          thread_key, Value (Effect evt_l),
          string_fix_value_map, [],
          Run_Effect (Choose_Left :: trail, effect_stack)
        ),
        (
          thread_key, Value (Effect evt_r),
          string_fix_value_map, [],
          Run_Effect (Choose_Right :: trail, effect_stack)
        )
      ]
    in
      (
        new_threads,
        thread_suspension_map,
        running_config,
        chan_config,
        sync_config
      )
    end) |

    Send (chan_key, msg) => (let
      val chan_map = #chan_map chan_config
      (* Expectation: chan_key certainly exists in chan_map; raise exception otherwise *)
      val chan = Chan_Map.lookup (chan_map, chan_key)

      val running_set = #running_set running_config
      val new_running_key = #new_running_key running_config

      val waiting_send = (new_running_key, thread_key, trail, contin_stack, msg)
      val chan' = clean_chan running_set chan ([waiting_send], [])
      val (_, waiting_recvs) = chan'
      val (new_threads, sync_config') = (List.foldl  
        (fn (waiting_recv, (new_threads, sync_config) => (let
          val (synched_threads, sync_config') = sync_send_recv (waiting_send, waiting_recv)
        in
           synched_threads @ new_threads
        end )
        ([], sync_config)
        waiting_recvs
      )
    in
      (
        new_threads,
        thread_suspension_map,
        running_config,
        chan_config,
        sync_config'
      )
    end)
    (*
    ** TODO **
    Recv of Chan_Key.ord_key |
    *)
  )

  fun concur_step (thread_config, running_config, chan_config, sync_config, hole_key) =
  (case (#thread_list thread_config) of
    [] => ( (*print "all done!\n";*) NONE) |
    (thread_key, t, string_fix_value_map, term_stack, thread_mode) :: threads' =>
    (case (t, term_stack, thread_mode) of

      (* exec effect case *)
      (Value (Effect effect), [], Exec_Effect effect_stack) => (let
        val (new_threads, new_thread_key') = exec_effect_step (effect, effect_stack)
        val thread_config' = {
          new_thread_key = new_thread_key', 
          thread_list = threads' @ new_threads,
          thread_suspension_map = thread_suspension_map 
        }
      in
        (thread_config', running_config, chan_config, sync_config, hole_key)
      end) |

      (* run event case *)
      (Value (Event event), [], Run_Event (trail, event_stack)) => (let

        val (
          new_threads, thread_suspension_map',
          running_config, chan_config, sync_config
        ) =
        run_event_step (
          event, event_stack, thread_suspension_map,
          running_config, chan_config, sync_config
        )

        val thread_config' = {
          new_thread_key = new_thread_key', 
          thread_list = threads' @ new_threads,
          thread_suspension_map = thread_suspension_map 
        }
      in
        (thread_config', running_config', chan_config', sync_config', hole_key)
      end) |

      (* eval term case *)
      _ => (let
        val result = eval_term_step (t, string_fix_value_map, term_stack, hole_key)

        val (new_threads, hole_key') =
        (case result of
          SOME (t', string_fix_value_map', term_stack', hole_key') => 
          (
            [(thread_key, t', string_fix_value_map', term_stack', thread_mode)],
            hole_key'
          ) | 
          NONE => ([], hole_key)
        )

        val thread_config' = {
          thread_key = thread_key, 
          thread_list = threads' @ new_threads,
          thread_suspension_map = thread_suspension_map 
        }
      in
        (thread_config', running_config, chan_config, sync_config, hole_key')
      end)

    )
  )


  fun eval t = (let

    val thread_id = Thread_Key.zero 
    val string_fix_value_map = [] 
    val contin_stack = []
    val hole_key = Hole_Key.zero
    val thread = (t, string_fix_value_map, contin_stack)

    fun loop cfg = (case (concur_step cfg) of
      NONE => () |
      SOME (cfg') =>
        loop cfg' 
    )

    val thread_store = [(thread_id, thread)]
  
  in
    loop (thread_store, hole_key)
  end)


end (* struct *)





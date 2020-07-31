structure Tree =
struct

  (* start structures *)

  structure Thread_Key = Key_Fn (val tag = "thread")
  structure Thread_Map = RedBlackMapFn (Thread_Key)

  structure Syncing_Key = Key_Fn (val tag = "thread")
  structure Syncing_Set = RedBlackSetFn (Syncing_Key)

  structure Chan_Key = Key_Fn (val tag = "chan")
  structure Chan_Map = RedBlackMapFn (Chan_Key)

  structure Send_Comm_Key = Key_Fn (val tag = "comm_send")
  structure Send_Comm_Map = RedBlackMapFn (Send_Comm_Key)

  structure Recv_Comm_Key = Key_Fn (val tag = "comm_recv")
  structure Recv_Comm_Map = RedBlackMapFn (Recv_Comm_Key)

  structure Hole_Key = Key_Fn (val tag = "_g")

  structure String_Map = RedBlackMapFn
  (struct
    type ord_key = string
    val compare = String.compare
  end)


  (* start types *)
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
      (string * (infix_option * term)) list *
      int
    ) |

    Intro_Mutual_Rec of (
      (string * (infix_option * term)) list *
      int
    ) |

    Select of (term * int) |

    (* event *)
    Intro_Send of (term * int) |
    Intro_Recv of (term * int) |
    Intro_Latch of (term * int) |
    Intro_Choose of (term * int) |
    Intro_Offer of (term * int) |
    Intro_Abort of int |

    (* effect *)
    Intro_Return of (term * int) |
    Intro_Sync of (term * int) |
    Intro_Bind of (term * int) |
    Intro_Exec of (term * int) |

    (* number *)
    Add_Num of (term * int) |
    Sub_Num of (term * int) |
    Mul_Num of (term * int) |
    Div_Num of (term * int) |

    (* value *)
    Value of (value * int)

  and value =
    Blank |
    List of value list |

    Func of (
      ((term * term) list) *
      ((infix_option * value) String_Map.map) *
      ((infix_option * ((term * term) list)) String_Map.map) 
    ) (* Func (lams, symbol_map, mutual_map) *) |

    Rec of (string * (infix_option * value)) list |


    Event of event |

    Effect of effect |
  
    String of string |

    Num of string |

    Chan of Chan_Key.ord_key |

    Thread of Thread_Key.ord_key |

    Error of string

  and effect =
    Return of value |
    Bind of (
      effect *
      ((term * term) list) *
      ((infix_option * value) String_Map.map) *
      ((infix_option * (term * term) list) String_Map.map)
    ) |
    Exec of effect |
    Sync of event 

  and event =
    Offer of value |
    Abort |
    Alloc_Chan | 
    Send of Chan_Key.ord_key * value  |
    Recv of Chan_Key.ord_key |
    Latch of (
      event *
      ((term * term) list) *
      ((infix_option * value) String_Map.map) *
      ((infix_option * (term * term) list) String_Map.map)
    ) |
    Choose of event * event

  datatype contin_mode =
    Contin_With |
    Contin_Norm |
    Contin_App |
    Contin_Bind |
    Contin_Latch

  type contin = (
    contin_mode * 
    ((term * term) list) *
    ((infix_option * value) String_Map.map) *
    ((infix_option * (term * term) list) String_Map.map)
  )

  datatype past_event =  
    Left_Choice |
    Right_Choice |
    Send_Comm of (
      Thread_Key.ord_key *
      past_event list
    ) |
    Recv_Comm of (
      Thread_Key.ord_key *
      past_event list
    )

  type communication_map = (Syncing_Key.ord_key * past_event list) Thread_Map.map

  type search_thread = 
  (
    Syncing_Key.ord_key *
    past_event list *
    communication_map *
    contin list
  )

  datatype thread_mode =
    Exec_Effect of contin list | 
    Sync_Event of search_thread

  type thread = (
    Thread_Key.ord_key *
    term *
    (infix_option * value) String_Map.map *
    contin list * (* term continuation *)
    thread_mode
  )

  type waiting_send = (
    Thread_Key.ord_key *
    search_thread *
    value
  )

  type waiting_recv = (
    Thread_Key.ord_key *
    search_thread
  )

  type channel = waiting_send list * waiting_recv list
  

  type completion =
  (
    Syncing_Key.ord_key *
    past_event list *
    communication_map *
    value
  )

  type global_context =
  {
    new_thread_key : Thread_Key.ord_key,
    suspension_map : (contin list) Thread_Map.map,

    completions_map : (completion list) Thread_Map.map,

    new_syncing_key : Syncing_Key.ord_key,
    syncing_set : Syncing_Set.set, 

    new_chan_key : Chan_Key.ord_key,
    chan_map : channel Chan_Map.map,

    new_hole_key : Hole_Key.ord_key
  }

  (* start terms *)

  fun extends (longer_path, shorter_path) =
  (case (longer_path, shorter_path) of
    (_, []) => true | 
    ([], y :: ys) => false |
    (x :: xs, ys) => (x :: xs = ys) orelse extends (xs, ys)
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
    Send _ => "send" |
    Recv _ => "recv" |
    Latch _ => "latch" |
    Choose _ => "choose" |
    Offer _ => "offer" | 
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

    Value (v, pos) => value_to_string v |

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

  and value_to_string v =
  (case v of

    List (vs: value list) => surround "" ( 
      String.concatWith "\n" (List.map (fn v => ("# " ^ (value_to_string v))) vs)
    ) |

    Func (lams, fnc_store, mutual_map) => String.surround "val" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Rec fs => String.surround "val" (
      String.concatWith ",\n"
      (List.map from_field_value_to_string fs)
    ) |

    _ => raise (Fail "NOT YET IMPLEMENTED")

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

  and event_value_to_string evt = (case evt of  

    Alloc_Chan => "alloc_chan" |

    Send (k, msg) => String.surround "send_value " (
      (Chan_Key.to_string k) ^ (value_to_string msg)
    ) |

    Recv k => String.surround "recv_value " (
      (Chan_Key.to_string k)
    ) |

    _ => raise (Fail "(NOT IMPLE: event_value_to_string)")

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

  fun set_completions_map (context : global_context, completions_map) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = #suspension_map context,
    completions_map = completions_map,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = #new_chan_key context,
    chan_map = #chan_map context,
 
    new_hole_key = #new_hole_key context
  }


  fun set_syncing_set (context : global_context, syncing_set) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = #suspension_map context,
    completions_map = #completions_map context,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = syncing_set, 
 
    new_chan_key = #new_chan_key context,
    chan_map = #chan_map context,
 
    new_hole_key = #new_hole_key context
  }

  fun set_suspension_map (context : global_context, suspension_map) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = suspension_map,
    completions_map = #completions_map context,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = #new_chan_key context,
    chan_map = #chan_map context,
 
    new_hole_key = #new_hole_key context
  }

  fun set_new_thread_key (context : global_context, new_thread_key) =
  {
    new_thread_key = new_thread_key,
    suspension_map = #suspension_map context,
    completions_map = #completions_map context,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = #new_chan_key context,
    chan_map = #chan_map context,
 
    new_hole_key = #new_hole_key context
  }


  fun set_comm_context
  (
    context : global_context,
    (
      send_completions_map,
      new_send_comm_key,
      recv_completions_map,
      new_recv_comm_key
    )
  ) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = #suspension_map context,
    completions_map = #completions_map context,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = #new_chan_key context,
    chan_map = #chan_map context,
 
    new_hole_key = #new_hole_key context
  }
 
  fun set_new_chan_key (context : global_context, new_chan_key) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = #suspension_map context,
    completions_map = #completions_map context,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = new_chan_key,
    chan_map = #chan_map context,
 
    new_hole_key = #new_hole_key context
  }
 
  fun set_chan_map (context : global_context, chan_map) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = #suspension_map context,
    completions_map = #completions_map context,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = #new_chan_key context,
    chan_map = chan_map,
 
    new_hole_key = #new_hole_key context
  }
 
  fun set_new_syncing_key (context : global_context, new_syncing_key) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = #suspension_map context,
    completions_map = #completions_map context,
 
    new_syncing_key = new_syncing_key, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = #new_chan_key context,
    chan_map = #chan_map context,
 
    new_hole_key = #new_hole_key context
  }

  fun set_new_hole_key (context : global_context, new_hole_key) =
  {
    new_thread_key = #new_thread_key context,
    suspension_map = #suspension_map context,
    completions_map = #completions_map context,
 
    new_syncing_key = #new_syncing_key context, 
    syncing_set = #syncing_set context, 
 
    new_chan_key = #new_chan_key context,
    chan_map = #chan_map context,
 
    new_hole_key = new_hole_key
  }


  fun values_equal (v_a, v_b) =
  (
    raise (Fail "values_equal not yet implemented");
    true
  )

  fun match_symbolic_term_insert symbol_map (pattern, symbolic_term) =
  (case (pattern, symbolic_term) of
    (Value (Blank, _), _) => SOME symbol_map |

    (Sym (Id (id, _), _), _) => (let
      val mutual_map = String_Map.empty
      val thunk = Func (
        [(Value (Blank, ~1), symbolic_term)],
        symbol_map, mutual_map
      )
    in
      SOME (String_Map.insert (symbol_map, id, (NONE, thunk)))
    end) |

    (Id (p_id, _), Id (st_id, _)) =>
    (if p_id = st_id then
      SOME symbol_map
    else
      NONE
    ) |

    (Assoc (p, _), Assoc (st, _)) =>
      match_symbolic_term_insert symbol_map (p, st) |

    (Log (p, _), Log (st, _)) =>
      match_symbolic_term_insert symbol_map (p, st) |

    (Intro_List (p1, p2, _), Intro_List (st1, st2, _)) => (
      (Option.mapPartial
        (fn symbol_map' =>
          match_symbolic_term_insert symbol_map' (p2, st2)
        )
        (match_symbolic_term_insert symbol_map (p1, st1))
      )
    ) |

    (Intro_Func (p_lams, _), Intro_Func (st_lams, _)) =>
      from_lams_match_symbolic_term_insert symbol_map (p_lams, st_lams) |

    (App (p1, p2, _), App (st1, st2, _)) =>
    (Option.mapPartial
      (fn symbol_map' =>
        match_symbolic_term_insert symbol_map' (p2, st2)
      )
      (match_symbolic_term_insert symbol_map (p1, st1))
    ) |

    (Compo (p1, p2, _), Compo (st1, st2, _)) => (
      (Option.mapPartial
        (fn symbol_map' =>
          match_symbolic_term_insert symbol_map' (p2, st2)
        )
        (match_symbolic_term_insert symbol_map (p1, st1))
      )
    ) |

    (With (p1, p2, _), With (st1, st2, _)) =>
    (Option.mapPartial
      (fn symbol_map' =>
        match_symbolic_term_insert symbol_map' (p2, st2)
      )
      (match_symbolic_term_insert symbol_map (p1, st1))
    ) |


    (Intro_Mutual_Rec (p_fields, _), Intro_Mutual_Rec (st_fields, _)) =>
    (
      from_fields_match_symbolic_term_insert symbol_map (p_fields, st_fields)
    ) |


    (Select (p, _), Select (st, _)) =>
    (
      match_symbolic_term_insert symbol_map (p, st)
    ) |

(*
TODO:
    (Event p_transactions, Event st_transactions) =>
      match_symbolic_transactions_insert symbol_map (p_transactions, st_transactions) |
*)

(*
**    (List (ps, _), Value (List (sts, _))) =>
**    (if (List.length ps = List.length sts) then
**      (List.foldl
**        (fn ((p, st), symbol_map_op) => 
**          (Option.mapPartial
**            (fn symbol_map' =>
**              match_symbolic_term_insert symbol_map' (p, st)
**            )
**            symbol_map_op
**          )
**        )
**        (SOME symbol_map)
**        (ListPair.zip (ps, sts))
**      )
**    else
**      NONE
**    ) |
*)


    (Value p_v, Value st_v) =>
    (if values_equal (p_v, st_v) then
      SOME symbol_map
    else
      NONE
    ) |

    (Add_Num (p, _), Add_Num (st, _)) => (
      match_symbolic_term_insert symbol_map (p, st)
    ) |

    (Sub_Num (p, _), Sub_Num (st, _)) => (
      match_symbolic_term_insert symbol_map (p, st)
    ) |

    (Mul_Num (p, _), Mul_Num (st, _)) => (
      match_symbolic_term_insert symbol_map (p, st)
    ) |

    (Div_Num (p, _), Div_Num (st, _)) => (
      match_symbolic_term_insert symbol_map (p, st)
    ) |

    _ => raise (Fail "match_symbolic_term_insert case not yet implemented")

  )

  and from_lams_match_symbolic_term_insert symbol_map (p_lams, st_lams) = 
  (if (List.length p_lams = List.length st_lams) then
    (List.foldl
      (fn (((p1, p2), (st1, st2)), symbol_map_op) =>
        (Option.mapPartial
          (fn symbol_map' =>
            (Option.mapPartial
              (fn symbol_map' =>
                match_symbolic_term_insert symbol_map' (p2, st2)
              )
              (match_symbolic_term_insert symbol_map (p1, st1))
            )
          )
          symbol_map_op
        )
      )
      (SOME symbol_map)
      (ListPair.zip (p_lams, st_lams))
    )
  else
    NONE
  )

  and from_fields_match_symbolic_term_insert symbol_map (p_fields, st_fields) =
  (if (List.length p_fields = List.length st_fields) then
    (List.foldl
      (fn (((p_key, (p_fop, p)), (st_key, (st_fop, st))), symbol_map_op) =>
        (if p_key = st_key andalso p_fop = st_fop then
          (Option.mapPartial
            (fn symbol_map' =>
              match_symbolic_term_insert symbol_map (p, st)
            )
            symbol_map_op
          )
        else 
          NONE
        )
      )
      (SOME symbol_map)
      (ListPair.zip (p_fields, st_fields))
    )
  else
    NONE
  )

  fun match_value_insert (symbol_map, pat, value) = (case (pat, value) of

    (Assoc (pat', _), _) => (
      match_value_insert (symbol_map, pat', value)
    ) |

    (Value (Blank, _), _) => SOME symbol_map |

    (Id (str, _), v) => (
      SOME (String_Map.insert (symbol_map, str, (NONE, v)))
    )|

    (Intro_List (t, t', _), List (v :: vs)) =>
    (Option.mapPartial
      (fn symbol_map' =>
        match_value_insert (symbol_map', t, v)
      )
      (match_value_insert (symbol_map, t', List vs))
    ) |

    (Intro_Rec (p_fields, _), Rec v_fields) => (
      from_fields_match_value_insert
      symbol_map
      (p_fields, v_fields)
    ) |

    (
      Intro_Func ([(Value (Blank, _), p_body)], _),
      Func ([(Value (Blank, _), st_body)], _, _)
    ) => (
      (* function value's local stores are ignored; only syntax is matched; *)
      (* it's up to the user to determine if syntax can actually be evaluated in alternate context *)
      (* variables in pattern are specified by pattern_var syntax (sym f); *)
      (* it may then be used in new context and evaluated with f () *) 
      match_symbolic_term_insert symbol_map (p_body, st_body)
    ) |


    (Value (Num n, _), Num nv) =>
    (if n = nv then
      SOME symbol_map
    else
      NONE
    ) |

    _ => NONE

    (* **TODO**

    (List ([], _), List ([], _)) => SOME symbol_map | 

    (List (t :: ts, _), List (v :: vs, _)) =>
      (Option.mapPartial
        (fn symbol_map' =>
          match_value_insert (symbol_map', t, v)
        )
        (match_value_insert (symbol_map, List (ts, ~1), List (vs, ~1)))
      ) |


    (Intro_Event_Send (t, _), Event_Send_Intro (v, _)) =>
      match_value_insert (symbol_map, t, v) |

    (Intro_Event_Recv (t, _), Event_Recv_Intro (v, _)) =>
      match_value_insert (symbol_map, t, v) |

    (Func p_fnc, Func v_fnc) => (
      if fnc_equal (p_fnc, v_fnc) then
        SOME symbol_map
      else
        NONE
    ) |

    (String (str, _), String (strv, _)) => (
      if str = strv then
        SOME symbol_map
      else
        NONE
    ) |

    (Intro_Rec (p_fields, _), Rec_Intro (v_fields, _)) =>
    (case (p_fields, v_fields) of
      ([], []) => SOME symbol_map |

      ((pk, t) :: ps, _ :: _) =>
      (let
        val (match, remainder) = (List.partition  
          (fn (k, v) => k = pk)
          v_fields
        )
      in
        (case match of
          [(k, v)] => (Option.mapPartial
            (fn symbol_map' => match_value_insert (symbol_map', t, v))
            (match_value_insert (symbol_map, Intro_Rec (ps, ~1), Rec_Intro (remainder, ~1)))
          ) |

          _ => NONE
        )
      end) |

      _ =>
        NONE
      
    ) |

    *)
  )

  and from_fields_match_value_insert symbol_map (p_fields, v_fields) =
  (case p_fields of
    [] => SOME symbol_map |
    (pname, (pfix_op, p)) :: pfs =>
    (let

      val (key_matches, vfs) =
      (List.partition
        (fn (vname, (vfix_op, _)) =>
          pname = vname andalso
          (pfix_op = vfix_op orelse pfix_op = NONE)
        )
        v_fields
      )

      fun match_term key_matches =
      (case key_matches of
        [] => NONE |
        [(vname,(vfix_op, v))] => (Option.mapPartial
          (fn symbol_map' =>
            SOME (String_Map.insert (symbol_map', vname, (vfix_op, v)))
          )
          (match_value_insert (symbol_map, p, v))
        ) |
        _ :: key_matches' => match_term key_matches'
      )

      val symbol_map_op = match_term key_matches

    in
      (Option.mapPartial
        (fn symbol_map' =>
          from_fields_match_value_insert symbol_map' (pfs, vfs)
        )
        symbol_map_op
      )
    end)
  )


  fun hole k = Id (Hole_Key.to_string k, ~1)


  fun continue (result, contin) =
  (let
    val (cmode, lams, symbol_map', mutual_map) = contin
  
    val symbol_map'' =
    (case result of
      Rec fields =>
      (if cmode = Contin_With then
        List.foldl
        (fn ((k, v), symbol_map') =>
          String_Map.insert (symbol_map', k, v)
        )
        symbol_map'
        fields
      else
        symbol_map'
      ) |

      _ => symbol_map'
    )

    (* embed mutual_map within self's functions *)
    val fnc_store = (String_Map.map
      (fn (fix_op, lams) =>
        (fix_op, Func (lams, symbol_map'', mutual_map))
      )
      mutual_map
    )

    val symbol_map''' = (
      String_Map.unionWith
      (fn (_, b) => b)
      (symbol_map'', fnc_store)
    )

    fun match_first lams = (case lams of
      [] => NONE |
      (p, t) :: lams' =>
      (case (match_value_insert (symbol_map''', p, result)) of
        NONE => match_first lams' |
        SOME symbol_map'''' => (
          SOME (t, symbol_map'''')
        )
      )
    )

  in
    (case (match_first lams) of
      NONE =>
      (
        Value (Error (
          "result - " ^
          (value_to_string result) ^
          " - does not match continuation hole pattern"
        ), ~1),
        symbol_map'''
      ) |

      SOME (t_body, symbol_map'''') =>
      (t_body, symbol_map'''')
    )
  end)


  fun apply global_context
  (
    t_fn, t_arg, pos,
    symbol_map,
    contin_stack
  ) =
  (case t_fn of
    Id (id, _) =>
    (case (String_Map.find (symbol_map, id)) of
      SOME (_, v_fn) =>
      (
        (
          App (Value (v_fn, ~1), t_arg, pos), 
          symbol_map,
          contin_stack
        ),
        global_context 
      ) |

      _ =>
      (
        (
          Value (Error ("apply arg variable " ^ id ^ " cannot be resolved"), ~1),
          symbol_map,
          contin_stack
        ),
        global_context
      )

    ) |

    Value (Func (lams, fnc_store, mutual_map), _) =>
    (
      (
        t_arg,
        symbol_map,
        (Contin_App, lams, fnc_store, mutual_map) :: contin_stack
      ),
      global_context
    ) |

    Value (v, pos) =>
    (
      (
        Value (Error ("application of non-function: " ^ (value_to_string v)), pos),
        symbol_map,
        contin_stack
      ),
      global_context 
    ) |

    _ =>
    (let
      val new_hole_key = #new_hole_key global_context
      val contin =
      (
        Contin_Norm,
        [( hole new_hole_key, App (hole new_hole_key, t_arg, pos) )],
        symbol_map,
        String_Map.empty 
      )

      val global_context' = set_new_hole_key (global_context, Hole_Key.inc new_hole_key)
    in
      (
        (
          t_fn,
          symbol_map,
          contin :: contin_stack
        ),
        global_context
      )
    end)
  )


  fun associate_infix symbol_map t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t1' = associate_infix symbol_map t1
    in
      (case (String_Map.find (symbol_map, id)) of
        SOME (SOME (direc, prec), rator) => (case t1' of 
          Compo (Compo (t1a, Id (id1, pos1), p1a), t1b, p1b) =>
          (case (String_Map.find (symbol_map, id1)) of
            SOME (SOME (direc', prec'), rator') =>
            (if (prec' = prec andalso direc = Right) orelse (prec > prec') then
              Compo (
                Compo (t1a, Id (id1, pos1), p1a),
                associate_infix symbol_map (Compo (Compo (t1b, Id (id, pos), p1b), t2, p2)),
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

  fun to_app symbol_map t = (case t of
    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (
      (case (String_Map.find (symbol_map, id)) of
        SOME (SOME (direc, prec), rator) => (
          App (
            Id (id, pos),
            Intro_List (
              to_app symbol_map t1,
              Intro_List (to_app symbol_map t2, Value (Blank, ~1), pos),
              pos
            ),
            pos
          )
        ) |

        _ => (
          App (
            App (to_app symbol_map t1, Id (id, pos), p1),
            to_app symbol_map t2,
            p2
          )
        )
      )
    ) |
    _ => t
  )


  fun reduce_single global_context
  (
    t : term,
    norm_f,
    reduce_f,
    symbol_map : (infix_option * value) String_Map.map,
    contin_stack : contin list
  ) =
  (case t of
    (Id (id, _)) =>
    (case (String_Map.find (symbol_map, id)) of
      SOME (NONE, v) =>
      (
        (Value (reduce_f v, ~1), symbol_map, contin_stack),
        global_context
      ) |

      _  =>
      (
        (
          Value (Error ("reduce single variable " ^ id ^ " cannot be resolved"), ~1),
          symbol_map,
          contin_stack
        ),
        global_context 
      )
    ) |

    Value (v, pos) =>
    (case (reduce_f v) of
      Error msg =>
      (
        (Value (Error msg, pos), symbol_map, contin_stack),
        global_context 
      ) |

      result =>
      (
        (Value (result, pos), symbol_map, contin_stack),
        global_context 
      )

    ) |

    _ =>
    (let
      val new_hole_key = #new_hole_key global_context
      val contin = (
        Contin_Norm,
        [( hole new_hole_key, norm_f (hole new_hole_key) )],
        symbol_map,
        String_Map.empty 
      )
    in
      (
        (t, symbol_map, contin :: contin_stack),
        set_new_hole_key (global_context, Hole_Key.inc new_hole_key)
      )
    end)
  )

(*
    t : term,
    norm_f,
    reduce_f,
    symbol_map : (infix_option * value) String_Map.map,
    contin_stack : contin list
*)

  fun reduce_list global_context
  (
    ts : term list,
    norm_f,
    reduce_f,
    symbol_map : (infix_option * value) String_Map.map,
    contin_stack : contin list
  ) =
  (let
    fun loop (prefix, postfix) =
    (case postfix of
      [] =>
      (case (reduce_f prefix) of 
        Error msg =>
        (
          (Value (Error msg, ~1), symbol_map, contin_stack),
          global_context 
        ) |

        v =>
        (
          (Value (v, ~1), symbol_map, contin_stack),
          global_context 
        )

      ) |

      Id (id, _) :: xs =>
      (case (String_Map.find (symbol_map, id)) of
        SOME (NONE, v) => loop (prefix @ [v], xs) |
        _ => (
          (
            Value (Error ("reduce list variable " ^ id ^ " cannot be resolved"), ~1),
            symbol_map,
            contin_stack
          ),
          global_context 
        )

      ) |

      Value (v, _) :: xs => loop (prefix @ [v], xs) |

      x :: xs =>
      (let
        val new_hole_key = #new_hole_key global_context
        val contin =
        (
          Contin_Norm,
          [(
            hole new_hole_key,
            norm_f (
              (map (fn v => Value (v, ~1)) prefix) @
              (hole new_hole_key :: xs)
            )
          )],
          symbol_map,
          String_Map.empty 
        )
        
      in
        (
          (x, symbol_map, contin :: contin_stack),
          set_new_hole_key (global_context, Hole_Key.inc new_hole_key)
        )
      end)

    )

  in
    loop ([], ts)
  end)


  fun eval_term_step global_context
  (
    t,
    symbol_map : (infix_option * value) String_Map.map,
    contin_stack
  ) =
  (case t of

    Value (v, _) => (case contin_stack of 
      [] => NONE |
      contin :: contin_stack' => (let
        val (t', symbol_map') = continue (v, contin)
        val thread_snippet = (t', symbol_map', contin_stack')
      in
        SOME (thread_snippet, global_context)
      end)
    ) |

    Assoc (t', pos) => SOME
    (
      (t', symbol_map, contin_stack),
      global_context 
    ) |

    Log (t', pos) =>
    SOME (reduce_single global_context (
      t',
      fn t => Log (t, pos),
      fn v => (
        print ((value_to_string v) ^ "\n");
        v
      ),
      symbol_map,
      contin_stack
    )) | 

    Id (id, pos) =>
    (case (String_Map.find (symbol_map, id)) of

      SOME (NONE, v) => SOME
      (
        (Value (v, ~1), symbol_map, contin_stack),
        global_context 
      ) |

      _ => SOME
      (
        (
          Value (Error ("variable " ^ id ^ " cannot be resolved"), ~1),
          symbol_map,
          contin_stack
        ),
        global_context 
      )


    ) |

    Intro_List (t, t', pos) =>
    SOME (reduce_list global_context (
      [t, t'],
      (fn
        [t, t'] => Intro_List (t, t', pos) |
        _ => raise (Fail "Internal: Intro_List")
      ),
      (fn
        [v, Blank] => List [v] |
        [v, List vs] => List (v :: vs) |
        _ => Error "cons with non-list"
      ),
      symbol_map,
      contin_stack
    )) |

    Intro_Func (lams, pos) =>
    SOME (
      (
        Value (Func (lams, symbol_map, String_Map.empty), pos),
        symbol_map,
        contin_stack
      ),
      global_context 
    ) |

    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t_m = associate_infix symbol_map t
      val t' = to_app symbol_map t_m 
    in
      SOME (
        (t', symbol_map, contin_stack),
        global_context
      )
    end) |


    Compo (t1, t2, pos) =>
    SOME (
      (App (t1, t2, pos), symbol_map, contin_stack),
      global_context
    ) |


    App (t_fn, t_arg, pos) =>
    SOME (apply global_context (
      t_fn, t_arg, pos,
      symbol_map, contin_stack
    )) |

    With (t1, t2, _) =>
    (let
      val contin =
      (
        Contin_With,
        [(Value (Blank, ~1), t2)],
        symbol_map,
        String_Map.empty
      )
    in
      SOME (
        (
          t1,
          symbol_map,
          contin :: contin_stack
        ),
        global_context
      )
    end) |

    Intro_Rec (fields, pos) =>
    (let

      val mutual_map =
      (List.foldl
        (fn
          ((k, (fix_op,  Intro_Func (lams, _))), mutual_map) => 
          (
            String_Map.insert (mutual_map, k, (fix_op, lams))
          ) |

          (_, mutual_map) => mutual_map
        )
        String_Map.empty
        fields
      )
      
      (* embed mutual ids into ts' functions *)
      val fields' =
      (List.map
        (fn
          (k, (fix_op, Intro_Func (lams, pos))) => (
            (k, (fix_op, Value (Func (lams, symbol_map, mutual_map), ~1)))
          ) |

          field => field 
        )
       fields 
      )
    in
      SOME (
        (Intro_Mutual_Rec (fields', pos), symbol_map, contin_stack),
        global_context
      )
    end) |

    Intro_Mutual_Rec (fields, pos) => (let
      val ts = (map (fn (k, (fix_op, t)) => t) fields)

      fun map_to_fields ts =
      (List.map
        (fn ((key, (fix_op, _)), t) => (key, (fix_op, t)))
        (ListPair.zip (fields, ts))
      )
    in
      SOME (reduce_list global_context (
        ts,
        fn ts => Intro_Mutual_Rec (map_to_fields ts, pos),
        fn vs => Rec (map_to_fields vs),
        symbol_map, contin_stack
      ))
    end) |

    Select (t, pos) =>
    SOME (reduce_single global_context (
      t,
      fn t => Select (t, pos),
      (fn
        List ([Rec fields, String key]) =>
        (case (
          List.find
          (fn (k, v) => k = key)
          fields
        ) of
          SOME (_, (_, v)) => v |
          NONE => Error "selection not found"
        ) |

        _ => Error "selecting from non-record"
      ),
      symbol_map,
      contin_stack
    )) |

    Add_Num (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Add_Num (t, pos),
      (fn
        List ([Num n1, Num n2]) =>
          Num (num_add (n1, n2)) |
        _ => Error "adding non-numbers"
      ),
      symbol_map,
      contin_stack
    )) |

    Sub_Num (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Sub_Num (t, pos),
      (fn
        List ([Num n1, Num n2]) => (
          Num (num_sub (n1, n2))
        ) |
        _ => Error "subtracting non-numbers"
      ),
      symbol_map,
      contin_stack
    )) |

    Mul_Num (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Mul_Num (t, pos),
      (fn
        List ([Num n1, Num n2]) => (
          Num (num_mul (n1, n2))
        ) |
        _ => Error "multplying non-numbers"
      ),
      symbol_map,
      contin_stack
    )) |

    Div_Num (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Div_Num (t, pos),
      (fn
        List ([Num n1, Num n2]) => (
          Num (num_div (n1, n2))
        ) |
        _ => Error "dividing non-numbers"
      ),
      symbol_map,
      contin_stack
    )) |

    Sym (_, pos) =>
    SOME (
      (Value (Error "symbol used in non-pattern", pos), symbol_map, contin_stack),
      global_context 
    ) |

    Intro_Send (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Send (t, pos),
      (fn
        List ([Chan k, msg]) =>
          Event (Send (k, msg)) |
        _ => Error "intro_send without channel-message pair"
      ),
      symbol_map,
      contin_stack
    )) |

    Intro_Recv (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Recv (t, pos),
      (fn
        Chan k =>
          Event (Recv k) |
        _ => Error "intro_recv without channel"
      ),
      symbol_map,
      contin_stack
    )) |

    Intro_Latch (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Latch (t, pos),
      (fn
        List [Event event, Func (lams, fnc_store, mutual_map)] =>
          Event (Latch (event, lams, fnc_store, mutual_map)) |
        _ => Error "intro_latch without event-funciton pair"
      ),
      symbol_map,
      contin_stack
    )) |

    Intro_Choose (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Choose (t, pos),
      (fn
        List [Event event_left, Event event_right] =>
          Event (Choose (event_left, event_right)) |
        _ => Error "intro_choose without event-event pair"
      ),
      symbol_map,
      contin_stack
    )) |

    Intro_Offer (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Offer (t, pos),
      (fn v => Event (Offer v)),
      symbol_map,
      contin_stack
    )) |

    Intro_Abort pos =>
    SOME (
      (
        Value (Event Abort, pos),
        symbol_map,
        contin_stack
      ),
      global_context
    ) |

    Intro_Sync (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Sync (t, pos),
      (fn
        Event event => Effect (Sync event) |
        _ => Error "intro_sync without event"
      ),
      symbol_map,
      contin_stack
    )) |

    Intro_Exec (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Exec (t, pos),
      (fn
        Effect effect => Effect (Exec effect) |
        _ => Error "intro_exec without effect"
      ),
      symbol_map,
      contin_stack
    )) |

    Intro_Return (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Return (t, pos),
      (fn v => Effect (Return v)),
      symbol_map,
      contin_stack
    )) |

    Intro_Bind (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Bind (t, pos),
      (fn
        List [Effect effect, Func (lams, fnc_store, mutual_map)] =>
          Effect (Bind (effect, lams, fnc_store, mutual_map)) |
        _ => Error "intro_bind without effect-funciton pair"
      ),
      symbol_map,
      contin_stack
    ))

  )

  fun exec_effect_step global_context (effect, thread_key, effect_stack) =
  (case effect of
    Return v =>
    (case effect_stack of
      [] =>
      (let
        val new_thread =
        (
          Value (v, ~1),
          {
            thread_key = thread_key,
            symbol_map = String_Map.empty,
            term_stack = [],
            thread_mode = Exec_Effect effect_stack
          }
        )
      in
        ([new_thread], global_context)
      end) |

      contin :: contin_stack =>
      (let
        val (t', symbol_map') = continue (v, contin)

        val new_thread =
        (
          t',
          {
            thread_key = thread_key,
            symbol_map = symbol_map',
            term_stack = [],
            thread_mode = Exec_Effect effect_stack
          }
        )
      in
        ([new_thread], global_context)
      end)
    ) |

    Bind (effect', lams, fnc_store, mutual_map) =>
    (let
      val new_thread = 
      (
        Value (Effect effect', ~1),
        {
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Exec_Effect
          (
            (Contin_Bind, lams, fnc_store, mutual_map) ::
            effect_stack
          )
        }
      )
    in
      ([new_thread], global_context)
    end) |

    Exec effect' => (let
      val parent_thread = 
      (
        Value (Effect (Return Blank), ~1),
        {
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Exec_Effect effect_stack
        }
      )

      val new_thread_key = #new_thread_key global_context
      val child_thread = 
      (
        Value (Effect effect', ~1),
        {
          thread_key = new_thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Exec_Effect [] 
        }
      )

      val global_context' = set_new_thread_key (global_context, Thread_Key.inc new_thread_key) 
      
    in
      ([parent_thread, child_thread], global_context')
    end) |


    Sync evt => (let
      val new_syncing_key = #new_syncing_key global_context

      val new_thread =
      (
        Value (Event evt, ~1),
        { 
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Sync_Event (new_syncing_key, [], Thread_Map.empty, [])
        }
      )

      val global_context' = set_new_syncing_key (global_context, Syncing_Key.inc new_syncing_key) 
    in
      ([new_thread], global_context')
    end)

  )

  fun add_communication
  (comm_map, comm_thread_key, new_comm_syncing_key, new_comm_path) =  
  (
    (case (Thread_Map.find (comm_map, comm_thread_key)) of
      NONE => SOME (Thread_Map.insert (
        comm_map,
        comm_thread_key,
        (new_comm_syncing_key, new_comm_path)
      )) |
      SOME (comm_syncing_key, comm_path) =>
      (if not (new_comm_syncing_key = comm_syncing_key) then
        raise (Fail "search thread sync keys do not match for same thread")
      else if (extends (new_comm_path, comm_path)) then
        SOME (Thread_Map.insert (
          comm_map,
          comm_thread_key,
          (new_comm_syncing_key, new_comm_path)
        ))
      else
        NONE
      )

    )
  )

  fun comm_send_recv (waiting_send, waiting_recv) =
  (let

    val (send_thread_key, send_search_thread, msg) =
    waiting_send 

    val (send_syncing_key, send_path, send_comm_map, send_stack) =
    send_search_thread

    val (recv_thread_key, recv_search_thread) =
    waiting_recv 

    val (recv_syncing_key, recv_path, recv_comm_map, recv_stack) =
    recv_search_thread

    val send_head = Send_Comm (recv_thread_key, recv_path) 
    
    val recv_head = Recv_Comm (send_thread_key, send_path) 

    (* update recv_comm_map if send thread extends previous communication partner of recv thread *) 

    val recv_comm_map_op_f = (fn () =>
      add_communication
      (recv_comm_map, send_thread_key, send_syncing_key, send_head :: send_path)
    )

    val send_comm_map_op_f = (fn () =>
      add_communication
      (send_comm_map, recv_thread_key, recv_syncing_key, recv_head :: recv_path)
    )

  in
    Option.mapPartial
    (fn recv_comm_map' =>
      Option.map
      (fn send_comm_map' =>
        (let
          val send_thread =
          (
            Value (Event (Offer Blank), ~1),
            {
              thread_key = send_thread_key,
              symbol_map = String_Map.empty,
              term_stack = [],
              thread_mode = Sync_Event (
                send_syncing_key,
                send_head :: send_path,
                send_comm_map',
                send_stack
              )
            }
          )

          val recv_thread =
          (
            Value (Event (Offer msg), ~1),
            {
              thread_key = recv_thread_key,
              symbol_map = String_Map.empty,
              term_stack = [],
              thread_mode = Sync_Event (
                recv_syncing_key,
                recv_head :: recv_path,
                recv_comm_map',
                recv_stack
              )
            }
          )
        in
          (send_thread, recv_thread)
        end)
      )
      (send_comm_map_op_f ())
    )
    (recv_comm_map_op_f ())
  end)

  fun clean_chan syncing_set
  (
    chan,
    new_sends : waiting_send list,
    new_recvs : waiting_recv list
  ) =
  (let
    val (waiting_sends, waiting_recvs) = chan
    val waiting_sends' = (List.foldl
      (fn (send as (_, search_thread as (syncing_key, _, _, _), _), waiting_sends') =>
        if Syncing_Set.member (syncing_set, syncing_key) then
          send :: waiting_sends'
        else
          waiting_sends
      )
      new_sends
      waiting_sends
    )

    val waiting_recvs' = (List.foldl
      (fn (recv as (_, search_thread as (syncing_key, _, _, _)), waiting_recvs') =>
        (if Syncing_Set.member (syncing_set, syncing_key) then
          recv :: waiting_recvs'
        else
          waiting_recvs
        )
      )
      new_recvs
      waiting_recvs
    )
  in
    (waiting_sends', waiting_recvs')
  end)


  fun find_commit_maps (global_context : global_context)
  (
    commit_map : completion Thread_Map.map,
    comm_list : (Thread_Key.ord_key * (Syncing_Key.ord_key * past_event list)) list 
  ) =
  (case comm_list of
    (comm_thread_key, (comm_syncing_key, comm_path)) :: comm_list' =>
    (if Syncing_Set.member (#syncing_set global_context, comm_syncing_key) then
      (case Thread_Map.find (commit_map, comm_thread_key) of
        SOME (come_completion as (_, comm_complete_path, _, _)) =>
        (if extends (comm_complete_path, comm_path) then
          find_commit_maps global_context (commit_map, comm_list') 
        else
          []
        ) |

        NONE =>
        (let
          val completions_map = #completions_map global_context 
          val completions = Thread_Map.lookup (completions_map, comm_thread_key)
          val commit_maps = List.concat (
            List.map
            (fn completion as (_, complete_path, completion_comm_map, _) => 
              (let
                val commit_map' = Thread_Map.insert (commit_map, comm_thread_key, completion)
                val completion_comm_list = Thread_Map.listItemsi completion_comm_map
              in
                find_commit_maps global_context (commit_map', completion_comm_list)
              end)
            )
            completions
          )

          val commit_maps' = List.concat (
            List.map
            (fn commit_map => 
              find_commit_maps global_context (commit_map, comm_list')
            )
            commit_maps 
          )
        in
          commit_maps'
        end)
      )
    else
      []
    ) |

    [] => [commit_map]
  )


  fun add_completion (completions_map, thread_key, completion) =
  (let
    val completions_op = Thread_Map.find (completions_map, thread_key) 
    val completions =
    (case completions_op of
      SOME completions => completion :: completions |
      NONE => [completion]
    )
  in
    Thread_Map.insert (completions_map, thread_key, completions)
  end)

  fun sync_event_step global_context
  (
    event,
    thread_key,
    search_thread as (syncing_key, path, comm_map, event_stack)
  ) =
  (case event of
    Offer v =>
    (case event_stack of
      [] =>
      (let
        val completion : completion = (syncing_key, path, comm_map, v)

        val completions_map = #completions_map global_context

        (** add new completion to own path **)
        val completions_map' = add_completion (completions_map, thread_key, completion)

        (** find a all completion combinations that are commitable **) 
        (** completion combination = Map of thread_id -> completion **)
        val init_commit_map = Thread_Map.singleton (thread_key, completion)
        val commit_maps : (completion Thread_Map.map) list =
        (find_commit_maps global_context (init_commit_map, Thread_Map.listItemsi comm_map))
        val final_commit_map = List.hd commit_maps

        (** remove syncing keys for commitable paths **)
        val syncing_set = #syncing_set global_context
        val syncing_set' = 
        (
          Thread_Map.foldl
          (fn (compl as (syncing_key, _, _, _), syncing_set) =>
            Syncing_Set.delete (syncing_set, syncing_key)
          ) 
          syncing_set
          final_commit_map
        )

        (** take offerings of commitable completions and continue thread suspension **)
        val suspension_map = #suspension_map global_context
        val (synced_threads, suspension_map') =
        (
          Thread_Map.foldli
          (fn
            (
              thread_key,
              completion as (_, _, _, v),
              (synced_threads, suspension_map)
            ) =>
            (let
              val (suspension_map', effect_stack) =
              Thread_Map.remove (suspension_map, thread_key)

              val synced_thread =
              (
                Value (Effect (Return v), ~1),
                {
                  thread_key = thread_key,
                  symbol_map = String_Map.empty,
                  term_stack = [],
                  thread_mode = Exec_Effect effect_stack 
                }
              )
            in
              (
                synced_thread :: synced_threads,
                suspension_map'
              )
            end)
          )
          ([], suspension_map)
          final_commit_map
        )

        (* remove completions that are in commit_map *)
        val completions_map' =
        (Thread_Map.foldli
          (fn (commit_thread_key, _, completions_map) =>
            #1 (Thread_Map.remove (completions_map, commit_thread_key)) 
          )
          completions_map
          final_commit_map
        )

        val global_context' = set_syncing_set (global_context, syncing_set')
        val global_context' = set_suspension_map (global_context', suspension_map')
        val global_context' = set_completions_map (global_context, completions_map')

      in
        (synced_threads, global_context')
      end) |

      effect_contin :: effect_stack' =>
      (let
        val (t', symbol_map) = continue (v, effect_contin)
        val new_thread =
        (
          t',
          {
            thread_key = thread_key,
            symbol_map = symbol_map,
            term_stack = [],
            thread_mode = Sync_Event (syncing_key, path, comm_map, effect_stack')
          }
        )
      in
        ([new_thread], global_context)
      end)
    ) |

    Abort => ([], global_context) |

    Alloc_Chan =>
    (let
      val new_chan = ([], [])    
      val new_chan_key = #new_chan_key global_context
      val chan_map = #chan_map global_context

      val chan_map' = Chan_Map.insert (chan_map, new_chan_key, new_chan)
      val new_chan_key' = Chan_Key.inc new_chan_key

      val global_context' = (
        set_new_chan_key
        (
          set_chan_map (global_context, chan_map'),
          new_chan_key'
        )
      )

      val new_thread =
      (
        Value (Event (Offer (Chan new_chan_key)), ~1),
        {
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Sync_Event search_thread 
        }
      )
    in
      ([new_thread], global_context')
    end) |

    Latch (event', lams, fnc_store, mutual_map) =>
    (let
      val new_thread = 
      (
        Value (Event event', ~1),
        {
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Sync_Event
          (
            syncing_key,
            path,
            comm_map,
            (Contin_Latch, lams, fnc_store, mutual_map) ::
            event_stack
          )
        }
      )
    in
      ([new_thread], global_context)
    end) |

    Choose (evt_l, evt_r) =>
    (let
      val left_thread =
      (
        Value (Event evt_l, ~1),
        {
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Sync_Event (syncing_key, Left_Choice :: path, comm_map, event_stack)
        }
      )

      val right_thread =
      (
        Value (Event evt_r, ~1),
        {
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Sync_Event (syncing_key, Right_Choice :: path, comm_map, event_stack)
        }
      )

    in
      ([left_thread, right_thread], global_context)
    end) |

    
    Send (chan_key, msg) =>
    (let

      val (syncing_key, path, comm_map, event_stack) = search_thread

      val chan_map = #chan_map global_context 
      (* Expectation: chan_key certainly exists in chan_map; raise exception otherwise *)
      val chan = Chan_Map.lookup (chan_map, chan_key)

      val syncing_set = #syncing_set global_context 

      val waiting_send = (thread_key, search_thread, msg)
      val chan' = clean_chan syncing_set (chan, [waiting_send], [])
      val (_, waiting_recvs) = chan'


      val new_threads =
      (List.foldl  
        (fn (waiting_recv, new_threads) =>
          (let
            val comm_threads =
            (case (comm_send_recv (waiting_send, waiting_recv)) of
              SOME (send_thread, recv_thread) => [send_thread, recv_thread] |
              NONE => []
            )
          in
            comm_threads @ new_threads
          end)
        )
        []
        waiting_recvs
      )

      val chan_map' = Chan_Map.insert (chan_map, chan_key, chan')
      val global_context' = set_chan_map (global_context, chan_map')

    in
      (new_threads, global_context')
    end) |

    Recv chan_key =>
    (let
      val chan_map = #chan_map global_context 
      (* Expectation: chan_key certainly exists in chan_map; raise exception otherwise *)
      val chan = Chan_Map.lookup (chan_map, chan_key)

      val syncing_set = #syncing_set global_context 

      val waiting_recv = (thread_key, search_thread)
      val chan' = clean_chan syncing_set (chan, [], [waiting_recv])
      val (waiting_sends, _) = chan'


      val new_threads =
      (List.foldl  
        (fn (waiting_send, new_threads) =>
          (let
            val comm_threads =
            (case (comm_send_recv (waiting_send, waiting_recv)) of
              SOME (send_thread, recv_thread) => [send_thread, recv_thread] |
              NONE => []
            )
          in
            comm_threads @ new_threads
          end)
        )
        []
        waiting_sends
      )

      val chan_map' = Chan_Map.insert (chan_map, chan_key, chan')
      val global_context' = set_chan_map (global_context, chan_map')

    in
      (new_threads, global_context')
    end)
  )

  fun concur_step global_context threads =
  (case threads of
    [] => NONE |
    (t, thread_context as {thread_key, symbol_map, term_stack, thread_mode}) :: threads' =>
    (case (t, term_stack, thread_mode) of
      (* exec effect case *)
      (Value (Effect effect, _), [], Exec_Effect effect_stack) =>
      (let
        val (new_threads, global_context') = (
          exec_effect_step
          global_context
          (effect, thread_key, effect_stack)
        )

      in
        SOME (threads @ new_threads, global_context')
      end) |

      (* sync event case *)
      (Value (Event event, _), [], Sync_Event search_thread) =>
      (let
        val (new_threads, global_context') =
        (
          sync_event_step
          global_context
          (event, thread_key, search_thread)
        )
      in
        SOME (threads' @ new_threads, global_context')
      end) |

      (* eval term case *)
      _ =>
      (let
        val result = eval_term_step global_context (t, symbol_map, term_stack)

        val (new_threads, global_context') =
        (case result of
          SOME ((t', symbol_map', term_stack'), global_context') => 
          (
            [(
              t',
              {
                thread_key = thread_key,
                symbol_map = symbol_map',
                term_stack = term_stack',
                thread_mode = thread_mode
              }
            )],
            global_context'
          ) | 
          NONE => ([], global_context)
        )
      in
        SOME (threads' @ new_threads, global_context')
      end)

    )
  )


  fun eval t =
  (let

    val thread_context = {
      thread_key = Thread_Key.zero,
      symbol_map = String_Map.empty, 
      term_stack = [],
      thread_mode = Exec_Effect [] 
    }

    val thread = (t, thread_context)

    val global_context = {
      new_thread_key = Thread_Key.inc (#thread_key thread_context),
      suspension_map = Thread_Map.empty,
      completions_map = Thread_Map.empty,

      new_syncing_key = Syncing_Key.zero, 
      syncing_set = Syncing_Set.empty, 

      new_chan_key = Chan_Key.zero,
      chan_map = Chan_Map.empty,

      new_hole_key = Hole_Key.zero
    }

    fun loop global_context threads =
    (case (concur_step global_context threads) of
      NONE => () |
      SOME (threads, global_context') =>
        loop global_context' threads 
    )

  in
    loop global_context [thread]
  end)


end (* struct *)





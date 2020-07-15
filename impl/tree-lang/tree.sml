structure Tree =
struct


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

  structure Hole_Key = Key_Fn (val tag = "_g")

  structure String_Map = RedBlackMapFn
  (struct
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
    Intro_Abort of (term * int) |

    (* effect *)
    Intro_Return of (term * int) |
    Intro_Run of (term * int) |
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
    Bind of effect * contin |
    Exec of effect |
    Run of event 

  and event =
    Offer of value |
    Abort |
    Alloc_Chan | 
    Send of Chan_Key.ord_key * value  |
    Recv of Chan_Key.ord_key |
    Latch of event * contin |
    Choose of event * event

  and contin = Contin of (
    contin_mode * 
    ((term * term) list) *
    ((infix_option * value) String_Map.map) *
    ((infix_option * (term * term) list) String_Map.map)
  )

  and contin_mode =
    Contin_With | Contin_Norm | Contin_App |
    Contin_Bind

  datatype past_event =  
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
  
  type history = (Thread_Key.ord_key * past_event list)

  type global_context =
  {

    new_thread_key : Thread_Key.ord_key,
    suspension_map : (contin list) Thread_Map.map,

    new_running_key : Running_Key.ord_key,
    running_set : Running_Set.set, 

    new_chan_key : Chan_Key.ord_key,
    chan_map : channel Chan_Map.map,

    new_sync_send_key : Sync_Send_Key.ord_key,
    send_completion_map : (history list) Sync_Send_Map.map,

    new_sync_recv_key : Sync_Recv_Key.ord_key,
    recv_completion_map : (history list) Sync_Recv_Map.map,

    new_hole_key : Hole_Key.ord_key
  }

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
    val Contin (cmode, lams, symbol_map', mutual_map) = contin
  
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


  fun apply (
    t_fn, t_arg, pos,
    symbol_map,
    contin_stack,
    hole_key
  ) = (case t_fn of
    Id (id, _) =>
    (case (String_Map.find (symbol_map, id)) of
      SOME (_, v_fn) =>
      (
        App (Value (v_fn, ~1), t_arg, pos), 
        symbol_map,
        contin_stack,
        hole_key
      ) |

      _ =>
      (
        Value (Error ("apply arg variable " ^ id ^ " cannot be resolved"), ~1),
        symbol_map,
        contin_stack,
        hole_key
      )

    ) |

    Value (Func (lams, fnc_store, mutual_map), _) =>
    (
      t_arg,
      symbol_map,
      (Contin_App, lams, fnc_store, mutual_map) :: contin_stack,
      hole_key
    ) |

    Value (v, pos) =>
    (
      Value (Error ("application of non-function: " ^ (value_to_string v)), pos),
      symbol_map,
      contin_stack,
      hole_key
    ) |

    _ =>
    (
      t_fn,
      symbol_map,
      (
        Contin_Norm,
        [(
          hole hole_key,
          App (hole hole_key, t_arg, pos)
        )],
        symbol_map,
        String_Map.empty 
      ) :: contin_stack,
      Hole_Key.inc hole_key
    )
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


  fun reduce_single (
    t, norm_f, reduce_f,
    symbol_map,
    contin_stack,
    hole_key
  ) = (case t of

    (Id (id, _)) =>
    (case (String_Map.find (symbol_map, id)) of
      SOME (NONE, v) =>
      (
        Value (reduce_f v, ~1),
        symbol_map,
        contin_stack,
        hole_key
      ) |

      _  =>
      (
        Value (Error ("reduce single variable " ^ id ^ " cannot be resolved"), ~1),
        symbol_map,
        contin_stack,
        hole_key
      )

    ) |

    Value (v, pos) =>
    (case (reduce_f v) of
      Error msg =>
      (
        Value (Error msg, pos), 
        symbol_map,
        contin_stack,
        hole_key
      ) |

      result =>
      (
        Value (result, pos),
        symbol_map,
        contin_stack,
        hole_key
      )

    ) |

    _ =>
    (let
      val contin = (
        Contin_Norm,
        [( hole hole_key, norm_f (hole hole_key) )],
        symbol_map,
        []
      )
    in
      (
        t,
        symbol_map,
        contin :: contin_stack,
        Hole_Key.inc hole_key
      )
    end)

  )

  fun reduce_list
  (
    ts, norm_f, reduce_f,
    symbol_map, contin_stack,
    hole_key
  ) =
  (let
    fun loop (prefix, postfix) =
    (case postfix of
      [] =>
      (case (reduce_f prefix) of 
        Error msg =>
        (
          Value (Error msg, ~1), 
          symbol_map, contin_stack,
          hole_key
        ) |

        v =>
        (
          Value (v, ~1),
          symbol_map,
          contin_stack,
          hole_key
        )

      ) |

      Id (id, _) :: xs =>
      (case (String_Map.find (symbol_map, id)) of
        SOME (NONE, v) => loop (prefix @ [v], xs) |
        _ => (
          Value (Error ("reduce list variable " ^ id ^ " cannot be resolved"), ~1),
          symbol_map, contin_stack,
          hole_key
        )

      ) |

      Value (v, _) :: xs => loop (prefix @ [v], xs) |

      x :: xs =>
      (let
        val contin =
        (
          Contin_Norm,
          [(
            hole hole_key,
            norm_f (
              (map (fn v => Value (v, ~1)) prefix) @
              (hole hole_key :: xs)
            )
          )],
          symbol_map,
          []
        )
      in
        (
          x,
          symbol_map,
          contin :: contin_stack,
          Hole_Key.inc hole_key
        )
      end)

    )

  in
    loop ([], ts)
  end)
  

  fun eval_term_step (t, symbol_map, contin_stack, hole_key) = (case t of

    _ => (* TODO *) raise (Fail "eval_term_step")
    (*
    Value (v, _) => (case contin_stack of 
      [] => NONE |
      contin :: contin_stack' => (let
        val (t', symbol_map') = continue (v, contin)
      in
        SOME (t', symbol_map', contin_stack', hole_key)
      end)
    ) |



    Assoc (t', pos) =>
    SOME (
      t', symbol_map, contin_stack, hole_key
    ) |

    Log (t', pos) => SOME (reduce_single (
      t',
      fn t => Log (t, pos),
      fn v => (
        print ((value_to_string v) ^ "\n");
        v
      ),
      symbol_map,
      contin_stack,
      hole_key
    )) | 

    Id (id, pos) =>
    (case (String_Map.find (symbol_map, id)) of
      SOME (NONE, v) =>
      (
        Value (v, ~1),
        symbol_map,
        contin_stack,
        hole_key
      ) |

      _ =>
      (
        Value (Error ("variable " ^ id ^ " cannot be resolved"), ~1),
        symbol_map,
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
        [v, Blank] => List ([v], pos) |
        [v, List vs] => List (v :: vs, pos) |
        _ => Error "cons with non-list"
      ),
      symbol_map,
      contin_stack,
      hole_key

    )) |


    Intro_Func (lams, pos) =>
      (
        Value (Func (lams, symbol_map, []), ~1),
        symbol_map,
        contin_stack,
        hole_key
      ) |

    (*
    Func_Mutual (lams, [], mutual_map, pos) =>
        (
          Value (Func (lams, symbol_map, mutual_map), ~1),
          symbol_map,
          contin_stack,
          hole_key
        ) |
    *)


    Compo (Compo (t1, Id (id, pos), p1), t2, p2) => (let
      val t_m = associate_infix symbol_map t
      val t' = to_app symbol_map t_m 
    in
      (t', symbol_map, contin_stack, hole_key)
    end) |

    Compo (t1, t2, pos) => (
      App (t1, t2, pos), symbol_map, contin_stack, hole_key
    ) |


    App (t_fn, t_arg, pos) => apply (
      t_fn, t_arg, pos,
      symbol_map, contin_stack,
      hole_key
    ) |


    With (t1, t2, _) =>
    (
      t1,
      symbol_map,
      SOME (Contin_With, [(hole hole_key, t2)], symbol_map, []),
      Hole_Key.inc hole_key
    ) |

    Intro_Rec (fields, pos) =>
    (let

      val mutual_map =
      (List.mapPartial
        (fn
          (k, (fix_op,  Intro_Func (lams, _))) => 
            SOME (k, (fix_op, lams)) |
          _ => NONE
        )
        fields
      )
      
      (* embed mutual ids into ts' functions *)
      val fields' =
      (List.map
        (fn
          (k, (fix_op, Intro_Func (lams, pos))) =>
            (k, (fix_op, Value (Func (lams, symbol_map, mutual_map))), ~1) |
          field => field 
        )
       fields 
      )
    in
      (
        Intro_Mutual_Rec (fields', pos), 
        symbol_map, contin_stack, hole_key
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
        symbol_map, contin_stack,
        hole_key
      ))
    end) |

    Select (t, pos) => SOME (reduce_single (
      t,
      fn t => Select (t, pos),
      (fn
        List ([Rec (fields, _), String (key, _)]) =>
        (case String_Map.find (fields, key) of
          SOME (_, v) => v |
          NONE => Error "selection not found"
        ) |

        _ => Error "selecting from non-record"
      ),
      symbol_map,
      contin_stack,
      hole_key

    )) |

    Add_Num (t, pos) => SOME (reduce_single (
      t, fn t => Add_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)]) =>
          Num (num_add (n1, n2)) |
        _ => Error "adding non-numbers"
      ),
      symbol_map,
      contin_stack,
      hole_key
    )) |

    Sub_Num (t, pos) => SOME (reduce_single (
      t, fn t => Sub_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)]) => (
          Num (num_sub (n1, n2))
        ) |
        _ => Error "subtracting non-numbers"
      ),
      symbol_map,
      contin_stack,
      hole_key
    )) |

    Mul_Num (t, pos) => SOME (reduce_single (
      t, fn t => Mul_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_mul (n1, n2))
        ) |
        _ => Error "multplying non-numbers"
      ),
      symbol_map,
      contin_stack,
      hole_key
    )) |

    Div_Num (t, pos) => SOME (reduce_single (
      t, fn t => Div_Num (t, pos),
      (fn
        List ([Num (n1, _), Num (n2, _)], _) => (
          Num (num_div (n1, n2))
        ) |
        _ => Error "dividing non-numbers"
      ),
      symbol_map,
      contin_stack,
      hole_key
    ))
    *)

  )


  (*
  (t, thread_context as {thread_key, symbol_map, contin_stack, thread_mode}) :: threads' =>
  *)
  fun mk_effect_thread (t, thread_key, symbol_map, effect_stack) =  
  (
    t,
    {
      thread_key = thread_key,
      symbol_map = symbol_map,
      contin_stack = [],
      thread_mode = Exec_Effect effect_stack
    }
  )


  fun exec_effect_step new_thread_key (effect, thread_key, effect_stack) =
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
            contin_stack = [],
            thread_mode = Exec_Effect effect_stack
          }
        )
      in
        ([new_thread], new_thread_key)
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
            contin_stack = [],
            thread_mode = Exec_Effect effect_stack
          }
        )
      in
        ([new_thread], new_thread_key)
      end)
    ) |

    _ => (* TODO *)([], Thread_Key.zero)
    (*

    Bind (effect', v) => (case v of
      Func (lams, fnc_store, mutual_map, _) =>
      (
        [(
          thread_id,
          Value (Effect effect', ~1),
          symbol_map,
          [],
          Exec_Effect (Contin_Bind, lams, fnc_store, mutual_map) :: effect_stack
        )],
        new_thread_key
      ) |

      _ =>
      [(
        thread_id,
        Value (Error "bind with non-function", ~1),
        symbol_map,
        [],
        Exec_Effect effect_stack
      )]
    ) |

    Exec effect' => (let
      val parent_thread = 
      (
        thread_id,
        Value (Effect (Return Blank), ~1),
        symbol_map,
        [],
        Exec_Effect effect_stack
      )

      val new_thread = 
      (
        new_thread_key,
        Value (Effect effect', ~1),
        symbol_map,
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
    *)
  )


(*

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
      Value (Event (Offer Blank), ~1),
      String_Map.empty,
      [],
      Run_Effect (
        send_running_key, send_head :: send_trail, send_contin
      )
    )

    val recv_thread = (
      recv_thread_key,
      Value (Event (Offer msg), ~1),
      String_Map.empty,
      [],
      Run_Effect (
        recv_running_key, recv_head :: recv_trail, recv_contin
      )
    )

    val new_threads = [send_thread, recv_thread]

  in
    (new_threads, sync_config')
  end)

  fun run_event_step (
    thread_key, event, trail, contin_stack,
    suspension_map,
    running_config, chan_config, sync_config
  ) = (case event of
    Offer v =>  (case contin_stack of
      [] => (* TODO: Try to commit or just leave around *) |
      contin :: contin_stack' => (let
        val (t', symbol_map) = continue (v, contin)
        val new_threads = [(
          thread_key,
          t',
          symbol_map,
          [],
          Run_Effect (trail, effect_stack) 
        )]
      in
        (
          new_threads,
          suspension_map,
          running_config,
          chan_config,
          sync_config
        )
      end)
    ) |

    Abort =>
    (
      [],
      suspension_map,
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
        Value (Event (Offer (Chan new_chan_key)), ~1),
        String_Map.empty,
        [],
        Run_Effect (trail, effect_stack) 
      )]

    in
      (
        new_threads,
        suspension_map,
        running_config,
        chan_config',
        sync_config
      )
    end) |
      
    Latch (event', contin) =>
    (let
      val contin_stack' = contin :: contin_stack

      val new_threads = [(
        thread_key,
        Value (Event event', ~1),
        String_Map.empty,
        [],
        Run_Effect (trail, contin_stack') 
      )]
    in
      (
        new_threads,
        suspension_map,
        running_config,
        chan_config,
        sync_config
      )
    end) |

    Choose (evt_l, evt_r) => (let
      val new_threads = [
        (
          thread_key, Value (Effect evt_l, ~1),
          String_Map.empty, [],
          Run_Effect (Choose_Left :: trail, effect_stack)
        ),
        (
          thread_key, Value (Effect evt_r, ~1),
          String_Map.empty, [],
          Run_Effect (Choose_Right :: trail, effect_stack)
        )
      ]
    in
      (
        new_threads,
        suspension_map,
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
        (fn (waiting_recv, (new_threads, sync_config)) => let
          val (synched_threads, sync_config') = sync_send_recv sync_config (waiting_send, waiting_recv)
        in
           synched_threads @ new_threads
        end)
        ([], sync_config)
        waiting_recvs
      )
    in
      (
        new_threads,
        suspension_map,
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

*)

   fun set_new_thread_key (context : global_context, new_thread_key) =
   {
     new_thread_key = new_thread_key,
     suspension_map = #suspension_map context,

     new_running_key = #new_running_key context, 
     running_set = #running_set context, 

     new_chan_key = #new_chan_key context,
     chan_map = #chan_map context,

     new_sync_send_key = #new_sync_send_key context,
     send_completion_map = #send_completion_map context,

     new_sync_recv_key = #new_sync_recv_key context,
     recv_completion_map = #recv_completion_map context,

     new_hole_key = #new_hole_key context
   }


  fun concur_step global_context threads =
  (case threads of
    [] => NONE |
    (t, thread_context as {thread_key, symbol_map, contin_stack, thread_mode}) :: threads' =>
    (case (t, contin_stack, thread_mode) of
      (* exec effect case *)
      (Value (Effect effect, _), [], Exec_Effect effect_stack) =>
      (let
        val (new_threads, new_thread_key') = (
          exec_effect_step
          (#new_thread_key global_context)
          (effect, thread_key, effect_stack)
        )

        val threads' = threads' @ new_threads

        val global_context' = set_new_thread_key (global_context, new_thread_key') 

      in
        SOME (threads', global_context')
      end) |

      _ => (* TODO *) NONE
      (*


      (* run event case *)
      (Value (Event event, _), [], Run_Event (trail, event_stack)) => (let

        val (
          new_threads, suspension_map',
          running_config, chan_config, sync_config
        ) =
        run_event_step (
          event, event_stack, suspension_map,
          running_config, chan_config, sync_config
        )

        val thread_config' = {
          new_thread_key = new_thread_key', 
          thread_list = threads' @ new_threads,
          suspension_map = suspension_map 
        }
      in
        (thread_config', running_config', chan_config', sync_config', hole_key)
      end) |

      (* eval term case *)
      _ => (let
        val result = eval_term_step (t, symbol_map, contin_stack, hole_key)

        val (new_threads, hole_key') =
        (case result of
          SOME (t', symbol_map', contin_stack', hole_key') => 
          (
            [(thread_key, t', symbol_map', contin_stack', thread_mode)],
            hole_key'
          ) | 
          NONE => ([], hole_key)
        )

        val thread_config' = {
          thread_key = thread_key, 
          thread_list = threads' @ new_threads,
          suspension_map = suspension_map 
        }
      in
        (thread_config', running_config, chan_config, sync_config, hole_key')
      end)
      *)

    )
  )


  fun eval t = (let

    val thread_context = {
      thread_key = Thread_Key.zero,
      symbol_map = String_Map.empty, 
      contin_stack = [],
      thread_mode = Exec_Effect [] 
    }

    val thread = (t, thread_context)

    val global_context = {
      new_thread_key = Thread_Key.inc (#thread_key thread_context),
      suspension_map = Thread_Map.empty,

      new_running_key = Running_Key.zero, 
      running_set = Running_Set.empty, 

      new_chan_key = Chan_Key.zero,
      chan_map = Chan_Map.empty,

      new_sync_send_key = Sync_Send_Key.zero,
      send_completion_map = Sync_Send_Map.empty,

      new_sync_recv_key = Sync_Recv_Key.zero,
      recv_completion_map = Sync_Recv_Map.empty,

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





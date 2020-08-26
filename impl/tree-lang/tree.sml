structure Tree =
struct

  (* start structures *)

  structure Thread_Key = Key_Fn (val tag = "thread")
  structure Thread_Map = RedBlackMapFn (Thread_Key)

  structure Syncing_Key = Key_Fn (val tag = "thread")
  structure Syncing_Set = RedBlackSetFn (Syncing_Key)

  structure Chan_Key = Key_Fn (val tag = "chan")
  structure Chan_Map = RedBlackMapFn (Chan_Key)


  structure Loc_Key = Key_Fn (val tag = "loc")
  structure Loc_Map = RedBlackMapFn (Loc_Key)

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
    Sym of (string * int) |
    Reflect of (term * ((term * term) list) * int) |

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
      (* fields *)
      (string * (infix_option * term)) list *
      (* contextualized *)
      bool *
      (*pos*)
      int
    ) |

    Select of (term * int) |

    (* event *)
    Intro_Send of (term * int) |
    Intro_Recv of (term * int) |
    Intro_Latch of (term * int) |
    Intro_Choose of (term * int) |
    Intro_Offer of (term * int) |

    (* effect *)
    Intro_Return of (term * int) |
    Intro_Bind of (term * int) |
    Intro_Sync of (term * int) |
    Intro_Spawn of (term * int) |
    Intro_Alloc_Loc of (term * int) |
    Intro_Propagate of (term * int) |

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
    ) |

    Rec of (string * (infix_option * value)) list |

    Adjustment of adjustment |

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
    Spawn of effect |
    Sync of event |
    Alloc_Loc of reaction | 
    Propagate of adjustment

  and reaction =
    Chill of value |
    React of (
      Loc_Key.ord_key * 
      ((term * term) list) *
      ((infix_option * value) String_Map.map) *
      ((infix_option * (term * term) list) String_Map.map)
    )

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

  and adjustment =
    Change of Loc_Key.ord_key * value |
    Combine of adjustment * adjustment

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
    Spawn_Effect of contin list | 
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

    Sym (str, _) =>
    (
      "(sym " ^ str ^ ")"
    ) |

    Reflect (t, lams, _) =>
    surround "reflect" (
      (to_string t) ^ "\n" ^
      (surround "" (
        (String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams))
      ))
    ) |

    Id (id, _) => id |

    Assoc (t, _) => "(" ^ (to_string t) ^ ")" |
    
    Log (t, _) => "log " ^  (to_string t) |

    Intro_List (t1, t2, pos) => (
      "(# " ^ (to_string t1) ^ " " ^ (to_string t2) ^ ")"
    ) |

    Intro_Func (lams, pos) => String.surround "" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    App (t1, t2, pos) => surround "apply" (
      (to_string t1) ^ " " ^ (to_string t2)
    ) |

    Compo (t1, t2, pos) => "compo " ^ (to_string t1) ^ " " ^ (to_string t2) |

    With (t1, t2, pos) => "with " ^ (to_string t1) ^ "\n" ^ (to_string t2) |

    Intro_Rec (fs, contextualized, pos) => String.surround "" (
      String.concatWith ",\n" (List.map from_field_to_string fs)
    ) |

    Select (t, _) => "select " ^ (to_string t) |

    Intro_Send (t, _) => "send " ^ (to_string t) |
    Intro_Recv (t, _) => "recv " ^ (to_string t) |
    Intro_Latch (t, _) => "latch " ^ (to_string t) |
    Intro_Choose (t, _) => "choose " ^ (to_string t) |
    Intro_Offer (t, _) => "offer " ^ (to_string t) |

    Intro_Return (t, _) => "return " ^ (to_string t) |
    Intro_Sync (t, _) => "sync " ^ (to_string t) |
    Intro_Bind (t, _) => "bind " ^ (to_string t) |
    Intro_Spawn (t, _) => "exec " ^ (to_string t) |

    Add_Num (t, _) => "add " ^ (to_string t) |
    Sub_Num (t, _) => "sub " ^ (to_string t) |
    Mul_Num (t, _) => "mul " ^ (to_string t) |
    Div_Num (t, _) => "div " ^ (to_string t) |

    Value (v, _) => from_value_to_string v

  )

  and from_value_to_string v =
  (case v of

    Blank => "()" |

    List (vs: value list) => surround "list_value" ( 
      String.concatWith "\n" (List.map (fn v => ("# " ^ (from_value_to_string v))) vs)
    ) |

    Func (lams, fnc_store, mutual_map) => String.surround "func_value" (
      String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams)
    ) |

    Rec fs => String.surround "rec_value" (
      String.concatWith ",\n"
      (List.map from_field_value_to_string fs)
    ) |

    Event event => from_event_to_string event |

    Effect effect => from_effect_to_string effect |

    String str => str |

    Num num_str => num_str |

    Chan key => "(chan " ^ (Chan_Key.to_string key) ^ ")" |

    Thread key => "(thread " ^ (Thread_Key.to_string key) ^ ")" |

    Error str => "(error " ^ str ^ ")"

  )


  and from_field_value_to_string (name, (fix_op, v)) = String.surround "" (
    "def "  ^ name ^ (from_infix_option_to_string fix_op) ^ " : " ^ (from_value_to_string v)
  )

  and from_lam_to_string (t1, t2) = String.surround "" (
    "case "  ^ (to_string t1) ^ " => " ^ (to_string t2)
  )

  and from_field_to_string (name, (fix_op, t)) = String.surround "" (
    "def "  ^ name ^ (from_infix_option_to_string fix_op) ^ " : " ^ (to_string t)
  )

  and from_event_to_string evt =
  (case evt of  
    Offer v => "(offer_value " ^ (from_value_to_string v) ^ ")" |
    Abort => "abort" |
    Alloc_Chan => "alloc_chan" |
    Send (k, msg) =>
    surround "send_value " (
      (Chan_Key.to_string k) ^ (from_value_to_string msg)
    ) |

    Recv k =>
    surround "recv_value " (
      (Chan_Key.to_string k)
    ) |

    Latch (event, lams, _, _) =>
    surround "latch_value" (
      (from_event_to_string event) ^ "\n" ^
      (surround ""
        (String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams))
      )
    ) |

    Choose (event1, event2) =>
    surround "choose_value" (
      (from_event_to_string event1) ^ "\n" ^
      (from_event_to_string event2)
    )
  )

  and from_effect_to_string effect =
  (case effect of  
    Return v => "(return_value " ^ (from_value_to_string v) ^ ")" |
    Bind (effect, lams, _, _) =>
    surround "bind_value" (
      (from_effect_to_string effect) ^ "\n" ^
      (surround ""
        (String.concatWith "\n" (List.map (fn t => (from_lam_to_string t)) lams))
      )
    ) |
    Spawn effect => "(exec " ^ (from_effect_to_string effect) ^ ")" |
    Sync event => "(sync " ^ (from_event_to_string event) ^ ")"
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


  fun contextualize symbol_map fields =
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

    val fields' =
    (List.map
      (fn
        (* embed symbol map mutual ids into ts' functions *)
        (k, (fix_op, Intro_Func (lams, pos))) =>
        (
          (k, (fix_op, Value (Func (lams, symbol_map, mutual_map), ~1)))
        ) |

        (* otherwise *)
        field => field 
      )
      fields 
    )
  in
    fields'
  end)

  fun embed_mutual_map (symbol_map, mutual_map) =
  (let
    (* embed mutual_map within self's functions *)
    val fnc_store = (String_Map.map
      (fn (fix_op, lams) =>
        (fix_op, Func (lams, symbol_map, mutual_map))
      )
      mutual_map
    )

    val symbol_map' = (
      String_Map.unionWith
      (fn (_, b) => b)
      (symbol_map, fnc_store)
    )
  in
    symbol_map'
  end)


  fun values_equal rewrite_map (p : value, v : value) =
  (case (p, v) of 
    (Blank, _) => true |

    (List ps, List vs) => lists_equal rewrite_map (ps, vs) |

    (Func fp, Func fv) => funcs_equal rewrite_map (fp, fv) |


    (Rec fields_a, Rec fields_b) =>
    (let
      fun loop (aa, bb) =
      (case (aa, bb) of
        ([], []) => true |
        ((str_a, (fix_a, a)) :: aa, (str_b, (fix_b, b)) :: bb) =>
        (
          str_a = str_b andalso
          fix_a = fix_b andalso
          values_equal rewrite_map (a, b) andalso
          loop (aa, bb)
        ) |
        _ => false
      )
    in
      loop (fields_a, fields_b)
    end) |

    (Event event_a, Event event_b) =>
    events_equal rewrite_map (event_a, event_b) |

    (Effect effect_a, Effect effect_b) =>
    effects_equal rewrite_map (effect_a, effect_b) |

    (String str_a, String str_b) =>
    str_a = str_b |

    (Num num_a, Num num_b) =>
    num_a = num_b |

    (Chan key_a, Chan key_b) =>
    key_a = key_b |

    (Thread key_a, Thread key_b) =>
    key_a = key_b |

    (Error str_a, Error str_b) =>
    str_a = str_b |

    _ => false 

  )

  and events_equal rewrite_map (event_a, event_b) =
  (case (event_a, event_b) of
    (Offer v_a, Offer v_b) =>
    (
      values_equal rewrite_map (v_a, v_b)
    ) |

    (Abort, Abort) =>
    (
      true
    ) |

    (Alloc_Chan, Alloc_Chan) =>
    (
      true 
    ) |

    (Send (key_a, v_a), Send (key_b, v_b)) =>
    (
      key_a = key_b andalso values_equal rewrite_map (v_a, v_b) 
    ) |

    (Recv key_a, Recv key_b) =>
    (
      key_a = key_b 
    ) |

    (
      Latch (event_a, lams_a, symbol_map_a, mutual_map_a),
      Latch (event_b, lams_b, symbol_map_b, mutual_map_b)
    ) =>
    (
      events_equal rewrite_map (event_a, event_b) andalso
      funcs_equal rewrite_map
      (
        (lams_a, symbol_map_a, mutual_map_a),
        (lams_b, symbol_map_b, mutual_map_b)
      )
    ) |

    (Choose (a1, a2), Choose (b1, b2)) =>
    (
      events_equal rewrite_map (a1, b1) andalso
      events_equal rewrite_map (a2, b2)
    ) |

    _ => false
  )

  and effects_equal rewrite_map (effect_a, effect_b) =
  (case (effect_a, effect_b) of
    (Return v_a, Return v_b) =>
    (
      values_equal rewrite_map (v_a, v_b)
    ) |

    (
      Bind (effect_a, lams_a, symbol_map_a, mutual_map_a),
      Bind (effect_b, lams_b, symbol_map_b, mutual_map_b)
    ) =>
    (
      effects_equal rewrite_map (effect_a, effect_b) andalso
      funcs_equal rewrite_map
      (
        (lams_a, symbol_map_a, mutual_map_a),
        (lams_b, symbol_map_b, mutual_map_b)
      )
    ) |

    (Spawn effect_a, Spawn effect_b) =>
    (
      effects_equal rewrite_map (effect_a, effect_b)
    ) |

    (Sync event_a, Sync event_b) =>
    (
      events_equal rewrite_map (event_a, event_b)
    ) |

    _ => false
  )

  and funcs_equal rewrite_map
  (
    (p_lams, p_symbol_map, p_mutual_store),
    (f_lams, f_symbol_map, f_mutual_store)
  ) =
  (case (p_lams, f_lams) of 
    ([], []) => true |
    (p_lam :: p_lams', f_lam :: f_lams') =>
    (
      lams_equal rewrite_map (
        (p_lam, p_symbol_map, p_mutual_store),
        (f_lam, f_symbol_map, f_mutual_store)
      ) andalso
      funcs_equal rewrite_map (
        (p_lams', p_symbol_map, p_mutual_store),
        (f_lams', f_symbol_map, f_mutual_store)
      )
    ) |
    _ => false 
  
  )

  and lams_equal rewrite_map
  (
    (p_lam, p_symbol_map, p_mutual_store),
    (f_lam, f_symbol_map, f_mutual_store)
  ) = 
  (let
    val (p_param, p_body) = p_lam
    val (f_param, f_body) = f_lam

    val rewrite_map_op : (string String_Map.map) option =
    find_rewrites rewrite_map (p_param, f_param)


    val p_symbol_map = embed_mutual_map (p_symbol_map, p_mutual_store)
    val f_symbol_map = embed_mutual_map (f_symbol_map, f_mutual_store)

    val equal =
    (case rewrite_map_op of
      NONE => false |
      SOME rewrite_map =>
        symbolic_equal rewrite_map
        (
          (p_body, p_symbol_map),
          (f_body, f_symbol_map)
        )
    )
  in
    equal
  end)

  and symbolic_equal rewrite_map
  (
    (a, symbol_map_a),
    (b, symbol_map_b)
  ) =
  (case (a, b) of
    
    (Id (str_a, _), Id (str_b, _)) =>
    (
      ids_rep_equal rewrite_map ((str_a, symbol_map_a), (str_b, symbol_map_b))
    ) |

    (Assoc (a, _), _) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )
    ) |

    (_, Assoc (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )
    ) |

    (Log (a, _), _) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )
    ) |

    (_, Log (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )
    ) |

    (Intro_List (a1, a2, _), Intro_List (b1, b2, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a1, symbol_map_a),
        (b1, symbol_map_b)
      ) andalso

      symbolic_equal rewrite_map
      (
        (a2, symbol_map_a),
        (b2, symbol_map_b)
      )
      
    ) |

    (Intro_Func (lams_a, _), Intro_Func (lams_b, _)) =>
    (
      funcs_equal rewrite_map
      (
        (lams_a, symbol_map_a, String_Map.empty),
        (lams_b, symbol_map_b, String_Map.empty)
      ) 
    ) |

    (App (a1, a2, _), App (b1, b2, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a1, symbol_map_a),
        (b1, symbol_map_b)
      ) andalso

      symbolic_equal rewrite_map
      (
        (a2, symbol_map_a),
        (b2, symbol_map_b)
      )
      
    ) |

    (Compo (a1, a2, _), Compo (b1, b2, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a1, symbol_map_a),
        (b1, symbol_map_b)
      ) andalso

      symbolic_equal rewrite_map
      (
        (a2, symbol_map_a),
        (b2, symbol_map_b)
      )
      
    ) |

    (With (a1, a2, _), With (b1, b2, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a1, symbol_map_a),
        (b1, symbol_map_b)
      ) andalso

      symbolic_equal rewrite_map
      (
        (a2, symbol_map_a),
        (b2, symbol_map_b)
      )
      
    ) |

    (
      Intro_Rec (fields_a, contextualized_a, _),
      Intro_Rec (fields_b, contextualized_b, _)
    ) =>
    (let
      val fields_a = (if contextualized_a then
        fields_a
      else
        contextualize symbol_map_a fields_a
      )

      val fields_b = (if contextualized_b then
        fields_b
      else
        contextualize symbol_map_b fields_b
      )

      fun loop (aa : (string * (infix_option * term)) list, bb : (string * (infix_option * term)) list) =
      (case (aa, bb) of
        ([], []) => true |

        (a :: aa', b :: bb') =>
        (let
          val (str_a, (fix_a, a')) = a
          val (str_b, (fix_b, b')) = b
        in
          str_a = str_b andalso
          (fix_a = fix_b) andalso
          symbolic_equal rewrite_map ((a', symbol_map_a), (b', symbol_map_b)) andalso
          loop (aa', bb')
        end) |

        _ => false
      )
      
    in
      loop (fields_a, fields_b) 
    end) |

    (Select (a, _), Select (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Send (a, _), Intro_Send (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Recv (a, _), Intro_Recv (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Latch (a, _), Intro_Latch (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Choose (a, _), Intro_Choose (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Offer (a, _), Intro_Offer (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |


    (Intro_Return (a, _), Intro_Return (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Sync (a, _), Intro_Sync (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Bind (a, _), Intro_Bind (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Intro_Spawn (a, _), Intro_Spawn (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Add_Num (a, _), Add_Num (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Sub_Num (a, _), Sub_Num (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Mul_Num (a, _), Mul_Num (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |


    (Div_Num (a, _), Div_Num (b, _)) =>
    (
      symbolic_equal rewrite_map
      (
        (a, symbol_map_a),
        (b, symbol_map_b)
      )      
    ) |

    (Value (v_a, _), Value (v_b, _)) =>
    (
      values_equal rewrite_map (v_a, v_b)
    ) |

    _ => false 

  )

  and ids_rep_equal rewrite_map
  (
    (str_a, symbol_map_a),
    (str_b, symbol_map_b)
  ) =
  (let
    val a_result_op = String_Map.find (symbol_map_a, str_a)
    val b_result_op = String_Map.find (symbol_map_b, str_b)
  in
    (case (a_result_op, b_result_op) of 
      (SOME (fix_a, v_a : value), SOME (fix_b, v_b : value)) =>
      (let
        val rewrite_map' = String_Map.insert (rewrite_map, str_a, str_b)
      in
        fix_a = fix_b andalso
        (values_equal rewrite_map' (v_a, v_b))
      end) |
      (SOME _, NONE) => false |
      (NONE, SOME _) => false |
      (NONE, NONE) => 
      (case String_Map.find (rewrite_map, str_a) of
        SOME (str_b') => str_b = str_b' |
        NONE => false
      )
    )
  end)


  and lists_equal rewrite_map (ps, vs) = 
  (case (ps, vs) of 
    ([], []) => true |
    (p :: ps', v :: vs') =>
    (
      values_equal rewrite_map (p, v) andalso
      lists_equal rewrite_map (ps', vs')
    ) |
    _ => false
  )


  (* terms must be intros/patterns *)
  (* terms must be syntactically identical except for IDs *)
  (* terms cannot be or contain a function *)
  and find_rewrites rewrite_map (a : term, b : term) : (string String_Map.map) option =
  (case (a, b) of

    (Id (str_a, _), Id (str_b, _)) =>
    (
      SOME (String_Map.insert (rewrite_map, str_a, str_b))
    ) |

    (Assoc (a, _), _) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (_, Assoc (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Log (a, _), _) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (_, Log (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_List (a1, a2, _), Intro_List (b1, b2, _)) =>
    (
      Option.mapPartial
      (fn rewrite_map' =>
        (find_rewrites rewrite_map (a2, b2))
      )
      (find_rewrites rewrite_map (a1, b1))
    ) |

    (App (a1, a2, _), App (b1, b2, _)) =>
    (
      Option.mapPartial
      (fn rewrite_map' =>
        (find_rewrites rewrite_map (a2, b2))
      )
      (find_rewrites rewrite_map (a1, b1))
    ) |

    (Compo (a1, a2, _), Compo (b1, b2, _)) =>
    (
      Option.mapPartial
      (fn rewrite_map' =>
        (find_rewrites rewrite_map (a2, b2))
      )
      (find_rewrites rewrite_map (a1, b1))
    ) |

    (With (a1, a2, _), With (b1, b2, _)) =>
    (
      Option.mapPartial
      (fn rewrite_map' =>
        (find_rewrites rewrite_map (a2, b2))
      )
      (find_rewrites rewrite_map (a1, b1))
    ) |

    (
      Intro_Rec (fields_a, _, _),
      Intro_Rec (fields_b, _, _)
    ) =>
    (let
      fun loop rewrite_map (aa, bb) =
      (case (aa, bb) of
        ([], []) => SOME rewrite_map |
        ((str_a, (fix_a, a)) :: aa', (str_b, (fix_b, b)) :: bb') =>
        (if (str_a = str_b) andalso fix_a = fix_b then 
          (Option.mapPartial
            (fn rewrite_map' =>
              loop rewrite_map' (aa', bb')
            )
            (find_rewrites rewrite_map (a, b))
          )
        else 
          NONE
        ) |
        _ => NONE

      )
    in
      loop rewrite_map (fields_a, fields_b)
    end) |

    (Select (a, _), Select (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Send (a, _), Intro_Send (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Recv (a, _), Intro_Recv (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Latch (a, _), Intro_Latch (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Choose (a, _), Intro_Choose (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Offer (a, _), Intro_Offer (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Return (a, _), Intro_Return (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Sync (a, _), Intro_Sync (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Bind (a, _), Intro_Bind (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Intro_Spawn (a, _), Intro_Spawn (b, _)) =>
    (
      find_rewrites rewrite_map (a, b)
    ) |

    (Value (Func _, _), _ ) => NONE |
    (_, Value (Func _, _)) => NONE |

    (Value (v_a, _), Value (v_b, _)) =>
    (if values_equal rewrite_map (v_a, v_b) then
      SOME rewrite_map
    else
      NONE
    ) |

    _ => NONE 
  )

  fun symbolic_match ((pattern, symbol_map), (symbolic_term, target_symbol_map)) =
  (case (pattern, symbolic_term) of
    (Value (Blank, _), _) => SOME symbol_map |

    (Sym (id, _), _) =>
    (let
      val thunk = Func (
        [(Value (Blank, ~1), symbolic_term)],
        target_symbol_map,
        String_Map.empty
      )
    in
      SOME (String_Map.insert (symbol_map, id, (NONE, thunk)))
    end) |

    (Id (p_id, _), Id (st_id, _)) =>
    (let
      val eq = ids_rep_equal String_Map.empty ((p_id, symbol_map), (st_id, target_symbol_map))
    in
      if eq then SOME symbol_map else NONE
    end) |

    (Assoc (p, _), _) =>
    (
      symbolic_match
      (
        (p, symbol_map),
        (symbolic_term, target_symbol_map)
      )
    ) |

    (_, Assoc (target, _)) =>
    (
      symbolic_match 
      (
        (pattern, symbol_map),
        (target, target_symbol_map)
      )
    ) |

    (Log (p, _), _) =>
    (
      symbolic_match
      (
        (p, symbol_map),
        (symbolic_term, target_symbol_map)
      )
    ) |

    (_, Log (target, _)) =>
    (
      symbolic_match
      (
        (pattern, symbol_map),
        (target, target_symbol_map)
      )
    ) |

    (Intro_List (p1, p2, _), Intro_List (t1, t2, _)) => (
      (Option.mapPartial
        (fn symbol_map' =>
          symbolic_match ((p2, symbol_map'), (t2, target_symbol_map))
        )
        (symbolic_match ((p1, symbol_map), (t1, target_symbol_map)))
      )
    ) |

    (Intro_Func (p_lams, _), Intro_Func (t_lams, _)) =>
    (let
      val eq =
      funcs_equal String_Map.empty
      (
        (p_lams, symbol_map, String_Map.empty),
        (t_lams, target_symbol_map, String_Map.empty)
      )
    in
      if eq then SOME symbol_map else NONE
    end) |

    (
      Intro_Func (p_lams, _),
      Value (Func (t_lams, target_symbol_map, target_mutual_map), _)
    ) =>
    (let
      val eq =
      funcs_equal String_Map.empty
      (
        (p_lams, symbol_map, String_Map.empty),
        (t_lams, target_symbol_map, target_mutual_map)
      )
    in
      if eq then SOME symbol_map else NONE
    end) |

    (App (p1, p2, _), App (t1, t2, _)) => (
      (Option.mapPartial
        (fn symbol_map' =>
          symbolic_match ((p2, symbol_map'), (t2, target_symbol_map))
        )
        (symbolic_match ((p1, symbol_map), (t1, target_symbol_map)))
      )
    ) |

    (Compo (p1, p2, _), Compo (t1, t2, _)) => (
      (Option.mapPartial
        (fn symbol_map' =>
          symbolic_match ((p2, symbol_map'), (t2, target_symbol_map))
        )
        (symbolic_match ((p1, symbol_map), (t1, target_symbol_map)))
      )
    ) |

    (With (p1, p2, _), With (t1, t2, _)) => (
      (Option.mapPartial
        (fn symbol_map' =>
          symbolic_match ((p2, symbol_map'), (t2, target_symbol_map))
        )
        (symbolic_match ((p1, symbol_map), (t1, target_symbol_map)))
      )
    ) |

    (
      Intro_Rec (p_fields, p_contextualized, _),
      Intro_Rec (t_fields, t_contextualized, _)
    ) =>
    (let
      val p_fields = (if p_contextualized then
        p_fields
      else
        contextualize symbol_map p_fields
      )

      val t_fields = (if t_contextualized then
        t_fields
      else
        contextualize target_symbol_map t_fields
      )

      fun loop symbol_map (aa : (string * (infix_option * term)) list, bb : (string * (infix_option * term)) list) =
      (case (aa, bb) of
        ([], []) => NONE |

        (a :: aa', b :: bb') =>
        (let
          val (str_a, (fix_a, a')) = a
          val (str_b, (fix_b, b')) = b
        in
          (if (str_a = str_b andalso (fix_a = fix_b)) then
            Option.mapPartial
            (fn symbol_map' =>
              loop symbol_map' (aa', bb')  
            )
            (symbolic_match ((a', symbol_map), (b', target_symbol_map)))
          else
            NONE
          )
        end) |

        _ => NONE 
      )
      
    in
      loop symbol_map (p_fields, t_fields) 
    end) |

    (Select (p, _), Select (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Send (p, _), Intro_Send (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Recv (p, _), Intro_Recv (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Latch (p, _), Intro_Latch (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Choose (p, _), Intro_Choose (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Offer (p, _), Intro_Offer (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |


    (Intro_Return (p, _), Intro_Return (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Sync (p, _), Intro_Sync (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Bind (p, _), Intro_Bind (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Intro_Spawn (p, _), Intro_Spawn (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Add_Num (p, _), Add_Num (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Sub_Num (p, _), Sub_Num (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Mul_Num (p, _), Mul_Num (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Div_Num (p, _), Div_Num (t, _)) =>
    (
      symbolic_match ((p, symbol_map), (t, target_symbol_map))
    ) |

    (Value (v_p, _), Value (v_t, _)) =>
    (if values_equal String_Map.empty (v_p, v_t) then
      SOME symbol_map
    else
      NONE
    ) |

    _ => NONE
  )



  fun match_term symbol_map (pat, value) = (case (pat, value) of

    (Assoc (pat', _), _) => (
      match_term symbol_map (pat', value)
    ) |

    (Value (Blank, _), _) => SOME symbol_map |

    (Id (str, _), v) => (
      SOME (String_Map.insert (symbol_map, str, (NONE, v)))
    ) |

    (Intro_List (t, t', _), List (v :: vs)) =>
    (Option.mapPartial
      (fn symbol_map' =>
        match_term symbol_map' (t, v)
      )
      (match_term symbol_map (t', List vs))
    ) |

    (Intro_Rec (p_fields, contextualized, _), Rec v_fields) =>
    (let
      val p_fields =
      (if contextualized then
        p_fields
      else
        contextualize symbol_map p_fields
      )
    in
      (
        from_fields_match_term
        symbol_map
        (p_fields, v_fields)
      )
    end) |

    (Intro_Send (t, _), Event (Send (chan_key, msg))) =>
    (
      match_term symbol_map (t, List [Chan chan_key, msg])
    ) |

    (Intro_Recv (t, _), Event (Recv chan_key)) =>
    (
      match_term symbol_map (t, Chan chan_key)
    ) |

    (Intro_Latch (t, _), Event (Latch (event, lams, symbol_map, mutual_map))) =>
    (
      match_term symbol_map (t,
        List [Event event, Func (lams, symbol_map, mutual_map)]
      )
    ) |

    (Intro_Choose (t, _), Event (Choose (event_left, event_right))) =>
    (
      match_term symbol_map (t,
        List [Event event_left, Event event_right]
      )
    ) |

    (Intro_Offer (t, _), Event (Offer v)) =>
    (
      match_term symbol_map (t, v)
    ) |


    (Intro_Return (t, _), Effect (Return v)) =>
    (
      match_term symbol_map (t, v)
    ) |

    (Intro_Sync (t, _), Effect (Sync event)) =>
    (
      match_term symbol_map (t, Event event)
    ) |

    (Intro_Bind (t, _), Effect (Bind (effect, lams, symbol_map, mutual_map))) =>
    (
      match_term symbol_map (t,
        List [Effect effect, Func (lams, symbol_map, mutual_map)]
      )
    ) |

    (Intro_Spawn (t, _), Effect (Spawn effect)) =>
    (
      match_term symbol_map (t, Effect effect)
    ) |

    (Value (vp, _), _) =>
    (if values_equal String_Map.empty (vp, value) then
      SOME symbol_map
    else 
      NONE
    ) |

    _ => NONE

  )

  and from_fields_match_term symbol_map (p_fields, v_fields) =
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

      fun match_terms key_matches =
      (case key_matches of
        [] => NONE |
        [(vname,(vfix_op, v))] => (Option.mapPartial
          (fn symbol_map' =>
            SOME (String_Map.insert (symbol_map', vname, (vfix_op, v)))
          )
          (match_term symbol_map (p, v))
        ) |
        _ :: key_matches' => match_terms key_matches'
      )

      val symbol_map_op = match_terms key_matches

    in
      (Option.mapPartial
        (fn symbol_map' =>
          from_fields_match_term symbol_map' (pfs, vfs)
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

    val symbol_map''' = embed_mutual_map (symbol_map'', mutual_map)


    fun match_first lams = (case lams of
      [] => NONE |
      (p, t) :: lams' =>
      (case (match_term symbol_map''' (p, result)) of
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
          (from_value_to_string result) ^
          " - does not match continuation hole pattern"
        ), ~1),
        symbol_map'''
      ) |

      SOME (t_body, symbol_map'''') =>
      (t_body, symbol_map'''')
    )
  end)

  fun reflect global_context
  (
    t, lams, pos,
    symbol_map,
    contin_stack
  ) =
  (case t of
    Id (id, _) =>
    (case (String_Map.find (symbol_map, id)) of
      SOME (_, v) =>
      (
        (
          Reflect (Value (v, ~1), lams, pos), 
          symbol_map,
          contin_stack
        ),
        global_context 
      ) |

      _ =>
      (
        (
          Value (Error ("reflect variable " ^ id ^ " cannot be resolved"), ~1),
          symbol_map,
          contin_stack
        ),
        global_context
      )

    ) |

    Value (Func ([(Value (Blank, _), body)], fnc_symbol_map, mutual_map), _) =>
    (let

      val fnc_symbol_map = embed_mutual_map (fnc_symbol_map, mutual_map)

      fun match_first lams = (case lams of
        [] => NONE |
        (p, t) :: lams' =>
        (case (symbolic_match ((p, symbol_map), (body, fnc_symbol_map))) of
          NONE => match_first lams' |
          SOME symbol_map' => (
            SOME (t, symbol_map')
          )
        )
      )

      val (t', symbol_map') =
      (case (match_first lams) of
        NONE =>
        (
          Value (Error (
            "thunk body - " ^ (to_string body) ^ " - does not match reflection pattern"
          ), ~1),
          symbol_map
        ) |

        SOME (t', symbol_map') =>
        (t', symbol_map')
      )
    in
      (
        (
          t',
          symbol_map',
          contin_stack
        ),
        global_context
      )
    end) |

    Value (v, pos) =>
    (
      (
        Value (Error ("reflection of non-thunk: " ^ (from_value_to_string v)), pos),
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
        [( hole new_hole_key, Reflect (hole new_hole_key, lams, pos) )],
        symbol_map,
        String_Map.empty 
      )

      val global_context' = set_new_hole_key (global_context, Hole_Key.inc new_hole_key)
    in
      (
        (
          t,
          symbol_map,
          contin :: contin_stack
        ),
        global_context
      )
    end)
  )





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
        Value (Error ("application of non-function: " ^ (from_value_to_string v)), pos),
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
        print ((from_value_to_string v) ^ "\n");
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

    Intro_Rec (fields, contextualized, pos) => (let

      val fields =
      (if contextualized then
        fields
      else
        contextualize symbol_map fields
      )

      val ts = (map (fn (k, (fix_op, t)) => t) fields)

      fun map_to_fields ts =
      (List.map
        (fn ((key, (fix_op, _)), t) => (key, (fix_op, t)))
        (ListPair.zip (fields, ts))
      )
    in
      SOME (reduce_list global_context (
        ts,
        fn ts => Intro_Rec (map_to_fields ts, true, pos),
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


    Reflect (t, lams, pos) =>
    SOME (reflect global_context (
      t, lams, pos,
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

    Intro_Spawn (t, pos) =>
    SOME (reduce_single global_context (
      t, fn t => Intro_Spawn (t, pos),
      (fn
        Effect effect => Effect (Spawn effect) |
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
            thread_mode = Spawn_Effect effect_stack
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
            thread_mode = Spawn_Effect effect_stack
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
          thread_mode = Spawn_Effect
          (
            (Contin_Bind, lams, fnc_store, mutual_map) ::
            effect_stack
          )
        }
      )
    in
      ([new_thread], global_context)
    end) |

    Spawn effect' => (let
      val parent_thread = 
      (
        Value (Effect (Return Blank), ~1),
        {
          thread_key = thread_key,
          symbol_map = String_Map.empty,
          term_stack = [],
          thread_mode = Spawn_Effect effect_stack
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
          thread_mode = Spawn_Effect [] 
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
                  thread_mode = Spawn_Effect effect_stack 
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
      (Value (Effect effect, _), [], Spawn_Effect effect_stack) =>
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

      (*
      ** TODO **
      ** create reaction mode
      **
      ** chill : 'a -> 'a reaction
      ** react : 'a loc -> ('a -> 'b reaction) -> 'b reaction
      **
      ** change : 'a loc * 'a -> adjustment 
      ** combine : adjustment * adjustment -> adjustment
      **
      ** propagate : adjustment -> unit effect
      ** extract : 'a loc -> 'a effect
      **
      *)
      (*
      ** TODO **
      (Value (Adju adjustment, _), [], Propa_Adju react_thread) =>
      (let
        val (new_threads, global_context') =
        (
          propagate_adustment_step
          global_context
          (adjustment, thread_key, react_thread)
        )
      in
        SOME (threads' @ new_threads, global_context')
      end) |
      *)

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
      thread_mode = Spawn_Effect [] 
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





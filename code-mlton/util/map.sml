signature MAP = sig
  type key
  type 'a map
  val empty : 'a map
  val insert : 'a map * key * 'a -> 'a map
  val insertList : 'a map * (key * 'a) list -> 'a map
  val lookup : 'a map * key -> 'a option
  val remove : 'a map * key -> 'a map
  val removeList : 'a map * key list -> 'a map
  val listValues : 'a map -> 'a list 
  val listEntries : 'a map -> (key * 'a) list 
  val mergeRight : 'a map -> 'a map -> 'a map  
end

functor MapFn(Key : HASH_KEY): MAP = struct 

  type key = Key.hash_key

  datatype 'a map = Map of 'a IntBinaryMap.map 

  local 
    val count = ref 0
    val ht = HashTable.mkTable (Key.hashVal, Key.sameKey) (128, Fail "Env: ht")
  in 

    fun intKey name =
    case HashTable.find ht name of
      SOME i => i
    | NONE =>
      let 
        val i = !count
        val _ = HashTable.insert ht (name, i)
        val _ = count := i + 1
      in i
      end
  
    fun listEntries (Map imap) =
    let
      val htPairs = HashTable.listItemsi ht
      fun getValue (str, i) = 
        SOME (str, IntBinaryMap.lookup (imap, i)) handle _ => NONE
    in
      List.mapPartial getValue htPairs
    end

  end

  val empty = Map IntBinaryMap.empty

  fun insert (Map imap, name, a) = Map (IntBinaryMap.insert (imap, intKey name, a))

  fun lookup (Map imap, name) =
    SOME (IntBinaryMap.lookup (imap, intKey name)) handle _ => NONE

  fun insertList (Map imap, pairs) =
    Map (foldl
      (fn ((name, a), imap') =>
        IntBinaryMap.insert (imap', intKey name, a))
      imap pairs)

  fun remove (Map imap, name) = Map (#1 (IntBinaryMap.remove (imap, intKey name)))

  fun removeList (Map imap, names) =
  case names of
    nil => Map imap
  | name :: names' =>
      removeList (Map (#1 (IntBinaryMap.remove (imap, intKey name))), names')

  fun listValues (Map imap) = IntBinaryMap.listItems imap 

  fun mergeRight (Map imap1) (Map imap2) = 
    Map (IntBinaryMap.unionWith (fn (l, r) => r) (imap1, imap2))

end
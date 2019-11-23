signature SET = sig
  type key
  type set
  val empty : set
  val add : set * key -> set
  val addList : set * key list -> set
  val member : set * key -> bool 
  val subtract : set * key -> set
  val subtractList : set * key list -> set
  val listItems : set -> key list 
  val numItems : set -> int 
end

functor SetFn(Key : HASH_KEY): SET = struct 

  type key = Key.hash_key

  structure ISet = IntBinarySet
  datatype set = Set of ISet.set 

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
  
    fun listItems (Set iset) =
    let
      val htPairs = HashTable.listItemsi ht
      fun getValue (str, i) = 
        if  ISet.member (iset, i) then
          SOME str
        else
          NONE
    in
      List.mapPartial getValue htPairs
    end

  end

  fun numItems set = List.length (listItems set) 

  val empty = Set ISet.empty

  fun add (Set iset, name) = Set (ISet.add (iset, intKey name))

  fun addList (Set iset, keys) =
    Set (foldl
      (fn (key, iset') =>
        ISet.add (iset', intKey key))
      iset keys)

  fun member (Set iset, key) = ISet.member (iset, intKey key)

  fun subtract (Set iset, key) = Set (ISet.subtract (iset, intKey key))

  fun subtractList (Set iset, keys) = Set (ISet.subtractList (iset, map intKey keys))

end
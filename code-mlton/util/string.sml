structure String = struct

local

  structure RE = RegExpFn (
    structure P = AwkSyntax
    structure E = ThompsonEngine
  )

in 
  
  fun split_regex (str, regex) = (let

    fun getc s i =
      if (i < (String.size s)) then
        SOME (String.sub (s, i), i+1)
      else
        NONE

    val m_op = RE.find regex (getc str) 0 
    val chars = explode str
  in
    (case m_op of
      NONE => [str] |
      SOME (MatchTree.Match ({pos, len}, _), _) => (let
        val front = List.take (chars, pos)
        val back = List.drop (chars, pos + len)
      in
        (implode front) :: (split_regex (implode back, regex))
      end)
    )
  end)


  fun split_pattern (str, regex_str) = 
    split_regex (str, RE.compileString regex_str)
end

  fun surround_with open_brace tag body close_brace = (let
    val abc = open_brace ^ tag
    val bodyLines = String.tokens (fn c => c = #"\n") body
    val indentedLines = map (fn l => "  " ^ l ^ "\n") bodyLines
    val indentedBody = String.concat indentedLines 
    val xyz = if body = "" then close_brace else "\n" ^ indentedBody ^ close_brace 
  in
    abc ^ xyz 
  end)


  fun surround tag body = surround_with "(" tag body ")"

  open String
end

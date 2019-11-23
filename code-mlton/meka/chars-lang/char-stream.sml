structure CharStream = struct
  fun makeTokenStream charStream = 
    LrParser.Stream.streamify (Chars.makeLexer charStream)
end
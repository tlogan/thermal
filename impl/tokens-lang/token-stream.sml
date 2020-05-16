structure TokenStream = struct

  structure ParserData = Tokens.ParserData

  exception ParseError = LrParser.ParseError
  
  fun parse (lookahead, tokenStream, error) =
  let
    fun extract (a,b) = (ParserData.Actions.extract a,b)
  in
    extract
    (
      LrParser.parse
      {
        table = ParserData.table,
        lexer = tokenStream,
        lookahead = lookahead,
        saction = ParserData.Actions.actions,
        arg = (),
        void = ParserData.Actions.void,
        ec =
        {
          is_keyword = ParserData.EC.is_keyword,
          noShift = ParserData.EC.noShift,
          preferred_change = ParserData.EC.preferred_change,
          errtermvalue = ParserData.EC.errtermvalue,
          error=error,
          showTerminal = ParserData.EC.showTerminal,
          terms = ParserData.EC.terms
        }
      }
    )
  end

end

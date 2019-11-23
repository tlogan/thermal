structure Token = struct

  open Tokens.Tokens
  open Tokens.ParserData.Token

  fun toString (TOKEN (term, _)) = Tokens.ParserData.EC.showTerminal term 

  fun format tok = let
    val TOKEN (term, (_, lp, cp)) = tok
    val bigString = concat [
      Int.toString lp, ":", Int.toString cp, " ",
      (toString tok)]
    in bigString
    end

  fun isEOF tok =
    (toString tok) = "EOF"

end

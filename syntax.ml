type term =
  | TmZero of Lexing.position
  | TmTrue of Lexing.position
  | TmFalse of Lexing.position
  | TmIsZero of Lexing.position * term
  | TmSucc of Lexing.position * term
  | TmPred of Lexing.position * term
  | TmIf of Lexing.position * term * term * term

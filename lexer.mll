{
  let reserved = [
    ("true", Parser.TRUE);
    ("false", Parser.FALSE);
    ("iszero", Parser.ISZERO);
    ("succ", Parser.SUCC);
    ("pred", Parser.PRED);
    ("if", Parser.IF);
    ("then", Parser.THEN);
    ("else", Parser.ELSE);
  ]
}
rule main = parse
| ' ' { main lexbuf }
| '0' { Parser.ZERO }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| ['a'-'z']+ as id {
  List.assoc id reserved
}
| eof { exit 1 }

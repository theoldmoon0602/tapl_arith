%{ 
  open Syntax
%}

%token ZERO ISZERO
%token TRUE FALSE
%token LPAREN RPAREN
%token SUCC PRED
%token IF THEN ELSE

%start toplevel
%type <Syntax.term> toplevel

%%

toplevel: Term { $1 }

Term:
  | ZERO { TmZero($startpos) }
  | TRUE { TmTrue($startpos) }
  | FALSE { TmFalse($startpos) }
  | ISZERO LPAREN body=Term RPAREN { TmIsZero($startpos, body) }
  | SUCC LPAREN body=Term RPAREN { TmSucc($startpos, body) }
  | PRED LPAREN body=Term RPAREN { TmPred($startpos, body) }
  | IF c=Term THEN t=Term ELSE e=Term { TmIf($startpos, c, t, e) }

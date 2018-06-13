open Syntax

let position_to_string pos = 
  "file:" ^ pos.Lexing.pos_fname ^ ", line:" ^ string_of_int(pos.Lexing.pos_lnum) ^ ", col:" ^ string_of_int(pos.Lexing.pos_cnum)

let rec term_to_string_with_p term =
  match term with
  |TmZero (p) -> "0[" ^ position_to_string p ^ "]"
  |TmTrue (p) -> "true[" ^ position_to_string p ^ "]"
  |TmFalse (p) -> "false[" ^ position_to_string p ^ "]"
  |TmIsZero (p, t) -> "iszero(" ^ term_to_string_with_p t ^ ")[" ^ position_to_string p ^ "]"
  |TmSucc (p, t) -> "succ(" ^ term_to_string_with_p t ^ ")[" ^ position_to_string p ^ "]"
  |TmPred (p, t) -> "pred(" ^ term_to_string_with_p t ^ ")[" ^ position_to_string p ^ "]"
  |TmIf (p, c, t, e) -> "if " ^ term_to_string_with_p c ^ " then " ^ term_to_string_with_p t ^ " else " ^ term_to_string_with_p e ^ "[" ^ position_to_string p ^ "]"

let rec term_to_string term =
  match term with
  |TmZero (_) -> "0"
  |TmTrue (_) -> "true"
  |TmFalse (_) -> "false"
  |TmIsZero (_, t) -> "iszero(" ^ term_to_string t ^ ")"
  |TmSucc (_, t) -> "succ(" ^ term_to_string t ^ ")"
  |TmPred (_, t) -> "pred(" ^ term_to_string t ^ ")"
  |TmIf (_, c, t, e) -> "if " ^ term_to_string c ^ " then " ^ term_to_string t ^ " else " ^ term_to_string e


let () =
  let t = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  print_string (term_to_string t) |> ignore

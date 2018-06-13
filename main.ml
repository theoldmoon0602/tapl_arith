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

let rec isnumericval t =
  match t with
  |TmZero(_) -> true
  |TmSucc(_,t1) -> isnumericval t1
  |_ -> false

let rec isval t =
  match t with
  |TmTrue(_) -> true
  |TmFalse(_) -> true
  |t when isnumericval t -> true
  |_ -> false

exception NoRuleApplies

let rec eval1 t =
  match t with
  |TmIf(_, TmTrue(_), ifthen, _) -> ifthen
  |TmIf(_, TmFalse(_), _, ifelse) -> ifelse
  |TmIf(p, cond, ifthen, ifelse) ->
      let cond' = eval1 cond in
      TmIf(p, cond', ifthen, ifelse)
  |TmSucc(p, t1) ->
      let t1' = eval1 t1 in
      TmSucc(p, t1')
  |TmPred(p, TmZero(_)) ->
      TmZero(p)
  |TmPred(p, TmSucc(_, nv1)) when isnumericval nv1 ->
      nv1
  |TmPred(p, t1) ->
      let t1' = eval1 t1 in
      TmPred(p, t1')
  |TmIsZero(p, TmZero(_)) ->
      TmTrue(p)
  |TmIsZero(p, TmSucc(_, nv1)) when isnumericval nv1 ->
      TmFalse(p)
  |TmIsZero(p, t1) ->
      let t1' = eval1 t1 in
      TmIsZero(p, t1')
  |_ -> raise NoRuleApplies

let rec eval t =
  try
    let t' = eval1 t in
    eval t'
  with NoRuleApplies -> t


let () =
  let t = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let r = eval t in
  print_string (term_to_string r) |> ignore

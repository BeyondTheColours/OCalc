type op = Mult |Add |Sub |Div
;;

type expr =
|Num of int
|Phrase of op*expr*expr
|Bracket of expr
;;

let string_from_op o =
  match o with 
  |Mult -> "*"
  |Add -> "+"
  |Sub -> "-"
  |Div -> "/"
;;

let rec string_of_tree tr =
  match tr with
  |Num(n) -> "Num("^(string_of_int n)^")"
  |Phrase(op, e1, e2) ->
    begin
      let s1 = string_of_tree e1 in
      let s2 = string_of_tree e2 in
        match op with
        |Add -> "Phrase(Add,"^s1^","^s2^")"
        |Mult -> "Phrase(Mult,"^s1^","^s2^")"
        |Sub -> "Phrase(Sub,"^s1^","^s2^")"
        |Div -> "Phrase(Div,"^s1^","^s2^")"
    end
  |Bracket(e) -> "Bracket("^(string_of_tree e)^")"
;;

let rec draw_tree tr =
  match tr with
  |Num(n) -> string_of_int n
  |Phrase(o, e1, e2) -> "("^(draw_tree e1)^(string_from_op o)^(draw_tree e2)^")"
  |Bracket(e) -> "("^(draw_tree e)^")"
;;

let rec depth tr =
  match tr with
  |Num(x) -> 1
  |Phrase(op, e1, e2) -> max (1+(depth e1)) (1+(depth e2))
  |Bracket(e1) -> 1 + (depth e1)
;;

let ( >> ) a b =
  let rec add_lists a b =
    match a with
    |[] -> b
    |h::t -> add_lists t (h::b)
  in add_lists (List.rev a) b
;;

let find_at_depth d tr =
  let rec find_at_depth_worker d tr acc =
  match tr with
  |Num(x) -> if d = 0 then (Some([Num(x)]>>acc)) else None
  |Bracket(e) ->
    if d = 0 then Some(e::acc)
    else
      begin
        match find_at_depth_worker (d-1) e acc with
        |None -> None
        |Some(x) -> Some(x >> acc)
      end
  |Phrase(op, e1, e2) ->
    if d = 0 then (Some(e1::e2::acc))
    else
      match find_at_depth_worker (d-1) e1 acc with
      |None ->
        begin
          match find_at_depth_worker (d-1) e2 acc with
          |None -> None
          |Some(x) -> Some(x >> acc)
        end
      |Some(y) ->
        begin
          match find_at_depth_worker (d-1) e2 acc with
          |None -> Some(y >> acc)
          |Some(x) -> Some(y >> x >> acc)
        end
  in find_at_depth_worker d tr []
;;

(* TODO - finish functions to visualise parse tree *)
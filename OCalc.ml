open ConsString

type op =
|Mult
|Add
|Sub
|Div
;;

let op_from_string o =
  match o with
  |"*" -> Mult
  |"+" -> Add
  |"-" -> Sub
  |"/" -> Div
  |x -> raise(Failure("Could not convert \""^x^"\" to type op"))
;;

let string_from_op o =
  match o with 
  |Mult -> "*"
  |Add -> "+"
  |Sub -> "-"
  |Div -> "/"

type expr =
|Num of int
|Phrase of op*expr*expr
|Bracket of expr
;;

let numify n =
  try Num(int_of_string n) with
  |Failure(x) -> raise(Invalid_argument(n))
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

let rec eval e =
  match e with
  |Num(n) -> n
  |Phrase(op, e1, e2) ->
    begin
      match op with
      |Mult -> (eval e1) * (eval e2)
      |Add -> (eval e1) + (eval e2)
      |Sub -> (eval e1) - (eval e2)
      |Div ->
        try (eval e1)/(eval e2) with
        |Division_by_zero -> raise(Division_by_zero)
    end
  |Bracket(e) -> eval e
;;

let rec depth tr =
  match tr with
  |Num(x) -> 1
  |Phrase(op, e1, e2) -> max (1+(depth e1)) (1+(depth e2))
  |Bracket(e1) -> 1 + (depth e1)
;;

let rec in_list a b =
  match b with
  |[] -> false
  |h::t -> if h = a then true else in_list a t
;;

let rec is_number_worker n nums =
  match n with
  |Nil -> true
  |Cons(c, t) ->
    if in_list (Char.escaped c) nums then is_number_worker t nums
    else false
;;

let is_number n = is_number_worker (ConsString.string_to_cons_string n) ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"];;

let string_to_eq_list s = 
  let rec string_to_eq_list_worker s acc temp =
    match s with
    |Nil -> if temp = "" then List.rev acc else List.rev(temp::acc)
    |Cons(c, t) ->
      let c_ = (Char.escaped c) in
        if is_number c_ then string_to_eq_list_worker t acc (temp^c_)
        else
          if temp = "" then string_to_eq_list_worker t (c_::acc) temp
          else string_to_eq_list_worker t (c_::temp::acc) ""
  in string_to_eq_list_worker (ConsString.string_to_cons_string s) [] "" 
;;

let rm_br eq =
  let rec rm_br_worker eq count res =
    match eq with
    |[] -> (List.rev res, [])
    |h::t ->
        if h = "(" && count = 0
        then rm_br_worker t (count+1) res
        else if h = ")" && count-1 = 0 then (List.rev res, t)
        else let pos_or_neg =
              if h = ")" then -1
              else if h = "(" then 1
              else 0 in
          rm_br_worker t (count+pos_or_neg) (h::res)
  in rm_br_worker eq 0 []
  ;;

let rec gen_parse_tree eq =
  match eq with
  |[] -> Num(0)
  |h::t ->
    begin
      if is_number h = true then
        match t with
        |[] -> numify h
        |h1 :: t1 ->
          begin
            if h1 = "+" || h1 = "-" then
              Phrase(op_from_string h1, numify h, gen_parse_tree t1)

            else if h1 = "/" || h1 = "*" then
              match t1 with
              |[] -> raise(Invalid_argument("Unexpected end of expression. A number should follow symbol : "^h1))
              |h2 :: [] -> Phrase(op_from_string h1, numify h, numify h2)
              |h2 :: t2 ->
                begin
                  match t2 with
                  |[] -> Phrase(op_from_string h1, numify h, numify h2)(* Pretty sure this is reduntant but the editor disagrees? *)
                  |h3 :: [] -> raise(Invalid_argument("Unexpected end of expression. A number should follow symbol : "^h3))
                  |h3 :: t3 ->
                    if h2 = "(" then
                      begin
                        match rm_br t1 with
                        |(eq1, []) -> Phrase(op_from_string h1, numify h, Bracket(gen_parse_tree eq1))
                        |(eq1, h4::t4) ->
                          let m = Phrase(op_from_string h1, numify h, Bracket(gen_parse_tree eq1)) in
                          Phrase(op_from_string h4, m, gen_parse_tree t4)
                      end
                    else
                      let m = Phrase(op_from_string h1, numify h, numify h2) in
                      Phrase(op_from_string h3, m, gen_parse_tree t3)
                      
                end
            else raise(Failure("Expected an operator but found "^h1))
          end
      else if h = "(" then
        let parts = rm_br eq in
        match parts with
        |(eq1, []) -> Bracket(gen_parse_tree eq1)
        |(eq1, h1::t1) ->
          let br_gen_eq1 = Bracket(gen_parse_tree eq1) in
          if h1 = "+" || h1 = "-" then
            Phrase(op_from_string h1, br_gen_eq1, gen_parse_tree t1)
          else if h1 = "*" || h1 = "/" then
            match t1 with
            |[] -> raise(Failure("Reached end of expression but found "^h1))
            |h2 :: [] -> Phrase(op_from_string h1, br_gen_eq1, numify h2)
            |h2 :: t2 ->
              if h2 = "(" then
                match rm_br t1 with
                |(eq2, []) -> Phrase(op_from_string h1, br_gen_eq1, gen_parse_tree eq2)
                |(eq2, h3::t3) ->
                  let m = Phrase(op_from_string h1, br_gen_eq1, gen_parse_tree eq2) in
                  Phrase(op_from_string h3, m, gen_parse_tree t3) 

              else raise(Failure("Invalid equation. Unexpected character "^h2))
        
        else raise(Failure("Invalid equation. Unexpected character "^h))

      else raise(Failure("Invalid equation. Unexpected character "^h))
    end
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

let intro = print_endline("Enter your equation: ");;
let get_test = read_line();;

let test = string_to_eq_list get_test;;

let out = gen_parse_tree test;;
let tree = draw_tree out;;
let eval_out = eval out;;
let p_out = print_endline("= "^(string_of_tree out));;
let p_tree = print_endline("= "^tree);;
let p_eval = print_endline("= "^string_of_int eval_out^"\n");;
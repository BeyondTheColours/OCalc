type t = Nil | Cons of char*t
                 
  let rec fold_left f c_s acc =
    match c_s with |Nil -> acc | Cons(c, t) -> fold_left f t (f c acc)
                                                 
  let string_to_cons_string s =
    let rec string_to_cons_string_worker s count acc =
      if count = 0 then Cons(s.[0], acc)
      else string_to_cons_string_worker s (count-1) (Cons(s.[count], acc))
    in string_to_cons_string_worker s ((String.length s)-1) Nil
  
  let cons_string_to_string c_s =
    let char_concat c s = s^(Char.escaped c)
    in fold_left char_concat c_s ""
      
  let rev c_s =
    let add_char c c_s = Cons(c, c_s)
    in fold_left add_char c_s Nil
                                                 
  let concat a b =
    let add_char c c_s = Cons(c, c_s)
    in fold_left add_char (rev a) b

  let ( ^^ ) a b = concat a b

  let length c_s =
    let rec length_worker c_s acc =
      match c_s with
      |Nil -> acc
      |Cons(c, t) -> length_worker t (acc+1)
    in length_worker c_s 0

   (* let contains c_s ch =
      let is_eq_acc a b = (a = ch) || b
      in fold_left is_eq_acc c_s false
      This implementation of contains is more fun but not as good as the simple pattern matching one below*)
      
      (* is_eq_acc type signature - char -> bool -> bool
      a : char, b : bool and ch : char (ch is provided in the outer 'contains' function)
      is_eq_acc compares a with ch and takes that boolean value OR the boolean value of b.
      If (a = ch) ever evaluates to true, the accumulator of fold_left will remain true forever due to the OR ( || ) comparison
      Unfortunately, this implementation will always search through the entire c_s as there is no break
      up finding a match*)

    let rec contains c_s ch =
      match c_s with
      |Nil -> false
      |Cons(c, t) -> if c = ch then true else contains t ch
    (* Better implementation of contains as it stops when the first case of (c = ch) is found *)

    let count c_s ch =
      let is_eq_inc a b =
        if a = ch then (b+1) else b
      in fold_left is_eq_inc c_s 0
      (* Got to use fold_left within contains *)
      (* counts the number of times a cons_string (c_s) contains a given char ch *)

    let op_count c_s ch =
      let rec count_worker c_s ch count =
        match c_s with
        |Nil -> if count = 0 then None else Some(count)
        |Cons(c, t) ->
          match count_worker t ch count with
          |None -> if c = ch then Some(count+1) else Some(count)
          |Some(x) -> if c = ch then Some(x+1) else Some(x)
      in count_worker c_s ch 0
      (* Alternative implementation of count returning an int option
      rather than an int *)
;;



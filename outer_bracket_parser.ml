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
;;

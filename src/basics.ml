let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = 
  let (a, b) = tup in
  (b, a)
let rev_triple (tup : 'a * 'b * 'c) = 
  
  let (a, b, c) = tup in
  (c, b, a)

let is_odd x = 
  x mod 2 <> 0

let is_older (date1: int * int * int) (date2: int * int * int) = 
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2

let to_us_format (date1: int * int * int) = 
  let (y, m, d) = date1 in
  (m, d, y)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = 
  if p = 0 then 1
  else x * pow x (p-1)

let rec fac n = 
  if n = 1 then 1
  else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = 
  match lst with
  | [] -> failwith "Index out of bounds"
  | h :: t ->
    if idx = 0 then h
    else get_nth (idx - 1, t)

let larger lst1 lst2 = 
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  if len1 > len2 then lst1
  else if len2 > len1 then lst2
  else []

let sum lst1 lst2 = 
  let rec aux 1 = 
    math 1 with 
    | [] -> 0
    | h :: t -> h + aux t
  in
  aux lst1 + aux lst2

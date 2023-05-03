type 'a code =
    Dummy
  | Atomic of string * 'a * 'a code ref
  | Sequence of string * 'a code list * 'a code ref
  | Concurrent of string * 'a code list * 'a code ref


let set_parent p =
  let rec set_parent' c p =
    match c with
      Dummy -> failwith "Dummy not expected."
    | Atomic(_, v, pref) -> pref := p
    | Sequence(_, children, pref) ->
        pref := p;
        List.iter (fun c' -> set_parent' c' c) children
    | Concurrent(_, children, pref) ->
        pref := p;
        List.iter (fun c' -> set_parent' c' c) children
  in
  match p with
    Dummy -> failwith "Dummy not expected."
  | Atomic(_, _, _) -> ()
  | Sequence(_, children, pref) -> 
      List.iter (fun c' -> set_parent' c' p) children
  | Concurrent(_, children, pref) ->
      List.iter (fun c' -> set_parent' c' p) children

let lspace l =
  let ws = "    " in
  let rec iter l = if l = 0 then "" else ws ^ (iter (l - 1)) in
  "\n" ^ iter l

let code_name = function
    Dummy -> "Dummy"
  | Atomic(n, _, _) -> n
  | Sequence(n, _, _) -> n
  | Concurrent(n, _, _) -> n

let string_of_code_name c =
  match c with
    Dummy -> "Dummy"
  | Atomic(n, v, pref) ->
      let pname = (code_name !pref) in
      "Atomic(" ^ n ^ ", " ^ pname ^ ")"
  | Sequence(n, children, pref) ->
      let pname = (code_name !pref) in
      "Sequence(" ^ n ^ ", " ^ pname ^ ")"
  | Concurrent(n, children, pref) ->
      let pname = (code_name !pref) in
      "Concurrent(" ^ n ^ ", " ^ pname ^ ")"

let rec first c =
  match c with
    Dummy -> failwith "Dummy not expected."
  | Atomic(_, _, _) -> [c]
  | Sequence(_, children, pref) -> first (List.hd children)
  | Concurrent(_, children, pref) ->
      let lof = List.map first children in List.flatten lof

let rec last_of_list = function
    [] -> failwith "Empty list can't have a last element"
  | [le] -> le
  | _ :: [le] -> le
  | _ :: t -> last_of_list t

let rec last c =
  match c with
    Dummy -> failwith "Dummy not expected."
  | Atomic(_, _, _) -> [c]
  | Sequence(_, children, pref) -> last (last_of_list children)
  | Concurrent(_, children, pref) ->
      let lol = List.map last children in List.flatten lol

let parent c =
  match c with
    Dummy -> failwith "Dummy not expected."
  | Atomic(_, _, pref) -> !pref
  | Sequence(_, _, pref) -> !pref
  | Concurrent(_, _, pref) -> !pref

let is_last_or_first_child fname f1 =
  let f c =      
    match parent c with
      Dummy -> failwith (fname ^ ": Dummy not expected.")
    | Atomic(_, _, _) ->  failwith (fname ^ ": Unreachable code! Atomic code can't be a parent.")
    | Sequence(_, children, _) -> (f1 children) == c
    | Concurrent(_, _, _) -> failwith (fname ^ ": No last child defined for concurrent parent.")
  in
  f

let is_last_child = is_last_or_first_child "is_last_child" last_of_list
let is_first_child = is_last_or_first_child "is_first_child" List.hd

let rec string_of_list string_of_element = function
    [] -> ""
  | h :: t -> string_of_element h ^ " " ^ (string_of_list string_of_element t)

let next_or_prev_element n1 n2 =
  let rec f e l =
    match l with
      [] -> failwith "next_element: empty list"
    | [e'] -> failwith "next_element: too few elements"
    | _ ->
        if e == (List.nth l n1) then (List.nth l n2)
        else f e (List.tl l)
  in
  f

let next_element = next_or_prev_element 0 1
let prev_element = next_or_prev_element 1 0

let next_or_prev_sibling fname f1 =
  let f c =
    match parent c with
      Dummy -> failwith (fname ^ ": Dummy not expected.")
    | Atomic(_, _, _) ->  failwith (fname ^ ": Unreachable code! Atomic code can't be a parent.")
    | Sequence(_, children, _) -> f1 c children
    | Concurrent(_, _, _) -> failwith (fname ^ ": No last child defined for concurrent parent.")
  in
  f 

let next_sibling = next_or_prev_sibling "next_sibling" next_element
let prev_sibling = next_or_prev_sibling "prev_sibling" prev_element

let next_or_prev f1 f2 f3 =
  let rec f c =
    let p = parent c in
    match p with
      Dummy -> []
    | Atomic(_, _, _) -> failwith "Unreachable code! Atomic code can't be a parent."
    | Sequence(_, _, _) -> if f1 c then f p else f2 (f3 c)
    | Concurrent(_, _, _) -> f p
  in
  f

let next = next_or_prev is_last_child first next_sibling
let prev = next_or_prev is_first_child last prev_sibling

let print_code c =
  let rec print_indented c l =
    print_string ((lspace l) ^ string_of_code_name c);
    match c with
      Dummy -> ()
    | Atomic(n, v, pref) -> ()
    | Sequence(n, children, pref) ->
        List.iter (fun c' -> print_indented c' (l + 1)) children
    | Concurrent(n, children, pref) ->
        List.iter (fun c' -> print_indented c' (l + 1)) children
  in
  print_indented c 0

let ax = Atomic("ax", "A.X", ref Dummy)
and ex = Atomic("ex", "E.X", ref Dummy)
and cx = Atomic("cx", "C.X", ref Dummy)
and fx = Atomic("fx", "F.X", ref Dummy)
and ln = Atomic("ln", "L.X", ref Dummy)
and hn = Atomic("hn", "H.N", ref Dummy)
and mn = Atomic("mn", "M.N", ref Dummy)
and jn = Atomic("jn", "J.N", ref Dummy)

let s1 = Sequence("s1", [ax; ex], ref Dummy)
and s2 = Sequence("s2", [cx; fx], ref Dummy)
and s3 = Sequence("s3", [ln; hn], ref Dummy)
and s4 = Sequence("s4", [mn; jn], ref Dummy)

let c1 = Concurrent("c1", [s1; s2], ref Dummy)
and c2 = Concurrent("c2", [s3; s4], ref Dummy)

let gx = Atomic("gx", "G.X", ref Dummy)
and ta = Atomic("ta", "t.a", ref Dummy)
and nn = Atomic("nn", "N.N", ref Dummy)

let cec = Sequence("cec", [c1; gx; ta; nn; c2], ref Dummy)
 
let _ = set_parent cec

let t1 () = print_code cec

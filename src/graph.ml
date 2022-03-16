type ugt = int list list
type vgt = ugt

exception UnknownNode of int
exception InvalidGraph
exception IndexOutofBounds

let empty = []
let size x = List.length x
let add graph = (size graph, graph @ [ [] ])
let add_no_id graph = snd (add graph)

let connect graph id1 id2 =
  if id1 > size graph then raise (UnknownNode id1)
  else if id2 > size graph then raise (UnknownNode id2)
  else
    (*As long as no points are removed, this check is unnecessary. if
      List.nth graph (id1-1) = [] then UnknownNode id1 else if List.nth
      graph (id2-1) = [] then UnknownNode id2 else *)
    let rec con counter = function
      | some :: more ->
          if id1 = counter then (id2 :: some) :: con (counter + 1) more
          else if id2 = counter then
            (id1 :: some) :: con (counter + 1) more
          else some :: con (counter + 1) more
      | [] -> []
    in
    con 1 graph

let rec unverify vg = vg

let rec verify ug =
  match ug with
  | [] -> []
  | [] :: t -> raise InvalidGraph
  | h :: t -> verify t

(** [get_element_at_index lst i] is the element within lst at the index
    i. Raises: IndexOutofBounds if i >= List.length lst *)
let rec get_element_at_index lst i =
  if i >= List.length lst then raise IndexOutofBounds
  else
    match lst with
    | [] -> raise IndexOutofBounds
    | h :: t -> if i = 0 then h else get_element_at_index t (i - 1)

let neighbors graph id = get_element_at_index graph (id - 1)

(** [set graph] is a set-like list of all ids within [graph]*)
let set graph =
  let rec range c u =
    if c > 0 then (u - c + 1) :: range (c - 1) u else []
  in
  let sz = size graph in
  range sz sz

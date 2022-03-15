type gt = int list list
type ugt = gt
type vgt = gt

exception UnknownNode of int
exception InvalidGraph

let empty = []

let size = fun x -> List.length x

let add graph = (size graph, graph @ [[]])

let connect graph id1 id2 = 
    if id1 >= size graph then raise (UnknownNode id1) else
    if id2 >= size graph then raise (UnknownNode id2) else
    (*As long as no points are removed, this check is unnecessary. 
    if List.nth graph (id1-1) = [] then UnknownNode id1 else 
    if List.nth graph (id2-1) = [] then UnknownNode id2 else *)
    let rec con counter = function 
    | some :: more -> 
        if id1 = counter then (id2 :: some) :: con (counter + 1) more else
        if id2 = counter then (id1 :: some) :: con (counter + 1) more else
        some :: con (counter + 1) more
    | [] -> [] 
    in
    con 1 graph
    
let verify ug = raise (Failure "Unimplemented")

let neighbors graph id = raise (Failure "Unimplemented")

(** [set graph] is a set-like list of all ids within [graph]*)  
let set graph =
    let rec range c u = if c > 0 then u - c + 1 :: range (c-1) u else [] in
    let sz = size graph in range sz sz 




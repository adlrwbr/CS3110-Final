(* represents an undirected graph *)

type gt = int list list

exception UnknownNode of int

let size graph = graph |> List.filter (fun x -> x <> []) |> List.length

(** Gets the element of a list starting from 1 instead of 0.*)
let ith list n = if size list < n then [] else List.nth list (n-1)

(** Checks to see if the [graph] satisfies its invariants *)
let rep_ok graph = false

let empty = [ [] ]

let rec add_r id connections graph copy counter = 
assert (not (List.mem id connections)); assert (0 < id);
match connections with
| current :: more -> 
let row = List.sort_uniq compare (ith graph counter) in
    if counter = id then 
            List.sort_uniq compare (ith graph counter @ copy) 
            :: add_r id connections graph copy (counter + 1)
    else if current = counter then 
        if List.mem id row then 
            row :: add_r id more graph copy (counter + 1)
        else (id :: row) :: add_r id more graph copy (counter + 1)
    else ith graph counter :: add_r id connections graph copy (counter + 1)
| [] -> 
    if counter <= size graph || counter <= id then
        if counter = id then
            List.sort_uniq compare (ith graph counter @ copy) 
            :: add_r id [] graph copy (counter + 1)
        else
            ith graph counter :: add_r id [] graph copy (counter + 1)
    else []

let add_node graph id connections= 
    let connections = (List.sort_uniq compare connections) in 
    add_r id connections graph connections 1

(*let connect_node graph id1 id2= 
    add_r id connections graph id1 [id2] 
    [CHECK IMPL & if even relevant UTILITY*)

let rec contains graph id = match graph with
| some :: more -> if List.mem id some then contains id more else false
| [] -> false

let neighbors graph id = List.nth graph (id-1)

let set graph = List.sort_uniq compare (List.flatten graph)

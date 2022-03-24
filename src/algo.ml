(** [remove_all list1 list2] is [list1] without the elements of [list2]*)
let rec remove_all (list1 : 'a list) (list2 : 'a list) : 'a list = 
match list2 with 
| head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
| [] -> list1

let last (list : 'a list) : 'a = List.nth list (List.length list - 1);;
(** [last list] is the last element in the [list] 
    Requires: [list] is not empty. *)

(* TODO: use "is" verb *)
(** [relate f list] traverses [list] comparing the first two elements with [f].
If [f] is true, first continues, else the second continues. Check repeats with
the continuing element and the third, etc. Finishes once there is
one element that has survived the comparison chain. 
Requires: [list] contains at least one element.*)
let rec relate (f : 'a -> 'a -> bool) (list : 'a list) : 'a = match list with
| cur :: next :: more -> if f cur next then relate f (cur :: more) 
                         else relate f (next :: more)
| cur :: [] -> cur
| [] -> raise (Failure "No elements")

let rec bfsr graph ids counter memory output =
    let next_targs = remove_all 
        (List.sort_uniq compare 
            (match ids with
            | current :: more -> Graph.neighbors graph current 
            @ bfsr graph more counter (current :: memory) output
            | [] -> []
        )) 
        memory
    in
    (
        if next_targs = [] then output
        else bfsr graph next_targs counter (memory @ next_targs) (output @ next_targs)
        (*if next_targs = [] then output 
        else *)
    )

let bfs (graph : 'a) (id : int) : int list = [id] @ bfsr graph [id] 0 [id] []
(** [bfs graph ids] is a set-like list of all nodes in [graph] sorted by number of edges
taken to reach id in [ids].*)

let shortest_path start finish graph =
    raise (Failure "Unimplemented")

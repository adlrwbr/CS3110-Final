(** [remove_all list1 list2] is [list1] without the elements of [list2]*)
let rec remove_all (list1 : 'a list) (list2 : 'a list) : 'a list = 
match list2 with 
| head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
| [] -> list1

let last (list : 'a list) : 'a = List.nth list (List.length list - 1);;
(** [last list] is the last element in the [list] 
    Requires: [list] is not empty. *)

let rec relate f list = match list with
| cur :: next :: more -> if f cur next then relate f (cur :: more) 
                         else relate f (next :: more)
| cur :: [] -> cur
| [] -> raise (Failure "No elements")

let rec setbfsr graph ids counter memory output =
    let next_targs = remove_all 
        (List.sort_uniq compare 
            (match ids with
            | current :: more -> Graph.neighbors graph current 
            @ setbfsr graph more counter (current :: memory) output
            | [] -> []
        )) 
        memory
    in
    (
        if next_targs = [] then output
        else setbfsr graph next_targs counter (memory @ next_targs) (output @ next_targs)
        (*if next_targs = [] then output 
        else *)
    )

let setbfs graph id = [id] @ setbfsr graph [id] 0 [id] []
(** [bfs graph ids] is a set-like list of all nodes in [graph] sorted by number of edges
taken to reach id in [ids].*)

(** [gather_neighbors graph queue] is a list of pairs where:
the first component is the id of some node
the second component is the list of all its connections.  *)
let rec gather_neighbors graph queue except =
    match queue with    (**This remove_all prevents back_tracking but isn't necessary.*)
    | elem :: more -> 
        (remove_all (Graph.neighbors graph elem) except) :: gather_neighbors graph more except
    | [] -> []

let make_setlike list = List.sort_uniq compare list

let rec bfs graph queue memory output = 
    let links = gather_neighbors graph queue memory in
    let output = output @ [links] in 
    let queue = make_setlike (List.flatten links) in 
    if (remove_all queue memory) = [] then output else
    let memory = make_setlike (memory @ queue) in 
    (bfs graph queue memory output)

let breadth_first graph id = bfs graph [id] [id] [[[id]]]

let rec chop_until f = function
| some :: more -> if f some then (some :: more) else chop_until f more
| [] -> []

let rec index_helper elem list ctr = match list with 
| maybe :: more -> if elem = maybe then ctr else index_helper elem more (ctr +1)
| [] -> raise (Failure "IndexOutOfBounds")

(**[index elem list] is the index of [elem] in [list].
Requires: List.mem elem list = true *)
let rec index elem list = index_helper elem list 0

let rec build_trace_rec elem list memory =
    match list with
    | seat :: more -> ()


(**
Requires elem is 
let rec build_trace elem list = build_trace_rec sitting_id list [] *)


let shortest_path start finish graph = raise (Failure "Unimplemented")
(*
    let search_domain = breadth_first graph start |> List.rev in 
    let truncated_domain = chop_until (List.mem finish) search_domain in *)

    



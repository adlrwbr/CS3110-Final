(* represents an undirected graph *)

type gt = int list list
(** the graph abstract type *)

exception UnknownNode of int
(** raised when a node with an unknown ID is referenced in a graph *)

(** [size graph] is the number of unique nodes contained in [graph] *)
let size graph = List.length graph

(** shorthand for List.nth *)
let ith list n = if size list < n then [] else List.nth list (n-1)
(** Checks if [graph] satisfies invariants.*)

let rep_ok graph = false

(** [empty] is a [graph] with no nodes *)
let empty = [ [] ]

let g = empty
let e = empty

let rec help_close (id:int) (connect_to:int list) (counter:int) graph (id_cons: int list) = 
assert (not (List.mem id connect_to));
assert (0 < id);
match connect_to with
| current :: more -> 
let row = ith graph counter in
    (*if counter + 1 = id then
        if current = counter then 
            if List.mem id row then 
                row @ help_close id more (counter + 1) graph id_cons
            else 
                (id :: row) @ help_close id more (counter + 1) graph id_cons
        else 
            ith graph counter @ help_close id connect_to (counter + 1) graph id_cons
    else*)
     if current = counter then 
        if List.mem id row then 
            row :: help_close id more (counter + 1) graph id_cons
        else 
            (id :: row) :: help_close id more (counter + 1) graph id_cons
    else 
        ith graph counter :: help_close id connect_to (counter + 1) graph id_cons
| [] -> 
    if counter <= size graph then
        if counter = id then
            List.sort_uniq compare (ith graph counter @ id_cons) :: help_close id [] (counter + 1) graph id_cons
        else
            ith graph counter :: help_close id [] (counter + 1) graph id_cons
    else []

(** [close_on id connect_to graph] makes sure *)
let co graph connect_to id = help_close id connect_to 1 graph connect_to

(** [add_node graph id_list] is a tuple containing the modified [graph]
    with an additional node and that new node’s id  *)
let add_node graph id_list =
    if id_list = [] then assert false 
    else if graph = empty then [id_list] 
    else graph @ [id_list]

(** [connect_nodes graph id1 id2] is the modified graph of [gt] with an
    added connection between two existing nodes [id1] and [id2]. Raises
    UnknownNode if either node DNE in [gt] *)
let connect_nodes graph id1 id2 = []

(** [neighbors graph id] is the list of nodes in [graph] that [id] is
    connected to *)
let neighbors graph id = []

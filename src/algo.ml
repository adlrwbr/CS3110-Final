let rec remove_all list1 list2 = 
match list2 with 
| head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
| [] -> list1

let rec bsfr graph ids counter memory =
    let next_targs = List.sort_uniq compare (match ids with
    | current :: more -> Graph.neighbors graph current 
    @ bsfr graph more counter (current :: memory)
    | [] -> []
    ) in (
        next_targs
    )

<<<<<<< HEAD
let bfs graph id = let vg = Graph.verify graph in
    [id] @ bsfr vg [id] 0 [id]


    
=======
let bfs graph id = id :: bsfr graph [id] 0 [id]
>>>>>>> 1dae923174930b2339cc0d10f1a2604b78561339

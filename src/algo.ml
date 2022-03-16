let rec remove_all list1 list2 = 
match list2 with 
| head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
| [] -> list1

let tl list = List.nth list (List.length list - 1);;

let rec relate f list = match list with
| cur :: next :: more -> if f cur next then relate f (cur :: more) else relate f (next :: more)
| cur :: [] -> cur
| [] -> assert false

let rec bsfr graph ids counter memory =
    let next_targs = List.sort_uniq compare (match ids with
    | current :: more -> Graph.neighbors graph current 
    @ bsfr graph more counter (current :: memory)
    | [] -> []
    ) in (
        next_targs
    )

(*let bfs graph id = let vg = Graph.verify graph in
    [id] @ bsfr vg [id] 0 [id]*)


    

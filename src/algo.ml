let rec remove_all list1 list2 = 
match list2 with 
| head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
| [] -> list1

let last list = List.nth list (List.length list - 1);;

let rec relate f list = match list with
| cur :: next :: more -> if f cur next then relate f (cur :: more) 
                         else relate f (next :: more)
| cur :: [] -> cur
| [] -> assert false

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

let bfs graph id = [id] @ bfsr graph [id] 0 [id] []

let shortest_path start finish graph =
    raise (Failure "Unimplemented")

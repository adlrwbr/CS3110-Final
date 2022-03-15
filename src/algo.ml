open Graph

let rec remove_all list1 list2 = 
match list2 with 
| head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
| [] -> list1

let rec bsfr graph ids counter memory =
    let next_targs = List.sort_uniq compare (match ids with
    | current :: more -> neighbors graph current 
    @ bsfr graph more counter (current :: memory)
    | [] -> []
    ) in (
        next_targs
    )

let bfs graph ids = bsfr graph ids 0 []


    

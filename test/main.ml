open OUnit2
open Pathfinder
open Graph

let pullout added_graph = match added_graph with | (x,y) -> y

let my_graph = add empty |> pullout |> add |> pullout

let verifiable = connect my_graph 1 2
let unverifiable = connect (add my_graph |> pullout |> add |> pullout) 3 4

let tests = "graph test suite" >::: [
    "graph which is verifiable" >:: (fun _ -> let _ = verify verifiable in ());
    "graph which is not verifiable" >:: (fun _ -> assert_raises (InvalidGraph) (fun () -> verify unverifiable))
]

let _ = run_test_tt_main tests
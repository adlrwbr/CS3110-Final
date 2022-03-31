open OUnit2
open Pathfinder
open Graph
open Algo

(* TODO: test everything
let pullout added_graph = match added_graph with | (x,y) -> y

let ap graph = graph |> add 1 |> pullout

let my_graph = empty |> ap |> ap

let verifiable = connect 1 2 my_graph
let unverifiable = connect 3 4 (my_graph |> ap |> ap)

let one_five_cycle = empty |> ap |> ap |> ap |> ap |> ap
|> connect 1 2 |> connect 2 3 |> connect 3 4 |> connect 4 5


(** Cumulative test suite. *)
let graph_tests = "graph test suite" >::: [
    "graph which is verifiable" >:: (fun _ -> let _ = verify verifiable in ());
    "graph which is not verifiable" >:: (fun _ -> assert_raises (InvalidGraph) 
    (fun () -> verify unverifiable));

]

let _ = run_test_tt_main graph_tests
*)

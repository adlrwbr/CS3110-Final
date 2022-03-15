type vgt = int list list
type ugt = int list list

exception UnknownNode of int
exception InvalidGraph

let empty = [ [] ]

let add graph = raise (Failure "Unimplemented")

let connect graph id1 id2 = raise (Failure "Unimplemented")

let verify ug = raise (Failure "Unimplemented")

let size graph = graph |> List.length

let neighbors graph id = raise (Failure "Unimplemented")

let set graph = raise (Failure "Unimplemented")

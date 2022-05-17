type ugt = {
          nodes : (int , int list) Hashtbl.t;
          edges : (int * int , float) Hashtbl.t
          }
type vgt = {
          nodes : (int , int list) Hashtbl.t;
          edges : (int * int , float) Hashtbl.t
          }

exception UnknownNode of int
exception UnknownEdge
exception InvalidGraph
exception IndexOutofBounds

let empty () : ugt = { nodes = Hashtbl.create 16; edges = Hashtbl.create 16 }
(* TODO: Sean, using [weights] as a global works fine when we only deal with
   one graph, but if multiple graphs have the same ids then this will
   cause an issue *)
let size (graph : ugt) = Hashtbl.length graph.nodes

let add (id : int) (graph : ugt) =
  if Hashtbl.mem graph.nodes id then failwith @@ Int.to_string id
  else Hashtbl.add graph.nodes id []

let rec add_many (ids : int list) (graph : ugt) = match ids with 
  id :: more -> add id graph; add_many more graph
  | [] -> ()

let edge id1 id2 = if id1 < id2 then (id1, id2) else (id2, id1)

let weight (graph : vgt) id1 id2  =
  if Hashtbl.mem graph.edges (edge id1 id2) then
    Hashtbl.find graph.edges (edge id1 id2)
  else raise UnknownEdge

let connect id1 id2 weight (graph : ugt) : unit =
  if not @@ Hashtbl.mem graph.nodes id1 then raise (UnknownNode id1)
  else if not @@ Hashtbl.mem graph.nodes id2 then raise (UnknownNode id2)
  else Hashtbl.add graph.edges (edge id1 id2) weight;
  Hashtbl.replace graph.nodes id1
    (List.sort_uniq compare @@ (id2 :: Hashtbl.find graph.nodes id1));
  Hashtbl.replace graph.nodes id2
    (List.sort_uniq compare @@ (id1 :: Hashtbl.find graph.nodes id2))

let unverify (vg : vgt) : ugt = {nodes = vg.nodes; edges = vg.edges}

let rec help_verify ug queue =
  match queue with
  | some :: more -> Hashtbl.remove ug some
  | [] -> ()

(** TODO Find way to verify graph. *)
let verify (ug : ugt) : vgt = {nodes = ug.nodes; edges = ug.edges}

let neighbors graph id =
  if Hashtbl.mem graph.nodes id = false then []
  else
    (*let rec string_of_intl = function [] -> "" | some :: [] ->
      (string_of_int some) | some :: more-> (string_of_int
      some)^","^string_of_intl more in let _ = print_endline
      (string_of_int id ^ "-->" ^string_of_intl @@ Hashtbl.find graph
      id) in*)
    Hashtbl.find graph.nodes id
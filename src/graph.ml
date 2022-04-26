type ugt = (int, int list) Hashtbl.t
type vgt = (int, int list) Hashtbl.t

exception UnknownNode of int
exception UnknownEdge
exception InvalidGraph
exception IndexOutofBounds

let empty () = Hashtbl.create 16
let weights = Hashtbl.create 16
(* TODO: Sean, using [weights] as a global works fine when we only deal with
   one graph, but if multiple graphs have the same ids then this will
   cause an issue *)
let size graph = Hashtbl.length graph

let add (id : int) (graph : ugt) =
  if Hashtbl.mem graph id then failwith @@ Int.to_string id
  else Hashtbl.add graph id []

let edge id1 id2 = if id1 < id2 then (id1, id2) else (id2, id1)

let weight id1 id2 =
  if Hashtbl.mem weights (edge id1 id2) then
    Hashtbl.find weights (edge id1 id2)
  else raise UnknownEdge

let connect id1 id2 weight graph : unit =
  if not @@ Hashtbl.mem graph id1 then raise (UnknownNode id1)
  else if not @@ Hashtbl.mem graph id2 then raise (UnknownNode id2)
  else Hashtbl.add weights (edge id1 id2) weight;
  Hashtbl.replace graph id1
    (List.sort_uniq compare @@ (id2 :: Hashtbl.find graph id1));
  Hashtbl.replace graph id2
    (List.sort_uniq compare @@ (id1 :: Hashtbl.find graph id2))

let unverify (vg : vgt) : ugt = vg

let rec help_verify ug queue =
  match queue with
  | some :: more -> Hashtbl.remove ug some
  | [] -> ()

(** TODO Find way to verify graph. *)
let verify (ug : ugt) : vgt = ug

let neighbors graph id =
  if Hashtbl.mem graph id = false then []
  else
    (*let rec string_of_intl = function [] -> "" | some :: [] ->
      (string_of_int some) | some :: more-> (string_of_int
      some)^","^string_of_intl more in let _ = print_endline
      (string_of_int id ^ "-->" ^string_of_intl @@ Hashtbl.find graph
      id) in*)
    Hashtbl.find graph id

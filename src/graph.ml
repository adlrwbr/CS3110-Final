
type ugt = (int, int list) Hashtbl.t
type vgt = (int, int list) Hashtbl.t

exception UnknownNode of int
exception InvalidGraph
exception IndexOutofBounds

let empty = Hashtbl.create 16

let evg : vgt = Hashtbl.create 16

let size graph = Hashtbl.length graph

let add (id : int) (graph : ugt) =
  match Hashtbl.find graph id with
  | exception Not_found ->
      (Hashtbl.add graph id []; graph)
  (* id has already been added *)
  | _ -> Int.to_string id |> failwith

let connect id1 id2 graph : ugt =
  if not (List.mem id2 (Hashtbl.find graph id1))
    then Hashtbl.add graph id1 (id2 :: Hashtbl.find graph id1);
  if not (List.mem id1 (Hashtbl.find graph id2)) 
    then Hashtbl.add graph id2 (id1 :: Hashtbl.find graph id2);
  graph

let unverify (vg : vgt) : ugt = vg

let rec help_verify ug queue = 
  match queue with
  | some :: more -> Hashtbl.remove ug some
  | [] -> ()

let verify (ug : ugt) : vgt = ug (** TODO Find way to verify graph. *)

let neighbors graph id = List.sort_uniq compare (Hashtbl.find graph id)

(** Representation of the underlying graph that corresponds to a world.

    This module represents a connected graph generated from the world,
    with nodes and edges.

    REPRESENTATION INVARIANT: There can be no duplicate connections to
    the same node. I.e. [...\[2;2;3\]...] is not allowed because we
    cannot connect the same node to 2 twice.*)

type ugt
(** The unverified graph abstract type. Represents a bidirectional graph
    which may contain islands. *)

type vgt
(** The verified graph abstract type. Guarenteed to not contain islands. *)

exception UnknownNode of int
(** Raised when an unknown node is encountered. *)

exception UnknownEdge
(** Raised when an unknown edge is encountered. *)

exception InvalidGraph
(** Raised when an invalid unverified graph is encountered. *)

val empty : unit -> ugt
(** [empty] is a graph with no nodes *)

val size : ugt -> int
(** [size graph] is the number of unique nodes contained in [graph] *)

val add : int -> ugt -> unit
(** [add id graph] modifies [graph] with an additional node [id].
    Raises: [Failure id] if the [id] already exists in the [graph] *)

val add_many : int list -> ugt -> unit
(** [add_many ids graph] modifies [graph] all additional nodes in [ids].
    Raises: [Failure id] if any [id] already exists in the [graph] *)

val connect : int -> int -> float -> ugt -> unit
(** [connect id1 id2 weight graph] modifies the graph by adding an edge
    b/w nodes [id1] and [id2]. The edge has a value of [weight].
    Requires: [id1] != [id2] Raises: [UnknownNode id] if either [id1] or
    [id2] DNE within the graph *)

val weight : int -> int -> float
(** [weight id1 id2] is the weight of an edge between nodes [id1] and
    [id2]. Raises: [UnknownEdge] if the edge DNE *)

val unverify : vgt -> ugt
(** [unverify graph] converts the verified graph into one that is no
    longer guaranteed to be *)

val verify : ugt -> vgt
(** [verify graph] is the verified graph. Raises InvalidGraph if [graph]
    is not verifiable. *)

val neighbors : vgt -> int -> int list
(** [neighbors graph id] is the list of nodes in [graph] to which [id]
    is connected, sorted by least to greatest id.*)

(** REPRESENTATION INVARIANT: There can be no duplicate connections to
the same node. I.e. [...[2;2;3]...] is not allowed because we cannot
connect the same node to 2 twice.*)

type ugt
(** the unverified graph abstract type. Represents a bidirectional graph
    which may contain islands *)

type vgt
(** the verified graph abstract type. Guarenteed to not contain islands *)

exception UnknownNode of int

exception InvalidGraph
(** identifies an unverified graph that cannot be verified *)

val empty : ugt
(** [empty] is a graph with no nodes *)

val size : ugt -> int
(** [size graph] is the number of unique nodes contained in [graph] *)

val add : int -> ugt -> ugt
(** [add id graph] is a modified [graph] with an additional node specified by
    [id].
    Raises: [Failure id] if the [id] already exists in the [graph] *)

(**BUG SPOTTED OR SPEC UPDATE NEEDED - SEAN
If you repeat a connect command, it duplicates the connections. Not setlike! *)
val connect : int -> int -> float -> ugt -> ugt
(** [connect id1 id2 weight graph] is a modified unverified graph w/ an additional
    edge b/w nodes [id1] and [id2]. The edge has a value of [weight].
    Requires: [id1] != [id2]
    Raises: [UnknownNode id] if either [id1] or [id2] DNE within the graph *)

val unverify : vgt -> ugt
(** [unverify graph] converts the verified graph into one that is
    no longer guaranteed to be  *)

val verify : ugt -> vgt
(** [verify graph] is the verified graph. Raises InvalidGraph if [graph]
    is not verifiable. *)

val neighbors : vgt -> int -> int list
(** [neighbors graph id] is the list of nodes in [graph] to which [id]
    is connected, sorted by least to greatest id.*)

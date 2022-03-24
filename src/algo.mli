(** contains helper functions for various data structure *)

val shortest_path : string -> string -> Graph.vgt -> string list
(** [shortest_path start finish graph] is an inclusive list of the ids of the
    nodes along the shortest path from [start] to [finish], inclusive.
    Example: [ [start]; ...; [finish] ] *)

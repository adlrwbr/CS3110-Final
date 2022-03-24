(** represents the world model in MVC pattern *)

exception IllegalWorld of string
(** IllegalWorld exception with user-friendly error message *)

type wt
(** the abstract world type *)

type lt
(** the abstract location type *)

val size_x : float
(** the width of the world *)

val size_y : float
(** the height of the world *)

val empty : string -> wt
(** [empty name] is an empty world named [name] *)

val add_loc : string -> string -> Road.t -> float -> wt -> lt * wt
(** [add_loc name category road pos world] is the tuple ([loc], [newworld])
    where [newworld] is [world] w/ an additional location [loc].
    Requires: [world] contains [road] *)

val add_road : Road.t -> wt -> wt
(** [add_road road world] is a modified [world] with an additional [road] and
    intersections *)

val locations : wt -> lt list
(** [locations world] is a list of all locations contained in [world] *)

val name : lt -> string
(** [name loc] is the name of the location [loc] *)

val category : lt -> string
(** [category loc] is the category of the location [loc] *)

val loc_coord : lt -> float * float
(** [loc_coord loc] is the world-space (x, y) coordinate of location [loc] *)

val roads : wt -> Road.t list
(** [roads world] is the list of all roads in the [world] *)

val midchord : Road.t -> float * float
(** TODO: add doc *)

val distance : float * float -> float * float -> float
(** TODO: add doc *)

val reduce : wt -> Graph.vgt
(** [reduce world] is a graph representing the simplified state of the world
    where intersections and locations are nodes connected by roads
    Raises: [IllegalWorld] if world cannot be reduced into a verified graph *)

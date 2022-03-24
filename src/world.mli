(** represents the world model in MVC pattern *)

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
(** [add_road road world] is a modified [world] with an additional [road] *)

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
(** [midpt road] finds the midpoint of the [road] passed in SOLELY by
    looking at the start and end nodes. *)

val distance : float * float -> float * float -> float
(** [distance point1 point2] returns the euclidean distance between two points.*)
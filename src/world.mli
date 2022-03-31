(** represents the world model in MVC pattern *)

exception IllegalWorld of string
(** IllegalWorld exception with user-friendly error message *)

type wt
(** the abstract world type *)

type lt
(** the abstract location type *)

type path
(** an abstract type that represents a path from one [lt] to another *)

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

val midpt : Road.t -> float * float
(** [midpt road] is the midpoint of the [road] *)

val distance : float * float -> float * float -> float
(** [distance point1 point2] is the euclidean distance between [point1] and
    [point2]. *)

val rep_ok : wt -> bool
(** [rep_ok world] is whether or not all locations in the [world] connect to
    each other via roads in the [world] *)

val directions : wt -> lt -> lt -> path
(** [directions world start finish] is a path from [start] to [finish] within
    [world]
    Requires: [rep_ok world] *)

val path_coords : path -> (float * float) list
(** [path_coords p] is a list of all world coordinates along the path [p] *)

val nearroad : (float * float) -> wt -> (float * Road.t)
(** Sorry for crappy spec it's late:
This satisfies the spec for [controller.ml]'s [nearest_road], I just needed
access to helper methods and I did not want to bother with privacy changes. *)

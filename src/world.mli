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

val from_json : Yojson.Basic.t -> wt
(** [from_json j] is the world that [j] represents. Requires: [j] is
    a valid JSON world representation. *)

val to_json : wt -> Yojson.Basic.t
(** [to_json w] is the serialized world [w]. *)

val add_loc : string -> string -> Road.t -> float -> wt -> lt * wt
(** [add_loc name category road pos world] is the tuple ([loc],
    [newworld]) where [newworld] is [world] w/ an additional location
    [loc]. Requires: [world] contains [road] *)

val delete_loc : wt -> lt -> wt
(** [delete_loc world loc] is a modified [world] with [loc] removed *)

val add_road : Road.t -> wt -> wt
(** [add_road road world] is a modified [world] with an additional
    [road] and intersections.
    Raises: if [world] contains a road with the same name as [road] *)

val delete_road : wt -> Road.t -> wt
(** [delete_road world road] is a modified [world] with [road] and any
    locations/intersections on [road] removed *)

val roads_at_coord : float * float -> wt -> Road.t list
(** [roads_at_coord coord world] is a list of roads located at [coord] *)

val locations : wt -> lt list
(** [locations world] is a list of all locations contained in [world] *)

val name : wt -> string
(** [name world] is the name of the world [world] *)

val loc_category : lt -> string
(** [loc_category loc] is the category of the location [loc] *)

val loc_name : lt -> string
(** [loc_name loc] is the name of the location [loc] *)

val loc_coord : lt -> float * float
(** [loc_coord loc] is the world-space (x, y) coordinate of location
    [loc] *)

val roads : wt -> Road.t list
(** [roads world] is the list of all roads in the [world] *)

val distance : float * float -> float * float -> float
(** [distance point1 point2] is the euclidean distance between [point1]
    and [point2]. *)

val rep_ok : wt -> bool
(** [rep_ok world] is whether or not all locations in the [world]
    connect to each other via roads in the [world] *)

val directions : wt -> lt -> lt -> path
(** [directions world start finish] is a path from [start] to [finish]
    within [world] Requires: [rep_ok world] *)

val path_coords : path -> (float * float) list
(** [path_coords p] is a list of all world coordinates along the path
    [p] *)

val nearroad : float * float -> wt -> float * Road.t
(** [nearroad c world] is a tuple ([pos, road]) that specifies a
    position [pos] on a road [road] in [world] nearest the provided
    world-space coordinate pair [c]. *)

(** Representation of the world model.

    This module represents a world build through the GUI by the users,
    containing a locations, roads, and intersections. *)

exception IllegalWorld of string
(** Raised when an illegal world representation is encountered.
    [IllegalWorld s] where s is a user-friendly error message *)

exception RoadNameConflict of string
(** Raised when a two roads are attempted to be named the same.
    [RoadNameConflict s] where s is the name of the road *)

exception ParseError of string
(** Raised when the JSON file cannot be parsed. [ParseError s] where s
    is a user-friendly error message *)

type wt
(** The abstract type of values representing world types. *)

type lt
(** The abstract type of values representing location types. *)

type path
(** The abstract type that represents a path from one [lt] to another. *)

val size_x : float
(** [size_x] is the width of the world. *)

val size_y : float
(** [size_y] is the height of the world. *)

val empty : string -> wt
(** [empty name] is an empty world named [name] *)

val from_json : Yojson.Basic.t -> wt
(** [from_json j] is the world that [j] represents. Requires: [j] is a
    valid JSON world representation. Raises: ParseError if [j] is an
    invalid world format *)

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
    [road] and intersections. Raises: [IllegalWorld n] if [world]
    contains a road with the same name [n] as [road] *)

val delete_road : wt -> Road.t -> wt
(** [delete_road world road] is a modified [world] with [road] and any
    locations/intersections on [road] removed *)

val locations : wt -> lt list
(** [locations world] is a list of all locations contained in [world] *)

val intersections : wt -> Road.it list
(** [locations world] is a list of all locations contained in [world] *)

val name : wt -> string
(** [name world] is the name of the world [world] *)

val rename : wt -> string -> wt
(** [rename w name] is the world [w] renamed to [name] *)

val loc_category : lt -> string
(** [loc_category loc] is the category of the location [loc] *)

val loc_name : lt -> string
(** [loc_name loc] is the name of the location [loc] *)

val loc_coord : lt -> float * float
(** [loc_coord loc] is the world-space (x, y) coordinate of location
    [loc] *)

val roads : wt -> Road.t list
(** [roads world] is the list of all roads in the [world] *)

val rep_ok : wt -> bool
(** [rep_ok world] is whether or not all locations in the [world]
    connect to each other via roads in the [world] *)

val directions : wt -> lt -> lt -> path
(** [directions world start finish] is a path from [start] to [finish]
    within [world] Requires: [rep_ok world] *)

val path_coords : path -> (float * float) list
(** [path_coords p] is a list of all world coordinates along the path
    [p] *)

val near_road : float * float -> wt -> float * Road.t
(** [nearroad c world] is a tuple ([pos, road]) that specifies a
    position [pos] on a road [road] in [world] nearest the provided
    world-space coordinate pair [c]. *)

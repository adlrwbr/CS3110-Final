(** Representation of the road and road intersections in the world
    model.

    This module represents a road and road intersections created through
    the GUI by the users, represented by its two endpoints. *)

type t
(** The abstract type of values representing road types. *)

type it = {
  road1 : t;
  road2 : t;
  pos_on_road1 : float;
  pos_on_road2 : float;
}
(** The type of values representing intersections. *)

val create : string -> float * float -> float * float -> t
(** [create name startPoint endPoint] is a newly created road stretching
    between [startPoint] and [endPoint] *)

val name : t -> string
(** [name road] is the name of the road [road] *)

val road_coords : t -> (float * float) * (float * float)
(** [coords road] is a tuple of 2 (x, y) coordinates that define a road *)

val midpt : t -> float * float
(** [midpt road] is the midpoint of the [road] *)

val intersection : t -> t -> it option
(** [intersection r1 r2] is an intersection between [r1] and [r2] or
    [None] if the two roads do not intersect. *)

val inter_coord : it -> float * float
(** [inter_coord inter] is the (x, y) coordinate of intersection [inter] *)

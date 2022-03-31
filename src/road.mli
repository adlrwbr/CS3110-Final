(** represents the road model in MVC pattern *)

type t
(** the abstract road type *)

type it = {
  road1 : t;
  road2 : t;
  pos_on_road1 : float;
  pos_on_road2 : float;
}
(** the intersection type *)

val create : string -> float * float -> float * float -> t
(** [create name startPoint endPoint] is a newly created road stretching
    b/w [startPoint] and [endPoint] *)

val name : t -> string
(** [name road] is the name of the road [road] *)

val road_coords : t -> (float * float) * (float * float)
(** [coords road] is a tuple of 2 (x, y) coordinates that define a road *)

val midpt : t -> float * float
(** [midpt road] is the midpoint of the [road] *)

val intersection : t -> t -> it option
(** [intersection r1 r2] is an intersection between [r1] and [r2] or [None] if
    the two roads do not intersect. *)

val inter_coords : it -> float * float
(** [inter_coords inter] is the coordinate pair of the intersection [inter] *)

(** represents the road model in MVC pattern *)

type t
(** the abstract road type *)

type intersection
(** the abstract intersection type *)

val create : string -> float * float -> float * float -> t
(** [create name startPoint endPoint] is a newly created road stretching
    b/w [startPoint] and [endPoint] *)

val coords : t -> (float * float) * (float * float)
(** [coords road] is a tuple of 2 (x, y) coordinates that define a road *)

val name : t -> string
(** [name road] is the name of the road [road] *)

(** represents the view in MVC pattern *)

(** [init] instantiates a blank map *)
val init : unit

(** [draw world] draws [world] onto the GUI *)
val draw : World.wt -> unit

(** [world_to_pixel (x, y)] is an integer coordinate pair in pixel space from
    the float coordinates in World *)
val world_to_pixel : float * float -> int * int

(** [pixel_to_world (x, y)] is a float coordinate pair in world space from
    the integer pixel coordinates in View *)
val pixel_to_world : int * int -> float * float

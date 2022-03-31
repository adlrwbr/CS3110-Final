open Button

val edit_mode : World.wt -> World.wt

val start : unit -> unit
(** [start] starts the app *)

val button_touching_point : float * float -> button -> bool
(** [button_touching_point coord b] is whether or not the point [coord]
    is touching the button [b] *)

val hit_buttons : World.wt -> button list -> float * float -> World.wt
(** [hit_buttons w buttons coord] attempts to hit all enabled buttons
    that are at point [coord] *)

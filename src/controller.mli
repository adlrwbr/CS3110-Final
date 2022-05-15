(** Representation of the user mouse and keyboard interactions with the
    GUI.

    This module represents a collection of functions that take care of
    mouse and keyboard interactions with the GUI. *)

open Button

val edit_mode : World.wt -> World.wt
(** [edit_mode world] is a world edited by the user that may be reduced
    into a graph by [World.reduce] without raising an exception *)

val start : unit -> unit
(** [start ()] starts the program, creating the loop and initializing
    the view. *)

val button_touching_point : float * float -> button -> bool
(** [button_touching_point coord b] is whether or not the point [coord]
    is touching the button [b] *)

val hit_buttons : World.wt -> button list -> float * float -> World.wt
(** [hit_buttons w buttons coord] attempts to hit all enabled buttons
    that are at point [coord] *)

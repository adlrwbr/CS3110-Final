(** represents the view in MVC pattern *)

(** [init] instantiates a blank map *)
val init : unit

(** [draw world] draws [world] onto the GUI *)
val draw : World.wt -> unit

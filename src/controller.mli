(** represents the controller in MVC pattern *)
val midchord : Road.t -> float * float

(** [distance pt1 pt2] returns the cartesian distance between pt1 and pt2 *)
val distance : float * float -> float * float -> float

(** [start] starts the app *)
val start : unit -> unit

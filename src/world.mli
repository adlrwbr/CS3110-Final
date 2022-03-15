(** represents the world model in MVC pattern *)

type wt
(** the abstract world type *)

type lt
(** the abstract location type *)

type rt
(** the abstract road type *)

<<<<<<< HEAD
(** [add world name category road pos] is the tuple ([loc], [newworld]) where
    [newworld] is [world] w/ an additional location [loc] *)
val add : wt -> string -> string -> rt -> float -> lt * wt
=======
val add : wt -> string -> string -> rt -> int -> lt * wt
(** [add world name category road pos] is the tuple ([loc], [newworld])
    where [newworld] is [world] w/ an additional location [loc] *)
>>>>>>> 4e7a620a7bb518ede733aeb9db4baab4f11e8361

val name : wt -> lt -> string
(** [name world loc] is the name of the location [loc] *)

val category : wt -> lt -> string
(** [category world loc] is the category of the location [loc] *)

val locations : wt -> lt list
(** [locations world] is a list of all locations contained in [wt] *)

val loc_coord : lt -> float * float
(** [coord loc] is the world-space (x, y) coordinate of location [loc] *)

val roads : wt -> rt list
(** [roads world] is the list of all roads in the [world] *)

val road_coords : rt -> (float * float) list
(** [road_coords road] is a sequence of coordinates that define a road *)

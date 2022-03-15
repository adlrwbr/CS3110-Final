(** represents the world model in MVC pattern *)

type wt
(** the abstract world type *)

type lt
(** the abstract location type *)

type rt
(** the abstract road type *)

val empty : string -> wt
(** [empty name] is an empty world named [name] *)

val add : string -> string -> rt -> float -> wt -> lt * wt
(** [add name category road pos world] is the tuple ([loc], [newworld])
    where [newworld] is [world] w/ an additional location [loc] *)

val locations : wt -> lt list
(** [locations world] is a list of all locations contained in [world] *)

val name : lt -> string
(** [name loc] is the name of the location [loc] *)

val category : lt -> string
(** [category loc] is the category of the location [loc] *)

val loc_coord : lt -> float * float
(** [coord loc] is the world-space (x, y) coordinate of location [loc] *)

val roads : wt -> rt list
(** [roads world] is the list of all roads in the [world] *)

val road_coords : rt -> (float * float) list
(** [road_coords road] is a sequence of coordinates that define a road *)

(** represents the world model in MVC pattern *)

(** the abstract world type *)
type wt

(** the abstract location type *)
type lt

(** the abstract road type *)
type rt

(** [add world name category road pos] is the tuple ([loc], [newworld]) where
    [newworld] is [world] w/ an additional location [loc] *)
val add : wt -> string -> string -> rt -> lt * wt

(** [name world loc] is the name of the location [loc] *)
val name : wt -> lt -> string

(** [category world loc] is the category of the location [loc] *)
val category : wt -> lt -> string

(** [locations world] is a list of all locations contained in [wt] *)
val locations : wt -> lt list

(** [coord loc] is the world-space (x, y) coordinate of location [loc] *)
val loc_coord : lt -> float * float

(** [roads world] is the list of all roads in the [world] *)
val roads : wt -> rt list

(** [road_coords road] is a sequence of coordinates that define a road *)
val road_coords : rt -> (float * float) list

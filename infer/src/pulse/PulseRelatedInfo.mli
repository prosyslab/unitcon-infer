open! IStd

val yojson_of_t : Var.t list -> Yojson.Safe.t

val search_err_proc : Procdesc.t -> Location.t -> Var.t list

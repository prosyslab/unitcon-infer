open! IStd

module Use : sig
  (** Var is subtype of Exp **)
  type t =
    | Use of Var.t * Location.t
        (** if Var is program variable and formal parameter, add also Use elem **)
  [@@deriving compare]

  val make_use : Var.t -> Location.t -> t

  val pp : Format.formatter -> t -> unit
end

module Set : module type of AbstractDomain.FiniteSet (Use)

val yojson_of_t : Set.t -> Yojson.Safe.t

val search_err_proc : Procdesc.t -> Location.t -> Set.t

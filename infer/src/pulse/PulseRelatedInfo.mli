open! IStd

module DefUse : sig
  type variable = Var.t * Location.t [@@deriving compare, equal]

  type t =
    | Use of variable
    | Def of variable
    | Connect of (variable * variable)
    | Related of Var.t
    | Param of Var.t
  [@@deriving compare, equal]

  val pp : Format.formatter -> t -> unit
end

module Set : module type of AbstractDomain.FiniteSet (DefUse)

val yojson_of_t : Set.t -> Yojson.Safe.t

val search_err_proc : Procdesc.t -> Location.t -> Set.t

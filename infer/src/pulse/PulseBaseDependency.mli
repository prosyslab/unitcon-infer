open! IStd
module F = Format
open PulseBasicInterface

module Symbol : sig
  type t = AbsSym of AbstractValue.t | VarSym of Var.t [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit
end

module Set : sig
  include module type of PrettyPrintable.MakePPSet (Symbol) with type elt = Symbol.t

  val add : elt -> t -> t

  val union : t -> t -> t

  val yojson_of_t : t -> Yojson.Safe.t

  val pp : F.formatter -> t -> unit

  val subst_var : AbstractValue.t * AbstractValue.t -> t -> t
  end

include PrettyPrintable.MonoMap with type key = Symbol.t and type value = Set.t

val yojson_of_t : t -> Yojson.Safe.t

val subst_var : AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t

val compare : t -> t -> int

val equal : t -> t -> bool

val of_abstract_value : AbstractValue.t -> Symbol.t

val of_var : Var.t -> Symbol.t

val pp : F.formatter -> t -> unit

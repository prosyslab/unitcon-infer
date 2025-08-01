open! IStd

module G : sig
  module RelatedVar : sig
    include module type of Var

    val hash : 'a -> int
  end

  include module type of Graph.Persistent.Digraph.ConcreteBidirectional (RelatedVar)
end

module Set : module type of AbstractDomain.FiniteSet (Var)

type t =
  { formal_params: Var.t list
  ; curr_loc: Location.t
  ; graph: G.t
  ; used_at_curr: Set.t
  ; related_args: Var.t list
  ; used_args: Var.t list }

val empty : t

val compare : t -> t -> int

val equal : t -> t -> bool

val yojson_of_t : t -> [> `Assoc of (string * Yojson.Safe.t) list]

val pp_summary : Format.formatter -> t -> (string * Yojson.Safe.t) list

val get_formal_params : t -> Var.t list

val get_used_at_curr : Location.t -> t -> Set.t

val find_relation : Var.t list -> Set.t -> G.t -> Var.t list

val find_usage : Var.t list -> G.t -> Var.t list

val get_usage_relation : Sil.instr -> t -> Set.t * G.t

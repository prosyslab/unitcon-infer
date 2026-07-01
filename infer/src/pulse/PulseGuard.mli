open! IStd
open PulseBasicInterface
module BaseDependency = PulseBaseDependency

type source_line = int [@@deriving compare, equal, yojson_of]

type operand_origin = SourceVar of Var.t | TempIdent of Ident.t | MemoryOf of Var.t list | Unknown
[@@deriving compare, equal, yojson_of]

type used_entity =
  {origin: operand_origin; abstract_value: AbstractValue.t option; dependency: BaseDependency.value}
[@@deriving compare, equal]

type kind =
  | ExplicitPrune of
      { if_kind: (Sil.if_kind[@yojson.opaque])
      ; is_then_branch: bool
      ; condition: (Exp.t[@compare.ignore] [@equal.ignore] [@yojson.opaque]) }
  | NullCheck of {is_non_null: bool}
  | InitializedCheck
  | ModelGuard of {desc: string}
[@@deriving compare, equal, yojson_of]

type t = {id: int; kind: kind; line: source_line; entities: used_entity list; timestamp: string}
[@@deriving compare, equal]

type trace = t list [@@deriving compare, equal]

val empty_trace : trace

val add : t -> trace -> trace

val source_line_of_location : Location.t -> source_line

val yojson_of_used_entity : used_entity -> Yojson.Safe.t

val yojson_of_t : t -> Yojson.Safe.t

val yojson_of_trace : t list -> Yojson.Safe.t

val pp : Format.formatter -> t -> unit

val pp_trace : Format.formatter -> trace -> unit

val vars_of_exp : Exp.t -> Var.t list

val mk_memory_entity : Var.t list -> AbstractValue.t option -> used_entity

val mk_unknown_entity : AbstractValue.t option -> used_entity

val entities_of_exp : Exp.t -> used_entity list

val set_dependency : BaseDependency.value -> used_entity -> used_entity

val mk_explicit_prune :
     location:Location.t
  -> timestamp:Timestamp.t
  -> entities:used_entity list
  -> if_kind:Sil.if_kind
  -> is_then_branch:bool
  -> condition:Exp.t
  -> t

val mk_null_check :
     location:Location.t
  -> timestamp:Timestamp.t
  -> entities:used_entity list
  -> abstract_value:AbstractValue.t option
  -> t

val mk_initialized_check :
     location:Location.t
  -> timestamp:Timestamp.t
  -> entities:used_entity list
  -> abstract_value:AbstractValue.t option
  -> t

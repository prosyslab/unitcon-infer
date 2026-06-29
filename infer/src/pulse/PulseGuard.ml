open! IStd
open PulseBasicInterface

type source_line = int [@@deriving compare, equal, yojson_of]

type operand_origin = SourceVar of Var.t | TempIdent of Ident.t | MemoryOf of Var.t list | Unknown
[@@deriving compare, equal, yojson_of]

type used_entity =
  { origin: operand_origin
  ; exp: (Exp.t option[@compare.ignore] [@equal.ignore] [@yojson.opaque])
  ; abstract_value: AbstractValue.t option
  ; line: source_line }
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

let empty_trace = []

let add guard trace = guard :: trace

let source_line_of_location (location : Location.t) = location.line

let timestamp_to_string timestamp = Format.asprintf "%a" Timestamp.pp timestamp

let yojson_of_used_entity {abstract_value; line; _} =
  `Assoc
    [("abstract_value", [%yojson_of: AbstractValue.t option] abstract_value); ("line", `Int line)]


let yojson_of_t {line; entities; _} =
  `Assoc [("line", `Int line); ("entities", `List (List.map entities ~f:yojson_of_used_entity))]


let yojson_of_trace trace = `List (List.map trace ~f:yojson_of_t)

let pp_origin fmt = function
  | SourceVar var ->
      Format.fprintf fmt "SourceVar(%a)" Var.pp var
  | TempIdent id ->
      Format.fprintf fmt "TempIdent(%a)" Ident.pp id
  | MemoryOf vars ->
      Format.fprintf fmt "MemoryOf(%a)" (Pp.seq ~sep:"," Var.pp) vars
  | Unknown ->
      Format.pp_print_string fmt "Unknown"


let pp_entity fmt {origin; abstract_value; line} =
  match abstract_value with
  | Some value ->
      Format.fprintf fmt "{origin=%a; value=%a; line=%d}" pp_origin origin AbstractValue.pp value
        line
  | None ->
      Format.fprintf fmt "{origin=%a; line=%d}" pp_origin origin line


let pp_kind fmt = function
  | ExplicitPrune {is_then_branch} ->
      Format.fprintf fmt "ExplicitPrune(%b)" is_then_branch
  | NullCheck {is_non_null} ->
      Format.fprintf fmt "NullCheck(%b)" is_non_null
  | InitializedCheck ->
      Format.pp_print_string fmt "InitializedCheck"
  | ModelGuard {desc} ->
      Format.fprintf fmt "ModelGuard(%s)" desc


let pp fmt guard =
  Format.fprintf fmt "{id=%d; kind=%a; line=%d; entities=[%a]}" guard.id pp_kind guard.kind
    guard.line (Pp.seq ~sep:"; " pp_entity) guard.entities


let pp_trace fmt trace = Pp.seq ~sep:"; " pp fmt trace

let vars_of_exp exp =
  Var.get_all_vars_in_exp exp |> Sequence.to_list |> List.dedup_and_sort ~compare:Var.compare


let mk_entity ~origin ~location ?exp ?abstract_value () =
  {origin; exp; abstract_value; line= source_line_of_location location}


let mk_memory_entity ~location vars =
  mk_entity ~origin:(MemoryOf vars) ~location ?exp:None ?abstract_value:None ()


let mk_unknown_entity ~location exp = mk_entity ~origin:Unknown ~location ?exp:(Some exp) ()

let rec collect_entities ~location exp =
  match (exp : Exp.t) with
  | Var id ->
      [mk_entity ~origin:(TempIdent id) ~location ?exp:(Some exp) ()]
  | Lvar pvar ->
      [mk_entity ~origin:(SourceVar (Var.of_pvar pvar)) ~location ?exp:(Some exp) ()]
  | Lfield (base_exp, _, _) ->
      let base_vars = vars_of_exp base_exp in
      let entity =
        if List.is_empty base_vars then mk_unknown_entity ~location exp
        else mk_memory_entity ~location base_vars
      in
      entity :: collect_entities ~location base_exp
  | Lindex (base_exp, index_exp) ->
      let base_vars = vars_of_exp base_exp in
      let base_entities =
        if List.is_empty base_vars then [] else [mk_memory_entity ~location base_vars]
      in
      List.rev_append base_entities
        (List.rev_append
           (collect_entities ~location base_exp)
           (collect_entities ~location index_exp) )
  | BinOp (_, lhs, rhs) ->
      List.rev_append (collect_entities ~location lhs) (collect_entities ~location rhs)
  | UnOp (_, exp, _) | Cast (_, exp) | Exn exp ->
      collect_entities ~location exp
  | Closure _ | Const _ | Sizeof _ ->
      []


let entities_of_exp ~location exp =
  let entities = collect_entities ~location exp in
  if List.is_empty entities then [mk_unknown_entity ~location exp]
  else List.dedup_and_sort ~compare:compare_used_entity entities


let mk_id ~location ~timestamp tag =
  Hashtbl.hash
    ( SourceFile.to_rel_path location.Location.file
    , location.Location.line
    , location.Location.col
    , Format.asprintf "%a" Timestamp.pp timestamp
    , tag )


let mk_explicit_prune ~location ~timestamp ~if_kind ~is_then_branch ~condition =
  let kind = ExplicitPrune {if_kind; is_then_branch; condition} in
  { id= mk_id ~location ~timestamp "explicit_prune"
  ; kind
  ; line= source_line_of_location location
  ; entities= entities_of_exp ~location condition
  ; timestamp= timestamp_to_string timestamp }


let with_fallback_entity ~location ~abstract_value entities =
  if List.is_empty entities then
    [mk_entity ~origin:Unknown ~location ?exp:None ?abstract_value:(Some abstract_value) ()]
  else
    List.map entities ~f:(fun entity ->
        if Option.is_some entity.abstract_value then entity
        else {entity with abstract_value= Some abstract_value} )


let mk_null_check ~location ~timestamp ~entities ~abstract_value =
  { id= mk_id ~location ~timestamp "null_check"
  ; kind= NullCheck {is_non_null= true}
  ; line= source_line_of_location location
  ; entities= with_fallback_entity ~location ~abstract_value entities
  ; timestamp= timestamp_to_string timestamp }


let mk_initialized_check ~location ~timestamp ~entities ~abstract_value =
  { id= mk_id ~location ~timestamp "initialized_check"
  ; kind= InitializedCheck
  ; line= source_line_of_location location
  ; entities= with_fallback_entity ~location ~abstract_value entities
  ; timestamp= timestamp_to_string timestamp }

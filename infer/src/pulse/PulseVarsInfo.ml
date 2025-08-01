open! IStd
module F = Format

module G = struct
  module RelatedVar = struct
    include Var

    let hash = Caml.Hashtbl.hash
  end

  include Graph.Persistent.Digraph.ConcreteBidirectional (RelatedVar)
end

module GraphPath = Graph.Path.Check (G)

module Set = struct
  include AbstractDomain.FiniteSet (Var)
end

type t =
  { formal_params: Var.t list
  ; curr_loc: Location.t
  ; graph: G.t
  ; used_at_curr: Set.t
  ; related_args: Var.t list
  ; used_args: Var.t list }

let empty =
  { formal_params= []
  ; curr_loc= Location.dummy
  ; graph= G.empty
  ; used_at_curr= Set.empty
  ; related_args= []
  ; used_args= [] }


let compare info1 info2 =
  if phys_equal info1 info2 then 0
  else
    [%compare: Var.t list * Location.t * Var.t list * Var.t list]
      (info1.formal_params, info1.curr_loc, info1.related_args, info1.used_args)
      (info2.formal_params, info2.curr_loc, info2.related_args, info2.used_args)


let equal = [%compare.equal: t]

let yojson_of_args t =
  `List (List.fold ~init:[] ~f:(fun acc v -> `String (F.asprintf "%a" Var.pp v) :: acc) t)


let yojson_of_t {related_args; used_args} =
  let used_args = yojson_of_args used_args in
  let related_args = yojson_of_args related_args in
  `Assoc [("UsedArg", used_args); ("RelatedArg", related_args)]


let pp_summary _ {related_args; used_args} =
  let used_args = yojson_of_args used_args in
  let related_args = yojson_of_args related_args in
  [("UsedArg", used_args); ("RelatedArg", related_args)]


let get_formal_params vars_info = vars_info.formal_params

let get_used_at_curr loc vars_info =
  if Location.equal loc vars_info.curr_loc then vars_info.used_at_curr else Set.empty


let find_relation params t g =
  let pc = GraphPath.create g in
  Set.fold
    (fun dv acc ->
      List.fold_left ~init:acc
        ~f:(fun acc uv ->
          let exist_path = try GraphPath.check_path pc uv dv with _ -> false in
          if exist_path && not (List.mem ~equal:Var.equal acc uv) then uv :: acc else acc )
        params )
    t []


let find_usage params g =
  List.fold_left ~init:[]
    ~f:(fun acc param -> if G.mem_vertex g param then param :: acc else acc)
    params


(* use ->* define
 * When it defines variables used at the  error location,
   it checks whether formal parameters are used. *)
let add_relation dv uv g = G.add_edge g uv dv

let add_relation_all dv exp g =
  let vs = Var.get_all_vars_in_exp exp in
  Sequence.fold ~init:g ~f:(fun g v -> add_relation dv v g) vs


let add_used_at_curr exp t =
  let vs = Var.get_all_vars_in_exp exp in
  Sequence.fold ~init:t ~f:(fun t v -> Set.add v t) vs


let get_usage_relation instr {curr_loc; graph; used_at_curr; _} =
  let t = used_at_curr in
  match instr with
  | Sil.Load {id: Ident.t; e: Exp.t; loc: Location.t} ->
      let dv = Var.of_id id in
      let t = if Location.equal loc curr_loc then add_used_at_curr e t else t in
      (t, add_relation_all dv e graph)
  | Sil.Store {e1: Exp.t; e2: Exp.t; loc: Location.t} ->
      let t = if Location.equal loc curr_loc then add_used_at_curr e2 t else t in
      let g =
        Var.get_all_vars_in_exp e1
        |> Sequence.fold ~init:graph ~f:(fun g dv ->
               Var.get_all_vars_in_exp e2
               |> Sequence.fold ~init:g ~f:(fun g uv -> add_relation dv uv g) )
      in
      (t, g)
  | Sil.Call ((id, _), _, arg_ts, loc, _) ->
      let dv = Var.of_id id in
      let t =
        if Location.equal loc curr_loc then
          List.fold_left ~init:t ~f:(fun t (exp, _) -> add_used_at_curr exp t) arg_ts
        else t
      in
      (t, List.fold ~init:graph ~f:(fun g (exp, _) -> add_relation_all dv exp g) arg_ts)
  | _ ->
      (t, graph)

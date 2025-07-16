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

module NodeSet = struct
  include AbstractDomain.FiniteSet (Procdesc.Node)
end

module Set = struct
  include AbstractDomain.FiniteSet (Var)
end

let yojson_of_t t =
  `List (List.fold ~init:[] ~f:(fun acc v -> `String (F.asprintf "%a" Var.pp v) :: acc) t)


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


let rec find_equal_node loc nodes =
  match nodes with
  | hd :: _ when Location.equal (Procdesc.Node.get_loc hd) loc ->
      Some hd
  | _ :: tl ->
      find_equal_node loc tl
  | [] ->
      None


let add_used_at_error exp t =
  let vs = Var.get_all_vars_in_exp exp in
  Sequence.fold ~init:t
    ~f:(fun t v -> match v with Var.ProgramVar _ -> Set.add v t | Var.LogicalVar _ -> t)
    vs


let add_relation dv uv g = G.add_edge g uv dv

let add_relation_all dv exp g =
  let vs = Var.get_all_vars_in_exp exp in
  Sequence.fold ~init:g ~f:(fun g v -> add_relation dv v g) vs


let search_instr err_loc t instr g =
  match instr with
  | Sil.Load {id: Ident.t; e: Exp.t; loc: Location.t} ->
      let dv = Var.of_id id in
      let t = if Location.equal loc err_loc then add_used_at_error e t else t in
      (t, add_relation_all dv e g)
  | Sil.Store {e1: Exp.t; e2: Exp.t; loc: Location.t} ->
      let t = if Location.equal loc err_loc then add_used_at_error e2 t else t in
      let g =
        Var.get_all_vars_in_exp e1
        |> Sequence.fold ~init:g ~f:(fun g dv ->
               Var.get_all_vars_in_exp e2
               |> Sequence.fold ~init:g ~f:(fun g uv -> add_relation dv uv g) )
      in
      (t, g)
  | Sil.Call ((id, _), _, arg_ts, loc, _) ->
      let dv = Var.of_id id in
      let t =
        if Location.equal loc err_loc then
          List.fold_left ~init:t ~f:(fun t (exp, _) -> add_used_at_error exp t) arg_ts
        else t
      in
      (t, List.fold ~init:g ~f:(fun g (exp, _) -> add_relation_all dv exp g) arg_ts)
  | _ ->
      (t, g)


let search_one_node err_loc t node g =
  let instrs = Procdesc.Node.get_instrs node in
  Instrs.fold ~init:(t, g) ~f:(fun (t, g) instr -> search_instr err_loc t instr g) instrs


let rec get_nodes_from_exn_node node t =
  if
    Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind node) Procdesc.Node.Start_node
    || NodeSet.mem node t
  then t
  else
    let pred_nodes = Procdesc.Node.get_preds node in
    List.fold_left ~init:(NodeSet.add node t)
      ~f:(fun t pred -> get_nodes_from_exn_node pred t)
      pred_nodes


let search_err_proc proc_desc location =
  let formals =
    Procdesc.get_pvar_formals proc_desc
    |> List.fold_left ~init:[] ~f:(fun acc (pvar, _) -> Var.of_pvar pvar :: acc)
  in
  let nodes = Procdesc.get_nodes proc_desc in
  let exn_node = find_equal_node location nodes in
  match exn_node with
  | Some node ->
      let all_nodes = get_nodes_from_exn_node node NodeSet.empty in
      let t, g =
        NodeSet.fold
          (fun node (t, g) -> search_one_node location t node g)
          all_nodes (Set.empty, G.empty)
      in
      find_relation formals t g
  | None ->
      []

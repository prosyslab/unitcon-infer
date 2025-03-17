open! IStd
module F = Format

module Use = struct
  (** Var is subtype of Exp **)
  type t =
    | Use of Var.t * Location.t
        (** if Var is program variable and formal parameter, add Use elem **)
  [@@deriving compare]

  let make_use v loc = Use (v, loc)

  let yojson_of_t = function Use (v, _) -> `String (F.asprintf "%a" Var.pp v)

  let pp fmt = function Use (v, l) -> F.fprintf fmt "Use %a (%a)" Var.pp v Location.pp l
end

module NodeSet = struct
  include AbstractDomain.FiniteSet (Procdesc.Node)
end

module Set = struct
  include AbstractDomain.FiniteSet (Use)

  let yojson_of_t t =
    `List
      (fold
         (fun elem acc ->
           let elem = Use.yojson_of_t elem in
           if List.mem ~equal:Yojson.Safe.equal acc elem then acc else elem :: acc )
         t [] )


  let pp fmt t =
    F.fprintf fmt "[" ;
    iter (fun v -> F.fprintf fmt "%a, " Use.pp v) t ;
    F.fprintf fmt "]"
end

let yojson_of_t = Set.yojson_of_t

let rec find_equal_node loc nodes =
  match nodes with
  | hd :: _ when Location.equal (Procdesc.Node.get_loc hd) loc ->
      Some hd
  | _ :: tl ->
      find_equal_node loc tl
  | [] ->
      None


let search_instr pvar_formals instr t =
  let add_use_var v loc t =
    match v with
    | Var.ProgramVar pv ->
        if List.mem ~equal:Pvar.equal pvar_formals pv then Set.add (Use.make_use v loc) t else t
    | Var.LogicalVar _ ->
        t
  in
  let add_use_var_from_exp e loc t =
    let vs = Var.get_all_vars_in_exp e in
    Sequence.fold ~init:t ~f:(fun t v -> add_use_var v loc t) vs
  in
  match instr with Sil.Load {e: Exp.t; loc: Location.t} -> add_use_var_from_exp e loc t | _ -> t


let search_one_node pvar_formals node t =
  let instrs = Procdesc.Node.get_instrs node in
  Instrs.fold ~init:t ~f:(fun t instr -> search_instr pvar_formals instr t) instrs


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
  let pvar_formals =
    Procdesc.get_pvar_formals proc_desc
    |> List.fold_left ~init:[] ~f:(fun acc (pvar, _) -> pvar :: acc)
  in
  let nodes = Procdesc.get_nodes proc_desc in
  let exn_node = find_equal_node location nodes in
  match exn_node with
  | Some node ->
      let all_nodes = get_nodes_from_exn_node node NodeSet.empty in
      NodeSet.fold (fun node t -> search_one_node pvar_formals node t) all_nodes Set.empty
  | None ->
      Set.empty

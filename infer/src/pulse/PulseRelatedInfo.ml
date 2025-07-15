open! IStd
module F = Format

module DefUse = struct
  type variable = Var.t * Location.t [@@deriving compare, equal]

  type t =
    | Use of variable
    | Def of variable
    | Connect of (variable * variable)
    | Related of Var.t
    | Param of Var.t
  [@@deriving compare, equal]

  let make_use v loc = Use (v, loc)

  let make_def v loc = Def (v, loc)

  (* x := y --> Connect (y, x) *)
  let make_connect use def = Connect (use, def)

  let make_related v = Related v

  let make_param v = Param v

  let yojson_of_t = function
    | Use _ | Def _ | Connect _ | Param _ ->
        `Null
    | Related v ->
        `String (F.asprintf "%a" Var.pp v)


  let pp fmt = function
    | Use (v, l) ->
        F.fprintf fmt "Use %a (%a)" Var.pp v Location.pp l
    | Def (v, l) ->
        F.fprintf fmt "Def %a (%a)" Var.pp v Location.pp l
    | Connect ((v1, l1), (v2, l2)) ->
        F.fprintf fmt "Connect %a (%a) -> %a (%a)" Var.pp v1 Location.pp l1 Var.pp v2 Location.pp l2
    | Related v ->
        F.fprintf fmt "Related %a" Var.pp v
    | Param v ->
        F.fprintf fmt "Param %a" Var.pp v
end

module NodeSet = struct
  include AbstractDomain.FiniteSet (Procdesc.Node)
end

module Set = struct
  include AbstractDomain.FiniteSet (DefUse)

  let yojson_of_t t =
    `List
      (fold
         (fun elem acc ->
           let elem = DefUse.yojson_of_t elem in
           if Yojson.Safe.equal elem `Null || List.mem ~equal:Yojson.Safe.equal acc elem then acc
           else elem :: acc )
         t [] )


  let pp fmt t =
    F.fprintf fmt "[" ;
    iter (fun v -> F.fprintf fmt "%a, " DefUse.pp v) t ;
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


let add_connect_relation t =
  let add_connect_var (def : DefUse.t) (use : DefUse.t) t =
    match (def, use) with
    | Def (dv, dl), Use (uv, ul) when Location.equal dl ul ->
        Set.add (DefUse.make_connect (uv, ul) (dv, dl)) t
    | Connect (u1, (dv, _)), Connect ((uv, _), d2) when Var.equal dv uv ->
        Set.add (DefUse.make_connect u1 d2) t
    | _, _ ->
        t
  in
  Set.fold (fun e1 acc -> Set.fold (fun e2 set -> add_connect_var e1 e2 set) t acc) t t


let rec fix_point_connect t =
  let new_relation = add_connect_relation t in
  if Set.equal new_relation t then new_relation else fix_point_connect new_relation


let add_related_relation err_loc t =
  let err_used_var =
    Set.fold
      (fun e acc ->
        match e with Use (v, l) when Location.equal err_loc l -> (v, l) :: acc | _ -> acc )
      t []
  in
  Set.fold
    (fun e acc ->
      match e with
      | Connect ((uv, _), d)
        when List.mem ~equal:DefUse.equal_variable err_used_var d
             && Set.mem (DefUse.make_param uv) t ->
          Set.add (DefUse.make_related uv) acc
      | _ ->
          acc )
    t t


let search_instr instr t =
  let add_use_var v loc t = Set.add (DefUse.make_use v loc) t in
  let add_def_var v loc t = Set.add (DefUse.make_def v loc) t in
  let add_use_var_from_exp e loc t =
    let vs = Var.get_all_vars_in_exp e in
    Sequence.fold ~init:t ~f:(fun t v -> add_use_var v loc t) vs
  in
  match instr with
  | Sil.Load {e: Exp.t; loc: Location.t} ->
      add_use_var_from_exp e loc t
  | Sil.Store {e1: Exp.t; e2: Exp.t; loc: Location.t} ->
      let e1_vs = Var.get_all_vars_in_exp e1 in
      let e1_t = Sequence.fold ~init:t ~f:(fun t v -> add_def_var v loc t) e1_vs in
      let e2_vs = Var.get_all_vars_in_exp e2 in
      Sequence.fold ~init:e1_t ~f:(fun t v -> add_use_var v loc t) e2_vs
  | Sil.Call (_, _, arg_ts, loc, _) ->
      List.fold_left ~init:t
        ~f:(fun t (exp, _) ->
          let arg_vs = Var.get_all_vars_in_exp exp in
          Sequence.fold ~init:t ~f:(fun t v -> add_use_var v loc t) arg_vs )
        arg_ts
  | _ ->
      t


let search_one_node node t =
  let instrs = Procdesc.Node.get_instrs node in
  Instrs.fold ~init:t ~f:(fun t instr -> search_instr instr t) instrs


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
  let added_formals =
    Procdesc.get_pvar_formals proc_desc
    |> List.fold_left ~init:Set.empty ~f:(fun acc (pvar, _) ->
           Set.add (DefUse.make_param (Var.of_pvar pvar)) acc )
  in
  let nodes = Procdesc.get_nodes proc_desc in
  let exn_node = find_equal_node location nodes in
  match exn_node with
  | Some node ->
      let all_nodes = get_nodes_from_exn_node node NodeSet.empty in
      NodeSet.fold (fun node t -> search_one_node node t) all_nodes added_formals
      |> fix_point_connect |> add_related_relation location
  | None ->
      Set.empty

open! IStd
module F = Format
open PulseBasicInterface

module Symbol = struct
  type t = AbsSym of AbstractValue.t | VarSym of Var.t [@@deriving compare, equal]

  let pp fmt s =
    match s with
    | AbsSym s ->
        F.fprintf fmt " %a " AbstractValue.pp s
    | VarSym s ->
        F.fprintf fmt " %a " Var.pp s


  let yojson_of_t s = `String (F.asprintf " %a " pp s)
end

module Set = struct
  include PrettyPrintable.MakePPSet (Symbol) [@@deriving compare, equal]

  let yojson_of_t s = `List (fold (fun x acc -> Symbol.yojson_of_t x :: acc) s [])

  let pp fmt set = iter (fun s -> Symbol.pp fmt s) set

  let subst_var (v, v') s =
    fold
      (fun sym acc ->
        match sym with
        | AbsSym sym when AbstractValue.equal sym v ->
            add (Symbol.AbsSym v') acc
        | _ ->
            add sym acc )
      s empty
end

module M = PrettyPrintable.MakePPMonoMap (Symbol) (Set)
include M

let yojson_of_t m = [%yojson_of: (Symbol.t * Set.t) list] (M.bindings m)

let subst_var (v, v') dependency =
  let v_sym = Symbol.AbsSym v in
  let v'_sym = Symbol.AbsSym v' in
  let dependency =
    let v_appears_in_set =
      M.exists (fun _ dep -> Set.exists (fun dest -> Symbol.equal v_sym dest) dep) dependency
    in
    if v_appears_in_set then M.map (Set.subst_var (v, v')) dependency else dependency
  in
  match M.find_opt v_sym dependency with
  | None ->
      Sat dependency
  | Some use -> (
      let dependency = M.remove v_sym dependency in
      match M.find_opt v'_sym dependency with
      | None ->
          Sat (M.add v'_sym use dependency)
      | Some use' ->
          if Set.is_empty use then Sat dependency
          else if Set.is_empty use' then Sat (M.add v'_sym use dependency)
          else Unsat )


let compare = M.compare Set.compare

let equal = M.equal Set.equal

let of_abstract_value v = Symbol.AbsSym v

let of_var v = Symbol.VarSym v

let pp fmt m =
  let pp_item fmt (v, v_set) = F.fprintf fmt "%a -> (%a)" Symbol.pp v Set.pp v_set in
  PrettyPrintable.pp_collection ~pp_item fmt (M.bindings m)

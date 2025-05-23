(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

module X = Llair_to_Fol
open Fol
open Symbolic_heap

type t = Xsh.t [@@deriving compare, equal, sexp]

module Set = Xsh.Set

let pp fs q = Format.fprintf fs "@[{ %a@ }@]" Xsh.pp q

(* set by cli *)
let simplify_states = ref true
let simplify q = if !simplify_states then Xsh.simplify q else q

let init globals =
  IArray.fold globals Xsh.emp ~f:(fun global q ->
      match (global : Llair.GlobalDefn.t) with
      | {name; init= Some seq} ->
          let loc = X.global name in
          let siz =
            match Llair.Global.typ name with
            | Pointer {elt} -> Llair.Typ.size_of elt
            | _ -> violates Llair.GlobalDefn.invariant global
          in
          let len = Term.integer (Z.of_int siz) in
          let cnt = X.term ThreadID.init seq in
          Xsh.star q (Xsh.seg {loc; bas= loc; len; siz= len; cnt})
      | _ -> q )

let join p q =
  [%Dbg.call fun {pf} -> pf "@ %a@ %a" pp p pp q]
  ;
  (if p == q then p else Xsh.or_ p q |> simplify)
  |>
  [%Dbg.retn fun {pf} -> pf "%a" pp]

let joinN qs =
  [%Dbg.call fun {pf} -> pf "@ %a" Xsh.Set.pp qs]
  ;
  ( match Xsh.Set.classify qs with
  | Zero -> Xsh.orN qs
  | One q -> q
  | Many -> Xsh.orN qs |> simplify )
  |>
  [%Dbg.retn fun {pf} -> pf "%a" pp]

let dnf = Xsh.dnf

let resolve_int tid q e =
  match Term.get_z (Context.normalize (Xsh.ctx q) (X.term tid e)) with
  | Some z -> [Z.to_int z]
  | None -> []

let exec_assume tid q b =
  Exec.assume q (X.formula tid b)
  |> simplify
  |> fun q -> if Xsh.is_unsat_dnf q then None else Some q

let exec_kill tid r q = Exec.kill q (X.reg tid r) |> simplify

let exec_move tid res q =
  Exec.move q (IArray.map res ~f:(fun (r, e) -> (X.reg tid r, X.term tid e)))
  |> simplify

let exec_inst tid inst pre =
  let alarm kind =
    Alarm.v kind (Llair.Inst.loc inst) Llair.Inst.pp inst pp pre
  in
  let or_alarm = function
    | Some post -> Ok post
    | None -> Error (alarm Invalid_memory_access)
  in
  ( match (inst : Llair.inst) with
  | Move {reg_exps; _} ->
      Ok
        (Exec.move pre
           (IArray.map reg_exps ~f:(fun (r, e) ->
                (X.reg tid r, X.term tid e) ) ) )
  | Load {reg; ptr; len; _} ->
      Exec.load pre ~reg:(X.reg tid reg) ~ptr:(X.term tid ptr)
        ~len:(X.term tid len)
      |> or_alarm
  | Store {ptr; exp; len; _} ->
      Exec.store pre ~ptr:(X.term tid ptr) ~exp:(X.term tid exp)
        ~len:(X.term tid len)
      |> or_alarm
  | AtomicRMW {reg; ptr; exp; len; _} ->
      Exec.atomic_rmw pre ~reg:(X.reg tid reg) ~ptr:(X.term tid ptr)
        ~exp:(X.term tid exp) ~len:(X.term tid len)
      |> or_alarm
  | AtomicCmpXchg {reg; ptr; cmp; exp; len; len1; _} ->
      Exec.atomic_cmpxchg pre ~reg:(X.reg tid reg) ~ptr:(X.term tid ptr)
        ~cmp:(X.term tid cmp) ~exp:(X.term tid exp) ~len:(X.term tid len)
        ~len1:(X.term tid len1)
      |> or_alarm
  | Alloc {reg; num; len; _} ->
      Exec.alloc pre ~reg:(X.reg tid reg) ~num:(X.term tid num) ~len
      |> or_alarm
  | Free {ptr; _} -> Exec.free pre ~ptr:(X.term tid ptr) |> or_alarm
  | Nondet {reg; _} -> Ok (Exec.nondet pre (Option.map ~f:(X.reg tid) reg))
  | Builtin {reg; name; args; _} ->
      let areturn = Option.map ~f:(X.reg tid) reg in
      let actuals = IArray.map ~f:(X.term tid) args in
      Exec.builtin pre areturn name actuals |> or_alarm )
  |> Or_alarm.map ~f:simplify

let value_determined_by ctx us a =
  List.exists (Context.class_of ctx a) ~f:(fun b ->
      Term.Set.subset (Term.Set.of_iter (Term.atoms b)) ~of_:us )

let garbage_collect q ~wrt =
  [%Dbg.call fun {pf} -> pf "@ %a" pp q]
  ;
  (* only support DNF for now *)
  assert (Iter.length (Xsh.dnf q) = 1) ;
  let rec all_reachable_vars previous current q =
    if Term.Set.equal previous current then current
    else
      let new_set =
        Iter.fold (Xsh.heap q) current ~f:(fun seg current ->
            if value_determined_by (Xsh.ctx q) current seg.loc then
              List.fold
                (Context.class_of (Xsh.ctx q) seg.cnt)
                current
                ~f:(fun e c ->
                  Term.Set.union c (Term.Set.of_iter (Term.atoms e)) )
            else current )
      in
      all_reachable_vars current new_set q
  in
  let r_vars = all_reachable_vars Term.Set.empty wrt q in
  Xsh.filter_heap q ~f:(fun seg ->
      value_determined_by (Xsh.ctx q) r_vars seg.loc )
  |>
  [%Dbg.retn fun {pf} -> pf "%a" pp]

let and_eqs sub formals actuals q =
  let and_eq formal actual eqs =
    let actual' = Term.rename sub actual in
    Formula.eq (Term.var formal) actual' :: eqs
  in
  Xsh.andN (IArray.fold2_exn ~f:and_eq formals actuals []) q

let localize_entry tid globals actuals formals freturn locals shadow pre
    entry =
  (* Add the formals here to do garbage collection and then get rid of
     them *)
  let formals_set = Var.Set.of_iter (IArray.to_iter formals) in
  let freturn_locals =
    X.regs tid (Llair.Reg.Set.add_option freturn locals)
  in
  let wrt =
    Term.Set.of_iter
      (Iter.append
         (Iter.map ~f:X.global (Llair.Global.Set.to_iter globals))
         (Iter.map ~f:Term.var (IArray.to_iter formals)) )
  in
  let function_summary_pre = garbage_collect entry ~wrt in
  [%Dbg.info "function summary pre %a" pp function_summary_pre] ;
  let foot = Xsh.exists formals_set function_summary_pre in
  let (xs, foot), _ = Xsh.name_exists foot in
  let (xs_pre, pre), vx = Xsh.name_exists pre in
  assert (Var.Set.is_empty xs_pre) ;
  Var.Fresh.gen_ vx (fun vx ->
      let frame = Solver.infer_frame pre xs foot vx in
      let frame =
        try Option.get_exn frame
        with _ ->
          fail "Solver couldn't infer frame of a garbage-collected pre" ()
      in
      let foot = Xsh.qf foot vx in
      let q'' =
        Xsh.extend_voc freturn_locals (and_eqs shadow formals actuals foot)
      in
      (q'', frame) )

type from_call = {areturn: Var.t option; unshadow: Var.Subst.t; frame: Xsh.t}
[@@deriving compare, equal, sexp]

(** Express formula in terms of formals instead of actuals, and enter scope
    of locals: rename formals to fresh vars in formula and actuals, add
    equations between each formal and actual, and quantify fresh vars. *)
let call ~summaries tid ?(child = tid) ~globals ~actuals ~areturn ~formals
    ~freturn ~locals q =
  [%Dbg.call fun {pf} ->
    pf "@ @[<hv>locals: {@[%a@]}@ globals: {@[%a@]}@ q: %a@]"
      Llair.Reg.Set.pp locals Llair.Global.Set.pp globals pp q ;
    assert (
      (* modifs do not appear in actuals (otherwise incomplete) *)
      let fv_actuals =
        actuals
        |> IArray.to_iter
        |> Iter.map ~f:(X.term tid)
        |> Iter.flat_map ~f:Term.vars
      in
      not
        (Option.exists areturn ~f:(fun modif ->
             Iter.exists ~f:(Var.equal (X.reg tid modif)) fv_actuals ) ) )]
  ;
  let actuals = IArray.map ~f:(X.term tid) actuals in
  let areturn = Option.map ~f:(X.reg tid) areturn in
  let formals = IArray.map ~f:(X.reg child) formals in
  let freturn_locals =
    X.regs child (Llair.Reg.Set.add_option freturn locals)
  in
  let modifs = Var.Set.of_option areturn in
  (* quantify modifs, their current values will be overwritten and so should
     not be saved and restored on return *)
  let q = Xsh.exists modifs q in
  (* save current values of shadowed formals and locals with a renaming *)
  let formals_freturn_locals =
    Iter.fold ~f:Var.Set.add (IArray.to_iter formals) freturn_locals
  in
  let q, shadow = Xsh.freshen q ~wrt:formals_freturn_locals in
  let unshadow = Var.Subst.invert shadow in
  assert (Var.Set.disjoint modifs (Var.Subst.domain shadow)) ;
  (* pass arguments by conjoining equations between formals and actuals *)
  let entry = and_eqs shadow formals actuals q in
  (* note: locals and formals are in scope *)
  assert (Var.Context.contains (Xsh.vx entry) formals_freturn_locals) ;
  (* simplify *)
  let entry = simplify entry in
  ( if not summaries then (entry, {areturn; unshadow; frame= Xsh.emp})
    else
      let q, frame =
        localize_entry child globals actuals formals freturn locals shadow q
          entry
      in
      (q, {areturn; unshadow; frame}) )
  |>
  [%Dbg.retn fun {pf} (entry, {unshadow; frame}) ->
    pf "@[<v>unshadow: %a@ frame: %a@ entry: %a@]" Var.Subst.pp unshadow pp
      frame pp entry]

(** Leave scope of locals: existentially quantify locals. *)
let post tid locals _ q =
  [%Dbg.call fun {pf} ->
    pf "@ @[<hv>locals: {@[%a@]}@ q: %a@]" Llair.Reg.Set.pp locals Xsh.pp q]
  ;
  Xsh.exists (X.regs tid locals) q |> simplify
  |>
  [%Dbg.retn fun {pf} -> pf "%a" Xsh.pp]

(** Express in terms of actuals instead of formals: existentially quantify
    formals, and apply inverse of fresh variables for formals renaming to
    restore the shadowed variables. *)
let retn tid formals freturn {areturn; unshadow; frame} q =
  [%Dbg.call fun {pf} ->
    pf "@ @[<v>formals: {@[%a@]}%a%a@ unshadow: %a@ q: %a@ frame: %a@]"
      (IArray.pp ", " Llair.Reg.pp)
      formals
      (Option.pp "@ freturn: %a" Llair.Reg.pp)
      freturn
      (Option.pp "@ areturn: %a" Var.pp)
      areturn Var.Subst.pp unshadow pp q pp frame]
  ;
  let formals =
    Var.Set.of_iter (Iter.map ~f:(X.reg tid) (IArray.to_iter formals))
  in
  let freturn = Option.map ~f:(X.reg tid) freturn in
  let q =
    match areturn with
    | Some areturn -> (
        (* reenter scope of areturn just before exiting scope of formals *)
        let q = Xsh.extend_voc (Var.Set.of_ areturn) q in
        (* pass return value *)
        match freturn with
        | Some freturn ->
            Exec.move q (IArray.of_ (areturn, Term.var freturn))
        | None -> Exec.kill q areturn )
    | None -> q
  in
  (* exit scope of formals, except for areturn, which move/kill handled *)
  let outscoped =
    Var.Set.diff
      (Var.Set.union formals (Var.Set.of_option freturn))
      (Var.Set.of_option areturn)
  in
  let q = Xsh.exists outscoped q in
  (* reinstate shadowed values of locals *)
  let q = Xsh.rename unshadow q in
  (* reconjoin frame *)
  Xsh.star frame q
  (* simplify *)
  |> simplify
  |>
  [%Dbg.retn fun {pf} -> pf "%a" pp]

type term_code = Term.t option [@@deriving compare, sexp_of]

let term tid formals freturn q =
  let* freturn in
  let formals =
    Var.Set.of_iter (Iter.map ~f:(X.reg tid) (IArray.to_iter formals))
  in
  let freturn = X.reg tid freturn in
  let (xs, q), _ = Xsh.name_exists q in
  let outscoped = Var.Set.union formals (Var.Set.of_ freturn) in
  let xs = Var.Set.union xs outscoped in
  let retn_val_cls = Context.class_of (Sh.ctx q) (Term.var freturn) in
  List.find retn_val_cls ~f:(fun retn_val ->
      Var.Set.disjoint xs (Term.fv retn_val) )

let move_term_code tid reg code q =
  match code with
  | Some retn_val -> Exec.move q (IArray.of_ (X.reg tid reg, retn_val))
  | None -> q

let resolve_callee lookup tid ptr q =
  let ptr_var = Var.Fresh.gen_ (Xsh.vx q) (Var.Fresh.var "callee") in
  let q = Xsh.and_ (Formula.eq (X.term tid ptr) (Term.var ptr_var)) q in
  Iter.fold (Xsh.dnf q) [] ~f:(fun disj ->
      Context.class_of (Xsh.ctx disj) (Term.var ptr_var)
      |> List.filter_map ~f:(X.lookup_func lookup)
      |> List.append )

let recursion_beyond_bound = `prune

type summary = {xs: Var.Set.t; foot: t; post: t}

let pp_summary fs {xs; foot; post} =
  Format.fprintf fs "@[<v>xs: @[%a@]@ foot: %a@ post: %a @]" Var.Set.pp xs
    pp foot pp post

let create_summary tid ~locals ~formals ~entry ~current:post =
  [%Dbg.call fun {pf} ->
    pf "@ formals %a@ entry: %a@ current: %a"
      (IArray.pp ",@ " Llair.Reg.pp)
      formals pp entry pp post]
  ;
  let formals =
    Var.Set.of_iter (Iter.map ~f:(X.reg tid) (IArray.to_iter formals))
  in
  let locals = X.regs tid locals in
  let foot = Xsh.exists locals entry in
  let foot, subst =
    Xsh.freshen ~wrt:(Var.Set.union (Xsh.fv foot) (Xsh.fv post)) foot
  in
  let restore_formals q =
    Var.Set.fold formals q ~f:(fun var q ->
        let var = Term.var var in
        let renamed_var = Term.rename subst var in
        Xsh.and_ (Formula.eq renamed_var var) q )
  in
  (* Add back the original formals name *)
  let post = Xsh.rename subst post in
  let foot = restore_formals foot in
  let post = restore_formals post in
  [%Dbg.info "subst: %a" Var.Subst.pp subst] ;
  let xs = Var.Set.inter (Xsh.fv foot) (Xsh.fv post) in
  let xs = Var.Set.diff xs formals in
  let xs_and_formals = Var.Set.union xs formals in
  let foot = Xsh.exists (Var.Set.diff (Xsh.fv foot) xs_and_formals) foot in
  let post = Xsh.exists (Var.Set.diff (Xsh.fv post) xs_and_formals) post in
  let current = Xsh.extend_voc xs post in
  ({xs; foot; post}, current)
  |>
  [%Dbg.retn fun {pf} (fs, _) -> pf "@,%a" pp_summary fs]

let apply_summary q ({xs; foot; post} as fs) =
  [%Dbg.call fun {pf} -> pf "@ fs: %a@ q: %a" pp_summary fs pp q]
  ;
  let xs_in_q = Var.Context.diff xs (Xsh.vx q) in
  let xs_in_fv_q = Var.Set.inter xs (Xsh.fv q) in
  (* Between creation of a summary and its use, the vocabulary of q (q.us)
     might have been extended. That means infer_frame would fail, because q
     and foot have different vocabulary. This might indicate that the
     summary cannot be applied to q, however in the case where
     free-variables of q and foot match it is benign. In the case where free
     variables match, we temporarily reduce the vocabulary of q to match the
     vocabulary of foot. *)
  [%Dbg.info "xs inter q.us: %a" Var.Set.pp xs_in_q] ;
  [%Dbg.info "xs inter fv.q %a" Var.Set.pp xs_in_fv_q] ;
  let q, add_back =
    if Var.Set.is_empty xs_in_fv_q then (Xsh.exists xs_in_q q, xs_in_q)
    else (q, Var.Set.empty)
  in
  let frame =
    if Var.Set.is_empty xs_in_fv_q then (
      let (xs_q, q), _ = Xsh.name_exists q in
      assert (Var.Set.is_empty xs_q) ;
      let (xs_foot, foot), vx = Xsh.name_exists foot in
      assert (Var.Set.is_empty xs_foot) ;
      Var.Fresh.gen_ vx (Solver.infer_frame q xs foot) )
    else None
  in
  [%Dbg.info "frame %a" (Option.pp "%a" pp) frame] ;
  Option.map ~f:(Xsh.extend_voc add_back)
    (Option.map ~f:(Xsh.star post) frame)
  |>
  [%Dbg.retn fun {pf} r ->
    match r with None -> pf "None" | Some q -> pf "@,%a" pp q]

let%test_module _ =
  ( module struct
    let () = Dbg.init ~margin:68 ()
    let pp = Format.printf "@.%a@." Xsh.pp
    let vx = ref Var.Context.empty

    let var name =
      let x_ = Var.Fresh.var name vx in
      (x_, Term.var x_)

    let _, head = var "head"
    let _, a = var "a"
    let _, n = var "n"
    let _, b = var "b"
    let _, tail = var "tail"
    let seg_head = Xsh.seg {loc= head; bas= b; len= n; siz= n; cnt= a}
    let seg_a = Xsh.seg {loc= a; bas= b; len= n; siz= n; cnt= tail}
    let seg_cycle = Xsh.seg {loc= a; bas= b; len= n; siz= n; cnt= head}

    let%expect_test _ =
      pp (garbage_collect seg_head ~wrt:(Term.Set.of_list [])) ;
      [%expect {| emp |}]

    let%expect_test _ =
      pp
        (garbage_collect (Xsh.star seg_a seg_head)
           ~wrt:(Term.Set.of_list [a]) ) ;
      [%expect {| %a_2 -[ %b_4, %n_3 )-> ⟨%n_3,%tail_5⟩ |}]

    let%expect_test _ =
      pp
        (garbage_collect (Xsh.star seg_a seg_head)
           ~wrt:(Term.Set.of_list [head]) ) ;
      [%expect
        {|
          %head_1 -[ %b_4, %n_3 )-> ⟨%n_3,%a_2⟩
        * %a_2 -[ %b_4, %n_3 )-> ⟨%n_3,%tail_5⟩ |}]

    let%expect_test _ =
      pp
        (garbage_collect
           (Xsh.star seg_cycle seg_head)
           ~wrt:(Term.Set.of_list [a]) ) ;
      [%expect
        {|
          %head_1 -[ %b_4, %n_3 )-> ⟨%n_3,%a_2⟩
        * %a_2 -[ %b_4, %n_3 )-> ⟨%n_3,%head_1⟩ |}]
  end )

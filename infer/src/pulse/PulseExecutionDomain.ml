(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module Decompiler = PulseAbductiveDecompiler
module Diagnostic = PulseDiagnostic
module LatentIssue = PulseLatentIssue

(* The type variable is needed to distinguish summaries from plain states.

   Some of the variants have summary-typed states instead of plain states, to ensure we have
   normalized them and don't need to normalize them again. *)
type 'abductive_domain_t base_t =
  | ContinueProgram of 'abductive_domain_t
  | ExceptionRaised of 'abductive_domain_t
  | ExitProgram of AbductiveDomain.summary
  | AbortProgram of AbductiveDomain.summary
  | LatentAbortProgram of {astate: AbductiveDomain.summary; latent_issue: LatentIssue.t}
  | LatentInvalidAccess of
      { astate: AbductiveDomain.summary
      ; address: Decompiler.expr
      ; must_be_valid: (Trace.t * Invalidation.must_be_valid_reason option[@yojson.opaque])
      ; calling_context: ((CallEvent.t * Location.t) list[@yojson.opaque]) }
  | ISLLatentMemoryError of AbductiveDomain.summary
[@@deriving equal, compare, yojson_of]

type t = AbductiveDomain.t base_t

let continue astate = ContinueProgram astate

let leq ~lhs ~rhs =
  phys_equal lhs rhs
  ||
  match (lhs, rhs) with
  | AbortProgram astate1, AbortProgram astate2
  | ExitProgram astate1, ExitProgram astate2
  | ISLLatentMemoryError astate1, ISLLatentMemoryError astate2 ->
      AbductiveDomain.leq ~lhs:(astate1 :> AbductiveDomain.t) ~rhs:(astate2 :> AbductiveDomain.t)
  | ExceptionRaised astate1, ExceptionRaised astate2
  | ContinueProgram astate1, ContinueProgram astate2 ->
      AbductiveDomain.leq ~lhs:astate1 ~rhs:astate2
  | ( LatentAbortProgram {astate= astate1; latent_issue= issue1}
    , LatentAbortProgram {astate= astate2; latent_issue= issue2} ) ->
      LatentIssue.equal issue1 issue2
      && AbductiveDomain.leq ~lhs:(astate1 :> AbductiveDomain.t) ~rhs:(astate2 :> AbductiveDomain.t)
  | ( LatentInvalidAccess {astate= astate1; address= v1; must_be_valid= _}
    , LatentInvalidAccess {astate= astate2; address= v2; must_be_valid= _} ) ->
      Decompiler.equal_expr v1 v2
      && AbductiveDomain.leq ~lhs:(astate1 :> AbductiveDomain.t) ~rhs:(astate2 :> AbductiveDomain.t)
  | _ ->
      false


let pp fmt = function
  | AbortProgram astate ->
      F.fprintf fmt "{AbortProgram %a}" AbductiveDomain.pp (astate :> AbductiveDomain.t)
  | ContinueProgram astate ->
      AbductiveDomain.pp fmt astate
  | ExceptionRaised astate ->
      F.fprintf fmt "{ExceptionRaised %a}" AbductiveDomain.pp astate
  | ExitProgram astate ->
      F.fprintf fmt "{ExitProgram %a}" AbductiveDomain.pp (astate :> AbductiveDomain.t)
  | ISLLatentMemoryError astate ->
      F.fprintf fmt "{ISLLatentMemoryError %a}" AbductiveDomain.pp (astate :> AbductiveDomain.t)
  | LatentAbortProgram {astate; latent_issue} ->
      let diagnostic = LatentIssue.to_diagnostic latent_issue in
      let message = Diagnostic.get_message diagnostic in
      let location = Diagnostic.get_location diagnostic in
      F.fprintf fmt "{LatentAbortProgram(%a: %s) %a}" Location.pp location message
        AbductiveDomain.pp
        (astate :> AbductiveDomain.t)
  | LatentInvalidAccess {astate; address; must_be_valid= _} ->
      F.fprintf fmt "{LatentInvalidAccess(%a) %a}" Decompiler.pp_expr address AbductiveDomain.pp
        (astate :> AbductiveDomain.t)


let pp_summary fmt = function
  | AbortProgram astate ->
      AbductiveDomain.pp_summary fmt (astate :> AbductiveDomain.t)
  | ContinueProgram astate ->
      AbductiveDomain.pp_summary fmt astate
  | ExceptionRaised astate ->
      AbductiveDomain.pp_summary fmt astate
  | ExitProgram astate ->
      AbductiveDomain.pp_summary fmt (astate :> AbductiveDomain.t)
  | ISLLatentMemoryError astate ->
      AbductiveDomain.pp_summary fmt (astate :> AbductiveDomain.t)
  | LatentAbortProgram {astate} ->
      AbductiveDomain.pp_summary fmt (astate :> AbductiveDomain.t)
  | LatentInvalidAccess {astate} ->
      AbductiveDomain.pp_summary fmt (astate :> AbductiveDomain.t)


(* do not export this function as there lies wickedness: clients should generally care about what
   kind of state they are manipulating so let's not encourage them not to *)
let get_astate : t -> AbductiveDomain.t = function
  | ExceptionRaised astate | ContinueProgram astate ->
      astate
  | ExitProgram astate
  | AbortProgram astate
  | LatentAbortProgram {astate}
  | LatentInvalidAccess {astate}
  | ISLLatentMemoryError astate ->
      (astate :> AbductiveDomain.t)


let get_cost astate = (get_astate astate).AbductiveDomain.cost

let add_cost cost = function
  | ContinueProgram astate ->
      let cost = AbductiveDomain.add_cost cost astate.AbductiveDomain.cost in
      ContinueProgram (AbductiveDomain.set_cost cost astate)
  | ExceptionRaised astate ->
      let cost = AbductiveDomain.add_cost cost astate.AbductiveDomain.cost in
      ExceptionRaised (AbductiveDomain.set_cost cost astate)
  | ExitProgram astate ->
      let cost = AbductiveDomain.add_cost cost (astate :> AbductiveDomain.t).AbductiveDomain.cost in
      ExitProgram (AbductiveDomain.summary_with_cost cost (astate :> AbductiveDomain.t))
  | AbortProgram astate ->
      let cost = AbductiveDomain.add_cost cost (astate :> AbductiveDomain.t).AbductiveDomain.cost in
      AbortProgram (AbductiveDomain.summary_with_cost cost (astate :> AbductiveDomain.t))
  | LatentAbortProgram {astate; latent_issue} ->
      let cost = AbductiveDomain.add_cost cost (astate :> AbductiveDomain.t).AbductiveDomain.cost in
      let astate = AbductiveDomain.summary_with_cost cost (astate :> AbductiveDomain.t) in
      LatentAbortProgram {astate; latent_issue}
  | LatentInvalidAccess {astate; address; must_be_valid; calling_context} ->
      let cost = AbductiveDomain.add_cost cost (astate :> AbductiveDomain.t).AbductiveDomain.cost in
      let astate = AbductiveDomain.summary_with_cost cost (astate :> AbductiveDomain.t) in
      LatentInvalidAccess {astate; address; must_be_valid; calling_context}
  | ISLLatentMemoryError astate ->
      let cost = AbductiveDomain.add_cost cost (astate :> AbductiveDomain.t).AbductiveDomain.cost in
      ISLLatentMemoryError (AbductiveDomain.summary_with_cost cost (astate :> AbductiveDomain.t))


let is_visited_path_line line astate =
  AbductiveDomain.PathLines.mem line (get_astate astate).AbductiveDomain.path_lines


let add_path_lines line = function
  | ContinueProgram astate ->
      let path_lines = AbductiveDomain.add_path_lines line astate.AbductiveDomain.path_lines in
      ContinueProgram (AbductiveDomain.set_path_lines path_lines astate)
  | ExceptionRaised astate ->
      let path_lines = AbductiveDomain.add_path_lines line astate.AbductiveDomain.path_lines in
      ExceptionRaised (AbductiveDomain.set_path_lines path_lines astate)
  | ExitProgram astate ->
      let astate = (astate :> AbductiveDomain.t) in
      let path_lines = AbductiveDomain.add_path_lines line astate.AbductiveDomain.path_lines in
      ExitProgram (AbductiveDomain.summary_with_path_lines path_lines astate)
  | AbortProgram astate ->
      let astate = (astate :> AbductiveDomain.t) in
      let path_lines = AbductiveDomain.add_path_lines line astate.AbductiveDomain.path_lines in
      AbortProgram (AbductiveDomain.summary_with_path_lines path_lines astate)
  | LatentAbortProgram {astate; latent_issue} ->
      let astate = (astate :> AbductiveDomain.t) in
      let path_lines = AbductiveDomain.add_path_lines line astate.AbductiveDomain.path_lines in
      let astate = AbductiveDomain.summary_with_path_lines path_lines astate in
      LatentAbortProgram {astate; latent_issue}
  | LatentInvalidAccess {astate; address; must_be_valid; calling_context} ->
      let astate = (astate :> AbductiveDomain.t) in
      let path_lines = AbductiveDomain.add_path_lines line astate.AbductiveDomain.path_lines in
      let astate = AbductiveDomain.summary_with_path_lines path_lines astate in
      LatentInvalidAccess {astate; address; must_be_valid; calling_context}
  | ISLLatentMemoryError astate ->
      let astate = (astate :> AbductiveDomain.t) in
      let path_lines = AbductiveDomain.add_path_lines line astate.AbductiveDomain.path_lines in
      ISLLatentMemoryError (AbductiveDomain.summary_with_path_lines path_lines astate)


let is_unsat_cheap exec_state = PathCondition.is_unsat_cheap (get_astate exec_state).path_condition

type summary = AbductiveDomain.summary base_t [@@deriving compare, equal, yojson_of]

let equal_fast exec_state1 exec_state2 =
  phys_equal exec_state1 exec_state2
  ||
  match (exec_state1, exec_state2) with
  | AbortProgram astate1, AbortProgram astate2
  | ExitProgram astate1, ExitProgram astate2
  | ISLLatentMemoryError astate1, ISLLatentMemoryError astate2 ->
      phys_equal astate1 astate2
  | ContinueProgram astate1, ContinueProgram astate2 ->
      phys_equal astate1 astate2
  | _ ->
      false


let is_normal (exec_state : t) : bool =
  match exec_state with ExceptionRaised _ -> false | _ -> true


let is_exceptional (exec_state : t) : bool =
  match exec_state with ExceptionRaised _ -> true | _ -> false


let is_executable (exec_state : t) : bool =
  match exec_state with ContinueProgram _ | ExceptionRaised _ -> true | _ -> false


let exceptional_to_normal : t -> t = function
  | ExceptionRaised astate ->
      ContinueProgram astate
  | x ->
      x

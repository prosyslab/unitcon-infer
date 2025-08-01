(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Transfer functions that push abstract states across instructions. A typical client should
    implement the Make signature to allow the transfer functions to be used with any kind of CFG. *)

module type S = sig
  module CFG : ProcCfg.S

  (** abstract domain whose state we propagate *)
  module Domain : AbstractDomain.S

  (** read-only extra state (results of previous analyses, globals, etc.) *)
  type analysis_data

  (** type of the instructions the transfer functions operate on *)
  type instr

  val exec_instr :
    Domain.t -> analysis_data -> CFG.Node.t -> ProcCfg.InstrNode.instr_index -> instr -> Domain.t
  (** [exec_instr astate proc_data node idx instr] should usually return [astate'] such that
      [{astate} instr {astate'}] is a valid Hoare triple. In other words, [exec_instr] defines how
      executing an instruction from a given abstract state changes that state into a new one. This
      is usually called the {i transfer function} in Abstract Interpretation terms. [node] is the
      node containing the current instruction and [idx] is the index of the instruction in the node. *)

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
  (** print session name for HTML debug *)
end

module type SIL = sig
  include S with type instr := Sil.instr
end

module type HIL = sig
  include S with type instr := HilInstr.t
end

module type DisjunctiveConfig = sig
  val join_policy :
    [ `UnderApproximateAfter of int
      (** When the set of disjuncts gets bigger than [n] then just stop adding new states to it,
          drop any further states on the floor. This corresponds to an under-approximation/bounded
          approach. *) ]

  val widen_policy : [`UnderApproximateAfterNumIterations of int]
end

module type DisjReady = sig
  module CFG : ProcCfg.S

  module DisjDomain : AbstractDomain.Disjunct

  module NonDisjDomain : AbstractDomain.WithBottomTop

  type analysis_data

  val exec_instr :
       DisjDomain.t * NonDisjDomain.t
    -> analysis_data
    -> CFG.Node.t
    -> Sil.instr
    -> DisjDomain.t list * NonDisjDomain.t

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

module type DisjReadyForPriority = sig
  module CFG : ProcCfg.S

  module DisjDomain : AbstractDomain.Disjunct

  module NonDisjDomain : AbstractDomain.WithBottomTop

  type analysis_data

  type cost

  val exec_instr :
       DisjDomain.t * NonDisjDomain.t
    -> cost
    -> analysis_data
    -> CFG.Node.t
    -> Sil.instr
    -> DisjDomain.t list * NonDisjDomain.t

  val mk_cost_int : int -> cost

  val mk_cost_inf : cost

  val sort : DisjDomain.t list -> DisjDomain.t list

  val is_visited_path_line : int -> DisjDomain.t -> bool

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit

  val pp_cost : DisjDomain.t -> string
end

open! IStd

(** log messages at different levels of verbosity *)

module F = Format

(* If Logging has not been set up yet, Die can be used instead. Prefer to use the
   functions here, as they can do more logging. These functions are documented in Die. *)

include module type of Die

val result : ('a, F.formatter, unit) format -> 'a
(** Emit a result to stdout. Use only if the output format is stable and useful enough that it may
    conceivably get piped to another program, ie, almost never (use [progress] instead otherwise). *)

val debug : ('a, F.formatter, unit) format -> 'a
(** log debug info *)

val setup_error_summary_file : unit -> unit
(** Set up logging to go to the log file. Call this once the results directory has been set up. *)

val reset_formatters : unit -> unit
(** Reset the formatters used for logging. Call this when you fork(2). *)
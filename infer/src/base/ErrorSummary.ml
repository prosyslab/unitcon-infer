(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

 open! IStd

 (** log messages at different levels of verbosity *)
 
 module F = Format
 include Die
 
 (* error_summary files *)
 
 (* can be set up to emit to a file later on *)
 let error_summary_file = ref None
 
 type formatters =
   { file: F.formatter option  (** send to log file *)
   ;  }
 
 let print_formatters : (formatters ref * (unit -> formatters)) list ref = ref []
 
 (* shared ref is less punishing to sloppy accounting of newlines *)
 let is_newline = ref true

 let prev_category = ref ""
 
 let mk_file_formatter file_fmt =
   let out_functions_orig = F.pp_get_formatter_out_functions file_fmt () in
   let prefix = (Pid.to_int (Unix.getpid ())) |> string_of_int in
   let print_prefix_if_newline () =
    let category_has_changed =
      (* take category + PID into account *)
      not (phys_equal !prev_category prefix)
    in
    if !is_newline || category_has_changed then (
      if (not !is_newline) && category_has_changed then
        (* category change but previous line has not ended: print newline *)
      is_newline := false ;
      prev_category := prefix ;
      )
   in
   let out_string s p n =
     print_prefix_if_newline () ;
     out_functions_orig.out_string s p n
   in
   let out_indent n =
     print_prefix_if_newline () ;
     out_functions_orig.out_indent n
   in
   let out_newline () =
     print_prefix_if_newline () ;
     out_functions_orig.out_newline () ;
     is_newline := true
   in
   let out_spaces n =
     print_prefix_if_newline () ;
     out_functions_orig.out_spaces n
   in
   F.formatter_of_out_functions
     {F.out_string; out_flush= out_functions_orig.out_flush; out_indent; out_newline; out_spaces}
 
 
 let register_formatter =
  lazy (
   let mk_formatters () =
     match !error_summary_file with
     | Some (file_fmt, _) ->
       let file = mk_file_formatter file_fmt in
       {file= Some file;}
     | None ->
       {file= None;}
   in
   let formatters = mk_formatters () in
   let formatters_ref = ref formatters in
    print_formatters := (formatters_ref, mk_formatters) :: !print_formatters ;
    formatters_ref )
 
 
 let flush_formatters {file;} =
   Option.iter file ~f:(fun file -> F.pp_print_flush file ())
 
 let print ?(flush = false) (lazy formatters) =
   match (flush) with
   | true ->
        Option.iter !formatters.file ~f:(fun file -> F.pp_print_flush file ());
        Option.value_map !formatters.file
          ~f:(fun file_fmt -> F.fprintf file_fmt)
          ~default:(F.fprintf F.err_formatter)
   | _ ->
       (* to_console might be true, but in that case so is Config.print_logs so do not print to
          stderr because it will get props from the error summary file already *)
       Option.value_map !formatters.file
         ~f:(fun file_fmt -> F.fprintf file_fmt)
         ~default:(F.fprintf F.err_formatter)
 
 let close_logs () =
   let close_fmt (formatters, _) = flush_formatters !formatters in
   List.iter ~f:close_fmt !print_formatters ;
   Option.iter !error_summary_file ~f:(function file_fmt, chan ->
       F.pp_print_flush file_fmt () ;
       Out_channel.close chan )
 
 
 let register_epilogue () =
   Epilogues.register ~f:close_logs ~description:"flushing logs and closing log file"
 
 
 let reset_formatters () =
   let refresh_formatter (formatters, mk_formatters) =
     (* flush to be nice *)
     flush_formatters !formatters ;
     (* recreate formatters, in particular update PID info *)
     formatters := mk_formatters ()
   in
   List.iter ~f:refresh_formatter !print_formatters ;
   if not !is_newline then
     Option.iter !error_summary_file ~f:(function error_summary_file, _ -> F.pp_print_newline error_summary_file ()) ;
   is_newline := true ;
   register_epilogue ()
 
 
 let () = register_epilogue ()
 
let debug fmt = print ~flush:false register_formatter fmt
 
let result fmt = print ~flush:true register_formatter fmt
 
 (* create new channel from the error summary file, and dumps the contents of the temporary error summary buffer there *)
 let setup_error_summary_file () =
   match !error_summary_file with
   | Some _ ->
       (* already set up *)
       ()
   | None ->
       (* TODO T114149430 *)
       let fmt, chan, preexisting_logfile =
         (* if invoked in a sub-dir (e.g., in Buck integrations), log inside the original log file *)
         (* assumes the results dir exists already *)
         let error_summary_file_path =
           ResultsDirEntryName.get_path ~results_dir:Config.toplevel_results_dir ErrorSummarys
         in
         let preexisting_logfile = ISys.file_exists error_summary_file_path in
         let chan = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 error_summary_file_path in
         let file_fmt =
           F.formatter_of_out_channel chan
         in
         (file_fmt, chan, preexisting_logfile)
       in
       error_summary_file := Some (fmt, chan) ;
       if preexisting_logfile then is_newline := false ;
       reset_formatters () ;
       if Config.is_originator && preexisting_logfile then
        print ~flush:false register_formatter ""
 
 
(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2019  Frédéric Bour  <frederic.bour(_)lakaban.net>
                      Thomas Refis  <refis.thomas(_)gmail.com>
                      Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Misc
open Std
open Std.Result

let {Logger. log} = Logger.for_section "Mconfig_dot"

type file = {
  recurse    : bool;
  includes   : string list;
  path       : string;
  directives : Dot_protocol.Directive.Raw.t list;
}

module Cache = File_cache.Make (struct
    type t = file
    let read path =
      let ic = open_in path in
      let acc = ref [] in
      let recurse = ref false in
      let includes = ref [] in
      let tell l = acc := l :: !acc in
      try
        let rec aux () =
          let line = String.trim (input_line ic) in
          if line = "" then ()

          else if String.is_prefixed ~by:"B " line then
            tell (`B (String.drop 2 line))
          else if String.is_prefixed ~by:"S " line then
            tell (`S (String.drop 2 line))
          else if String.is_prefixed ~by:"SRC " line then
            tell (`S (String.drop 4 line))
          else if String.is_prefixed ~by:"CMI " line then
            tell (`CMI (String.drop 4 line))
          else if String.is_prefixed ~by:"CMT " line then
            tell (`CMT (String.drop 4 line))
          else if String.is_prefixed ~by:"EXT " line then
            tell (`EXT (rev_split_words (String.drop 4 line)))
          else if String.is_prefixed ~by:"FLG " line then
            tell (`FLG (Shell.split_command (String.drop 4 line)))
          else if String.is_prefixed ~by:"REC" line then
            recurse := true
          else if String.is_prefixed ~by:". " line then
            includes := String.trim (String.drop 2 line) :: !includes
          else if String.is_prefixed ~by:"STDLIB " line then
            tell (`STDLIB (String.drop 7 line))
          else if String.is_prefixed ~by:"SUFFIX " line then
            tell (`SUFFIX (String.drop 7 line))
          else if String.is_prefixed ~by:"READER " line then
            tell (`READER (List.rev (rev_split_words (String.drop 7 line))))
          else if String.is_prefixed ~by:"EXCLUDE_QUERY_DIR" line then
            tell `EXCLUDE_QUERY_DIR
          else if String.is_prefixed ~by:"#" line then
            ()
          else
            Logger.notify ~section:".merlin"
              "%s: unexpected directive \"%s\"" path line;
          aux ()
        in
        aux ()
      with
      | End_of_file ->
        close_in_noerr ic;
        let recurse = !recurse and includes = !includes in
        {recurse; includes; path; directives = List.rev !acc}
      | exn ->
        close_in_noerr ic;
        raise exn

    let cache_name = "Mconfig_dot"
  end)

let find fname =
  if Sys.file_exists fname && not (Sys.is_directory fname) then
    Some fname
  else
    let rec loop dir =
      let fname = Filename.concat dir ".merlin" in
      if Sys.file_exists fname && not (Sys.is_directory fname)
      then Some fname
      else
        let parent = Filename.dirname dir in
        if parent <> dir
        then loop parent
        else None
    in
    loop fname

let directives_of_files filenames =
  let marked = Hashtbl.create 7 in
  let rec process acc = function
    | x :: rest when Hashtbl.mem marked x ->
      process acc rest
    | x :: rest ->
      Hashtbl.add marked x ();
      let file = Cache.read x in
      let dir = Filename.dirname file.path in
      let rest =
        List.map ~f:(canonicalize_filename ~cwd:dir) file.includes @ rest
      in
      let rest =
        if file.recurse then (
          let dir =
            if Filename.basename file.path <> ".merlin"
            then dir else Filename.dirname dir
          in
          if dir <> file.path then
            match find dir with
            | Some fname -> fname :: rest
            | None -> rest
          else rest
        ) else rest
      in
      process (file :: acc) rest
    | [] -> List.rev acc
  in
  process [] filenames

let standard_library () =
  "%%STDLIB%%"

type config = {
  pass_forward : Dot_protocol.Directive.no_processing_required list;
  to_canonicalize : (string * Dot_protocol.Directive.include_path) list;
  stdlib : string option;
}

let empty_config = {
  pass_forward      = [];
  to_canonicalize   = [];
  stdlib            = None;
}

let prepend_config ~cwd ~cfg =
  List.fold_left ~init:cfg ~f:(fun cfg (d : Dot_protocol.Directive.Raw.t) ->
    match d with
    | `B _ | `S _ | `CMI _ | `CMT _  as directive ->
      { cfg with to_canonicalize = (cwd, directive) :: cfg.to_canonicalize }
    | `EXT _ | `SUFFIX _ | `FLG _ | `READER _
    | `EXCLUDE_QUERY_DIR as directive ->
      { cfg with pass_forward = directive :: cfg.pass_forward }
    | `STDLIB path ->
      let canon_path = canonicalize_filename ~cwd path in
      begin match cfg.stdlib with
      | None -> ()
      | Some p ->
        log ~title:"conflicting paths for stdlib" "%s\n%s" p canon_path
      end;
      { cfg with stdlib = Some canon_path }
  )

let process_one ~cfg {path;directives; _ } =
  let cwd = Filename.dirname path in
  prepend_config ~cwd ~cfg (List.rev directives)

let expand =
  let filter path =
    let name = Filename.basename path in
    name <> "" && name.[0] <> '.' &&
    try Sys.is_directory path
    with _ -> false
  in
  fun ~stdlib dir path ->
    let path = expand_directory stdlib path in
    let path = canonicalize_filename ~cwd:dir path in
    expand_glob ~filter path []

module Import_from_dune = struct
  let escape_only c s =
    let open String in
    let n = ref 0 in
    let len = length s in
    for i = 0 to len - 1 do
      if unsafe_get s i = c then incr n
    done;
    if !n = 0 then
      s
    else
      let b = Bytes.create (len + !n) in
      n := 0;
      for i = 0 to len - 1 do
        if unsafe_get s i = c then (
          Bytes.unsafe_set b !n '\\';
          incr n
        );
        Bytes.unsafe_set b !n (unsafe_get s i);
        incr n
      done;
      Bytes.unsafe_to_string b

  let need_quoting s =
    let len = String.length s in
    len = 0
    ||
    let rec loop i =
      if i = len then
        false
      else
        match s.[i] with
        | ' '
        | '\"'
        | '('
        | ')'
        | '{'
        | '}'
        | ';'
        | '#' ->
          true
        | _ -> loop (i + 1)
    in
    loop 0

  let quote s =
    let s =
      if Sys.win32 then
        (* We need this hack because merlin unescapes backslashes (except when
           protected by single quotes). It is only a problem on windows because
           Filename.quote is using double quotes. *)
        escape_only '\\' s
      else
        s
    in
    if need_quoting s then
      Filename.quote s
    else
      s
end

let postprocess cfg =
  let stdlib = Option.value ~default:standard_library cfg.stdlib in
  let pkg_paths, ppxsetup, failures = path_of_packages cfg.packages_to_load in
  let ppx =
    match Ppxsetup.command_line ppxsetup with
    | [] -> []
    | lst ->
      let cmd = List.concat_map lst ~f:(fun pp -> ["-ppx"; pp])
      in
      [ `FLG  cmd]
  in
  List.concat
    [ List.concat_map cfg.to_canonicalize ~f:(fun (dir, directive) ->
        let dirs =
          match directive with
          | `B path -> List.map (expand ~stdlib dir path) ~f:(fun p -> `B p)
          | `S path -> List.map (expand ~stdlib dir path) ~f:(fun p -> `S p)
          | `CMI path -> List.map (expand ~stdlib dir path) ~f:(fun p -> `CMI p)
          | `CMT path -> List.map (expand ~stdlib dir path) ~f:(fun p -> `CMT p)
        in
        (dirs :> Dot_protocol.directive list)
      )
    ; (cfg.pass_forward :> Dot_protocol.directive list)
    ; List.concat_map pkg_paths ~f:(fun p -> [ `B p; `S p ])
    ; ppx
    ; List.map failures ~f:(fun s -> `ERROR_MSG s)
    ]

let load dot_merlin_file =
  let directives = directives_of_files [ dot_merlin_file ] in
  let cfg =
    List.fold_left directives ~init:empty_config
      ~f:(fun cfg file -> process_one ~cfg file)
  in
  postprocess cfg

let dot_merlin_file =  Filename.concat (Sys.getcwd ()) ".merlin"

let rec main () =
  match Dot_protocol.Commands.read_input stdin with
  | Halt -> exit 0
  | File _path ->
    let directives = load dot_merlin_file in
    Dot_protocol.write ~out_channel:stdout directives;
    flush stdout;
    main ()
  | Unknown -> main ()

let () = main ()

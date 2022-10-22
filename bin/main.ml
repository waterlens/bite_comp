let usage_msg self = self ^ " <file> [-o <output>]"
let inputs = ref []
let output = ref ""
let anon_fun input = inputs := input :: !inputs
let spec = [ ("-o", Arg.Set_string output, "The path of the output file") ]
let () = Arg.parse spec anon_fun (usage_msg Sys.argv.(0))

open Lexing

let print_pos outch pos =
  Printf.fprintf outch "%s: line %d, column %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

open Bite_comp

let run file =
  let ch = open_in_bin file in
  let buf = Lexing.from_channel ch in
  Lexing.set_filename buf file;
  try
    let prog = Parser.prog Lexer.tokenize buf in
    Printf.printf "%s" @@ Syntax.show_prog prog; ()
  with
  | Parser.Error ->
      Printf.eprintf "Parser failed: %a\n%!" print_pos (lexeme_start_p buf)
  | Lexer.Error (_cause, location) ->
      Printf.eprintf "Lexer failed: %a\n%!" print_pos location

let rec runs file_list =
  match file_list with
  | [] -> ()
  | x :: xs ->
      run x;
      runs xs

let _ = Printexc.record_backtrace true
let _ = runs !inputs

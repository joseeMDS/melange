module M = Record_fold
(* TODO, get J.ml from command line *)
let ast =
  let lexbuf = Lexing.from_channel @@ open_in "j.ml" in
  Parse.implementation lexbuf

let () = Pprintast.structure Format.std_formatter ast;

let dump file_name maker =
  let oc = open_out file_name in
  let file = maker ast in
  Printf.fprintf oc "%s" file in

dump "js_record_fold.ml" M.make;

(* Format.printf("salve"); *)
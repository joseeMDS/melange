open Parsetree
open Ast_helper
open Asttypes

let loc = !default_loc

let lident_loc name = {txt= Longident.Lident(name); loc}


(* let let_ name body = Str.value Nonrecursive [ Vb.mk  *)
(*   (Pat.constraint_   *)
(*     (Pat.var {txt=name;loc}) *)
(*     (Typ.poly  *)
(*       [] *)
(*       (Typ.constr (lident_loc "fn") *)
(*         [Typ.constr (lident_loc name) [] ] *)
(*       ) *)
(*     ) *)
(*   ) *)
(*   body *)
(* ] *)

type mode = 
  | RecordFold
  | RecordIter
  | RecordMap

let last items = items |> List.rev |> List.hd 

let get_expr_txt expr = 
  match expr.pexp_desc with
  | Pexp_ident {txt= Lident name;_}  -> name
  | _ -> assert false


let extract_excludes (input: structure_item list) = 
  let type_declarations = match last input with
  | {pstr_desc = Pstr_type(Recursive, type_declarations) ; _} -> type_declarations
  | _ -> assert false
  in
  let attribute = match last type_declarations with
  |{ ptype_attributes=attribute::[]; _ } -> attribute
  | _ -> assert false
  in
  let items = match attribute.attr_payload with
  | PStr(stri::[])-> stri
  | _ -> assert false in
  let excludes = match items.pstr_desc with
  | Pstr_eval (
    {
      pexp_desc = Pexp_record(
        ( (_, {pexp_desc=Pexp_array(expressions); _}) ::[]) 
        ,_);
      _
    }, 

    []) -> expressions

  | _ -> assert false  in
  List.map get_expr_txt excludes


let string_to_mode mode = match mode with
  | "-record-fold" -> RecordFold
  | "-record-iter" -> RecordIter
  | "-record-map" -> RecordMap
  | s -> failwith (Format.sprintf "%s is not a valid option" s)
  

let () = 
  let ic = open_in Sys.argv.(1) in 
  seek_in ic 0;
  let mode = Sys.argv.(2) in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.implementation lexbuf in
  Printast.implementation Format.std_formatter ast;
  let excludes = extract_excludes ast in
  print_endline "excludes";
  List.iter print_endline excludes;
  print_endline mode


let mk_body = Melange_ocaml_tree.Helpers.make_body

let () = 
  let ic = open_in Sys.argv.(1) in 
  seek_in ic 0;
  let mode = Sys.argv.(2) in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.implementation lexbuf in
  print_endline "AST";
  Printast.implementation Format.std_formatter ast;
  (* let excludes = extract_excludes ast in *)
  print_endline "excludes";
  (* List.iter print_endline excludes; *)
  print_endline mode;
  let a = List.map mk_body ast in
  let x = List.flatten a in
  let z = String.concat "\n" x in
  print_endline z

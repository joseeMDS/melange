open Ppxlib

let loc = !Ast_helper.default_loc

module Builder = Ast_builder.Default
module Helper = Ast_helper

module SSet = Set.Make(String)

type 'a reducer = {
  eta: 'a;
  beta: string -> 'a
}
let ident_of_string s = Builder.pexp_ident ~loc {txt=Lident s; loc}

let construct_from_string ?(pattern=None) s = Builder.ppat_construct ~loc {txt=Lident s; loc} pattern

let skip_obj = {
  eta = [%expr "unknown"];
  beta = fun x -> [%expr "unknown" [%e ident_of_string x]]
}

let excludes ast = 
  let last list = list |> List.rev |> List.hd in
  let exclude_type_decl = (match (last ast) with
  | {pstr_desc=Pstr_type((_, type_declarations)); _} -> last type_declarations
  | _ -> assert false
  ) in
  let excludes_array = (match exclude_type_decl with
    | {ptype_attributes=[{attr_payload=PStr([{pstr_desc=Pstr_eval({pexp_desc=Pexp_record(([({txt=Lident("excludes"); _}, {pexp_desc=Pexp_array(l); _})], None)); _}, _);_}]); _}]; _} -> l
    | _ -> assert false
  ) in
  let excludes_values = List.map(fun expr -> match expr with
  | {pexp_desc=Pexp_ident({txt=Lident(name) ;_}); _} -> name
  | _ -> assert false
  )  excludes_array in
  
  SSet.of_list excludes_values

let is_supported typ ast =
    SSet.mem typ (excludes ast)
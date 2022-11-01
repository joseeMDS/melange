open Ppxlib

let loc = !Ast_helper.default_loc

module Builder = Ast_builder.Default
module Helper = Ast_helper

module SSet = Set.Make(String)

type 'a reducer = {
  eta: 'a;
  beta: string -> 'a;
  method_: 'a option
}


type support =
  | Supported of string
  | Excluded of string
  | Unsupported of string

let string_of_support = function
  | Supported(s)
  | Excluded(s)
  | Unsupported(s) -> s

type node = Parsetree.type_declaration * support

let with_loc v = {txt=v; loc}
let txt v = {txt=Lident v; loc}

let ident_of_string s = Builder.pexp_ident ~loc (txt s)

let construct_from_string ?(pattern=None) s = Builder.ppat_construct ~loc (txt s) pattern

let skip_obj = {
  eta = [%expr unknown];
  method_ = None;
  beta = fun x -> [%expr unknown [%e ident_of_string x]];
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

let all_types (ast : structure) =
  let acc = [] in
  let extract_type_name acc stri = match stri.pstr_desc with
      | Pstr_type(_, [type_declaration]) -> Format.eprintf "unsupported being added: %s\n" type_declaration.ptype_name.txt ;(type_declaration, Unsupported(type_declaration.ptype_name.txt)) :: acc
      | Pstr_type(_, type_declarations) ->
        let types_names = List.map (fun typ ->
          let name = typ.ptype_name.txt in
          Format.eprintf "name being added: %s\n" name;
          if SSet.mem name (excludes ast) then
            (typ, Excluded(name))
          else
            (typ,Supported(name))
        ) type_declarations in
         types_names @ acc 
      | _ -> failwith("expected only type declarations at j.ml") in
  List.fold_left extract_type_name acc ast

let core_type_name core_type = match core_type.ptyp_desc with
  | Ptyp_constr(({txt=Lident(name); _}, _))
  | Ptyp_constr(({txt=Ldot((Lident(name)), _); _}, _)) -> name
  | _ -> assert false

let find_core_type_support type_ types =
  let name = core_type_name type_ in
  match List.find_opt (fun (_, support) -> (String.compare (string_of_support support) (core_type_name type_)) == 0 ) types with
  | None -> Unsupported(name)
  | Some(x) -> x |> snd
let find_type_support type_name types =
  List.find (fun (_, support) -> (string_of_support support) == type_name ) types
  |> snd

let core_type_support (core_type : core_type) ast = match core_type.ptyp_desc with
  | Ptyp_constr(({txt=Ldot(Lident(name), _); _}, _)) -> Unsupported(name)
  | Ptyp_constr(({txt=Lident(name); _}, [])) -> find_type_support name ast
  | _ -> assert false


let node_of_core_type (type_) types =
  let type_name = type_.ptype_name.txt in
  List.find (fun (t, _) ->  t.ptype_name.txt == type_name) types

let is_supported type_name ast =
  not @@ SSet.mem type_name (excludes ast)

let supported_types all_types =
  List.filter_map (fun typ -> match snd typ with
    | Supported(name) -> Some(name)
    | _ -> None
  ) all_types
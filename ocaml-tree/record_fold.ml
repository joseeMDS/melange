open Ppxlib
let loc = !Ast_helper.default_loc

open Common

include Record_intf.Make(struct
  type t = Parsetree.structure

  let build_case lhs rhs = Builder.case ~lhs ~rhs ~guard:None

  let build_ppat_var txt = Builder.ppat_var {txt; loc} ~loc

  let build_args = function
    | [] -> None
    | [txt] -> Option.some @@ build_ppat_var txt
    | values -> Option.some @@ Builder.ppat_tuple ~loc (List.map build_ppat_var values)
 
  let _mk_branch = function
    | {pcd_name={txt;_};pcd_args=Pcstr_tuple([]); _} -> build_case (construct_from_string txt) [%expr st]
    | {pcd_name={txt;_};pcd_args=Pcstr_tuple(param_list); _} -> 
      let length = List.length param_list in
      let args = List.init length (fun n -> Format.sprintf("_x%i") n) |> build_args in
      build_case (construct_from_string txt ~pattern:args) [%expr st]
    | _ -> assert false
  
  let mk_structural_ty ast = function
    (* If the type is like M.t, its not supported *)
    | Pstr_type((_, [{ptype_kind=Ptype_abstract; ptype_manifest=Some({ptyp_desc=Ptyp_constr(({txt=Ldot(_); _}, _)); _}); _}])) -> skip_obj
    | Pstr_type((_, [{ptype_kind=Ptype_abstract; ptype_manifest=Some({ptyp_desc=Ptyp_constr(({txt=Lident(txt); _}, [])); _}); _}])) -> 
      let code = if is_supported txt ast then
        [%expr _self [%e ident_of_string txt]] else
          ident_of_string txt in
          {
            eta=[%expr (fun _self arg -> [%e code]  _self arg)];
            beta=(fun x -> [%expr fun _self [%e code] -> [%e ident_of_string x] _self])
          };
    | Pstr_type((_, [{ptype_kind=Ptype_record(_); _}])) -> skip_obj
    | Pstr_type((_, [{ptype_kind=Ptype_variant(_); _}])) -> skip_obj
    | _ -> assert false

    let get_manifest = function
      | Ptyp_constr((_, []))
      | Ptyp_tuple(_) -> assert false
      | _ -> assert false

  let mk_body_apply ast arg v = match mk_structural_ty v ast with
  | v when v = skip_obj -> None
  | v -> Some(v.beta arg)
  
  let _mk_body' ast = function
    | Pstr_type((_, 
    [{ptype_kind=Ptype_record(l); _}])) as ty ->
      let labels = List.map (fun l -> l.pld_name) l in 
      let args = List.mapi (fun i v -> ident_of_string (Format.sprintf "_%s%i" v.txt i) ) labels in
      let keys = List.map (fun v -> {txt= Lident v.txt; loc=v.loc}) labels in
      let values = List.combine keys args in
      (* not sure why this is unused *)
      let [@warning "-26"] record = Builder.pexp_record ~loc values None in
      let body = List.map (fun v -> mk_body_apply ty v.txt ast) labels
      |> List.filter ((!=) Option.none) |> List.map (Option.get) |> List.hd in
      [%expr fun _self _st [%e record] -> [%e body] st]

    | Pstr_type((_, [{ptype_kind=Ptype_variant(constructor_list); _}])) ->
      let _ = constructor_list in
      [%expr fun _self st -> assert false]
    | Pstr_type((_, [t])) -> (match t.ptype_manifest with
      | Some(x) -> get_manifest x.ptyp_desc
      | None -> failwith "j.ml should not contain an opaque type")
    | _ -> assert false
  

  let mk_body = [%expr 1]

  let _mk_method = [%stri let x : 'a. ('a, x) fn = [%e mk_body]]

  let inner_make (_ast : Parsetree.structure) = 
    let module SSet = Set.Make(String) in
    
    [%str
    open J
    let [@inline] unknown _ st _ = st
    let [@inline] option sub self st = fun v ->
      match v with
      | None -> st
      | Some v -> sub self st v
    let rec list sub self st = fun x  ->
      match x with
      | [] -> st
      | x::xs ->
        let st = sub self st x in
        list sub self st xs

      type 'state iter = ()

      and ('state, 'a) fn = 'state iter -> 'state ->  'a -> 'state

      let super : 'state iter = ()
  ]

  let make ast = Pprintast.string_of_structure (inner_make ast)
end)
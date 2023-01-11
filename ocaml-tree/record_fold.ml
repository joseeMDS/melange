open Ppxlib
let loc = !Ast_helper.default_loc

open Common

include Record_intf.Make(struct
  type t = Parsetree.structure

  let build_case lhs rhs = Builder.case ~lhs ~rhs ~guard:None

  let build_ppat_var txt = Builder.ppat_var {txt; loc} ~loc

  let build_pexp_let expr let_expr =
    let pat = build_ppat_var "st" in
    let vb = Builder.value_binding ~loc ~pat ~expr in
    (* let st = vb_expr in let_expr   *)
    Builder.pexp_let ~loc Nonrecursive [vb] let_expr

  let rec build_pexp_chain = function
    | [] -> (ident_of_string "st")
    | x::xs -> x (build_pexp_chain xs)


  let remove_options l = List.fold_right
    (fun x result ->
     match x with
     | Some i -> i :: result
     | None -> result )
    l []

  let build_args = function
    | [] -> None
    | [txt] -> Some(build_ppat_var txt)
    | values -> Some(Builder.ppat_tuple ~loc (List.map build_ppat_var values))

  let rec mk_branch all_types branch = match branch with
    | {pcd_name={txt;_};pcd_args=Pcstr_tuple([]); _} -> build_case (construct_from_string txt) [%expr st]
    | {pcd_name={txt;_};pcd_args=Pcstr_tuple(param_list); _} ->
      let length = List.length param_list in
      let args = List.init length (fun n -> Format.sprintf("_x%i") n) |> build_args in
      let body = List.mapi (fun i ty -> mk_body_apply all_types (ty, (find_core_type_support ty all_types)) (Format.sprintf "_x%i" i)) param_list |> remove_options in
      let body_expr = build_pexp_chain body in
      build_case (construct_from_string txt ~pattern:args) body_expr
    | _ -> assert false

   and mk_structural_ty all_types (core_type, supported)  = match core_type.ptyp_desc with
    | Ptyp_constr(({txt=Ldot(_); _}, _)) -> skip_obj
    | Ptyp_constr(({txt=Lident(_); _}, [])) ->
        (match supported with
        | Unsupported(_) -> skip_obj
        | Supported(name) ->
          let pexp_fields = Builder.pexp_field ~loc (ident_of_string "_self") (txt name) in
          ({
          eta = [%expr (fun _self arg -> [%e pexp_fields] _self arg)];
          beta = (fun x -> [%expr [%e pexp_fields] _self st [%e ident_of_string x]]);
          method_ = Some(pexp_fields)
          }
          )
        | Excluded(name) -> {
          eta = [%expr (fun _self arg -> [%e ident_of_string name] _self arg)];
          beta = (fun x -> [%expr [%e ident_of_string name] _self st [%e ident_of_string x]] );
          method_= Some((ident_of_string name))
        })
    |Ptyp_constr(({txt=Lident(type_name); _}, [type_parameter])) when type_name = "option" || type_name = "list" ->
      (
        let inner = mk_structural_ty all_types (type_parameter, supported) in
        let inner_code = match inner.method_ with
        | Some(expr) -> expr
        | None -> inner.eta in
        if inner == skip_obj then
          skip_obj
        else
        {
          eta = [%expr (fun _self st arg -> [%e ident_of_string type_name] [%e inner_code] _self st arg)];
          beta = (fun x -> [%expr [%e ident_of_string type_name] [%e inner_code] _self st [%e ident_of_string x]]);
          method_ = None;
        }
      )
    |Ptyp_tuple(types) ->
      let len = List.length types in
      let args = List.init len (Format.sprintf "_x%i") in
      let args_pat = List.map (fun arg -> Builder.ppat_var ~loc @@ with_loc arg) args in
      let tuple_pattern = Builder.ppat_tuple ~loc args_pat in
      let nodes = List.combine types args in
      let body = List.map (fun (typ, arg) -> mk_body_apply all_types (typ, (find_core_type_support typ all_types)) arg) nodes |> remove_options in
      let body_expr = build_pexp_chain body in
      {
        eta = [%expr fun self st [%p tuple_pattern] -> [%e body_expr]];
        (* Code not called *)
        beta = (fun _ -> [%expr assert false]);
        method_= None;
      }
    | _ -> (
      assert false
    )

  and mk_body_apply all_types node arg  = match mk_structural_ty all_types node  with
  | v when v == skip_obj -> None
  | v -> Option.some @@ build_pexp_let (v.beta arg)

  let mk_body all_types (type_, support) =
    match type_ with
    | {ptype_kind=Ptype_record(l); _} ->
      let labels = List.map (fun l -> l.pld_name) l in
      let args = List.mapi (fun i _ -> Builder.ppat_var ~loc (with_loc (Format.sprintf "_x%i" i)) ) labels in
      let keys = List.map (fun v -> {txt= Lident v.txt; loc=v.loc}) labels in
      let values = List.combine keys args in
      let record = Builder.ppat_record ~loc values Closed in
      let node_of_label label =
        let typ = label.pld_type in
        (typ, (find_core_type_support typ all_types)) in
      let body =
        List.mapi (fun index label -> mk_body_apply all_types (node_of_label label) (Format.sprintf "_x%i" index)) l
        |> remove_options
        |> build_pexp_chain in

      [%expr fun _self _st [%p record] -> [%e body] st]

    | {ptype_kind=Ptype_variant(constructor_list); _} ->
      let branches = List.map (mk_branch all_types) constructor_list in
      let function_ = Builder.pexp_function ~loc branches in
      [%expr fun _self st -> [%e function_]]
    | t -> (match t.ptype_manifest with
    | Some({ptyp_desc; _} as core_type) -> (match ptyp_desc with
        | Ptyp_tuple(_)
        | Ptyp_constr(_, _) ->
          let reducer = (mk_structural_ty all_types (core_type, support)) in
          reducer.eta
        | _ -> Pprintast.core_type Format.err_formatter core_type; failwith "Unsupported type")
      | None -> failwith "j.ml should not contain an opaque type")

  let mk_method all_types node =
    let name = string_of_support @@ snd node in
    let ppat_var s = Builder.ppat_var ~loc (with_loc s) in
    let ptyp_constr s = Builder.ptyp_constr ~loc (txt s) [] in
    let binding_pattern = [%pat? [%p ppat_var name]] in
    let type_ = [%type: ('a, [%t ptyp_constr name]) fn] in
    let binding_type = Builder.ptyp_poly ~loc [with_loc("a")] type_ in
    [%stri let [%p binding_pattern] : [%t binding_type] = [%e mk_body all_types node]]

  let inner_make (ast : Parsetree.structure) =
    let module SSet = Set.Make(String) in

      let all_types = (all_types ast) in
      let supported_types = supported_types all_types in

      let build_iter =
        let type_constr type_name = Builder.ptyp_constr ~loc (txt type_name) [] in
        let map_type type_name = Builder.label_declaration ~loc ~name:(with_loc type_name) ~mutable_:Immutable ~type_:[%type: ('state, [%t type_constr type_name]) fn] in
        let label_declarations = List.map map_type supported_types in
        let type_decl = Builder.type_declaration ~loc ~name:(with_loc("iter")) ~params:[([%type: 'state], (NoVariance, NoInjectivity))] ~cstrs:[] ~kind:(Ptype_record(label_declarations)) ~private_:Public ~manifest:None in
        let type_decl_2 = Builder.type_declaration ~loc ~name:(with_loc("fn")) ~params:[([%type: 'state], (NoVariance, NoInjectivity)); ([%type: 'a], (NoVariance, NoInjectivity))] ~cstrs:[] ~kind:Ptype_abstract ~private_:Public ~manifest:(Some([%type: 'state iter -> 'state ->  'a -> 'state])) in
        Builder.pstr_type ~loc Recursive [type_decl; type_decl_2]
        in
        let build_super =
           let types_name = List.map (fun type_name ->
              txt type_name, ident_of_string type_name
            ) supported_types in
           let values = Builder.pexp_record ~loc types_name None in

          let rhs = Builder.pexp_constraint ~loc values  [%type: 'state iter] in
          [%stri let super : 'state iter = [%e rhs]] in

          let body = List.map (mk_method all_types) all_types in
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

      [%%i build_iter]

  ] @ body @ [build_super]

  let make ast = Pprintast.string_of_structure (inner_make ast)
end)

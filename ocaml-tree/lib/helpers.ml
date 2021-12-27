open Ast_helper
open Ppxlib


let loc = !default_loc

let make_method name _def = [%stri let [%p Pat.var {txt=name; loc}]: [%t Typ.var name] = 1+1]

let make_branch _branch = assert false

let make_body def = 
  let typedefs = match def.pstr_desc with
  | Pstr_type(Recursive, typedefs) -> typedefs
  | _ -> failwith "Failed in getting typedefs" in

  let handle_typedef = function
    | {ptype_kind=Ptype_abstract;ptype_manifest=Some({ptyp_desc=Ptyp_tuple(_) ;ptyp_attributes=[];_}) ; _} -> "tuple"
    | {ptype_kind=Ptype_abstract;ptype_manifest=Some({ptyp_desc=Ptyp_constr({txt=Lident(_) ;_},[_] ) ;ptyp_attributes=[];_}) ; _} -> "parametized type"
    | {ptype_kind=Ptype_abstract;ptype_manifest=Some({ptyp_desc=Ptyp_constr({txt=Lident(_) ;_},[] ) ;ptyp_attributes=[];_}) ; _} -> "Normal type (String)"
    | {ptype_kind=Ptype_abstract;ptype_manifest=Some({ptyp_desc=Ptyp_constr({txt=Ldot(_) ;_}, []) ;ptyp_attributes=[];_}) ; _} -> "from other module"

    | {ptype_kind=Ptype_variant(constructors) ;ptype_manifest=None; _} -> 
        let branches = List.map (fun branch -> make_branch branch) constructors in
        Format.sprintf "fun _self st -> function \n| %s" (String.concat "\n" branches)

    | {ptype_kind=Ptype_record(constructors);ptype_manifest=None; _} -> 
        let len = List.length(constructors) in
        let args = List.init len (Format.sprintf "_x%i") in
        let get_constructor_name = fun {pld_name={txt=name;_ }; _} -> name in
        let pat_exp = List.init len (fun i -> Format.sprintf "%s = %s" (get_constructor_name (List.nth constructors i)) (List.nth args i)) in 
        let body = List.mapi (fun i x -> 
            let ty = List.nth constructors i in
        )


        

        "record"
    | _ -> "probably normal type" in

  let rec extract_typedefs = function 
    | [] -> [""] 
    | x::xs -> (handle_typedef x) :: extract_typedefs xs in


  extract_typedefs typedefs


let make_strutcural_type _def _mode = assert false

let make = Format.sprintf {|
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

  type 'state iter = {
    
  }

  and ('state, 'a) fn = 'state iter -> 'state -> 'a -> 'state

  let super : 'state iter = {
    
  }
|}



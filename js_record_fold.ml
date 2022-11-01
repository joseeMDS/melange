open J
let unknown _ st _ = st[@@inline ]
let option sub self st v =
  match v with | None -> st | Some v -> sub self st v[@@inline ]
let rec list sub self st x =
  match x with
  | [] -> st
  | x::xs -> let st = sub self st x in list sub self st xs
type 'state iter =
  {
  ident: ('state, ident) fn ;
  module_id: ('state, module_id) fn ;
  vident: ('state, vident) fn ;
  exception_ident: ('state, exception_ident) fn ;
  for_ident: ('state, for_ident) fn ;
  expression: ('state, expression) fn ;
  statement: ('state, statement) fn ;
  variable_declaration: ('state, variable_declaration) fn ;
  block: ('state, block) fn ;
  program: ('state, program) fn }
and ('state, 'a) fn = 'state iter -> 'state -> 'a -> 'state
let super : 'state iter =
  ({
     ident;
     module_id;
     vident;
     exception_ident;
     for_ident;
     expression;
     statement;
     variable_declaration;
     block;
     program
   } : 'state iter)
let label : 'a . ('a, label) fn = fun _self -> fun arg -> label _self arg
let ident : 'a . ('a, ident) fn = unknown
let module_id : 'a . ('a, module_id) fn =
  fun _self ->
    fun _st ->
      fun { id = _x0; kind = _x1 } ->
        (fun _self -> fun [%e ident_of_string x] -> id _self) st
let required_modules : 'a . ('a, required_modules) fn =
  fun _self -> fun st -> fun arg -> list required_modules _self st arg
let vident : 'a . ('a, vident) fn = fun _self -> fun st -> assert false
let exception_ident : 'a . ('a, exception_ident) fn =
  fun _self -> fun arg -> _self.exception_ident _self arg
let for_ident : 'a . ('a, for_ident) fn =
  fun _self -> fun arg -> _self.for_ident _self arg
let for_direction : 'a . ('a, for_direction) fn = unknown
let property_map : 'a . ('a, property_map) fn =
  fun _self ->
    fun st ->
      fun arg ->
        list
          (fun self ->
             fun st ->
               fun (_x0, _x1) ->
                 fun _x1 ->
                   fun _self -> fun [%e ident_of_string x] -> _x1 _self)
          _self st arg
let length_object : 'a . ('a, length_object) fn = unknown
let expression_desc : 'a . ('a, expression_desc) fn =
  fun _self -> fun st -> assert false
let for_ident_expression : 'a . ('a, for_ident_expression) fn =
  fun _self -> fun arg -> for_ident_expression _self arg
let finish_ident_expression : 'a . ('a, finish_ident_expression) fn =
  fun _self -> fun arg -> finish_ident_expression _self arg
let case_clause : 'a . ('a, case_clause) fn =
  fun _self ->
    fun _st ->
      fun { switch_body = _x0; should_break = _x1; comment = _x2 } ->
        (fun _self -> fun [%e ident_of_string x] -> switch_body _self) st
let string_clause : 'a . ('a, string_clause) fn =
  fun self ->
    fun st ->
      fun (_x0, _x1) ->
        fun _x1 -> fun _self -> fun [%e ident_of_string x] -> _x1 _self
let int_clause : 'a . ('a, int_clause) fn =
  fun self ->
    fun st ->
      fun (_x0, _x1) ->
        fun _x1 -> fun _self -> fun [%e ident_of_string x] -> _x1 _self
let statement_desc : 'a . ('a, statement_desc) fn =
  fun _self -> fun st -> assert false
let expression : 'a . ('a, expression) fn =
  fun _self ->
    fun _st ->
      fun { expression_desc = _x0; comment = _x1 } ->
        (fun _self -> fun [%e ident_of_string x] -> expression_desc _self) st
let statement : 'a . ('a, statement) fn =
  fun _self ->
    fun _st ->
      fun { statement_desc = _x0; comment = _x1 } ->
        (fun _self -> fun [%e ident_of_string x] -> statement_desc _self) st
let variable_declaration : 'a . ('a, variable_declaration) fn =
  fun _self ->
    fun _st ->
      fun { ident = _x0; value = _x1; property = _x2; ident_info = _x3 } ->
        (fun _self -> fun [%e ident_of_string x] -> ident _self) st
let block : 'a . ('a, block) fn =
  fun _self -> fun st -> fun arg -> list _self.block _self st arg
let program : 'a . ('a, program) fn =
  fun _self ->
    fun _st ->
      fun { block = _x0; exports = _x1; export_set = _x2 } ->
        (fun _self -> fun [%e ident_of_string x] -> block _self) st
let deps_program : 'a . ('a, deps_program) fn =
  fun _self ->
    fun _st ->
      fun { program = _x0; modules = _x1; side_effect = _x2 } ->
        (fun _self -> fun [%e ident_of_string x] -> program _self) st
let property_name : 'a . ('a, property_name) fn = unknown
let tag_info : 'a . ('a, tag_info) fn = unknown
let exports : 'a . ('a, exports) fn = unknown
let ident_info : 'a . ('a, ident_info) fn = unknown
let number : 'a . ('a, number) fn = unknown
let property : 'a . ('a, property) fn = unknown
let kind : 'a . ('a, kind) fn = unknown
let int_op : 'a . ('a, int_op) fn = unknown
let binop : 'a . ('a, binop) fn = unknown
let mutable_flag : 'a . ('a, mutable_flag) fn = unknown
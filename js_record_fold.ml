open J
let unknown _ st _ = st[@@inline ]
let option sub self st v =
  match v with | None -> st | Some v -> sub self st v[@@inline ]
let rec list sub self st x =
  match x with
  | [] -> st
  | x::xs -> let st = sub self st x in list sub self st xs
type 'state iter =
  | () 
and ('state, 'a) fn = 'state iter -> 'state -> 'a -> 'state
let super : 'state iter = ()
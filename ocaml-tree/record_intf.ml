module type S = sig
  type t
  val make: t -> string
end


module Make (M : S) = struct
  type t = M.t

  let make = M.make
  
end


module type Intf = sig
  module type S = S

  type t
  val make: t -> string
end
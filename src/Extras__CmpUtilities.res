module Cmp = Extras__Cmp

module type Equality = {
  type t
  let eq: (t, t) => bool
  let neq: (t, t) => bool
}

module type Ordering = {
  type t
  let lt: (t, t) => bool
  let lte: (t, t) => bool
  let gt: (t, t) => bool
  let gte: (t, t) => bool
  let min: (t, t) => t
  let max: (t, t) => t
}

module MakeEquals = (
  C: {
    type domain
    let cmp: Cmp.t<domain>
  },
): (Equality with type t := C.domain) => {
  let eq = C.cmp->(Cmp.eq(_, ...))
  let neq = C.cmp->(Cmp.neq(_, ...))
}

module MakeCompare = (
  C: {
    type domain
    let cmp: Cmp.t<domain>
  },
): (Ordering with type t := C.domain) => {
  let lt = C.cmp->(Cmp.lt(_, ...))
  let lte = C.cmp->(Cmp.lte(_, ...))
  let gt = C.cmp->(Cmp.gt(_, ...))
  let gte = C.cmp->(Cmp.gte(_, ...))
  let min = C.cmp->(Cmp.min(_, ...))
  let max = C.cmp->(Cmp.max(_, ...))
}

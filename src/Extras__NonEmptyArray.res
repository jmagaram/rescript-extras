module O = Belt.Option
module Cmp = Extras__Cmp

@unboxed
type t<'a> = NonEmptyArray(array<'a>)

@inline let unwrap = (NonEmptyArray(xs)) => xs

let fromArray = xs =>
  switch xs->Js.Array2.length {
  | 0 => None
  | _ => Some(NonEmptyArray(xs))
  }

external fromArrayUnsafe: array<'a> => t<'a> = "%identity"

let fromArrayExn = xs => xs->fromArray->O.getExn

@val external of1: 'a => t<'a> = "Array.of"
@val external of2: ('a, 'a) => t<'a> = "Array.of"
@val external of3: ('a, 'a, 'a) => t<'a> = "Array.of"
@variadic @val external ofMany: ('a, array<'a>) => t<'a> = "Array.of"

external toArray: t<'a> => array<'a> = "%identity"

let map = (xs, f) => xs->unwrap->Js.Array2.map(f)->NonEmptyArray
let mapi = (xs, f) => xs->unwrap->Js.Array2.mapi(f)->NonEmptyArray

@inline let size = xs => xs->unwrap->Js.Array2.length

let head = xs => xs->unwrap->Js.Array2.unsafe_get(0)
let last = xs => xs->unwrap->Js.Array2.unsafe_get(xs->size - 1)

let reduce = (xs, f) =>
  xs->unwrap->Js.Array2.reducei((sum, val, inx) => inx == 0 ? sum : f(sum, val), xs->head)

let minBy = (xs, cmp) => xs->reduce((i, j) => Cmp.min(cmp, i, j))
let maxBy = (xs, cmp) => xs->reduce((i, j) => Cmp.max(cmp, i, j))

let concat = (a, b) => Js.Array2.concat(a->unwrap, b->unwrap)->NonEmptyArray
let concatArray = (a, b) => Js.Array2.concat(a->unwrap, b)->NonEmptyArray

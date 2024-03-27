module T = Extras__Test
module C = Extras__Cmp
module A = Array

type person = {name: string, age: int}

let inOrder = (cmp, a, b) => cmp(a, b) == -1.0 && cmp(b, a) == 1.0
let isEqual = (cmp, a, b) => cmp(a, b) == 0.0 && cmp(b, a) == 0.0

let lessThanTests = (~small, ~big, ~cmp) => {
  let make = (~title, ~isTrue) =>
    T.fromPredicate(~category="Cmp", ~title, ~expectation="when different", () => isTrue)
  [
    make(~title="lt", ~isTrue=C.lt(cmp, small, big)),
    make(~title="lte", ~isTrue=C.lte(cmp, small, big)),
    make(~title="gt", ~isTrue=C.gt(cmp, big, small)),
    make(~title="gte", ~isTrue=C.gte(cmp, big, small)),
    make(~title="neq", ~isTrue=C.neq(cmp, big, small)),
    make(~title="eq", ~isTrue=C.eq(cmp, big, small) == false),
    make(~title="min", ~isTrue=C.min(cmp, big, small) == small),
    make(~title="max", ~isTrue=C.max(cmp, big, small) == big),
  ]
}

let areSameTests = (~a, ~b, ~cmp) => {
  let make = (~title, ~isTrue) =>
    T.fromPredicate(~category="Cmp", ~title, ~expectation="when same", () => isTrue)
  [
    make(~title="lt", ~isTrue=false == C.lt(cmp, a, b)),
    make(~title="lte", ~isTrue=C.lte(cmp, a, b)),
    make(~title="gt", ~isTrue=false == C.gt(cmp, a, b)),
    make(~title="gte", ~isTrue=C.gte(cmp, a, b)),
    make(~title="neq", ~isTrue=false == C.neq(cmp, a, b)),
    make(~title="eq", ~isTrue=C.eq(cmp, a, b)),
    make(~title="min", ~isTrue=C.min(cmp, a, b) == a),
    make(~title="max", ~isTrue=C.max(cmp, a, b) == a),
  ]
}

let otherTests = [
  T.fromPredicate(~category="Cmp", ~title="fromMap", () => {
    let target = C.fromMap(i => i.age, Int.compare, ...)
    let bob = {name: "bob", age: 3}
    let sue = {name: "sue", age: 8}
    inOrder(target, bob, sue)
  }),
  T.fromPredicate(~category="Cmp", ~title="fromIntResult", () => {
    let f = (x: int, y: int) => x < y ? -1 : x > y ? 1 : 0
    let target = C.fromIntResult(f, ...)
    inOrder(target, 3, 9) && isEqual(target, 3, 3)
  }),
  T.fromPredicate(~category="Cmp", ~title="int instance", () => {
    inOrder(Int.compare, 1, 2) && isEqual(Int.compare, 1, 1)
  }),
  T.fromPredicate(~category="Cmp", ~title="float instance", () => {
    inOrder(Float.compare, 1.0, 2.0) && isEqual(Float.compare, 1.0, 1.0)
  }),
  T.fromPredicate(~category="Cmp", ~title="bool instance", () => {
    inOrder(C.bool, false, true) && isEqual(C.bool, false, false)
  }),
]

let tests =
  [
    lessThanTests(~small=1, ~big=99, ~cmp=Int.compare),
    lessThanTests(~small=1, ~big=99, ~cmp=Int.compare),
    lessThanTests(~small="a", ~big="b", ~cmp=String.localeCompare),
    areSameTests(~a=1.0, ~b=1.0, ~cmp=Float.compare),
    areSameTests(~a=1, ~b=1, ~cmp=Int.compare),
    areSameTests(~a="abc", ~b="abc", ~cmp=String.localeCompare),
    otherTests,
  ]->A.flat

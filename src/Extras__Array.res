@val external of1: 'a => array<'a> = "Array.of"

let fromOption = opt =>
  switch opt {
  | None => []
  | Some(i) => of1(i)
  }

let isEmpty = xs => xs->Array.length == 0
let isNotEmpty = xs => xs->Array.length > 0

let exactlyOne = xs =>
  switch xs->Array.length {
  | 1 => xs->Array.getUnsafe(0)->Some
  | _ => None
  }

let head = xs => xs->Array.get(0)

let last = xs =>
  switch xs->isEmpty {
  | true => None
  | false => xs->Array.get(xs->Array.length - 1)
  }

let lastIndex = xs => xs->Array.length->(i => i == 0 ? None : Some(i - 1))

let prepend = (a, b) => Array.concat(b, a)

let filterSomeWith = (xs, f) => {
  let result = []
  for i in 0 to Array.length(xs) - 1 {
    let item = xs->Array.getUnsafe(i)
    switch f(item, i) {
    | None => ()
    | Some(i) => result->Array.push(i)->ignore
    }
  }
  result
}

let filterSome = xs => xs->filterSomeWith((value, _) => value)

let unfold = (state, generator) => {
  let push = Array.push
  let result = []
  let state = ref(state)
  let break = ref(false)
  while !break.contents {
    switch generator(state.contents) {
    | None => break := true
    | Some((item, nextState)) => {
        result->push(item)->ignore
        state := nextState
      }
    }
  }
  result
}

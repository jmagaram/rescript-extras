module Option = Belt.Option
module Result = Belt.Result
module Ex = Extras

exception ArgumentOfOfRange(string)

type rec t<'a> = (. unit) => node<'a>
and node<'a> =
  | End
  | Next('a, t<'a>)

module Node = {
  let end = End

  @inline
  let next = (x, xs) => Next(x, xs)

  @inline
  let toOption = n =>
    switch n {
    | End => None
    | Next((x, xs)) => Some(x, xs)
    }

  @inline let head = n => n->toOption->Option.map(((x, _)) => x)

  @inline
  let mapNext = (n, f) =>
    switch n {
    | End => End
    | Next(x, xs) => f(x, xs)
    }
}

let empty = (. ()) => Node.end

let nextNode = (xs: t<'a>) => xs(.)
let next = xs => xs->nextNode->Node.toOption

let cons = (x, xs) => (. ()) => Next(x, xs)
let startWith = (xs, x) => cons(x, xs)

let singleton = x => cons(x, empty)

/**
This is a foundation method for many of the functions in this library. It must
not be recursive to prevent stack overflows. This consumes at least 1 item in
`xs`.
*/
let findNode = (xs, f) => {
  let found = ref(None)
  let current = ref(xs)
  let break = ref(false)
  while !break.contents {
    switch current.contents->nextNode {
    | End => break := true
    | Next(x, xs) as node =>
      switch f(x) {
      | true =>
        found := Some(node)
        break := true
      | false => ()
      }
      current := xs
    }
  }
  found.contents->Option.getWithDefault(End)
}

let find = (xs, f) => xs->findNode(f)->Node.head

let mapNext = (xs, f) => (. ()) => xs->nextNode->Node.mapNext(f)

let rec concat = (xs, ys) =>
  (. ()) => {
    switch xs->nextNode {
    | End => ys->nextNode
    | Next(x, xs) => Next(x, concat(xs, ys))
    }
  }

let endWith = (xs, x) => concat(xs, singleton(x))
let prepend = (xs, ys) => concat(ys, xs)

let rec flatMap = (xs, f) =>
  (. ()) =>
    switch xs->nextNode {
    | End => End
    | Next(x, xs) => concat(f(x), flatMap(xs, f))(.)
    }

let flatten = xxs => xxs->flatMap(i => i)

let rec map = (xs, f) =>
  (. ()) => {
    switch xs->nextNode {
    | End => End
    | Next(x, xs) => Next(f(x), map(xs, f))
    }
  }

let head = xs =>
  switch xs->nextNode {
  | End => None
  | Next(x, _) => Some(x)
  }

let headTail = xs =>
  switch xs->nextNode {
  | End => None
  | Next(xs, x) => Some(xs, x)
  }

let forEach = (xs, f) => {
  let curr = ref(xs->nextNode)
  let break = ref(false)
  while !break.contents {
    switch curr.contents {
    | End => break := true
    | Next(x, xs) => {
        f(x)
        curr := xs->nextNode
      }
    }
  }
}

module Indexed = {
  type t<'a> = ('a, int)
  @inline let make = (~value, ~index) => (value, index)
  @inline let value = ((value, _): t<'a>) => value
  @inline let index = ((_, index): t<'a>) => index
  @inline let indexEquals = (i, other) => i->index == other
}

let indexed = xs => {
  let rec go = (xs, index) =>
    (. ()) =>
      switch xs->nextNode {
      | End => End
      | Next(x, xs) => Next(Indexed.make(~value=x, ~index), go(xs, index + 1))
      }
  go(xs, 0)
}

let rec unfold = (seed, f) =>
  (. ()) =>
    switch f(seed) {
    | None => End
    | Some(x, seed) => Next(x, unfold(seed, f))
    }

let init = (~count, f) => unfold(0, i => i < count ? Some(f(~index=i), i + 1) : None)

let replicate = (~count, ~value) => unfold(0, i => i < count ? Some(value, i + 1) : None)

let iterate = (seed, f) => unfold(seed, i => Some(i, f(i)))

let range = (~start, ~end) => {
  start <= end
    ? unfold(start, i => i <= end ? Some(i, i + 1) : None)
    : unfold(start, i => i >= end ? Some(i, i - 1) : None)
}

let rec infinite = f => (. ()) => Next(f(), infinite(f))

let rec tap = (xs, f) =>
  (. ()) =>
    switch xs->nextNode {
    | End => End
    | Next(x, xs) => {
        f(x)
        Next(x, tap(xs, f))
      }
    }

let cycleNonEmpty = xs => {
  let rec go = ys =>
    (. ()) =>
      switch ys->nextNode {
      | End => go(xs)(.)
      | Next(y, ys) => Next(y, go(ys))
      }
  go(xs)
}

let cycle = xs => xs->mapNext((x, xs') => cons(x, xs')->concat(xs->cycleNonEmpty)->nextNode)

let fromString = s =>
  switch s->Js.String2.length {
  | 0 => empty
  | len => range(~start=0, ~end=len - 1)->map(inx => s->Js.String2.charAt(inx))
  }

let fromArray = (~start=?, ~end=?, xs: array<'a>) => {
  switch xs->Ex.Array.isEmpty {
  | true =>
    start
    ->Option.orElse(end)
    ->Option.forEach(_ =>
      ArgumentOfOfRange("The array is empty but you provided start and/or end indexes.")->raise
    )
    empty
  | false => {
      let len = xs->Js.Array2.length
      let start = start->Option.getWithDefault(0)
      let end = end->Option.getWithDefault(len - 1)
      if start < 0 || start > len - 1 {
        ArgumentOfOfRange(
          `The start index ${start->Belt.Int.toString} is outside the array bounds.`,
        )->raise
      }
      if end < 0 || end > len - 1 {
        ArgumentOfOfRange(
          `The end index ${start->Belt.Int.toString} is outside the array bounds.`,
        )->raise
      }
      range(~start, ~end)->map(inx => xs->Js.Array2.unsafe_get(inx))
    }
  }
}

let rec fromList = xs => {
  (. ()) =>
    switch xs {
    | list{} => End
    | list{x, ...xs} => Next(x, fromList(xs))
    }
}

let fromOption = opt =>
  switch opt {
  | None => empty
  | Some(x) => singleton(x)
  }

let mapi = (xs, f) => xs->indexed->map(((x, index)) => f(~value=x, ~index))

let takeAtMost = (xs, count) => {
  if count == 0 {
    empty
  } else {
    let rec go = xs =>
      xs->mapNext(((x, index), xs) =>
        switch index >= count {
        | true => End
        | false => Next(x, go(xs))
        }
      )
    go(xs->indexed)
  }
}

let headTails = xs =>
  unfold(xs, xs => xs->headTail->Option.flatMap(((_, xs) as ht) => Some(ht, xs)))

let snd = ((_, b)) => b

let drop = (xs, count) =>
  switch count {
  | 0 => xs
  | n if n < 0 =>
    ArgumentOfOfRange(
      `'drop' requires a count of zero or more but youu asked for ${count->Belt.Int.toString}`,
    )->raise
  | count =>
    xs
    ->headTails
    ->indexed
    ->find(Indexed.indexEquals(_, count - 1))
    ->Option.map(Indexed.value)
    ->Option.map(snd)
    ->Option.getWithDefault(empty)
  }

let rec filter = (xs, f) =>
  (. ()) => {
    switch xs->nextNode {
    | End => End
    | Next(x, xs) =>
      switch f(x) {
      | true => Next(x, filter(xs, f))
      | false =>
        switch xs->headTails->find(((x, _)) => f(x)) {
        | None => End
        | Some((x, xs)) => Next(x, filter(xs, f))
        }
      }
    }
  }

let filteri = (xs, f) =>
  xs->indexed->filter(((value, index)) => f(~value, ~index))->map(((v, _)) => v)

let rec takeWhile = (xs, predicate) =>
  xs->mapNext((x, xs) =>
    switch predicate(x) {
    | false => End
    | true => Next(x, takeWhile(xs, predicate))
    }
  )

let rec takeUntil = (xs, f) =>
  xs->mapNext((x, xs) => {
    switch f(x) {
    | false => Next(x, takeUntil(xs, f))
    | true => Next(x, empty)
    }
  })

let filterMap = (xs, f) => xs->map(f)->filter(Option.isSome)->map(Option.getUnsafe)

let filterSome = xs => xs->filterMap(x => x)

let filterOk = xs =>
  xs->filterMap(x =>
    switch x {
    | Ok(ok) => Some(ok)
    | Error(_) => None
    }
  )

let scani = (xs, ~zero, f) => {
  let rec go = (xs, sum) =>
    (. ()) =>
      switch xs->nextNode {
      | End => End
      | Next((x, index), xs) => {
          let sum = f(~sum, ~value=x, ~index)
          Next(sum, go(xs, sum))
        }
      }
  concat(singleton(zero), go(xs->indexed, zero))
}

let scan = (xs, zero, f) => scani(xs, ~zero, (~sum, ~value, ~index as _) => f(sum, value))

let rec sortedMerge = (xs, ys, cmp) => {
  (. ()) =>
    switch (xs(.), ys(.)) {
    | (End, Next(_, _) as ys) => ys
    | (Next(_, _) as xs, End) => xs
    | (Next(x, xs), Next(y, ys)) => {
        let order = cmp(x, y)
        if order <= 0 {
          Next(x, sortedMerge(xs, concat(y->singleton, ys), cmp))
        } else {
          Next(y, sortedMerge(concat(x->singleton, xs), ys, cmp))
        }
      }
    | (End, End) => End
    }
}

let intersperse = (xs, separator) =>
  xs
  ->mapi((~value, ~index) => index == 0 ? singleton(value) : singleton(value)->startWith(separator))
  ->flatten

module UncurriedDeferred = {
  type t<'a> = (. unit) => 'a

  type toLazy<'a> = t<'a> => Lazy.t<'a>
  let toLazy: toLazy<'a> = (f: t<'a>) => {
    let g = () => f(.)
    Lazy.from_fun(g)
  }

  type fromLazy<'a> = Lazy.t<'a> => t<'a>
  let fromLazy: fromLazy<'a> = f => (. ()) => Lazy.force(f)

  type memoize<'a> = t<'a> => t<'a>
  let memoize: memoize<'a> = f => f->toLazy->fromLazy
}

let rec cache = seq =>
  UncurriedDeferred.memoize((. ()) =>
    switch seq->nextNode {
    | End => End
    | Next(value, seq) => Next(value, cache(seq))
    }
  )

let allPairs = (xx, yy) => xx->flatMap(x => yy->map(y => (x, y)))

let dropUntil = (xs, predicate) =>
  (. ()) =>
    xs
    ->headTails
    ->find(((x, _)) => predicate(x))
    ->Option.map(((x, xs)) => Node.next(x, xs))
    ->Option.getWithDefault(Node.end)

let dropWhile = (xs, predicate) =>
  (. ()) =>
    xs
    ->headTails
    ->find(((x, _)) => false == predicate(x))
    ->Option.map(((x, xs)) => Next(x, xs))
    ->Option.getWithDefault(Node.end)

let chunkBySize = (xs, length) => {
  if length <= 0 {
    ArgumentOfOfRange(
      `chunkBySize requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  xs
  ->map(i => Some(i))
  ->concat(replicate(~count=length - 1, ~value=None))
  ->scani(~zero=[], (~sum, ~value, ~index) => {
    switch value {
    | None => sum
    | Some(value) =>
      switch mod(index, length) {
      | 0 => [value]
      | _ =>
        sum->Js.Array2.push(value)->ignore
        sum
      }
    }
  })
  ->filteri((~value as _, ~index) => mod(index, length) == 0)
  ->drop(1)
}

let window = (xs, length) => {
  if length <= 0 {
    ArgumentOfOfRange(
      `windowed requires a length > 0. You asked for ${length->Belt.Int.toString}`,
    )->raise
  }
  xs
  ->scani(~zero=[], (~sum, ~value, ~index as _) => {
    if Js.Array2.length(sum) >= length {
      sum->Js.Array2.shift->ignore
    }
    sum->Js.Array2.push(value)->ignore
    sum
  })
  ->filter(i => Js.Array2.length(i) == length)
}

let pairwise = xs =>
  xs->window(2)->map(i => (i->Js.Array2.unsafe_get(0), i->Js.Array2.unsafe_get(1)))

let reduce = (xs, zero, concat) => {
  let sum = ref(zero)
  xs->forEach(x => sum := concat(sum.contents, x))
  sum.contents
}

let reducei = (xs, zero, concat) =>
  xs->indexed->reduce(zero, (sum, (value, index)) => concat(~sum, ~value, ~index))

let last = xs => xs->reduce(None, (_, x) => Some(x))

let toArray = xs =>
  xs->reduce([], (xs, i) => {
    xs->Js.Array2.push(i)->ignore
    xs
  })

let toString = xs => xs->reduce("", (total, i) => total ++ i)

let forEachi = (xs, f) => xs->indexed->forEach(((value, index)) => f(~value, ~index))

let some = (xs, f) => xs->find(f)->Option.isSome

let everyOrEmpty = (xs, f) => xs->find(i => !f(i))->Option.isNone

let findMapi = (xs, f) =>
  xs
  ->mapi((~value, ~index) => f(~value, ~index))
  ->find(Option.isSome(_))
  ->Option.map(Option.getUnsafe)

let findMap = (xs, f) => findMapi(xs, (~value, ~index as _) => f(value))

let rec map2 = (xx, yy, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    Ex.Option.map2(xx, yy, ((x, xx), (y, yy)) => Next(
      f(x, y),
      map2(xx, yy, f),
    ))->Option.getWithDefault(End)
  }

let rec map3 = (xx, yy, zz, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    let zz = zz->next
    Ex.Option.map3(xx, yy, zz, ((x, xx), (y, yy), (z, zz)) => Next(
      f(x, y, z),
      map3(xx, yy, zz, f),
    ))->Option.getWithDefault(End)
  }

let rec map4 = (xx, yy, zz, qq, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    let zz = zz->next
    let qq = qq->next
    Ex.Option.map4(xx, yy, zz, qq, ((x, xx), (y, yy), (z, zz), (q, qq)) => Next(
      f(x, y, z, q),
      map4(xx, yy, zz, qq, f),
    ))->Option.getWithDefault(End)
  }

let rec map5 = (xx, yy, zz, qq, mm, f) =>
  (. ()) => {
    let xx = xx->next
    let yy = yy->next
    let zz = zz->next
    let qq = qq->next
    let mm = mm->next
    Ex.Option.map5(xx, yy, zz, qq, mm, ((x, xx), (y, yy), (z, zz), (q, qq), (m, mm)) => Next(
      f(x, y, z, q, m),
      map5(xx, yy, zz, qq, mm, f),
    ))->Option.getWithDefault(End)
  }

let zip = (xx, yy) => map2(xx, yy, (x, y) => (x, y))
let zip3 = (xx, yy, zz) => map3(xx, yy, zz, (x, y, z) => (x, y, z))
let zip4 = (xx, yy, zz, qq) => map4(xx, yy, zz, qq, (x, y, z, q) => (x, y, z, q))
let zip5 = (xx, yy, zz, qq, mm) => map5(xx, yy, zz, qq, mm, (x, y, z, q, m) => (x, y, z, q, m))

let equals = (xs, ys, eq) => {
  let xs = xs->map(x => Some(x))->endWith(None)
  let ys = ys->map(y => Some(y))->endWith(None)
  zip(xs, ys)->everyOrEmpty(((x, y)) =>
    switch (x, y) {
    | (Some(x), Some(y)) => eq(x, y)
    | (None, None) => true
    | _ => false
    }
  )
}

let compare = (xs, ys, cmp) => {
  let xs = xs->map(x => Some(x))->endWith(None)
  let ys = ys->map(y => Some(y))->endWith(None)
  zip(xs, ys)
  ->map(((x, y)) =>
    switch (x, y) {
    | (Some(x), Some(y)) => cmp(x, y)
    | (None, Some(_)) => -1
    | (Some(_), None) => 1
    | (None, None) => 0
    }
  )
  ->find(i => i !== 0)
  ->Option.getWithDefault(0)
}

let length = xs => xs->reduce(0, (sum, _) => sum + 1)

let isEmpty = xs =>
  switch xs->nextNode {
  | End => true
  | _ => false
  }

let tail = xs => xs->drop(1)

let minBy = (xs, cmp) =>
  xs->reduce(None, (sum, x) => {
    switch sum {
    | None => Some(x)
    | Some(sum) => Some(cmp(x, sum) < 0 ? x : sum)
    }
  })

let maxBy = (xs, cmp) =>
  xs->reduce(None, (sum, x) => {
    switch sum {
    | None => Some(x)
    | Some(sum) => Some(cmp(x, sum) > 0 ? x : sum)
    }
  })

let rec interleave = (xs, ys) => {
  (. ()) => {
    switch xs->nextNode {
    | End => ys->nextNode
    | Next(x, xs) => Next(x, interleave(ys, xs))
    }
  }
}

let interleaveMany = xxs => {
  switch xxs->Js.Array2.length {
  | 0 => empty
  | length => {
      let xxs = xxs->Js.Array2.map(i => Some(i))
      let remain = ref(length)
      let consumeHead = inx => {
        xxs
        ->Js.Array2.unsafe_get(inx)
        ->Option.flatMap(xs => {
          switch xs->headTail {
          | None =>
            remain := remain.contents - 1
            xxs->Js.Array2.unsafe_set(inx, None)
            None
          | Some(h, t) =>
            xxs->Js.Array2.unsafe_set(inx, Some(t))
            Some(h)
          }
        })
      }
      range(~start=0, ~end=length - 1)
      ->cycle
      ->map(consumeHead)
      ->takeWhile(_ => remain.contents > 0)
      ->filterSome
    }
  }
}

let toExactlyOne = xs =>
  switch xs->headTail {
  | None => None
  | Some(x, xs) =>
    switch xs->isEmpty {
    | true => Some(x)
    | false => None
    }
  }

let isSortedBy = (xs, cmp) => xs->pairwise->everyOrEmpty(((a, b)) => cmp(a, b) <= 0)

// stopped here!
let windowBehind = (xs, size) => {
  if size <= 0 {
    ArgumentOfOfRange(`windowBehind requires a size greater than zero.`)->raise
  } else {
    xs
    ->scan([], (sum, i) => {
      if sum->Js.Array2.length === size {
        sum->Js.Array2.shift->ignore
      }
      sum->Js.Array2.push(i)->ignore
      sum
    })
    ->drop(1)
  }
}

let windowAhead = (xs, size) => {
  if size <= 0 {
    ArgumentOfOfRange(`windowAhead requires a size greater than zero.`)->raise
  } else {
    xs
    ->map(i => Some(i))
    ->concat(replicate(~count=size - 1, ~value=None))
    ->scani(~zero=[], (~sum, ~value as i, ~index) => {
      if index >= size {
        sum->Js.Array2.shift->ignore
      }
      i->Option.forEach(i => sum->Js.Array2.push(i)->ignore)
      sum
    })
    ->drop(size)
  }
}

let allOk = xs => {
  xs
  ->scan(Ok(empty), (sum, x) =>
    switch x {
    | Ok(ok) => sum->Result.map(oks => concat(oks, singleton(ok)))
    | Error(_) as err => err
    }
  )
  ->takeUntil(Result.isError(_))
  ->last
  ->Option.getUnsafe
}

let allSome = xs => {
  xs
  ->scan(Some(empty), (sum, x) =>
    switch x {
    | Some(ok) => sum->Option.map(oks => concat(oks, singleton(ok)))
    | None => None
    }
  )
  ->takeUntil(Option.isNone(_))
  ->last
  ->Option.flatMap(i => i)
}

let toOption = xs =>
  switch xs->nextNode {
  | End => None
  | Next(x, xs) => Some(xs->startWith(x))
  }

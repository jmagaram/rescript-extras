@genType
type t<'a> = ('a, 'a) => float

let eval = (t, x, y) => t(x, y)
let fromIntResult = (cmp, x, y) => cmp(x, y)->Int.toFloat
let fromMap = (f, t, x, y) => eval(t, f(x), f(y))

let reverse = (t, x, y) => eval(t, y, x)

let eq = (t, x, y) => eval(t, x, y) === 0.0
let neq = (t, x, y) => eval(t, x, y) !== 0.0

let lt = (t, x, y) => eval(t, x, y) < 0.0
let lte = (t, x, y) => eval(t, x, y) <= 0.0
let gt = (t, x, y) => eval(t, x, y) > 0.0
let gte = (t, x, y) => eval(t, x, y) >= 0.0

let min = (t, x, y) => lte(t, x, y) ? x : y
let max = (t, x, y) => lte(t, x, y) ? y : x

let bool = (a: bool, b: bool) => a < b ? -1.0 : a > b ? 1.0 : 0.0

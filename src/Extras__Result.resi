let fromArrayMap: (array<'a>, 'a => result<'b, 'c>) => result<array<'b>, 'c>
let fromArray: array<result<'a, 'b>> => result<array<'a>, 'b>
let mapError: (result<'a, 'b>, 'b => 'c) => result<'a, 'c>
let fromTryCatch: (unit => 'a) => result<'a, exn>
let toOption: result<'a, 'b> => option<'a>
let getError: result<'a, 'b> => option<'b>

let fromOption: option<'a> => array<'a>
@val external of1: 'a => array<'a> = "Array.of"
let isEmpty: array<'a> => bool
let isNotEmpty: array<'a> => bool
let exactlyOne: array<'a> => option<'a>
let head: array<'a> => option<'a>
let last: array<'a> => option<'a>
let lastIndex: array<'a> => option<int>
let prepend: (array<'a>, array<'a>) => array<'a>
let filterSome: array<option<'a>> => array<'a>
let unfold: ('a, 'a => option<('b, 'a)>) => array<'b>

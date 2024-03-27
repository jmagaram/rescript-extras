module Test = Extras__Test
module Pattern = Extras__Pattern
module Unknown = Extras__Unknown
module Literal = Extras__Literal

// ================================================================================
// Ensure basic int, string, date, and user-defined literals can be pattern matched
// ================================================================================

let isTypeOfTest = (~title, ~guard, ~ok, ~invalid1, ~invalid2, ~invalid3) =>
  Test.fromPredicate(~category="Patterns", ~title, ~expectation="isTypeOf", () =>
    true == ok->Array.every(i => i->Unknown.make->guard) &&
    false == invalid1->Unknown.make->guard &&
    false == invalid2->Unknown.make->guard &&
    false == invalid3->Unknown.make->guard
  )

let areEqual = (~title, ~equals, ~expectation, ~a, ~b) =>
  Test.fromPredicate(~category="Patterns", ~title=`${title} equals`, ~expectation, () =>
    equals(a, b) && equals(b, a)
  )

let areNotEqual = (~title, ~equals, ~expectation, ~a, ~b) =>
  Test.fromPredicate(~category="Patterns", ~title=`${title} equals`, ~expectation, () =>
    !equals(a, b) && !equals(b, a)
  )

module Negative1 = Literal.MakeInt({
  let value = -1
})

module Yes = Literal.MakeString({
  let value = "yes"
  let trimmed = true
  let caseInsensitive = true
})

// Option, Nullable, and Null wrappers
module OptionString = Pattern.MakeOption(Pattern.String)
module NullableString = Pattern.MakeNullable(Pattern.String)
module NullInt = Pattern.MakeNull(Pattern.Int)

// Tuples
module IntStringTuple = Pattern.MakeTuple2({
  module A = Pattern.Int
  module B = Pattern.String
})

let tests = {
  [
    isTypeOfTest(
      ~title="Null (null literal)",
      ~guard=Literal.Null.isTypeOf,
      ~ok=[null->Obj.magic],
      ~invalid1=false,
      ~invalid2=33,
      ~invalid3="abc",
    ),
    isTypeOfTest(
      ~title="True (bool literal)",
      ~guard=Literal.True.isTypeOf,
      ~ok=[true, Literal.True.value->Obj.magic],
      ~invalid1=false,
      ~invalid2=33,
      ~invalid3="abc",
    ),
    isTypeOfTest(
      ~title="Yes (string literal)",
      ~guard=Yes.isTypeOf,
      ~ok=["yes", "  YES", "  yEs"],
      ~invalid1="no",
      ~invalid2=33,
      ~invalid3=false,
    ),
    isTypeOfTest(
      ~title="Negative1 (int literal)",
      ~guard=Negative1.isTypeOf,
      ~ok=[-1],
      ~invalid1=0,
      ~invalid2=3.4,
      ~invalid3="abc",
    ),
    isTypeOfTest(
      ~title="Int",
      ~guard=Pattern.Int.isTypeOf,
      ~ok=[1, -1, 34, Int32.max_int],
      ~invalid1="abc",
      ~invalid2=false,
      ~invalid3={"a": 1},
    ),
    isTypeOfTest(
      ~title="String",
      ~guard=Pattern.String.isTypeOf,
      ~ok=["abc", "", "   a b c"],
      ~invalid1=false,
      ~invalid2=43,
      ~invalid3=4.3,
    ),
    isTypeOfTest(
      ~title="Bool",
      ~guard=Pattern.Bool.isTypeOf,
      ~ok=[true, false],
      ~invalid1=43,
      ~invalid2="abc",
      ~invalid3=4.3,
    ),
    isTypeOfTest(
      ~title="Date",
      ~guard=Pattern.Date.isTypeOf,
      ~ok=[Date.now()->Date.fromTime],
      ~invalid1="abc",
      ~invalid2=3,
      ~invalid3=Date.fromString("abc"),
    ),
    isTypeOfTest(
      ~title="MakeOption (string)",
      ~guard=OptionString.isTypeOf,
      ~ok=[Some("abc"), None, Some("")],
      ~invalid1=1,
      ~invalid2=false,
      ~invalid3=4.5,
    ),
    areNotEqual(
      ~title="MakeOption (string)",
      ~equals=OptionString.equals,
      ~expectation="when one undefined => false",
      ~a=None,
      ~b=Some("abc"),
    ),
    areEqual(
      ~title="MakeOption (string)",
      ~equals=OptionString.equals,
      ~expectation="when both undefined => true",
      ~a=None,
      ~b=None,
    ),
    areNotEqual(
      ~title="MakeOption (string)",
      ~equals=OptionString.equals,
      ~expectation="when both Some but values different => false",
      ~a=Some("xyz"),
      ~b=Some("abc"),
    ),
    areEqual(
      ~title="MakeOption (string)",
      ~equals=OptionString.equals,
      ~expectation="when both Some but values same => true",
      ~a=Some("xyz"),
      ~b=Some("xyz"),
    ),
    isTypeOfTest(
      ~title="MakeNullable (string)",
      ~guard=NullableString.isTypeOf,
      ~ok=[
        Nullable.make("abc"),
        Nullable.make(null->Obj.magic),
        Nullable.make(undefined->Obj.magic),
      ],
      ~invalid1=1,
      ~invalid2=false,
      ~invalid3=4.5,
    ),
    areEqual(
      ~title="MakeNullable (string)",
      ~expectation="when both undefined => true",
      ~a=Nullable.undefined,
      ~b=Nullable.undefined,
      ~equals=NullableString.equals,
    ),
    areEqual(
      ~title="MakeNullable (string)",
      ~expectation="when both null => true",
      ~a=null,
      ~b=null,
      ~equals=NullableString.equals,
    ),
    areNotEqual(
      ~title="MakeNullable (string)",
      ~expectation="when one undefined and other null => false",
      ~a=null,
      ~b=Nullable.undefined,
      ~equals=NullableString.equals,
    ),
    areEqual(
      ~title="MakeNullable (string)",
      ~expectation="when same string => true",
      ~a="abc"->Nullable.make,
      ~b="abc"->Nullable.make,
      ~equals=NullableString.equals,
    ),
    areNotEqual(
      ~title="MakeNullable (string)",
      ~expectation="when different string => false",
      ~a="abc"->Nullable.make,
      ~b="xyz"->Nullable.make,
      ~equals=NullableString.equals,
    ),
    areNotEqual(
      ~title="MakeNullable (string)",
      ~expectation="when one undefined and other is string => false",
      ~a="abc"->Nullable.make,
      ~b=Nullable.undefined,
      ~equals=NullableString.equals,
    ),
    areNotEqual(
      ~title="MakeNullable (string)",
      ~expectation="when one null and other is string => false",
      ~a="abc"->Nullable.make,
      ~b=null,
      ~equals=NullableString.equals,
    ),
    Test.fromPredicate(
      ~category="Patterns",
      ~title="MakeNullable (string)",
      ~expectation="null != undefined != abc for built-in Nullable using == or ===",
      () => {
        let a: Nullable.t<string> = null->Obj.magic
        let b: Nullable.t<string> = undefined->Obj.magic
        let c = Nullable.make("abc")
        a !== b && a !== c && b !== c && a != b && a != c && b != c
      },
    ),
    isTypeOfTest(
      ~title="MakeNull (int)",
      ~guard=NullInt.isTypeOf,
      ~ok=[3->Null.make, 6->Null.make, Null.null],
      ~invalid1="abc",
      ~invalid2=false,
      ~invalid3={"a": 1},
    ),
    areEqual(
      ~title="MakeNull (int)",
      ~equals=NullInt.equals,
      ~expectation="when both null => true",
      ~a=Null.null,
      ~b=Null.null,
    ),
    areEqual(
      ~title="MakeNull (int)",
      ~equals=NullInt.equals,
      ~expectation="when both same non-null value => true",
      ~a=3->Null.make,
      ~b=3->Null.make,
    ),
    areNotEqual(
      ~title="MakeNull (int)",
      ~equals=NullInt.equals,
      ~expectation="when one is null => false",
      ~a=Null.null,
      ~b=3->Null.make,
    ),
    areNotEqual(
      ~title="MakeNull (int)",
      ~equals=NullInt.equals,
      ~expectation="when both different non-null value => false",
      ~a=5->Null.make,
      ~b=3->Null.make,
    ),
    isTypeOfTest(
      ~title="Tuple",
      ~guard=IntStringTuple.isTypeOf,
      ~ok=[(1, "abc"), (4, "x")],
      ~invalid1="abc",
      ~invalid2=false,
      ~invalid3=(1, "abc", false), // too long
    ),
  ]
}

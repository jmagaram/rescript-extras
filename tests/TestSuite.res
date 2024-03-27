module Test = Extras__Test
module Task = Extras__Task
module Ex = Extras

let isLocalDevelopment = () => {
  try {
    let isLocal: bool = %raw(`process.env.NODE_ENV === "development"`)
    isLocal
  } catch {
  | _ => false
  }
}

let onlyShowFailures = false
let filter = test => [""]->Array.every(word => test->Test.hasKeyword(word))
let throwOnFailure = !isLocalDevelopment()

let tests =
  [
    Extras__CmpTests.tests,
    Extras__ArrayTests.tests,
    Extras__OptionTests.tests,
    Extras__ResultTests.allTests,
    Extras__TaskTest.tests,
    Extras__NonEmptyArrayTests.tests,
    Extras__LiteralTests.tests,
    Extras__UnionTests.tests,
    Extras__PatternTests.tests,
    Extras__UnknownTests.tests,
    Extras__TrampolineTests.tests,
  ]->Array.flat

Task.Result.make(
  ~promise=() => Ex.Test.runSuite(tests, ~filter, ~onlyShowFailures),
  ~onError=e => e,
)
->Task.forEach(s =>
  switch (s, throwOnFailure) {
  | (Ok(s), true) if s.fail > 0 => Exn.raiseError(`Tests failed: ${s.fail->Int.toString}`)
  | (Error(_), true) => Exn.raiseError("Could not complete the test suite.")
  | _ => ()
  }
)
->Task.toPromise
->ignore

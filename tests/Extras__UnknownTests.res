module T = Extras__Test
module U = Extras__Unknown

let tests = [
  T.fromPredicate(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when null => true",
    () => null->U.isNullOrUndefined == true,
  ),
  T.fromPredicate(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when undefined => true",
    () => undefined->U.isNullOrUndefined == true,
  ),
  T.fromPredicate(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when a number => false",
    () => 43->U.isNullOrUndefined == false,
  ),
  T.fromPredicate(
    ~category="Unknown",
    ~title="isNullOrUndefined",
    ~expectation="when some letters => false",
    () => "abc"->U.isNullOrUndefined == false,
  ),
  T.fromPredicate(
    ~category="Unknown",
    ~title="getBool",
    ~expectation="when property exists => Some",
    () => {"a": true}->U.getBool("a") == Some(true),
  ),
  T.fromPredicate(
    ~category="Unknown",
    ~title="getBool",
    ~expectation="when property exists => Some",
    () => {"a": false}->U.getBool("a") == Some(false),
  ),
  T.fromPredicate(
    ~category="Unknown",
    ~title="getBool",
    ~expectation="when object is null => None",
    () => null->U.getBool("a")->Option.isNone,
  ),
]

## Version 0.13.0

- Switch to `commonjs` module format; supposedly this works better for Rescript libraries
- Examples of using [rescript-struct](https://github.com/DZakh/rescript-struct) for unions, and compared to using functors in this package.

## Version 0.12.0

### Union

- Rename `match` to `matchABC` so if a convenience function is written to wrap the `onA`, `onB`, etc. it can use the friendlier `match` word.
- More tests to make sure they work and demonstrate how to use them in practice. Include examples with convenience functions so you don't have to remember or pattern match on `A`, `B`, and `C`.
- Add convenience functions to pattern match on a single case, like `toA` and `toB`.
- Fix bug in basic int, string, float, etc. patterns that made them unusable because the types were abstract.
- Fix bug where matching input could be anything but should have been just the union type.

### Other

- Fix bug in `Unknown.isNullOrUndefined`; only checked for undefined
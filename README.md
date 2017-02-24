# monk

This is an attempt at porting the Abbot interface for working with abstract
binding trees from SML to Haskell.

It leaves something to be desired in it's current state. I'm still not quite
sure how best to represent the abstraction the best in Haskell.

This work is unfinished, and should really just be considered an experiment in
it's current state. I have tried reimplementing the Abbot interface in two
different ways: one using type classes, and one using ADTs of interface
functions.

- The first try (with type classes) I got stuck and couldn't finish the
  implementation.

- The second try is "done" in the sense that it type checks and should work. I
  haven't tested it, but the logic is just copied from the Abbot implementation,
  so it's probably correct.

  This implementation ended up not looking like very idiomatic Haskell. See the
  source yourself, it's pretty wonky looking.

For now, I'll probably just be using [Bound] for representing binding, because
the library seems to be decently popular. Bound seems to be more similar to a
pure de Bruijn representation, rather than a locally nameless implementation,
but it should be fine.

## License

If you end up building on top of this code, please let me know! I'd be curious
to see what you come up with.

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://jez.io/MIT-LICENSE.txt)


[Bound]: http://hackage.haskell.org/package/bound-1.0.4/

# mooring 0.1.5

* Switch to S7 object scheme.
* Change `anchor()`, dropping the `depth` parameter.
* Change `mooring()`, gaining the `waterDepth` parameter.
* Rename `plot()` as `draw()`, as a hopefully temporary measure until the
  proper way of documenting S7 generics becomes clearer. (See
  https://stat.ethz.ch/pipermail/r-package-devel/2024q1/010266.html, but note
  that my comment on Jan 3 20:38:17 CET 2024 was incorrect: I realized later
  that I could build but then not document, or document but then not build.
  But I was just trying different syntax options willy-nilly, with no real
  understanding. I gave up and renamed the function, on the assumption that
  somebody will document how this is meant to be done, at some time in the near
  future.)

# mooring 0.1.4

* Fix major error in buoyancy calculation. See
  https://github.com/dankelley/mooring/issues/58 for details.

# mooring 0.1.3

* `knockdown()` uses an iteration method to improve accuracy.

# mooring 0.1.2

* Add `app2bs()`, which has a cleaner interface than `app2()`.

# mooring 0.1.1

* Add `clamped()`.
* Add `is.wire()` and similar functions.
* Add 8x8x16 cinder/concrete block anchors.
* Add CMAR components.
* Change `mooring()` documentation by improving plots.
* Change `plot.mooring(..., which="shape")` to draw the anchor.

# mooring 0.1.0

* Version prior to mid-October 2023.

# mooring 0.1.11

* Rewrite `plot` and `summary` functions to silence a warning about a mismatch
  with the base-R versions (issue #67)

# mooring 0.1.10

* Improve anchor drawing (issue #66).

# mooring 0.1.9

* Improve `clamped()` example.

# mooring 0.1.8

* Change `knockdown()` to increase speed by approximately 40X.

# mooring 0.1.7

* Rename `draw()` to `plot()`, undoing the change made in the 0.1.5 version.
* Rename `printMooring()` to `summary()`.  It really is a summary, not a
  printout, and we want users to know that they can examine the contents in the
  normal way, by naming the item.

# mooring 0.1.6

* Change `app2bs()` to increase processing speed, mainly by using coarser
  segments.
* Change `knockdown()` convergence criterion to focus on angles.
* Rename `discretise()` as `segmentize()`. (See
  https://github.com/dankelley/mooring/issues/61.)

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

# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Decompose chain and wire portions into shorter segments
#'
#' Break up `chain` and `wire` portions of a mooring into smaller
#' chunks, so that the deformation by a current can be traced more
#' accurately by [knockdown()].
#'
#' @template mTemplate
#'
#' @param by numeric value giving the rough size of the chunks,
#' which is 1m by default. This is considered in the context of
#' the total length of the element, `L` say. If `L/by` exceeds
#' 20, then chunks of length `by` are used.  Otherwise,
#' 20 chunks, each of length `L/20`, are used.
#'
#' @template debugTemplate
#'
#' @return `segmentize` returns a `"mooring"` object, based
#' on `m` except that wire portions are chopped up into shorter
#' pieces.
#'
#' @export
#'
#' @author Dan Kelley
segmentize <- function(m, by = 1, debug = 0) {
    byOrig <- by
    mooringDebug(debug, "segmentize() {\n")
    if (!is.mooring(m)) {
        stop("only works for objects created by mooring()")
    }
    if (by <= 0) {
        stop("by must be a positive number")
    }
    elementsNew <- list() # copy elements into this, then make mooring from it
    group <- 1L
    for (element in m@elements) {
        by <- byOrig
        if (is.wire(element) || is.chain(element)) {
            mooringDebug(debug, "  handling \"", gsub(".*:(.*)S7", "\\1", class(element)[1]), "\"\n", sep = "")
            height <- element@height
            n <- as.integer(1 + floor(height / by))
            mooringDebug(debug, "    initially, height=", height, ", by=", by, ", n=", n, "\n", sep="")
            # Ensure at least 20 chunks
            if (n < 20L) {
                n <- 20L
                by <- height / n
                # message("  LATER:", height, ", by=", by, ", n=", n)
            }
            mooringDebug(debug, "    later, height=", height, ", by=", by, ", n=", n, "\n", sep="")
            portion <- element
            portion@height <- height / n
            portion@area <- portion@area / n
            portion@buoyancy <- portion@buoyancy / n
            portion@group <- group # so we can undo this later
            for (i in seq_len(n)) {
                elementsNew[[1L + length(elementsNew)]] <- portion
            }
            group <- group + 1L
        } else {
            elementsNew[[1L + length(elementsNew)]] <- element
        }
    }
    # FIXME: why is rev() required here? Is it an S7-change issue?
    rval <- mooringS7(rev(elementsNew), waterDepth = m@waterDepth)
    rval@elements <- rev(rval@elements)
    # Compute z and tau values. (Leave x values alone.)
    # OLD z <- rev(-rval[[length(rval)]]$depth + cumsum(sapply(rval, \(x) x$height)))
    tau <- tension(rval, stagnant = TRUE) # FIXME: ok?
    zz <- -m@waterDepth
    for (i in rev(seq_along(rval@elements))) {
        zz <- zz + rval@elements[[i]]@height
        rval@elements[[i]]@z <- zz # z is defined at TOP of item
        rval@elements[[i]]@tau <- tau[i]
    }
    mooringDebug(debug, "} # segmentize()\n")
    rval
} # segmentize

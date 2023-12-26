# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Discretise chain and wire portions of a mooring
#'
#' Break up `chain` and `wire` portions of a mooring into smaller chunks,
#' so that the deformation by a current can be traced more
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
#' @return an object of the `"mooring"` class, identical
#' to `m` except that wire portions are chopped up into shorter
#' pieces.
#'
#' @export
#' @aliases discretize
#'
#' @author Dan Kelley
discretise <- function(m, by = 1, debug = 0) {
    mooringDebug(debug, "discretise() {\n")
    if (!is.mooring(m)) {
        print(class(m))
        stop("only works for objects created by mooring()")
    }
    if (by <= 0) {
        stop("by must be a positive number")
    }
    n <- length(m)
    rval <- list()
    class(rval) <- "mooring" # FIXME: this is tedious; perhaps switch to S4 system
    group <- 1L
    for (item in m) {
        mooringDebug(debug, "  handling item of class c(\"", paste(class(item), collapse = "\", \""), "\")\n", sep = "")
        if (inherits(item, "wire") || inherits(item, "chain")) {
            height <- item$height
            n <- as.integer(1 + floor(height / by))
            # message(    "INITIAL: height=", height, ", by=", by, ", n=", n)
            # Ensure at least 20 chunks
            if (n < 20L) {
                n <- 20L
                by <- height / n
                # message("  LATER:", height, ", by=", by, ", n=", n)
            }
            portion <- item
            portion$height <- height / n
            portion$area <- portion$area / n
            portion$buoyancy <- portion$buoyancy / n
            portion$group <- group # so we can undo this later
            for (i in seq_len(n)) {
                rval[[1L + length(rval)]] <- portion
            }
            group <- group + 1L
        } else {
            rval[[1L + length(rval)]] <- item
        }
    }
    nrval <- length(rval)
    waterDepth <- rval[[nrval]]$depth
    # Compute z and tau values. (Leave x values alone.)
    # OLD z <- rev(-rval[[length(rval)]]$depth + cumsum(sapply(rval, function(x) x$height)))
    tau <- tension(rval, stagnant = TRUE) # FIXME: ok?
    zz <- -waterDepth
    for (i in rev(seq_along(rval))) {
        zz <- zz + rval[[i]]$height
        rval[[i]]$z <- zz # z is defined at TOP of item
        rval[[i]]$tau <- tau[i]
    }
    class(rval) <- "mooring"
    attr(rval, "discretised") <- TRUE
    attr(rval, "waterDepth") <- attr(m, "waterDepth")
    mooringDebug(debug, "} # discretise()\n")
    rval
} # discretise

# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Discretise chain and wire portions of a mooring
#'
#' Break up `chain` and `wire` portions of a mooring into smaller chunks,
#' so that the deformation by a current can be traced more
#' accurately by [knockdown()].
#'
#' @template mTemplate
#'
#' @param by numeric value giving the rough size of the chunks.
#' The actual size is computed as the length of wire, divided
#' by the rounded ratio of that length to `by`. For example,
#' using `by=10` with a 95-m length of wire will result
#' in chunks of length 9.5m, not 10m.  In shallow water moorings,
#' the default value of 1m makes sense, but larger values
#' might be employed for moorings in the deep ocean. If `by`
#' exceeds the height of a wire portion, then that portion is not
#' subdivided.
#'
#' @return an object of the `"mooring"` class, identical
#' to `m` except that wire portions are chopped up into shorter
#' pieces.
#'
#' @export
#' @aliases discretize
#'
#' @author Dan Kelley
discretise <- function(m, by = 1) {
    if (!isMooring(m)) {
        stop("only works for objects created by mooring()")
    }
    if (by <= 0) {
        stop("by must be a positive number")
    }
    n <- length(m)
    rval <- list()
    group <- 1
    for (item in m) {
        isWire <- inherits(item, "wire")
        isChain <- inherits(item, "chain")
        if (isWire || isChain) {
            height <- item$height
            n <- max(1L, as.integer(round(height / by)))
            portion <- item
            portion$height <- height / n
            portion$area <- portion$area / n
            portion$buoyancy <- portion$buoyancy / n
            portion$group <- group # so we can undo this later
            for (i in seq_len(n)) {
                rval[[1 + length(rval)]] <- portion
            }
            group <- group + 1
        } else {
            rval[[1 + length(rval)]] <- item
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
    rval
} # discretise

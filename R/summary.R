# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Summarize a mooring
#'
#' @param object a mooring object, created by [mooring()].
#'
#' @param ... ignored.
#'
#' @examples
#' library(mooring)
#' # Simple case
#' m <- mooring(anchor(depth = 100), wire(length = 80), float("HMB 20"))
#' summary(m)
#' # Illustrate how it collects wire subintervals
#' md <- discretise(m)
#' mdk <- knockdown(md, 0.5)
#' summary(mdk)
#'
#' @export
#'
#' @aliases summary.mooring
#'
#' @author Dan Kelley
`summary.mooring::mooring` <- function(object, ...) {
    if (!is.mooring(object)) {
        stop("only works for objects created by mooring()")
    }
    print(m)
    if (FALSE) {
        e <- object@elements
        lastWasWire <- FALSE
        wireLength <- 0
        iWireStart <- 0
        for (i in seq_along(e)) {
            message("summary i=", i)
            ee <- e[[i]]
            if (is.wire(ee)) {
                if (!lastWasWire) {
                    iWireStart <- i
                }
                lastWasWire <- TRUE
                wireLength <- wireLength + ee@height
            } else {
                if (lastWasWire) {
                    # fake an element (and blank out the location)
                    W <- e[[iWireStart]]
                    W@height <- wireLength
                    # cat("iWireStart=", iWireStart, "\n")
                    # cat("DAN...\n");print(W)
                    cat(sprintf("AA %s at z=%.2fm to %.2fm\n", W@model, W@z, W@z - wireLength))
                }
                lastWasWire <- FALSE
                wireLength <- 0
                # cat("BOY..\n")
                cat(sprintf("BB %s at z=%.2fm\n", ee@model, ee@z))
            }
        }
    }
}

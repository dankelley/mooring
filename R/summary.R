# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Summarize a mooring
#'
#' @param object a mooring object, created by [mooring()].
#'
#' @param ... ignored.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 80), float(), waterDepth = 100)
#' summary(m)
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
    print(object)
    #<old> if (FALSE) {
    #<old>     e <- object@elements
    #<old>     lastWasWire <- FALSE
    #<old>     wireLength <- 0
    #<old>     iWireStart <- 0
    #<old>     for (i in seq_along(e)) {
    #<old>         message("summary i=", i)
    #<old>         ee <- e[[i]]
    #<old>         if (is.wire(ee)) {
    #<old>             if (!lastWasWire) {
    #<old>                 iWireStart <- i
    #<old>             }
    #<old>             lastWasWire <- TRUE
    #<old>             wireLength <- wireLength + ee@height
    #<old>         } else {
    #<old>             if (lastWasWire) {
    #<old>                 # fake an element (and blank out the location)
    #<old>                 W <- e[[iWireStart]]
    #<old>                 W@height <- wireLength
    #<old>                 # cat("iWireStart=", iWireStart, "\n")
    #<old>                 # cat("DAN...\n");print(W)
    #<old>                 cat(sprintf("AA %s at z=%.2fm to %.2fm\n", W@model, W@z, W@z - wireLength))
    #<old>             }
    #<old>             lastWasWire <- FALSE
    #<old>             wireLength <- 0
    #<old>             # cat("BOY..\n")
    #<old>             cat(sprintf("BB %s at z=%.2fm\n", ee@model, ee@z))
    #<old>         }
    #<old>     }
    #<old> }
}

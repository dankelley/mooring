# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Access something in a mooring
#'
#' Retrieves values from (a) a mooring element, as created with
#' [float()] or a similar function, or (b) a whole mooring, as created
#' with [mooring()].
#'
#' @template mTemplate
#'
#' @param i either (a) an integer specifying index of item to be returned, or
#' (b) a character string. The first case is used to look up components of
#' a mooring. The second case requires `i` to be `"area"`, `"buoyancy"`,
#' `"CD"`, or `"height"`, and the result is a single value if `m` is
#' an elementary object (example 1) or a whole mooring (example 2).
#'
#' @examples
#' library(mooring)
#' F <- float("HMB 20")
#' F["buoyancy"]
#' m <- mooring(anchor(depth = 120), wire(length = 100), F)
#' m["buoyancy"]
#'
#' @export
#'
#' @author Dan Kelley
`[.mooring` <- function(m, i) {
    known <- c("area", "buoyancy", "CD", "height")
    if (isMooring(m)) {
        if (is.numeric(i)) {
            # message("m[i] with i=",paste(i, collapse=" "))
            i <- subset(i, 0L < i & i <= length(m))
            # message(" >> i=",paste(i, collapse=" "))
            um <- unclass(m)
            rval <- lapply(i, function(mi) um[[mi]])
            class(rval) <- class(m)
            rval
        } else {
            # message("m char")
            if (i %in% known) {
                sapply(m, function(mi) mi[[i]])
            } else {
                stop("\"", i, "\" not handled; try one of: \"", paste(known, collapse = "\", \""), "\"")
            }
        }
    } else {
        if (length(class(m)) != 2) {
            stop("only works for a mooring, or an element of a mooring")
        }
        if (is.numeric(i)) {
            stop("integer lookup is not permitted for elementary objects")
        } else {
            # message("e char")
            if (i %in% known) {
                unclass(m)[[i]]
            } else {
                stop("'", i, "' not handled; try one of: '", paste(known, collapse = "', '"), "'")
            }
        }
    }
}

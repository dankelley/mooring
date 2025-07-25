# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

# Internal function used by is.float(), etc.
# FIXME: does this get used to pinpoint elements?
is.mooringInternal <- function(object, class) {
    # mooring() returns an unnamed list
    if (inherits(object, "mooring::mooringS7")) {
        # message("Case 0: a mooring::mooring")
        return(sapply(object@elements, \(o) is.mooringInternal(o, class)))
    }
    # message("not a mooring::mooring")
    if (!inherits(object, "S7_object")) {
        warning("This is not an S7 object. Please do not use this function in your code.")
        rval <- FALSE
    } else {
        # mooring element
        # message("Case 2: check if a ", class, " object")
        rval <- inherits(object, paste0("mooring::", class, "S7"))
    }
    # message("   returning: ", paste(rval, collapse = " "))
    rval
}

#' Determine which Mooring Elements are Anchors
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `anchor` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120)
#' is.anchor(m)
#'
#' @export
#'
#' @author Dan Kelley
is.anchor <- function(m) {
    is.mooringInternal(m, "anchor")
}

#' Determine which Mooring Elements are Chains
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `chain` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120)
#' is.chain(m)
#'
#' @export
#'
#' @author Dan Kelley
is.chain <- function(m) {
    is.mooringInternal(m, "chain")
}

#' Determine which Mooring Elements are Connectors
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `connector` sub-class.
#'
#' @export
#'
#' @author Dan Kelley
is.connector <- function(m) {
    is.mooringInternal(m, "connector")
}

#' Determine which Mooring Elements are Floats
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `float` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120)
#' is.float(m)
#'
#' @export
#'
#' @author Dan Kelley
is.float <- function(m) {
    # sapply(m, \(mi) inherits(mi, "float"))
    is.mooringInternal(m, "float")
}

#' Determine which Mooring Elements are Instruments
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `instruent` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(
#'     anchor(), wire(length = 100), instrument("RD ADCP"),
#'     wire(length = 100), float("HMB 20"),
#'     waterDepth = 120
#' )
#' is.instrument(m)
#'
#' @export
#'
#' @author Dan Kelley
is.instrument <- function(m) {
    # sapply(m, \(i) inherits(i, "instrument"))
    is.mooringInternal(m, "instrument")
}

#' Determine which Mooring Elements are Misc
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `misc` sub-class.
#'
#' @export
#'
#' @author Dan Kelley
is.misc <- function(m) {
    is.mooringInternal(m, "misc")
}

#' Determine Whether an Object is a Mooring
#'
#' An object is a mooring if it is an unnamed list and if each
#' element in that list is a mooring element created by
#' e.g. [anchor()], [wire()], or a related function.
#' For example, the
#' output of [mooring()] is a mooring, but the output of [anchor()] is not.
#' This function is mainly designed for use within the package that that, e.g.
#' [knockdown()] will produce an error if its first argument is not a mooring.
#'
#' @param m an object to be tested
#'
#' @export
is.mooring <- function(m = NULL) {
    inherits(m, "mooring::mooringS7")
}

#' Determine Whether an Object is a Mooring Element
#'
#' An object is a mooring element if it was created by
#' [anchor()], [wire()], or a related function, each of
#' which returns an element of the `"mooringElement"` class.
#'
#' @param m an object to be tested
#'
#' @export
is.mooringElement <- function(m = NULL) {
    inherits(m, "mooring::mooringElementS7")
}

#' Determine which Mooring Elements are Release
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `release` sub-class.
#'
#' @export
#'
#' @author Dan Kelley
is.release <- function(m) {
    is.mooringInternal(m, "release")
}

#' Determine which Mooring Elements are Wires
#'
#' @template mTemplate
#'
#' @return a logical vector of the same length as `m`, indicating
#' whether each element is of the `wire` sub-class.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 100), float("HMB 20"), waterDepth = 120)
#' is.wire(m)
#'
#' @export
#'
#' @author Dan Kelley
is.wire <- function(m) {
    # sapply(m, \(i) inherits(i, "wire"))
    is.mooringInternal(m, "wire")
}

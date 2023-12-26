# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

# Internal function used by is.float(), etc.  If object is
# created by mooring(), this returns a vector of logicals. Otherwise,
# it returns a single logical.
is.mooringInternal <- function(object, class)
{
    # mooring() returns an unnamed list
    if (!is.list(object)) {
        #message("case 1: not a list")
        rval <- FALSE
    } else if (is.null(names(object))) {
        # a whole mooring
        #message("Case 2: possibly a whole mooring")
        rval <- sapply(object, function(mi) inherits(mi, class))
    } else {
        # mooring element
        #message("Case 3: possibly a mooring element")
        rval <- inherits(object, class)
    }
    #message("   returning: ", paste(rval, collapse = " ")
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
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
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
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
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
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' is.float(m)
#'
#' @export
#'
#' @author Dan Kelley
is.float <- function(m) {
    #sapply(m, \(mi) inherits(mi, "float"))
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
#'     anchor(depth = 120), wire(length = 100), instrument("RD ADCP"),
#'     wire(length = 100), float("HMB 20")
#' )
#' is.instrument(m)
#'
#' @export
#'
#' @author Dan Kelley
is.instrument <- function(m) {
    #sapply(m, \(i) inherits(i, "instrument"))
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
    inherits(m, "mooring")
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
    inherits(m, "mooringElement")
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
#' m <- mooring(anchor(depth = 120), wire(length = 100), float("HMB 20"))
#' is.wire(m)
#'
#' @export
#'
#' @author Dan Kelley
is.wire <- function(m) {
    #sapply(m, \(i) inherits(i, "wire"))
    is.mooringInternal(m, "wire")
}

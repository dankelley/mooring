#' mooring: A Package for Analysing Oceanographic Moorings.
#'
#' The mooring package provides functions for working with
#' oceanographic moorings.
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre cable, and a float.
#' library(mooring)
#' A <- anchor()
#' C <- cable(100)
#' F <- float()
#' m <- A + C + F
#' print(m)
#' plot(m)
#'
#' @docType package
#'
#' @name mooring
NULL




#' Create an anchor object
#'
#' @param model character value indicating the model of the anchor.
#' @return an object of the `"mooring"` class, with `type` equal to `"anchor"`.
#' @family functions that create mooring objects
#' @export
#' @author Dan Kelley
anchor <- function(model="default_anchor")
{
    rval <- list(list(type="anchor", model=model, length=0.3)) # guess on length
    class(rval) <- "mooring"
    rval
}

#' Create a mooring-release object
#'
#' @param model character value indicating the model of the acoustic release.
#' @return an object of the `"mooring"` class, with `type` equal to `"release"`.
#' @family functions that create mooring objects
#' @export
#' @author Dan Kelley
release <- function(model="default_release")
{
    rval <- list(list(type="anchor", model=model, length=1.0)) # guess on length
    class(rval) <- "mooring"
    rval
}

#' Create a cable object
#'
#' @param length numeric value indicating the length of cable, in metres.
#' @param model character value indicating the model of the cable.
#' @return an object of the `"mooring"` class, with `type` equal to `"cable"`.
#' @family functions that create mooring objects
#' @export
#' @author Dan Kelley
cable <- function(length=NULL, model="Mooring Systems 3X19 3/16")
{
    if (is.null(length))
        stop("must specify length")
    rval <- list(list(type="cable", model=model, length=length))
    class(rval) <- "mooring"
    rval
}

#' Create a float object
#'
#' @param model character value indicating the model of the float.
#' @return an object of the `"mooring"` class, with `type` equal to `"float"`.
#' @family functions that create mooring objects
#' @export
#' @author Dan Kelley
float <- function(model="Hydro Float 20")
{
    rval <- list(list(type="float", model=model, length=0.5)) # guess on length
    class(rval) <- "mooring"
    rval
}

#' Combine two mooring objects
#'
#' The first object is "placed" the second.
#' @param m1,m2 objects of the `"mooring"` class.
#' @family functions that create mooring objects
#' @examples
#' A <- anchor()
#' C <- cable(100)
#' F <- float()
#' m <- A + C + F
#' print(m)
#' @export
#' @author Dan Kelley
`+.mooring` <- function(m1, m2)
{
    n1 <- length(m1)
    n2 <- length(m2)
    rval <- vector("list", n1+n2)
    for (i in seq_len(n1))
        rval[[i]] <- m1[[i]]
    for (i in seq_len(n2))
        rval[[n1 + i]] <- m2[[i]]
    class(rval) <- "mooring"
    rval
}

#' Print a mooring object
#'
#' @param x an object of the `"mooring"` class.
#' @param ... optional arguments.
#' @export
#' @author Dan Kelley
print.mooring <- function(x, ...)
{
    n <- length(x)
    if (0 == n) {
        stop("Empty object (programming error)\n")
    } else {
        if (n == 1) {
            cat("Single element:\n")
        } else {
            cat("Mooring with", n, "elements:\n")
        }
        for (i in seq_len(n)) {
            xi <- x[[i]]
            # FIXME: more if blocks for various types, to customize output. For example,
            # cable has buoyancy in kg/m, whereas other things have it in kg.  Also,
            # should report depth ranges, etc.
            if (xi$type == "cable") {
                cat(sprintf("  %s (\"%s\") %gm\n", xi$type, xi$model, xi$length), sep="")
            } else {
                cat(sprintf("  %s (\"%s\") %gm\n", xi$type, xi$model, xi$length), sep="")
            }
        }
    }
    invisible(x)
}

#' Plot a mooring object
#'
#' @param x an object of the `"mooring"` class.
#' @param ... optional arguments.
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre cable, and a float.
#' library(mooring)
#' m <- anchor() + cable(100) + float()
#' plot(m)

#' @importFrom graphics axis box lines mtext par points rect text
#' @export
#' @author Dan Kelley
plot.mooring <- function(x, ...)
{
    l <- cumsum(sapply(x, function(xi) xi$length))
    bottom <- -max(l) # Kludge ... maybe we want a water() function
    z <- bottom + l
    plot(rep(0, length(l)), z, xlim=c(-0.5, 0.5), xlab="", ylab="z [m]", type="n", axes=FALSE)
    axis(2)
    box()
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], bottom, col="#f5d9ab")
    points(rep(0, length(l)), z, pch="+")
    Z <- bottom
    for (i in seq_along(x)) {
        if (x[[i]]$type == "anchor") {
            points(0, bottom + l[i], pch=20)
            text(0, bottom + l[i], "Anchor", pos=4)
        } else if (x[[i]]$type == "float") {
            points(0, bottom + l[i], pch=20)
            text(0, bottom + l[i], "Float", pos=4)
        } else if (x[[i]]$type == "cable") {
            lines(rep(0, 2), bottom + c(l[i-1], l[i]))
        }
    }
}

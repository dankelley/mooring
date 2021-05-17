#' Create an anchor object
#'
#' @param model character value indicating the model of the anchor.
#' @return an object of the `"mooring"` class, with `type` equal to `"anchor"`.
#' @family functions that create mooring objects
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
#' @author Dan Kelley
float <- function(model="Hydro Float 20")
{
    rval <- list(list(type="float", model=model, length=0.5)) # guess on length
    class(rval) <- "mooring"
    rval
}

#' Print a mooring object
#'
#' @param m an object of the `"mooring"` class.
#' @author Dan Kelley
print.mooring <- function(m)
{
    n <- length(m)
    if (0 == n) {
        stop("Empty object (programming error)\n")
    } else {
        if (n == 1) {
            cat("Single element:\n")
        } else {
            cat("Mooring with", n, "elements:\n")
        }
        for (i in seq_len(n)) {
            mi <- m[[i]]
            # FIXME: more if blocks for various types, to customize output. For example,
            # cable has buoyancy in kg/m, whereas other things have it in kg.  Also,
            # should report depth ranges, etc.
            if (mi$type == "cable") {
                cat(sprintf("  %s (\"%s\") %gm\n", mi$type, mi$model, mi$length), sep="")
            } else {
                cat(sprintf("  %s (\"%s\") %gm\n", mi$type, mi$model, mi$length), sep="")
            }
        }
    }
    invisible(NULL)
}

#' Plot a mooring object
#'
#' @param m an object of the `"mooring"` class.
#' @importFrom graphics mtext par points rect
#' @author Dan Kelley
plot.mooring <- function(m)
{
    l <- cumsum(sapply(m, function(x) x$length))
    bottom <- -max(l) # Kludge ... maybe we want a water() function
    z <- bottom + l
    plot(rep(0, length(l)), z, xlim=c(-0.5, 0.5), xlab="", ylab="z [m]", type="n")
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], bottom, col="tan")
    points(rep(0, length(l)), z, pch="+")
    mtext("ROUGH plot -- testing if we can read elements")
}

#' Combine two mooring objects
#'
#' The first object is "placed" the second.
#' @param m1,m2 objects of the `"mooring"` class.
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

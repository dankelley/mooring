###################
# 1. overall docs #
###################

#' mooring: A Package for Analysing Oceanographic Moorings
#'
#' The mooring package provides functions for working with
#' oceanographic moorings.
#'
#' @examples
#' library(mooring)
#' # Illustrate the deformation of a 100-m mooring in a 0.5 m/s
#' # (roughly 1 knot) current. Buoyancy is provided with a float
#' # of diameter 20 inches.
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20"))
#' par(mfrow=c(1, 3))
#' plot(m)
#' # Must discretise the wire portion to resolve the shape.
#' md <- discretise(m)
#' mdk <- knockdown(md, u=0.5)
#' plot(mdk)
#'
#' @docType package
#'
#' @name mooring
NULL

#' Dataset of wire, chain, and float properties
#'
#' This dataset was constructed from the `mdcodes.mat` (Dewey 1999, 2021).
#' This is provided for use by [wire()], [chain()] and [float()].
#' Users will seldom need to consult this table directly, but those that
#' do should note that Dewey's lengths in cm have been converted to m.
#'
#' @name mooringElements
#' @docType data
#'
#' @usage data(mooringElements)
#'
#' @examples
#' library(mooring)
#' data(mooringElements)
#' mooringElements
#'
#' @references
#' Dewey, Richard K. "Mooring Design & Dynamics-a Matlab® Package for
#' Designing and Analyzing Oceanographic Moorings." Marine Models 1, no. 1
#' (December 1, 1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X
#' Dewey, Richard. "Mooring Design and Dynamics." Accessed May 15, 2021.
#' http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php
NULL

###################
# 2. setup        #
###################

g <- 9.8

#' Detect whether an object is a mooring
#'
#' An object is a mooring if it inherits from the `"mooring"` class,
#' and if it has no sub-classes.  For example, the output of [mooring()]
#' is a mooring, but the output of [anchor()] is not because the latter
#' has a sub-class (equal to `"anchor"`).  This function is mainly
#' designed for use within the package that that, e.g. [knockdown()]
#' will produce an error if its first argument is not a mooring.
#'
#' @param m an object to be tested
#'
#' @export
isMooring <- function(m=NULL) {
    is.list(m) && length(m) > 1 && inherits(m[[1]], "mooring")
    #!is.null(m) && length(class(m)) == 1 && class(m) == "mooring"
}


##################
# 3. Code        #
##################

#' Create an anchor object
#'
#' Create a anchor object,
#' either by looking up a known object from the database, or defining a new type.
#'
#' This must be the first element of a mooring constructed with
#' [mooring()].
#' Note that `depth` is not a characteristic of the anchor, but rather of
#' the domain into which it is placed.
#'
#' @templateVar model anchor
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @param depth numeric value giving water depth in m.
#'
#' @return `anchor` returns an object of the `"mooring"` class and `"anchor"` subclass.
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' # List known anchor types
#' anchor("?")
#'
#' @export
#'
#' @author Dan Kelley
anchor <- function(model="trainwheel", buoyancy=NULL, height=NULL, depth=0)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$anchors$name))
    w <- which(mooringElements$anchors$name == model)
    if (1 == length(w)) {
        me <- mooringElements$anchors[w,]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(height))
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        height <- me$height
        source <- me$source
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new anchor model")
        if (is.null(height)) stop("must supply height, if creating a new anchor model")
        source <- ""
    }
    rval <- list(model=model, source=source, buoyancy=buoyancy, height=height, depth=depth, area=0)
    class(rval) <- c("mooring", "anchor")
    rval
}                                      # anchor()

#' Create a mooring-release object
#'
#' Create a mooring-release object,
#' either by looking up a known object from the database, or defining a new type.
#'
#' @templateVar model release
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @template widthTemplate
#'
#' @template CDTemplate
#'
#' @return `release` returns an object of the `"mooring"` class and `"release"` subclass.
#'
#' @examples
#' library(mooring)
#' # List known wire types
#' release("?")
#'
#' @family functions that create mooring objects
#'
#' @export
#'
#' @author Dan Kelley
release <- function(model="eg&g 723a", buoyancy=NULL, height=NULL, width=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$releases$name))
    w <- which(mooringElements$releases$name == model)
    if (1 == length(w)) {
        me <- mooringElements$releases[w,]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(height))
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        if (!is.null(width))
            warning("ignoring supplied diameter, because \"", width, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        height <- me$height
        width <- me$width
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new release model")
        if (is.null(height)) stop("must supply height, if creating a new release model")
        if (is.null(width)) stop("must supply width, if creating a new release model")
        if (is.null(CD)) stop("must supply CD, if creating a new release model")
        source <- ""
    }
    rval <- list(model=model, source=source, buoyancy=buoyancy, height=height, width=width, CD=CD, area=height*width)
    class(rval) <- c("mooring", "release")
    rval
}                                      # release()

#' Create a wire object
#'
#' Creates a wire (or rope, etc.) object,
#' either by looking up a known object from the database, or defining a new type.
#'
#' @templateVar model wire
#' @template modelTemplate
#'
#' @template buoyancyPerMeterTemplate
#'
#' @template diameterTemplate
#'
#' @template CDTemplate
#'
#' @param length (mandatory) numeric value indicating the length (in m) of the wire.
#'
#' @return `wire` returns an object of the `"mooring"` class and `"wire"` subclass.
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' # List known wire types
#' wire("?")
#'
#' @export
#'
#' @importFrom utils data
#'
#' @author Dan Kelley
wire <- function(model="1/4in wire/jack", buoyancyPerMeter=NULL, diameter=NULL, CD=NULL, length=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$wires$name))
    if (is.null(length))
        stop("must supply length")
    w <- which(mooringElements$wires$name == model)
    if (1 == length(w)) {
        me <- mooringElements$wires[w,]
        if (!is.null(buoyancyPerMeter))
            warning("ignoring supplied buoyancyPerMeter, because \"", model, "\" is already in the database\n")
        if (!is.null(diameter))
            warning("ignoring supplied diameter, because \"", model, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancyPerMeter<- me$buoyancyPerMeter
        diameter <- me$diameter
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancyPerMeter)) stop("must supply buoyancyPerMeter, if creating a new wire model")
        if (is.null(diameter)) stop("must supply diameter, if creating a new wire model")
        if (is.null(CD)) stop("must supply CD, if creating a new wire model")
        source <- ""
    }
    rval <- list( model=model, source=source, buoyancyPerMeter=buoyancyPerMeter, diameter=diameter, CD=CD, height=length, diameter=diameter, area=length*diameter)
    class(rval) <- c("mooring", "wire")
    rval
}                                      # wire()

#' Create a chain object
#'
#' Create an object that describes mooring chain elements such as shackles,
#' either by looking up a known object from the database, or defining a new type.
#'
#' @templateVar model chain
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @template widthTemplate
#'
#' @template CDTemplate
#'
#' @return `chain` returns an object of the `"mooring"` class and `"chain"` subclass.
#'
#' @family functions that create mooring objects
#'
#' @export
#'
#' @importFrom utils data
#'
#' @examples
#' library(mooring)
#' # List known chain types
#' chain("?")
#'
#' @author Dan Kelley
chain <- function(model="1in buoy chain", buoyancy=NULL, height=NULL, width=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$chains$name))
    w <- which(mooringElements$chains$name == model)
    if (1 == length(w)) {
        me <- mooringElements$chains[w,]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(height))
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        if (!is.null(width))
            warning("ignoring supplied width, because \"", model, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        height <- me$height
        width <- me$width
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(height)) stop("must supply buoyancy, if creating a new chain model")
        if (is.null(height)) stop("must supply height, if creating a new chain model")
        if (is.null(width)) stop("must supply width, if creating a new chain model")
        if (is.null(CD)) stop("must supply CD, if creating a new chain model")
        source <- ""
    }
    rval <- list(model=model, source=source, buoyancy=buoyancy, height=height, width=width, CD=CD, area=height*width)
    class(rval) <- c("mooring", "chain")
    rval
}                                      # chain()


#' Create a float object
#'
#' Create a float object,
#' either by looking up a known object from the database, or defining a new type.
#'
#' Note that `HMB` in a name is a short-hand for `Hydrofloat Mooring Buoy`.  Data for these
#' floats was extracted from Reference 1 on 2021-05-19 by Dan Kelley, with the `CD` value
#' being set at 0.65 (in the absence of any data from the manufacture) because that
#' value is used in Dewey's dataset for many floats.
#'
#' @templateVar model float
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @template diameterTemplate
#'
#' @template CDTemplate
#'
#' @return `float` returns an object of the `"mooring"` class and `"float"` subclass.
#'
#' @references
#' 1. \url{https://deepwaterbuoyancy.com/wp-content/uploads/hydro-float-mooring-buoys-deepwater-buoyancy.pdf}
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' # List known float types
#' float("?")
#'
#' @export
#'
#' @author Dan Kelley
float <- function(model="Kiel SFS40in", buoyancy=NULL, height=NULL, diameter=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$floats$name))
    w <- which(mooringElements$floats$name == model)
    if (1 == length(w)) {
        me <- mooringElements$floats[w,]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(height))
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        if (!is.null(diameter))
            warning("ignoring supplied diameter, because \"", model, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        height <- me$height
        diameter <- me$diameter
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new float model")
        if (is.null(height)) stop("must supply height, if creating a new float model")
        if (is.null(diameter)) stop("must supply diameter, if creating a new float model")
        if (is.null(CD)) stop("must supply CD, if creating a new float model")
        source <- ""
    }
    rval <- list(model=model, source=source, buoyancy=buoyancy, height=height, diameter=diameter, CD=CD, area=height*diameter)
    class(rval) <- c("mooring", "float")
    rval
}                                      # float()

#' Create a instrument object
#'
#' Create an instrument object,
#' either by looking up a known object from the database, or defining a new type.
#'
#' @templateVar model instrument
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @template areaTemplate
#'
#' @template CDTemplate
#'
#' @return `instrument` returns an object of the `"mooring"` class and `"instrument"` subclass.
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' # List known instrument types
#' instrument("?")
#'
#' @export
#'
#' @author Dan Kelley
instrument <- function(model="sbe37 microcat clamp-on style", buoyancy=NULL, height=NULL, area=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$instruments$name))
    w <- which(mooringElements$instruments$name == model)
    if (1 == length(w)) {
        me <- mooringElements$instruments[w,]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(height))
            warning("ignoring supplied height, because \"", model, "\" is already in the database\n")
        if (!is.null(area))
            warning("ignoring supplied area, because \"", model, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        height <- me$height
        area <- me$area
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new instrument model")
        if (is.null(height)) stop("must supply height, if creating a new instrument model")
        if (is.null(area)) stop("must supply area, if creating a new instrument model")
        if (is.null(CD)) stop("must supply CD, if creating a new instrument model")
        source <- ""
    }
    rval <- list(model=model, source=source, buoyancy=buoyancy, height=height, area=area, CD=CD)
    class(rval) <- c("mooring", "instrument")
    rval
}                                      # instrument()

#' Combine two mooring objects (DEFUNCT)
#'
#' This has been replaced by [mooring()].
#'
#' @param m1,m2 DEFUNCT.
#'
#' @return DEFUNCT
#'
## @export
#' @author Dan Kelley
`+.mooring` <- function(m1, m2)
{
    stop("+ no longer works. Use mooring() instead")
    n1 <- length(m1)
    if (length(n1) < 1L)
        stop("n1 must have 1 or more elements")
    n2 <- length(m2)
    if (length(n2) < 1L)
        stop("n2 must have 1 or more elements")
    rval <- vector("list", n1+n2)
    mooringHeight <- 0 # for computing z
    for (i in seq_len(n1)) {
        rval[[i]] <- m1[[i]]
        rval[[i]]$x <- 0
        mooringHeight <- mooringHeight + m1[[i]]$height
    }
    for (i in seq_len(n2)) {
        rval[[n1 + i]] <- m2[[i]]
        rval[[n1 + i]]$x <- 0
        mooringHeight <- mooringHeight + m2[[i]]$height
    }
    height <- cumsum(sapply(rval, function(x) x$height))
    #> cat("height: ", paste(height, collapse=" "), "\n")
    depth <- if ("anchor" == class(m1[[1]])[2]) m1[[1]]$depth
        else sum(sapply(rval, function(x) x$height))
    T <- rev(cumsum(rev(buoyancy(rval[-1]))))
    T <- c(T[1], T)
    for (i in seq_len(n1+n2)) {
        rval[[i]]$z <- height[i] - depth
        rval[[i]]$T <- T[i]
    }
    class(rval) <- "mooring"
    rval
}                                      # +.mooring

#' CD of mooring elements
#'
#' This looks up element `CD` values for the elements
#' in a mooring.  For anchor items, which are assumed to
#' have no drag, it fills in 0.
#'
#' @template mTemplate
#'
#' @export
#'
#' @author Dan Kelley
CD <- function(m)
{
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    sapply(m, function(item) if ("anchor" == class(item)[2]) 0 else item$CD)
}


#' Drag on mooring elements
#'
#' This looks up element areas with [area()] and then computes
#' \eqn{(1/2)*area*rho*CD*u^2}{(1/2)*area*rho*CD*u^2}
#'
#' @template mTemplate
#'
#' @template uTemplate
#'
#' @template rhoTemplate
#'
#' @export
#'
#' @author Dan Kelley
drag <- function(m, u, rho=1027)
{
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    if (length(rho) > 1L && length(rho) != length(m))
        stop("if rho is a vector, it must be the same length as m, but the lengths are ",
             length(rho), " and ", length(m), " respectively")
    if (is.function(u)) {
        depth <- -sapply(m, function(M) M$z)
        u2 <- sapply(depth, u)^2
    } else {
        u2 <- u^2
    }
    0.5 * area(m) * rho * CD(m) * u2
}

#' Create a mooring
#'
#' Assemble components into a mooring, starting with an anchor, created with [anchor()],
#' and addind in wires, created with [wire()], instruments, created with [instrument()],
#' floats, created with [float()], and so forth.
#'
#' @param ... two or more elementary objects, e.g. as created by [anchor()],
#' [chain()], [wire()], or [float()].
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=100), wire(length=80), float("HMB 20"))
#'
#' @export
#'
#' @author Dan Kelley
mooring <- function(...)
{
    dots <- list(...)
    if (length(dots) < 3)
        stop("please supply two or more elements, starting with an anchor()")
    w <- which(sapply(dots, function(x) !inherits(x, "mooring")))
    if (length(w))
        stop("these are the indices of elements that are not of class 'mooringElement': ", paste(w, collapse=" "))
    rval <- dots
    height <- cumsum(sapply(rval, function(x) x$height))
    depth <- if ("anchor" == class(rval[[1]])[2]) rval[[1]]$depth
        else sum(sapply(rval, function(x) x$height))
    T <- rev(cumsum(rev(buoyancy(rval[-1]))))
    T <- c(T[1], T)
    x <- 0
    for (i in seq_along(rval)) {
        z <- height[i] - depth
        # If the line is too heavy for the flotation, run some of it along
        # the bottom.
        if (z < (-depth)) {
            message("DANNY i=", i, ", z=", z, ", depth=", depth)
            z <- -depth
            x <- x + depth
        }
        rval[[i]]$x <- x
        rval[[i]]$z <- z
        rval[[i]]$T <- T[i]
    }
    class(rval) <- "mooring"
    rval
}

#' Print a mooring object
#'
#' @param x an object of the `"mooring"` class.
#'
#' @param ... optional arguments (ignored).
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=100), wire(length=80), float("HMB 20"))
#'
#' @export
#'
#' @author Dan Kelley
print.mooring <- function(x, ...)
{
    elementary <- 2 == length(class(x))
    n <- if (elementary) 1 else length(x)
    if (elementary) {
        prefix <- ""
    } else {
        if (inherits(x[[1]], "anchor")) {
            cat("Mooring in", x[[1]]$depth, "m of water, with", n, "elements, listed top-down:\n")
        } else {
            cat("Mooring with", n, "elements, listed top-down:\n")
        }
        prefix <- "  "
    }
    for (i in rev(seq_len(n))) {
        xi <- if (elementary) x else x[[i]]
        if (inherits(xi, "anchor")) {
            cat(sprintf("%s'%s' anchor, %gkg, height %gm, in %gm water depth",
                        prefix, xi$model, xi$buoyancy, xi$height, xi$depth), sep='')
        } else if (inherits(xi, 'chain')) {
            cat(sprintf("%s'%s' chain, %gkg/m, height %gm, width %gm",
                        prefix, xi$model, xi$buoyancy, xi$height, xi$width), sep='')
        } else if (inherits(xi, 'float')) {
            cat(sprintf("%s'%s' float, %gkg, height %gm, diameter %gm",
                         prefix, xi$model, xi$buoyancy, xi$height, xi$diameter), sep='')
        } else if (inherits(xi, 'instrument')) {
            cat(sprintf("%s'%s' instrument, %gkg, area %gm^2",
                         prefix, xi$model, xi$buoyancy, xi$area), sep='')
        } else if (inherits(xi, 'release')) {
            cat(sprintf("%s'%s' release, %gkg, height %gm, width %gm",
                        prefix, xi$model, xi$buoyancy, xi$height, xi$width), sep='')
        } else if (inherits(xi, 'wire')) {
            cat(sprintf("%s%gm of '%s' wire, %gkg, diameter %gm",
                        prefix, xi$height, xi$model, xi$buoyancyPerMeter*xi$height, xi$diameter), sep="")
        } else {
            stop("unknown class c(\"", paste(class(xi), collapse="\", \""), "\")")
        }
        if (all(c("x", "z") %in% names(xi))) cat(sprintf(", x=%g m, z=%g m\n", xi$x, xi$z))
        else cat("\n")
    }
    invisible(x)
}

#' Plot a mooring object
#'
#' @param x an object of the `"mooring"` class.
#'
#' @param which character value indicating the desired plot, with
#' choices: `"shape"` and `"tension"`.
#'
#' @param showDepths logical value indicating whether to indicate the depths of
#' floats, to the left of the symbols.
#'
#' @param fancy logical value indicating whether to indicate the
#' water and sediments with filled rectangles.  The alternative
#' is a simpler plot.
#'
#' @param title character value indicating a title to put above
#' the plot.
#'
#' @param mar numeric vector of length 4, used to set margins.
#' The default value makes allowance for the axis along the top,
#' and tightens the margins on bottom and right.
#'
#' @param mgp numeric vector of length 3, used to set axis
#' geometry.  The default value moves numbers and labels closer
#' to the axes than the usual R default.
#'
#' @param ... optional arguments.
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre wire, and a float.
#' library(mooring)
#' m <- mooring(anchor(depth=100), wire(length=80), float("HMB 20"))
#' md <- discretise(m)
#' par(mfrow=c(1, 2))
#' plot(md)
#' plot(md, which="tension")
#'
#' @importFrom graphics abline axis box lines mtext par plot.window points rect text
#' @importFrom grDevices extendrange
#'
#' @export
#'
#' @author Dan Kelley
plot.mooring <- function(x, which="shape", showDepths=TRUE,
                         fancy=FALSE, title="",
                         mar=c(1.5, 3.5, 3.5, 1), mgp=c(2, 0.7, 0),
                         ...)
{
    if (!isMooring(x))
        stop("only works for objects created by mooring()")
    if (!which %in% c("shape", "tension"))
        stop("which must be \"shape\" or \"tension\"")
    m <- x # we only use 'x' above to obey the R rules on generics.
    colWater <- "#ccdcff"
    colBottom <- "#e6bb98"
    colStagnant <- "darkgray"
    dots <- list(...)
    debug <- 0L
    if ("debug" %in% names(dots))
        debug <- as.integer(max(0L, dots$debug))
    xshape <- x(m)
    xtension <- tension(m)
    z <- z(m)
    x <- if (which == "shape") xshape else if (which == "tension") xtension
    xstagnant <- if (which == "shape") rep(0, length(m)) else if (which == "tension") tension(m, stagnant=TRUE)
    mooringDebug(debug, x, overview=TRUE, round=2)
    mooringDebug(debug, z, overview=TRUE, round=2)
    depth <- -z
    waterDepth <- if ("anchor" == class(m[[1]])[2]) m[[1]]$depth
        else abs(min(depth))
    mooringDebug(debug, waterDepth, overview=TRUE)
    par(mar=mar, mgp=mgp)
    xlim <- extendrange(c(x, xstagnant))
    ylim <- c(waterDepth, 0)
    # Determine depth scale by doing a sort of dry run of a shape plot
    plot.window(0, 0, xlim=extendrange(xshape), ylim=ylim, asp=1, log="")
    usrShape <- par("usr")
    #> message("usrShape[3:4] is ", usrShape[3], " ", usrShape[4])
    plot(x, depth, xlim=xlim, ylim=usrShape[3:4], yaxs="i", asp=if (which=="shape") 1, type="l", xlab="", ylab="", axes=FALSE)
    xlab <- if (which == "shape") "Horizontal Coordinate [m]" else if (which == "tension") "Tension [kg]"
    ylab <- "Depth [m]"
    box()
    axis(2)
    mtext(ylab, side=2, line=par("mgp")[1], cex=par("cex"))
    axis(3)
    mtext(xlab, side=3, line=par("mgp")[1], cex=par("cex"))
    if (fancy) {
        box()
        usr <- par("usr")
        rect(usr[1], waterDepth, usr[2], 0, col=colWater, border=NA)
        grid(col="white")
        abline(h=0, col=colWater)
    } else {
        grid()
        abline(h=0, col="#0066ff", lwd=2)
        abline(h=waterDepth, col="#996633", lwd=2)
    }
    # Redraw to cover grid
    lines(x, depth, lwd=1.4*par("lwd"))
    # Draw conditions for u=0 case
    if (fancy)
        rect(usr[1], usr[3], usr[2], waterDepth, col=colBottom, border=NA)
    # Redraw in case line runs along bottom
    lines(x, depth, lwd=1.4*par("lwd"))
    if (which == "shape") {
        mooringLength <- sum(sapply(m, function(x) x$height))
        lines(rep(0, 2), waterDepth - c(mooringLength, 0), col=colStagnant, lwd=1.4*par("lwd"))
        points(0, waterDepth - mooringLength, pch=20, col=colStagnant)
    } else if (which == "tension") {
        lines(tension(m, stagnant=TRUE), depth, col=colStagnant, lwd=1.4*par("lwd"))
    }
    for (i in seq_along(m)) {
        type <- class(m[[i]])[2]
        xi <- x[i]
        zi <- m[[i]]$z
        if (type == "anchor") {
            if (debug)
                cat("i=", i, " (anchor at xi=", xi, ", zi=", zi, ")\n")
            points(xi, -zi, pch=20)
            text(xi, -zi, "A", pos=2)
        } else if (type == "float") {
            if (debug)
                cat("i=", i, " (float at xi=", xi, ", zi=", zi, ")\n")
            points(xi, -zi, pch=20, cex=1.4)
            text(xi, -zi, "F", pos=4)
            if (showDepths)
                text(xi, -zi, sprintf("%.1fm", -zi), pos=2)
        } else if (type == "instrument") {
            if (debug)
                cat("i=", i, " (instrument at xi=", xi, ", zi=", zi, ")\n")
            points(xi, -zi, pch=20, cex=1.4)
            text(xi, -zi, "I", pos=4)
            if (showDepths)
                text(xi, -zi, sprintf("%.1fm", -zi), pos=2)
         } else if (type == "wire") {
            #> message("draw wire??")
        }
    }
    mtext(title, side=1, cex=par("cex"))
}

#' Discretise the wire portions of a mooring
#'
#' Break up `wire` portions of a mooring into smaller chunks,
#' so that the deformation by a current can be traced more
#' accurately by [knockdown()].
#'
#' @param m an object of the `"mooring"` class.
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
#'
#' @author Dan Kelley
discretise <- function(m, by=1)
{
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    if (by <= 0)
        stop("by must be a positive number")
    rval <- list()
    group <- 1
    for (item in m) {
        if (class(item)[2] == "wire") {
            height <- item$height
            n <- max(1L, as.integer(round(height/by)))
            dheight <- height / n
            dwire <- item
            dwire$height <- dheight
            dwire$area <- dheight * dwire$diameter
            dwire$group <- group # so we can undo this later
            for (i in seq_len(n))
                rval[[1+length(rval)]] <- dwire
            group <- group + 1
        } else {
            rval[[1+length(rval)]] <- item
        }
    }
    # Fix up the z and T values
    z <- -rval[[1]]$depth + cumsum(sapply(rval, function(x) x$height))
    T <- tension(rval, stagnant=TRUE)
    for (i in seq_along(rval)) {
        rval[[i]]$z <- z[i]
        rval[[i]]$T <- T[i]
    }
    class(rval) <- "mooring"
    rval
}

#' Compute how a mooring is knocked down by a current
#'
#' The current may be a depth-independent or depth-dependent,
#' as specified by the `v` argument.
#'
#' @param m an object of the `"mooring"` class, usually created with
#' [discretise()].
#'
#' @template uTemplate
#'
#' @template debugTemplate
#'
#' @examples
#' # Illustrate importance of drag on the wire.
#' library(mooring)
#' m <- mooring(anchor(depth=100), wire(length=80), float("HMB 20"))
#' md <- discretise(m)
#' par(mfrow=c(1, 3))
#'
#' # Example 1: no current
#' plot(md)
#'
#' # Example 2: uniform 0.5 m/s (approx. 1 knot) current
#' k1 <- knockdown(md, u=0.5)
#' plot(k1, title="uniform 0.5 m/s")
#'
#' # Example 3: 0.5 m/s current but only in top 40m of water column
#' k2 <- knockdown(md, u=function(depth) ifelse(depth < 40, 0.5, 0))
#' plot(k2, title="0.5 m/s only in top 40m")
#'
#' @importFrom graphics grid
#' @importFrom utils tail
#' @export
#' @author Dan Kelley
knockdown <- function(m, u=1, debug=0L)
{
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    debug <- as.integer(max(0, debug))
    if (is.function(u)) {
        #z <- sapply(m, function(x) x$z)
        #mooringDebug(debug, z, overview=TRUE)
        warning("FIXME: u=function() case is not fully coded yet (no iteration is done)\n")
    }
    # Trim the anchor, which is not used in this calculation
    morig <- m
    waterDepth <- anchorHeight <- 0
    # Remove the anchor, after saving waterDepth and anchorHeight
    if (class(m[[1]])[2] == "anchor") {
        waterDepth <- m[[1]]$depth
        anchorHeight <- m[[1]]$height
        m <- tail(m, -1)
        class(m) <- "mooring"
    } else {
        stop("the mooring must start with an object creatred with anchor()")
    }
    # reverse elements, to make it simpler to work from top down
    mrev <- rev(m)
    class(mrev) <- "mooring"
    # Depth below surface (FIXME: how to have more water above?)
    depth <- cumsum(sapply(mrev, function(item) item$height))
    mooringDebug(debug, depth, overview=TRUE)
    B <- g * buoyancy(mrev)
    D <- drag(mrev, u)
    height <- unlist(lapply(mrev, function(item) item$height))

    # computation
    n <- length(mrev)
    # We won't define phi[1] and T[1], and will trim them
    # after the calculation.  The only purpose in setting up
    # n elements is to make the formulae match those in the
    # vignette.
    phi <- rep(NA, n)
    T <- rep(NA, n)
    phi[2] <- atan2(D[1], B[1]) # radians
    T[2] <- sqrt(D[1]^2 + B[1]^2)
    if (n > 2) {
        for (i in 3:n) {
            phi[i] <- atan2(D[i-1]+T[i-1]*sin(phi[i-1]), B[i-1]+T[i-1]*cos(phi[i-1]))
            T[i] <- sqrt((D[i-1]+T[i-1]*sin(phi[i-1]))^2+(B[i-1]+T[i-1]*cos(phi[i-1]))^2)
        }
    }
    phi <- tail(phi, -1L)
    phi <- ifelse(phi > pi/2, pi/2, phi)
    T <- tail(T, -1L)
    mooringDebug(debug, "Original values:\n")
    mooringDebug(debug, phi*180/pi, overview=TRUE, round=1)
    mooringDebug(debug, T/g, overview=TRUE, round=1)
    # Find x and z by integrating from bottom up.
    phiRev <- rev(phi)
    heightRev <- rev(height)[-1]
    x <- rev(cumsum(heightRev*sin(phiRev)))
    z <- rev(cumsum(heightRev*cos(phiRev))) - waterDepth
    mooringDebug(debug, x, overview=TRUE, round=2)
    mooringDebug(debug, z, overview=TRUE, round=2)
    rval <- morig
    # Add top of anchor, and bottom of anchor.  Then reverse, to match original 'm'.
    x <- rev(c(x, 0, 0))
    z <- rev(c(z, -waterDepth + morig[[1]]$height, -waterDepth))
    phi <- c(phi[1], phi, tail(phi, 1))
    T <- rev(c(T[1], T, tail(T, 1)))
    mooringDebug(debug, "Values after adding two elements:\n")
    mooringDebug(debug, x, overview=TRUE, round=2)
    mooringDebug(debug, z, overview=TRUE, round=2)
    for (i in seq_along(morig)) {
        rval[[i]]$x <- x[i]
        rval[[i]]$z <- z[i]
        rval[[i]]$phi <- phi[i]
        rval[[i]]$T <- T[i]
    }
    class(rval) <- "mooring"
    rval
}

#' Optionally print a debugging message
#'
#' A function used for debugging within the package.
#'
#' @param debug an integer that controls debugging. If this is positive,
#' then the other arguments are given to [cat()], to print a message. Note
#' that no newline is printed, unless included in the \code{...} arguments.
#'
#' @param v a value to be displayed.
#'
#' @param ... optional extra values to be displayed.
#'
#' @param overview logical value indicating whether to summarize `v` (ignoring
#' `...`), by displaying its name and some of its values.
#'
#' @param round either FALSE, for no rounding, or an integer, indicating the
#' rounding to be used, via a call to [round()]. This applies only to the case
#' where `overview` is TRUE.
#'
#' @importFrom utils head tail
#'
#' @export
#'
#' @author Dan Kelley
mooringDebug <- function(debug, v, ..., overview=FALSE, round=FALSE)
{
    debug <- as.integer(debug)
    if (debug > 0L) {
        if (overview) {
            msg <- paste(deparse(substitute(expr=v, env=environment())), " ", sep="")
            if (is.numeric(round))
                v <- round(v, as.integer(round))
            n <- length(v)
            if (n > 1)
                msg <- paste(msg, "[1:", n, "] ", sep="")
            if (n < 7)
                msg <- paste(msg, paste(v, collapse=" "), "\n", sep="")
            else
                msg <- paste(msg, paste(head(v, 3), collapse=" "), " ... ", paste(tail(v, 3), collapse=" "), "\n", sep="")
            cat(msg)
        } else {
            cat(v, ...)
        }
    }
    invisible(NULL)
}

# FIXME: put all the below into alphabetical order (once working)

#> #' Get angles from a mooring
#> #'
#> #' @param m an object of the `"mooring"` class.
#> #'
#> #' @return a numeric vector of angles (in degrees) made by each inter-element connector to the vertical.
#> #' Note that this has length 1 less than the length of `m`.
#> #'
#> #' @export
#> #'
#> #' @author Dan Kelley
#> angle <- function(m)
#> {
#>     180 / pi * sapply(m, function(mi) mi$phi)
#> }

#' Return depth, in m, of elements in mooring.
#'
#' This is the negative of [z()].
#'
#' @param m an object of the `"mooring"` class.
#'
#' @return a numeric vector of depth in metres.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20"))
#' depth(m)
#'
#' @export
#'
#' @author Dan Kelley
depth <- function(m)
{
    -z(m)
}

#' Return horizontal coordinate, in m, of elements in mooring.
#'
#' @param m an object of the `"mooring"` class.
#'
#' @return a numeric vector of horizontal coordinate in metres.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20"))
#' x(m)
#'
#' @export
#'
#' @author Dan Kelley
x <- function(m)
{
    sapply(m, function(mi) mi$x)
}

#' Return vertical coordinate, in m, of elements in mooring.
#'
#' This is the negative of [depth()].
#'
#' @param m an object of the `"mooring"` class.
#'
#' @return a numeric vector of vertical coordinate in metres.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20"))
#' z(m)
#'
#' @export
#'
#' @author Dan Kelley
z <- function(m)
{
    sapply(m, function(mi) mi$z)
}

#' Return tension, in kg, between elements in mooring.
#'
#' The first element (for the anchor) is repeated, so that the
#' length of the returned result matches the length of `m`.
#'
#' @param m an object of the `"mooring"` class.
#'
#' @param stagnant a logical indicating whether to compute tension
#' as though no currents were present.
#'
#' @return a numeric vector of tension, in kg.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20"))
#' md <- discretise(m)
#' mdk <- knockdown(md, u=0.5)
#' depth <- depth(mdk)
#' # Next is handled better by plot.mooring(mdk,which="tension")
#' plot(tension(mdk), depth, ylim=rev(range(depth)), type="l")
#'
#' @export
#'
#' @author Dan Kelley
tension <- function(m, stagnant=FALSE)
{
    if (stagnant || all(x(m) == 0)) {
        b <- buoyancy(m)[-1]
        T <- rev(cumsum(rev(b)))
        c(T[1], T) # repeat anchor
    } else {
        sapply(m, function(mi) mi$T/g)
    }
}

#' Get element areas
#'
#' @param m a mooring, created by [mooring()].
#'
#' @export
#'
#' @author Dan Kelley
area <- function(m)
{
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    sapply(m, function(mi) mi$area)
}

#' Buoyancy of elements in mooring, expressed in kg.
#'
#' The nonphysical unit of kg reflects a common convention used
#' by manufacturers of oceanographic mooring equipment. For calculations
#' of buoyancy *force*, the return value from this function
#' must be multiplied by the acceleration due to gravity,
#' g=9.8m/s^2.
#'
#' Note that the present version of this function
#' does not account for depth variations in seawater density,
#' for those tend to be well under 1 percent, and other
#' forces involved in mooring dynamics are much more uncertain
#' than that.  For example, Hamilton (1989) found that oscillations
#' in mooring lines could lead to enhanced drag, in some cases
#' necessitating an increase in CD for wire from 1.4 to 2.6
#' (see captions of his figures 12 and 13).
#'
#' @param m an object of the `"mooring"` class.
#'
#' @template debugTemplate
#'
#' @return a numeric vector of buoyancy, expressed in kg.
#'
#' @examples
#' library(mooring)
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20"))
#' buoyancy(m)
#'
#' @export
#'
#' @references
#' Hamilton, J. M. "The Validation and Practical Applications of a Sub-Surface
#' Mooring Model." Canadian Technical Report of Hydrography and Ocean
#' Sciences. Bedford Institute of Oceanography, 1989.
#'
#' @author Dan Kelley
buoyancy <- function(m, debug=0L)
{
    mooringDebug(debug, "buoyancy() {\n  class(m): ", paste(class(m), collapse=" "), "\n")
    rval <- if (isMooring(m)) {
        mooringDebug(debug, "  object is a mooring with", length(m), "elements, so will analyse them individually\n")
        sapply(m, function(mi) buoyancy(mi, debug=debug))
    } else {
        if (class(m)[2] == "wire") {
            mooringDebug(debug, "  object is a wire, so returning buoyancyPerMeter*height\n")
            m$buoyancyPerMeter * m$height
        } else {
            mooringDebug(debug, "  object is a not a wire, so returning buoyancy\n")
            m$buoyancy
        }
    }
    mooringDebug(debug, "} # buoyancy()\n")
    rval
}

###################
# 4. app          #
###################


g <- 9.8
#' mooring: A Package for Analysing Oceanographic Moorings.
#'
#' The mooring package provides functions for working with
#' oceanographic moorings.
#'
#' @examples
#' library(mooring)
#' # Illustrate the deformation of a 100-m mooring in a 0.5 m/s
#' # (roughly 1 knot) current. Buoyancy is provided with a float
#' # of diameter 20 inches.
#' m <- anchor(depth=120) + wire(length=100) + float("HMB 20")
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
#' @references
#' Dewey, Richard K. “Mooring Design & Dynamics—a Matlab® Package for
#' Designing and Analyzing Oceanographic Moorings.” Marine Models 1, no. 1
#' (December 1, 1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X.
#'
#' Dewey, Richard. “Mooring Design and Dynamics.” Accessed May 15, 2021.
#' http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php.
NULL



#' Create an anchor object
#'
#' This must be the first element of a mooring constructed with \code{\link{+.mooring}}.
#'
#' @param model character value indicating the model of the anchor.
#' At present, only `"default"` is permitted, and this is just
#' based on a guess on height (when laid on its side).
#'
#' @param depth numeric value giving water depth in m.
#'
#' @return an object of the `"mooring"` class, with `type` equal to `"anchor"`.
#' @family functions that create mooring objects
#'
#' @export
#'
#' @author Dan Kelley
anchor <- function(model="default", depth=0)
{
    # guess on height
    if (model != "default")
        stop("'model' must be \"default\"")
    # FIXME: guess a train wheel weighs 1000kg and is a foot wide
    rval <- list(list(type="anchor", model=model, buoyancy=-1000, height=0.3, depth=depth, x=0, z=0))
    class(rval) <- "mooring"
    rval
}

#' Create a mooring-release object
#'
#' @param model character value indicating the model of the acoustic release.
#' At present, only `"default"` is permitted, and this is just
#' based on a guess on height (when laid on its side).
#'
#' @return an object of the `"mooring"` class, with `type` equal to `"release"`.
#'
#' @family functions that create mooring objects
#'
#' @export
#' @author Dan Kelley
release <- function(model="default_release")
{
    rval <- list(list(type="anchor", model=model, height=1.0, x=0, z=0)) # guess on length
    class(rval) <- "mooring"
    rval
}

#' Create a wire object
#'
#' Creates an object that describes a mooring wire, or other type of line, e.g.
#' nylon ropes are considered wires, for this purpose.
#' Note that all dimensions are specified in m, not cm.
#'
#' @param model character value indicating the model name of the wire. The
#' default, `"1/4 wire/jack"`, is for quarter-inch jacketed steel wire.
#' Use `wire("?")` to get a listing of permitted values.
#' If `model` is not recognized, then
#' [wire] creates a new type, using the stated diameter and CD values;
#' otherwise, these arguments are ignored.
#'
#' @param buoyancy numeric value of the buoyancy per length of wire, in kg/m.
#'
#' @param length numeric value indicating the length of the wire, in metres.
#'
#' @param width numeric value for the width of the wire, in m.
#' This is ignored if `model` is recognized.
#'
#' @param CD numeric value for the drag coefficient for the wire.
#' This is ignored if `model` is recognized.
#'
#' @return [wire] returns an object of the `"mooring"` class, with `type` equal to `"wire"`.
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
wire <- function(model="1/4 wire/jack", buoyancy=NULL, length=NULL, width=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        cat("wire() takes the following strings for its 'model' argument:\n")
        print(sort(mooringElements$wires$name))
        return(invisible(NULL))
    }
    if (is.null(length))
        stop("must supply length (m) of the wire")
    w <- which(mooringElements$wires$name == model)
    if (1 == length(w)) {
        me <- mooringElements$wires[w,]
        if (!is.null(buoyancy))
            warning("ignoring supplied buoyancy, because \"", model, "\" is already in the database\n")
        if (!is.null(width))
            warning("ignoring supplied width, because \"", model, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancy <- me$buoyancy
        width <- me$width
        CD <- me$CD
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new wire model")
        if (is.null(width)) stop("must supply width, if creating a new wire model")
        if (is.null(CD)) stop("must supply CD, if creating a new wire model")
    }
    rval <- list(list(type="wire", model=model, buoyancy=buoyancy, height=length, width=width, CD=CD, x=0, z=0))
    class(rval) <- "mooring"
    rval
}

#' Create a chain object
#'
#' Creates an object that describes mooring chain elements such as shackles.
#' Note that all dimensions are specified in m, not cm.
#'
#' @param model character value indicating the model name of the chain. The
#' default is `"1\" buoy chain"`.
#' Use `chain("?")` to get a listing of permitted values.
#' If `model` is not recognized, then
#' [chain] creates a new type, using the stated diameter and CD values;
#' otherwise, these arguments are ignored.
#'
#' @param buoyancy numeric value of the buoyancy of the item, in kg.
#' This is ignored if `model` is recognized.
#'
#' @param height numeric value for the height of the element, in m.
#' This is ignored if `model` is recognized.
#'
#' @param width numeric value for the width of the element, in m.
#' This is ignored if `model` is recognized.
#'
#' @param CD numeric value for the drag coefficient for the item.
#' This is ignored if `model` is recognized.
#'
#' @return [chain] returns an object of the `"mooring"` class, with `type` equal to `"chain"`.
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
chain <- function(model="1\" buoy chain", buoyancy=NULL, height=NULL, width=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        cat("chain() takes the following strings for its 'model' argument:\n")
        print(sort(mooringElements$chains$name))
        return(invisible(NULL))
    }
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
        width <- me$cylinderWidth
        CD <- me$CD
    } else {
        if (is.null(height)) stop("must supply buoyancy, if creating a new chain model")
        if (is.null(height)) stop("must supply height, if creating a new chain model")
        if (is.null(width)) stop("must supply width, if creating a new chain model")
        if (is.null(CD)) stop("must supply CD, if creating a new chain model")
    }
    rval <- list(list(type="chain", model=model, buoyancy=buoyancy, height=height, width=width, CD=CD, x=0, z=0))
    class(rval) <- "mooring"
    rval
}


#' Create a float object
#'
#' Create a float object.  Some notes follow.
#' 1. `HMB` in a name is a short-hand for `Hydrofloat Mooring Buoy`.  Data for these
#' floats was extracted from Reference 1 on 2021-05-19 by Dan Kelley, with the `CD` value
#' being set at 0.65 (in the absence of any data from the manufacture) because that
#' value is used in Dewey's dataset for many floats.
#'
#' @param model character value indicating the model name of the float. The
#' default is `"Kiel SFS40in"`.
#' Use `float("?")` to get a listing of permitted values.
#' If `model` is not recognized, then
#' [float] creates a new type, using the stated diameter and CD values;
#' otherwise, these arguments are ignored.
#'
#' @param buoyancy numeric value of the buoyancy of the float, in kg.
#' This is ignored if `model` is recognized.
#'
#' @param height numeric value for the height of the element, in m.
#' This is ignored if `model` is recognized.
#'
#' @param diameter numeric value for the diameter of the float, in m.
#' This is ignored if `model` is recognized.
#'
#' @param CD numeric value for the drag coefficient for the item.
#'
#' @return [float] returns an object of the `"mooring"` class, with `type` equal to `"float"`.
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
    if (model == "?") {
        cat("float() takes the following strings for its 'model' argument:\n")
        print(sort(mooringElements$floats$name))
        return(invisible(NULL))
    }
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
    } else {
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new float model")
        if (is.null(height)) stop("must supply height, if creating a new float model")
        if (is.null(diameter)) stop("must supply diameter, if creating a new float model")
        if (is.null(CD)) stop("must supply CD, if creating a new float model")
    }
    rval <- list(list(type="float", model=model, buoyancy=buoyancy, height=height, diameter=diameter, CD=CD, x=0, z=0))
    class(rval) <- "mooring"
    rval
}

#' Combine two mooring objects
#'
#' The first object is "placed" above the second (see Examples).  Note that
#' the first element of the first object must be the result of a call
#' to [anchor()].
#'
#' @param m1,m2 objects of `"mooring"` class.  Each can be a single element,
#' e.g. as created by [wire()], etc., or a combination of elements, as are
#' returned by the present function.
#'
#' @return an object of the `"mooring"` class that holds the
#' combination of `m1` and `m2`.
#'
#' @family functions that create mooring objects
#'
#' @family functions that create mooring objects
#'
#' @examples
#' library(mooring)
#' m <- anchor(depth=100) + wire(length=80) + float("HMB 20")
#' print(m)
#' plot(m)
#'
#' @export
#' @author Dan Kelley
`+.mooring` <- function(m1, m2)
{
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
    depth <- if ("anchor" == m1[[1]]$type) m1[[1]]$depth else sum(sapply(rval, function(x) x$height))
    T <- rev(cumsum(rev(buoyancy(rval[-1]))))
    T <- c(T[1], T)
    for (i in seq_len(n1+n2)) {
        rval[[i]]$z <- height[i] - depth
        rval[[i]]$T <- T[i]
    }
    class(rval) <- "mooring"
    rval
}                                      # +.mooring

#' Print a mooring object
#'
#' @param x an object of the `"mooring"` class.
#'
#' @param ... optional arguments (ignored).
#'
#' @examples
#' library(mooring)
#' m <- anchor(depth=100) + wire(length=80) + float("HMB 20")
#' print(m)
#'
#' @export
#'
#' @author Dan Kelley
print.mooring <- function(x, ...)
{
    m <- x # we only use 'x' above to obey the R rules on generics.
    n <- length(x)
    if (0 == n)
        stop("Empty object (programming error)\n")
        if (n == 1) {
            cat("Single element:\n")
        } else {
            if (m[[1]]$type == "anchor") {
                cat("Mooring in", m[[1]]$depth, "m of water, with", n, "elements, listed top-down:\n")
            } else {
                cat("Mooring with", n, "elements, listed top-down:\n")
            }
        }
        for (i in rev(seq_len(n))) {
            xi <- x[[i]]
            # FIXME: more if blocks for various types, to customize output. For example,
            # wire has buoyancy in kg/m, whereas other things have it in kg.  Also,
            # should report depth ranges, etc.
            if (xi$type == "wire") {
                cat(sprintf("  %s   model=\"%s\", buoyancy=%g kg/m, length=%g m, width=%g m, x=%g m, z=%g m\n",
                            xi$type, xi$model, xi$buoyancy, xi$height, xi$width, xi$x, xi$z), sep="")
            } else if (xi$type == "chain") {
                cat(sprintf("  %s  model=\"%s\", buoyancy=%g kg/m, length=%g m, width=%g m, x=%g m, z=%g m\n",
                            xi$type, xi$model, xi$buoyancy, xi$height, xi$width, xi$x, xi$z), sep="")
            } else if (xi$type == "anchor") {
                cat(sprintf("  %s model=\"%s\", buoyancy=%g kg, height=%g m, x=%g m, z=%g m\n",
                            xi$type, xi$model, xi$buoyancy, xi$height, xi$x, xi$z), sep="")
            } else if (xi$type == "float") {
                cat(sprintf("  %s  model=\"%s\", buoyancy=%g kg, height=%g m, diameter=%g m, x=%g m, z=%g m\n",
                            xi$type, xi$model, xi$buoyancy, xi$height, xi$diameter, xi$x, xi$z), sep="")
            } else {
                stop("unknown type \"", xi$type, "\"")
            }
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
#' @param ... optional arguments.
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre wire, and a float.
#' library(mooring)
#' m <- anchor(depth=100) + wire(length=80) + float("HMB 20")
#' md <- discretise(m)
#' par(mfrow=c(1, 2))
#' plot(md)
#' plot(md, which="tension")
#'
#' @importFrom graphics abline axis box lines mtext par points rect text
#' @importFrom grDevices extendrange
#'
#' @export
#'
#' @author Dan Kelley
plot.mooring <- function(x, which="shape", showDepths=TRUE, fancy=FALSE, title="", ...)
{
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
    x <- if (which == "shape") x(m) else if (which == "tension") tension(m)
    xstagnant <- if (which == "shape") rep(0, length(m)) else if (which == "tension") tension(m, stagnant=TRUE)
    z <- sapply(m, function(mi) mi$z)
    mooringDebug(debug, x, overview=TRUE, round=2)
    mooringDebug(debug, z, overview=TRUE, round=2)
    depth <- -z
    waterDepth <- if ("anchor" == m[[1]]$type) m[[1]]$depth else abs(min(depth))
    mooringDebug(debug, waterDepth, overview=TRUE)
    omar <- par("mar")
    omgp <- par("mgp")
    par(mar=c(1.5, 3.5, 3.5, 1), mgp=c(2, 0.7, 0))
    xlim <- extendrange(c(x, xstagnant))
    ylim <- c(waterDepth, 0)
    plot(x, depth, xlim=xlim, ylim=ylim, asp=if (which=="shape") 1, type="l", xlab="", ylab="", axes=FALSE)
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
    if (which == "shape") {
        mooringLength <- sum(sapply(m, function(x) x$height))
        lines(rep(0, 2), waterDepth - c(mooringLength, 0), col=colStagnant, lwd=1.4*par("lwd"))
        points(0, waterDepth - mooringLength, pch=20, col=colStagnant)
    } else if (which == "tension") {
        lines(tension(m, stagnant=TRUE), depth, col=colStagnant, lwd=1.4*par("lwd"))
    }
    if (fancy)
        rect(usr[1], usr[3], usr[2], waterDepth, col=colBottom, border=NA)
    for (i in seq_along(m)) {
        type <- m[[i]]$type
        xi <- x[i]
        zi <- m[[i]]$z
        #> message("i=",i,", z[i]=", z[i], ", type=", m[[i]]$type)
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
        } else if (type == "wire") {
            #> message("draw wire??")
        }
    }
    mtext(title, side=1, cex=par("cex"))
    par(mar=omar, mgp=omgp)
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
    if (by <= 0)
        stop("by must be a positive number")
    rval <- list()
    group <- 1
    for (item in m) {
        #> message(item$type)
        if (item$type == "wire") {
            height <- item$height
            n <- max(1L, as.integer(round(height/by)))
            dheight <- height / n
            dwire <- item
            dwire$height <- dheight
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
#' @param u either a non-negative number indicating depth-independent velocity,
#' or a function that returns that velocity as a function of depth (m)
#' below the ocean surface; see Example 3.
#'
#' @param debug an integer controlling debugging.  The default value of 0
#' means to work silently. Use a positive value to cause the function to
#' print some information about intermediate results.
#'
#' @examples
#' # Illustrate importance of drag on the wire.
#' library(mooring)
#' m <- anchor(depth=100) + wire(length=80) + float("HMB 20")
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
    if (m[[1]]$type == "anchor") {
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
    rho <- 1027
    # Depth below surface (FIXME: how to have more water above?)
    depth <- cumsum(sapply(mrev, function(item) item$height))
    mooringDebug(debug, depth, overview=TRUE)
    B <- g * unlist(lapply(mrev, function(item) item$buoyancy))
    A <- unlist(lapply(mrev,
                       function(item) {
                           if (item$type == "float") item$height * item$diameter
                           else item$height * item$width
                       }))
    CD <- unlist(lapply(mrev, function(item) item$CD))
    height <- unlist(lapply(mrev, function(item) item$height))
    if (is.function(u)) {
        depth <- -sapply(mrev, function(M) M$z)
        u2 <- sapply(depth, u)^2
        #message("u is a function; length(z)=", length(z), ", length(u2)=", length(u2))
        mooringDebug(debug, "next is calculated velocity profile\n")
        mooringDebug(debug, z, overview=TRUE, round=2)
        mooringDebug(debug, u2, overview=TRUE, round=2)
    } else {
        u2 <- u^2
    }
    D <- 0.5 * A * rho * CD * u2
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
#' m <- anchor(depth=120) + wire(length=100) + float("HMB 20")
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
#' m <- anchor(depth=120) + wire(length=100) + float("HMB 20")
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
#' m <- anchor(depth=120) + wire(length=100) + float("HMB 20")
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
#' m <- anchor(depth=120) + wire(length=100) + float("HMB 20")
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

#' Return buoyancy of elements in mooring, in kg.
#'
#' This is a direct lookup for all types except `"wire"`.
#' In the wire case, the stored buoyancy is in kg/m and therefore
#' the length of the wire is multiplied by that value.
#'
#' @param m an object of the `"mooring"` class.
#'
#' @return a numeric vector of buoyancy, in kg.
#'
#' @examples
#' library(mooring)
#' m <- anchor(depth=120) + wire(length=100) + float("HMB 20")
#' buoyancy(m)
#'
#' @export
#'
#' @author Dan Kelley
buoyancy <- function(m)
{
    sapply(m, function(mi) { if (mi$type == "wire") mi$buoyancy * mi$height else mi$buoyancy })
}

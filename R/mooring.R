#' mooring: A Package for Analysing Oceanographic Moorings.
#'
#' The mooring package provides functions for working with
#' oceanographic moorings.
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre wire, and a float.
#' library(mooring)
#' m <- anchor() + wire(height=100) + float()
#' print(m)
#' plot(m)
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
#' @param model character value indicating the model of the anchor.
#'
#' @return an object of the `"mooring"` class, with `type` equal to `"anchor"`.
#' @family functions that create mooring objects
#'
#' @export
#'
#' @author Dan Kelley
anchor <- function(model="default_anchor")
{
    rval <- list(list(type="anchor", model=model, height=0.3, x=0)) # guess on height
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
    rval <- list(list(type="anchor", model=model, height=1.0, x=0)) # guess on length
    class(rval) <- "mooring"
    rval
}

#' Create a wire object
#'
#' Creates an object that describes a mooring wire, or other type of line, e.g.
#' nylon ropes are considered wires, for this purpose.
#' Note that all dimensions are specified in m, not cm.
#'
#' The permitted `model` values are listed alphabetically below, as listed with `wire("?")`:
# FIXME: rebuild this list if new items are added.
#'\preformatted{
#' "1 Nylon"          "1/2 AmSteel-Blue" "1/2 Dacron"      
#' "1/2 Dyneema"      "1/2 Kevlar"       "1/2 Neutral Wire"
#' "1/2 VLS"          "1/2 wire/jack"    "1/4 AmSteel-Blue"
#' "1/4 Kevlar"       "1/4 wire rope"    "1/4 wire/jack"   
#' "1/4-1/2 WarpSpd"  "11mm Perlon"      "1in Neutral Wire"
#' "3/16 wire rope"   "3/4 Dacron"       "3/4 Nylon"       
#' "3/4 Polyprop"     "3/8 AmSteel-Blue" "3/8 Kevlar"      
#' "3/8 wire rope"    "3/8 wire/jack"    "5/16 Kevlar"     
#' "5/16 wire rope"   "5/16 wire/jack"   "5/8 AmSteel-Blue"
#' "5/8 Kevlar"       "5/8 Nylon"        "7/16 Dacron"     
#' "7/16 Kevlar"      "7/16 VLS"         "9/16 Dacron"     
#' "9/16 Kevlar"      "BPS Cable"        "BPS Power/Coms"  
#'}
#'
#' @param model character value indicating the model name of the wire. The
#' default, `"1/4 wire/jack"`, is for quarter-inch jacketed steel wire. If
#' model is set to `"?"`, then the default is returned, but first a list
#' of possible model values is printed. If model is not recognized, then
#' [wire] creates a new type, using the stated diameter and CD values;
#' otherwise, these arguments are ignored.
#'
#' @param buoyancy numeric value of the buoyancy per length of wire, in kg/m.
#'
#' @param height numeric value indicating the length of the wire, in metres.
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
#' @export
#'
#' @importFrom utils data
#'
#' @author Dan Kelley
wire <- function(model="1/4 wire/jack", buoyancy=NULL, height=NULL, width=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        cat("wire() takes the following strings for its 'model' argument:\n")
        print(sort(mooringElements$wires$name))
        model <- "1/4 wire/jack"
    }
    if (is.null(height))
        stop("must supply height (m) of the wire")
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
    rval <- list(list(type="wire", model=model, buoyancy=buoyancy, height=height, width=width, CD=CD))
    class(rval) <- "mooring"
    rval
}

#' Create a chain object
#'
#' Creates an object that describes mooring chain elements such as shackles.
#' Note that all dimensions are specified in m, not cm.
#'
#' The permitted `model` values are listed alphabetically below, as listed with `chain("?")`:
# FIXME: rebuild this list if new items are added.
#'\preformatted{
#' [1] "1 chain SL"       "1\" buoy chain"   "1\" shackle"
#' [4] "1/2 chain LL"     "1/2 chain SL"     "1/2 galv link"
#' [7] "1/2 shac+3/8shac" "1/2 shackle"      "1/2 SS link"
#'[10] "1/2 swivel"       "1/2\" buoy chain" "1/4 chain SL"
#'[13] "3/4 swivel"       "3/4\" buoy chain" "3/4\" shackle"
#'[16] "3/8 chain SL"     "3/8 shackle"      "3/8\" buoy chain"
#'[19] "3T SeineSwivel"   "5/8 shackle"      "5/8\" buoy chain"
#'[22] "shac-3link-shac"  "shac-Pring-shac"  "shac-ring-shac"
#'}
#'
#' @param model character value indicating the model name of the item.
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
#' @return [chain] returns an object of the `"mooring"` class, with `type` equal to `"wire"`.
#'
#' @family functions that create mooring objects
#'
#' @export
#'
#' @importFrom utils data
#'
#' @author Dan Kelley
chain <- function(model="1\" buoy chain", buoyancy=NULL, height=NULL, width=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?") {
        cat("chain() takes the following strings for its 'model' argument:\n")
        print(sort(mooringElements$chains$name))
        model <- "1\" buoy chain"
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
    rval <- list(list(type="chain", model=model, buoyancy=buoyancy, height=height, width=width, CD=CD, x=0))
    class(rval) <- "mooring"
    rval
}


#' Create a float object
#'
#' Create a float object.
#'
#' The permitted `model` values are listed alphabetically below, as listed with `float("?")`:
# FIXME: rebuild this list if new items are added.
#'\preformatted{
#'  [1] "16in Viny"        "17 in glass"      "28in ORE"
#'  [4] "30in float"       "37in ORE"         "41in ORE"
#'  [7] "48in ORE"         "54cm Alum CAP3"   "54x30inClamShell"
#' [10] "61in ORE"         "ADCP FloatTech"   "BENTHOS 17in"
#' [13] "Billings-12in"    "BPS Float"        "CDMS 1.2m sphere"
#' [16] "CDMS 2m sphere"   "Double 16inViny"  "Double 17in"
#' [19] "FloatTech CF-12"  "Geodyn 8' Toroid" "HMB 20"
#' [22] "HMB 25"           "HMB 31"           "HMB 36"
#' [25] "HMB 40"           "HMB 44"           "HMB 49"
#' [28] "HMB 51"           "HMB 56"           "HMB 62"
#' [31] "Kiel SFS40in"     "Top of Tow Rope"  "Torp. flt, 3-28"
#' [34] "Tripple FT CF-12" "Trpl 12in glass"  "Trpl 16 in Viny"
#'}
#'
#' **Notes:**
#' 1. `HMB` in a name is a short-hand for `Hydrofloat Mooring Buoy`.  Data for these
#' floats was extracted from Reference 1 on 2021-05-19 by Dan Kelley, with the `CD` value
#' being set at 0.65 (in the absence of any data from the manufacture) because that
#' value is used in Dewey's dataset for many floats.
#'
#' @param model character value indicating the model of the float.
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
        model <- "Kiel SFS40in"
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
    rval <- list(list(type="float", model=model, buoyancy=buoyancy, height=height, diameter=diameter, CD=CD, x=0))
    class(rval) <- "mooring"
    rval
}

#' Combine two mooring objects
#'
#' The first object is "placed" above the second (see Examples).
#'
#' @param m1,m2 objects of the `"mooring"` class.
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
#' m <- anchor() + wire(height=100) + float()
#' print(m)
#' plot(m)
#'
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
#'
#' @param ... optional arguments (ignored).
#'
#' @examples
#' library(mooring)
#' m <- anchor() + wire(height=100) + float()
#' print(m)
#'
#' @export
#'
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
            cat("Mooring with", n, "elements, listed top-down:\n")
        }
        for (i in rev(seq_len(n))) {
            xi <- x[[i]]
            # FIXME: more if blocks for various types, to customize output. For example,
            # wire has buoyancy in kg/m, whereas other things have it in kg.  Also,
            # should report depth ranges, etc.
            if (xi$type == "wire") {
                cat(sprintf("  %s   model=\"%s\", buoyancy=%g kg/m, height=%g m, width=%g m\n", xi$type, xi$model, xi$buoyancy, xi$height, xi$width), sep="")
            } else if (xi$type == "chain") {
                cat(sprintf("  %s  model=\"%s\", buoyancy=%g kg/m, height=%g m, width=%g m\n", xi$type, xi$model, xi$buoyancy, xi$height, xi$width), sep="")
            } else if (xi$type == "anchor") {
                cat(sprintf("  %s model=\"%s\", height=%g m\n", xi$type, xi$model, xi$height), sep="")
            } else if (xi$type == "float") {
                cat(sprintf("  %s  model=\"%s\", buoyancy=%g kg, height=%g m, diameter=%g m\n", xi$type, xi$model, xi$buoyancy, xi$height, xi$diameter), sep="")
            } else {
                stop("unknown type \"", xi$type, "\"")
            }
        }
    }
    invisible(x)
}

#' Plot a mooring object
#'
#' @param x an object of the `"mooring"` class.
#'
#' @param ... optional arguments.
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre wire, and a float.
#' library(mooring)
#' m <- anchor() + wire(height=100) + float()
#' plot(m)

#' @importFrom graphics axis box lines mtext par points rect text
#'
#' @export
#'
#' @author Dan Kelley
plot.mooring <- function(x, ...)
{
    m <- x # we only use 'x' above to obey the R rules on generics.
    x <- rev(sapply(m, function(mi) mi$x))
    z <- rev(sapply(m, function(mi) mi$z))
    depth <- -z
    omar <- par("mar")
    omgp <- par("mgp")
    par(mar=c(3.0, 3.5, 1.5, 1), mgp=c(2, 0.7, 0))
    plot(x, depth, ylim=c(max(depth), 0), asp=1, type="l", xlab="Horizontal position [m]", ylab="Depth [m]")
    box()
    #> axis(2)
    #> axis(3)
    #> mtext("x [m]", side=3, line=par("mgp")[1])
    #> mtext("depth [m]", side=2, line=par("mgp")[1])
    usr <- par("usr")
    grid()
    rect(usr[1], usr[3], usr[2], max(depth), col="#f5d9ab")
    return(invisible())
    bottom <- -max(l) # Kludge ... maybe we want a water() function
    z <- bottom + l
    plot(rep(0, length(l)), z, xlim=c(-0.5, 0.5), xlab="", ylab="z [m]", type="n", axes=FALSE)
    axis(2)
    box()
    rect(usr[1], usr[3], usr[2], bottom, col="#f5d9ab")
    #points(rep(0, length(l)), z, pch="+")
    Z <- bottom
    for (i in seq_along(x)) {
        if (x[[i]]$type == "anchor") {
            points(0, bottom + l[i], pch=20)
            text(0, bottom + l[i], "Anchor", pos=4)
        } else if (x[[i]]$type == "float") {
            points(0, bottom + l[i], pch=20)
            text(0, bottom + l[i], "Float", pos=4)
        } else if (x[[i]]$type == "wire") {
            lines(rep(0, 2), bottom + c(l[i-1], l[i]))
        }
    }
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
            #message("WIRE height=", height, ", dheight=", dheight, ", n=", n)
            group <- group + 1
        } else {
            rval[[1+length(rval)]] <- item
        }
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
#' or a function that returns that velocity as a function of the vertical
#' coordinate.  For the second case, note that the vertical coordinate is
#' defined as height (in m) above the sea surface, so that e.g.
#' `v=function(z) 0.5*exp(z/100)` mould be used to specify a current that
#' is 0.5m/s at the surface, dropping exponentially through the water column,
#' reacing to 0.18m/s a hundred metres below the surface.
#'
#' @export
#' @author Dan Kelley
knockdown <- function(m, u=1)
{
    if (is.function(u))
        stop("FIXME: u=function() is not coded yet")
    # Trim the anchor, which is not used in this calculation
    morig <- m
    if (m[[1]]$type == "anchor") {
        m <- tail(m, -1)
        class(m) <- "mooring"
    }
    # reverse elements, to make it simpler to work from top down
    g <- 9.8
    rho <- 1027
    m <- rev(m)
    class(m) <- "mooring"
    # Depth below surface (FIXME: how to have more water above?)
    depth <- cumsum(sapply(m, function(item) item$height))
    B <- g * unlist(lapply(m, function(item) item$buoyancy))
    A <- unlist(lapply(m,
                       function(item) {
                           if (item$type == "float") item$height * item$diameter
                           else item$height * item$width
                       }))
    CD <- unlist(lapply(m, function(item) item$CD))
    height <- unlist(lapply(m, function(item) item$height))
    D <- 0.5 * A * rho * CD * u^2
    #> par(mfrow=c(2, 2))
    #> plot(B, -d);plot(D, -d)
    # computation
    n <- length(m)
    phi <- rep(NA, n)
    T <- rep(NA,n)
    phi[2] <- atan2(D[1], B[1]) # radians
    T[2] <- sqrt(D[1]^2 + B[1]^2)
    for (i in 3:n) {
        phi[i] <- atan2(D[i-1]+T[i-1]*sin(phi[i-1]), B[i-1]+T[i-1]*cos(phi[i-1]))
        T[i] <- sqrt((D[i-1]+T[i-1]*sin(phi[i-1]))^2+(B[i-1]+T[i-1]*cos(phi[i-1]))^2)
    }
    #> plot(phi, -d);plot(T, -d)
    x <- rev(cumsum(height * rev(sin(phi))))
    z <- rev(cumsum(height * rev(cos(phi))))
    depth <- cumsum(height)
    #>>> plot(x, depth, ylim=rev(range(depth)), asp=1, type="l")
    #>>> bottom <- sum(sapply(morig, function(item) item$height))
    #>>> message("bottom=",bottom)
    #>>> abline(h=bottom)
    #>>> lines(rep(0,2), c(bottom, 0), col=2)
    # reverse? change NA? append 0?
    bottom <- sum(sapply(morig, function(item) item$height))
    X <- c(tail(x, -1), 0, 0)
    Z <- c(tail(z, -1), 0, 0)
    rval <- morig
    for (i in seq_along(morig)) {
        rval[[i]]$x <- X[i]
        rval[[i]]$z <- Z[i] - bottom
    }
    class(rval) <- "mooring"
    rval
}

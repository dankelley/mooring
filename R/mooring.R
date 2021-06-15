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
#' @templateVar subclass anchor
#' @template modelTemplate
#'
#' @template buoyancyTemplate
#'
#' @template heightTemplate
#'
#' @param depth numeric value giving water depth in m.
#'
#' @template sourceTemplate
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
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="anchor"))
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
#' @templateVar subclass release
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
#' @template sourceTemplate
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
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="release"))
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
#' Creates a wire (or rope, chain, etc.) object,
#' either by looking up a known object from the database, or defining a new type.
#'
#' @templateVar subclass wire
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
#' @template sourceTemplate
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
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="wire"))
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
#' @templateVar subclass chain
#' @template modelTemplate
#'
#' @template buoyancyPerMeterTemplate
#'
#' @template widthTemplate
#'
#' @template CDTemplate
#'
#' @param length (mandatory) numeric value indicating the length (in m) of the wire.
#'
#' @template sourceTemplate
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
chain <- function(model="1in buoy chain", buoyancyPerMeter=NULL, width=NULL, CD=NULL, length=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$chains$name))
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="chain"))
    if (is.null(length))
        stop("must supply length")
    w <- which(mooringElements$chains$name == model)
    if (1 == length(w)) {
        me <- mooringElements$chains[w,]
        if (!is.null(buoyancyPerMeter))
            warning("ignoring supplied buoyancyPerMeter, because \"", model, "\" is already in the database\n")
        if (!is.null(width))
            warning("ignoring supplied width, because \"", model, "\" is already in the database\n")
        if (!is.null(CD))
            warning("ignoring supplied CD, because \"", model, "\" is already in the database\n")
        buoyancyPerMeter<- me$buoyancyPerMeter
        width <- me$width
        CD <- me$CD
        source <- me$source
    } else {
        if (is.null(buoyancyPerMeter)) stop("must supply buoyancyPerMeter, if creating a new chain model")
        if (is.null(width)) stop("must supply width, if creating a new chain model")
        if (is.null(CD)) stop("must supply CD, if creating a new chain model")
        source <- ""
    }
    rval <- list(model=model, source=source, buoyancyPerMeter=buoyancyPerMeter, height=length, width=width, CD=CD, area=length*width)
    class(rval) <- c("mooring", "chain")
    rval
}                                      # chain()

#' Create a connector object
#'
#' Create an object that describes mooring connector such as shackles,
#' either by looking up a known object from the database, or defining a new type.
#'
#' Many of the built-in items have `height` and `width` defined
#' to be zero, because the sources did not list these things.  Likely, this is not
#' terribly important, because these values are only used in computing drag, and
#' connectors tend to be small, and are not likely to have significant drag compared
#' with tens or hundreds of meters of wire, or with large floats.
#'
#' Also, it is worth noting that there are built-in connector objects that might not
#' be thought of as connectors, e.g. `"ballast weight"`.
#'
#' @templateVar subclass connector
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
#' @template sourceTemplate
#'
#' @return `connector` returns an object of the `"mooring"` class and `"connector"` subclass.
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
#' connector("?")
#'
#' @author Dan Kelley
connector <- function(model="swivel", buoyancy=NULL, height=NULL, width=NULL, CD=NULL)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    if (model == "?")
        return(sort(mooringElements$connectors$name))
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="connector"))
    w <- which(mooringElements$connectors$name == model)
    if (1 == length(w)) {
        me <- mooringElements$connectors[w,]
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
        if (is.null(buoyancy)) stop("must supply buoyancy, if creating a new connectors model")
        if (is.null(height)) stop("must supply height, if creating a new connectors model")
        if (is.null(width)) stop("must supply width, if creating a new connectors model")
        if (is.null(CD)) stop("must supply CD, if creating a new connectors model")
        source <- ""
    }
    rval <- list(model=model, source=source, buoyancy=buoyancy, height=height, width=width, CD=CD, area=height*width)
    class(rval) <- c("mooring", "connector")
    rval
}                                      # connector()

#' Create a float object
#' @templateVar subclass float
#'
#' Create a float object, either by looking up a known object from the database,
#' or defining a new type.
#'
#'
#' Note that `HMB` in a name is a short-hand for `Hydrofloat Mooring Buoy`.  Data for these
#' floats was extracted from Reference 1 on 2021-05-19 by Dan Kelley, with the `CD` value
#' being set at 0.65 (in the absence of any data from the manufacture) because that
#' value is used in Dewey's dataset for many floats.
#'
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
#' @template sourceTemplate
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
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="float"))
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
    # Floats are assumed to be circular in the flow direction, following
    # Dewey's convention, so the area is pi*radius^2.
    rval <- list(model=model, source=source, buoyancy=buoyancy, height=height, diameter=diameter, CD=CD, area=pi*(diameter/2)^2)
    class(rval) <- c("mooring", "float")
    rval
}                                      # float()

#' Create a instrument object
#'
#' Create an instrument object,
#' either by looking up a known object from the database, or defining a new type.
#'
#' @templateVar subclass instrument
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
#' @template sourceTemplate
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
    else if (substring(model, 1, 1) == "?")
        return(findElement(substring(model, 2), search="instrument"))
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
    depth <- if (inherits(m1[[1]], "anchor")) m1[[1]]$depth
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

#' Get mooring/element drag coefficient
#'
#' @template meTemplate
#'
#' @return `CD` returns a numeric vector of drag coefficient(s).
#'
#' @examples
#' library(mooring)
#' CD(float())
#'
#' @export
#'
#' @author Dan Kelley
CD <- function(m)
{
    if (isMooring(m)) {
        sapply(m, function(item) if ("anchor" == class(item)[2]) 0 else item$CD)
    } else {
        if (length(class(m)) == 2) m$CD else stop("area can only be computed for a mooring or an element")
    }
}


#' Get mooring/element drag force
#'
#' This looks up element areas with [area()] and drag
#' coefficients with [CD()], and then computes drag
#' force (in Newtons) with
#' \eqn{(1/2)*area*rho*CD*u^2}{(1/2)*area*rho*CD*u^2}
#'
#' @template meTemplate
#'
#' @template uTemplate
#'
#' @template rhoTemplate
#'
#' @return `drag` returns a numeric vector of horizontal drag force, expressed in N.
#'
#' @export
#'
#' @author Dan Kelley
drag <- function(m, u, rho=1027)
{
    if (length(rho) > 1L && length(rho) != length(m))
        stop("length of rho, ", length(rho), " must match length of m, ", length(m))
    uSquared <- if (is.function(u)) sapply(depth(m),u)^2 else u^2
    0.5 * area(m) * rho * CD(m) * uSquared
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
    n <- length(dots)
    if (n < 3L)
        stop("need 2 or more arguments")
    if (!inherits(dots[[1]], "anchor"))
       stop("first argument must be created with anchor()")
    w <- which(sapply(dots, function(x) !inherits(x, "mooring")))
    if (length(w))
        stop("these are the indices of elements that are not of class 'mooring': ", paste(w, collapse=" "))
    w <- which(2 != sapply(dots, function(x) length(class(x))))
    if (length(w))
        stop("these are the indices of elements that are not elementary: ", paste(w, collapse=" "))
    # All checks seem OK, so reverse parameters and create the return value.
    rval <- rev(dots)
    depth <- rval[[n]]$depth # NOTE: only anchor() objects have this, but we know we have one
    height <- rev(cumsum(sapply(rev(rval), function(x) x$height)))
    message("height: ", paste(height, collapse=" "))
    message("height-depth: ", paste(height-depth, collapse=" "))
    T <- cumsum(buoyancy(rval[-n]))
    message("n=", n, ", depth=", depth, ", length(T)=", length(T))
    x0 <- 0
    alongBottom <- 0L
    for (i in seq_along(rval)) {
        z <- height[i] - depth
        # If the mooring outweighs the flotation, run some of it along the bottom.
        if (z < (-depth)) {
            alongBottom <- alongBottom + 1L
            z <- -depth
            x0 <- x0 + height[i]
        }
        rval[[i]]$x <- x0
        rval[[i]]$z <- z               # z is at the *top* of the element
        rval[[i]]$T <- if (i == n) NA else T[i]
    }
    if (alongBottom)
        warning("insufficient mooring buoyancy; placed ", alongBottom, " elements on the bottom")
    class(rval) <- "mooring"
    attr(rval, "waterDepth") <- depth
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
        if (is.null(attr(x, "discretised"))) {
            cat("Mooring with", n, "elements, listed from the top down:\n")
        } else {
            if (is.null(attr(x, "u"))) {
                cat("Discretised mooring with", n, "elements, listed from the top down:\n")
            } else {
                cat("Discretised, knocked-over mooring with", n, "elements, listed from the top down:\n")
            }
        }
        prefix <- "  "
    }
    # The 'lastWas*' variables keep track of repeats, e.g. as created by discretise().
    # This scheme will not work if a mooring is contructed with wire or chain elements
    # that are not joined by a connector, but that should not happen if the mooring
    # reflects reality.  If this poses a problem, we could also look at the 'group'
    #' part of the item.
    lastWasChain <- FALSE
    lastWasWire <- FALSE
    i <- 1L
    while (i <= n) {
        xi <- if (elementary) x else x[[i]]
        #> cat("i=", i, " class=", paste(class(xi), collapse=","), "\n", sep="")
        if (inherits(xi, "anchor")) {
            cat(sprintf("%s%d: '%s' anchor, %gkg, height %gm, in %gm water depth",
                        prefix, i, xi$model, xi$buoyancy, xi$height, xi$depth), sep='')
            cat(sprintf(", x=%g m, z=%g m\n", xi$x, xi$z))
            lastWasChain <- lastWasWire <- FALSE
        } else if (inherits(xi, "chain")) {
            # See if there are more chain elements following this.
            count <- 1L
            while (count <= n) {
                if (!inherits(x[[i+count]], "chain"))
                    break
                count <- count + 1L
            }
            #> message("chain count: ", count)
            if (count == 1L) {
                cat(sprintf("%s%d: '%s' chain, %gkg, length %gm, width %gm\n",
                            prefix, i, xi$model,
                            xi$buoyancyPerMeter*xi$height,
                            xi$height, xi$width), sep="")
            } else {
                cat(sprintf("%s%d-%d: '%s' chain, %gm, length %gm, width %gm\n",
                            prefix, i, i+count-1L, xi$model,
                            xi$buoyancyPerMeter*xi$height,
                            xi$height, xi$width), sep="")
            }
            i <- i + count - 1 # account for skipped-over elements
        } else if (inherits(xi, 'connector')) {
            cat(sprintf("%s%d: '%s' connector, %gkg, height %gm, width %gm",
                        prefix, i, xi$model, xi$buoyancy, xi$height, xi$width), sep='')
            cat(sprintf(", x=%g m, z=%g m\n", xi$x, xi$z))
            lastWasChain <- lastWasWire <- FALSE
        } else if (inherits(xi, 'float')) {
            cat(sprintf("%s%d: '%s' float, %gkg, height %gm, diameter %gm",
                         prefix, i, xi$model, xi$buoyancy, xi$height, xi$diameter), sep='')
            cat(sprintf(", x=%g m, z=%g m\n", xi$x, xi$z))
            lastWasChain <- lastWasWire <- FALSE
        } else if (inherits(xi, 'instrument')) {
            cat(sprintf("%s%d: '%s' instrument, %gkg, area %gm^2",
                         prefix, i, xi$model, xi$buoyancy, xi$area), sep='')
            cat(sprintf(", x=%g m, z=%g m\n", xi$x, xi$z))
            lastWasChain <- lastWasWire <- FALSE
        } else if (inherits(xi, 'release')) {
            cat(sprintf("%s%d: '%s' release, %gkg, height %gm, width %gm",
                        prefix, i, xi$model, xi$buoyancy, xi$height, xi$width), sep='')
            cat(sprintf(", x=%g m, z=%g m\n", xi$x, xi$z))
            lastWasChain <- lastWasWire <- FALSE
        } else if (inherits(xi, "wire")) {
            # See if there are more wire elements following this.
            count <- 1L
            while (count <= n) {
                if (!inherits(x[[i+count]], "wire"))
                    break
                count <- count + 1L
            }
            #> message("wire count: ", wire)
            if (count == 1L) {
                cat(sprintf("%s%d: '%s' wire, %gkg, length %gm, diameter %gm\n",
                            prefix, i, xi$model,
                            xi$buoyancyPerMeter*xi$height,
                            xi$height,
                            xi$diameter), sep="")
            } else {
                cat(sprintf("%s%d-%d: '%s' wire, %gkg, length %gm, width %gm\n",
                            prefix, i, i+count-1L, xi$model,
                            xi$buoyancyPerMeter*xi$height,
                            xi$height, xi$diameter), sep=)
            }
            i <- i + count - 1 # account for skipped-over elements
        } else {
            stop("unknown class c(\"", paste(class(xi), collapse="\", \""), "\")")
        }
        i <- i + 1L
    }
    invisible(x)
}

#' Plot a mooring object
#'
#' @param x an object of the `"mooring"` class.
#'
#' @param which character value indicating the desired plot, with
#' choices: `"shape"` (the default), `"knockdown"`, `"tension"` and `"velocity"`.
#'
#' @param showInterfaces logical value indicating whether to indicate the water
#' surface with a blue line and the ocean bottom with a brown line.
#'
#' @param showDepths logical value indicating whether to indicate the depths of
#' floats, to the left of the symbols.
#'
#' @param showLabels logical value indicating whether to indicate anchors,
#' instruments and floats with `A`, `I` and `F`, respectively.
#'
#' @param showDetails logical value indicating whether to show details for anchors,
#' instruments and floats with text.
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
#' @param xlim optional numeric vector of length 2 that can
#' be used to specify the limits of the horizontal axis.
#'
#' @param type character value indicating type of plot. The default, `"l"`,
#' means to draw lines, while e.g. `"p"` means to draw points.
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
#' @importFrom graphics abline axis box lines mtext par plot.window points rect strwidth text
#' @importFrom grDevices extendrange
#'
#' @export
#'
#' @author Dan Kelley
plot.mooring <- function(x, which="shape",
                         showInterfaces=TRUE, showDepths=TRUE, showLabels=TRUE, showDetails=FALSE,
                         fancy=FALSE, title="",
                         mar=c(1.5, 3.5, 3.5, 1), mgp=c(2, 0.7, 0),
                         xlim=NULL,
                         type="l",
                         ...)
{
    m <- x # we only use 'x' above to obey the R rules on generics.
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    # Handle showDetails, converting it to a logical if not, and creating a list if required
    if (is.list(showDetails)) {
        detailsControl <- list(cex=if ("cex" %in% names(showDetails)) showDetails$cex else 0.8,
                               col=if ("col" %in% names(showDetails)) showDetails$col else "darkblue")
        showDetails <- TRUE
    } else if (is.logical(showDetails)) {
        detailsControl <- list(cex=0.8,
                               col="darkblue")
    }
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
    depth <- -z
    waterDepth <- if ("anchor" == class(m[[1]])[2]) m[[1]]$depth
        else abs(min(depth))
    mooringDebug(debug, waterDepth, overview=TRUE)
    par(mar=mar, mgp=mgp)
    # Determine depth scale by doing a sort of dry run of a shape plot
    xlimOrig <- xlim
    if (is.null(xlim)) xlim <- extendrange(c(x(m), 0))
    plot.window(0, 0, xlim=xlim, ylim=c(waterDepth, 0), asp=1, log="")
    xlim <- xlimOrig
    usrShape <- par("usr")
    # Handle velocity, which does not involve mooring elements and is a special case
    if (which == "velocity") {
        uattr <- attr(m, "u")
        d <- seq(attr(m, "waterDepth"), 0, length.out=200)
        velocityProfile <- if (is.function(uattr)) uattr(d) else rep(uattr, length(d))
        plot(velocityProfile, d, ylim=usrShape[3:4], yaxs="i", ylab="", xlab="", type=type, axes=FALSE)
        box()
        grid()
        if (showInterfaces) {
            abline(h=0, col=colWater, lwd=2)
            abline(h=waterDepth, col=colBottom, lwd=2)
        }
        if (type == "l") lines(velocityProfile, d, lwd=1.4*par("lwd"))
        else points(velocityProfile, d)
        axis(2)
        mtext("Depth [m]", side=2, line=par("mgp")[1], cex=par("cex"))
        axis(3)
        mtext("Velocity [m/s]", side=3, line=par("mgp")[1], cex=par("cex"))
        mtext(title, side=1, cex=par("cex"))
        return(invisible(NULL))
    }
    x <- switch(which,
                "shape"=x(m),
                "knockdown"=depth(m) - depth(m, stagnant=TRUE),
                "tension"=tension(m))
    if (is.null(x))
        stop("which must be \"shape\", \"knockdown\", \"tension\" or \"velocity\"")
    xstagnant <- if (which == "shape") rep(0, length(m)) else if (which == "tension") tension(m, stagnant=TRUE)
    mooringDebug(debug, x, overview=TRUE, round=2)
    mooringDebug(debug, z, overview=TRUE, round=2)
    ylim <- c(waterDepth, 0)
    # Determine depth scale by doing a sort of dry run of a shape plot
    #. message("xlim given? ", !is.null(xlim))
    if (is.null(xlim)) xlim <- extendrange(c(x, 0))
    plot.window(0, 0, xlim=xlim, ylim=ylim, asp=if (which=="shape") 1, log="")
    usrShape <- par("usr")
    #. message(oce::vectorShow(xlim))
    #. message("usrShape[3:4] is ", usrShape[3], " ", usrShape[4])
    plot(x, depth, xlim=xlim, ylim=usrShape[3:4], yaxs="i", asp=if (which=="shape") 1,
         type=type, xlab="", ylab="", axes=FALSE)
    xlab <- switch(which,
                   "shape"="Horizontal Coordinate [m]",
                   "knockdown"="Depth Increase [m]",
                   "tension"="Tension [kg]",
                   "velocity"="Velocity [m/s]")
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
        if (showInterfaces) {
            abline(h=0, col=colWater, lwd=2)
            abline(h=waterDepth, col=colBottom, lwd=2)
        }
    }
    # Redraw to cover grid
    if (type == "l") lines(x, depth, lwd=1.4*par("lwd"))
    else points(x, depth, lwd=1.4*par("lwd"))
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
    cex <- if (showDetails) detailsControl$cex else 1
    pch <- if (showDetails) detailsControl$pch else 20
    col <- if (showDetails) detailsControl$col else 1
    for (i in seq_along(m)) {
        type <- class(m[[i]])[2]
        xi <- x[i]
        zi <- m[[i]]$z
        if (type == "anchor") {
            if (debug)
                cat("i=", i, " (anchor at xi=", xi, ", zi=", zi, ")\n")
            points(xi, -zi, pch=pch, cex=cex, col=col)
            if (showLabels && !showDetails)
                text(xi, -zi, "A", pos=2)
        } else if (type == "float") {
            if (debug)
                cat("i=", i, " (float at xi=", xi, ", zi=", zi, ")\n")
            points(xi, -zi, pch=pch, cex=cex, col=col)
            if (showLabels && !showDetails)
                text(xi, -zi, "F", pos=4)
            if (showDepths && !showDetails) {
                if (abs(zi) < 1) text(xi, -zi, sprintf("%.3fm", -zi), pos=2)
                else text(xi, -zi, sprintf("%.1fm", -zi), pos=2)
            }
        } else if (type == "instrument") {
            if (debug)
                cat("i=", i, " (instrument at xi=", xi, ", zi=", zi, ")\n")
            points(xi, -zi, pch=pch, cex=cex, col=col)
            if (showLabels && !showDetails)
                text(xi, -zi, "I", pos=4)
            if (showDepths && !showDetails) {
                if (abs(zi) < 1) text(xi, -zi, sprintf("%.3fm", -zi), pos=2)
                else text(xi, -zi, sprintf("%.1fm", -zi), pos=2)
            }
        } else if (type == "wire") {
            #> message("draw wire??")
        }
    }
    if (showDetails) {
        labels <- NULL
        depths <- depthsStagnant <- NULL
        xs <- xsStagnant <- NULL
        #> message(oce::vectorShow(which))
        for (i in seq_along(m)) {
            mi <- m[[i]]
            if (inherits(mi, "anchor") || inherits(mi, "connector") || inherits(mi, "float") || inherits(mi, "instrument") || inherits(mi, "release")) {
                #> message("anchor, release or float at i=", i)
                depths <- c(depths, -mi$z)
                xs <- c(xs, if (which=="shape") mi$x else mi$z0 - mi$z)
                xsStagnant <- c(xsStagnant, 0)
                labels <- c(labels, mi$model)
            }
        }
        #> message(oce::vectorShow(xs))
        N <- length(depths)
        usr <- par("usr")
        fac <- 1 / 15
        cex <- 0.75
        xspace <- fac * (usr[2] - usr[1])
        yspace <- fac * (usr[4] - usr[3])
        X0 <- max(xs, na.rm=TRUE)
        #> message(oce::vectorShow(X0))
        X <- rep(X0+xspace, N)
        Y <- seq(usr[3]+yspace, usr[4]-yspace, length.out=N)
        widths <- strwidth(labels, cex=cex)
        text(X, Y, labels, pos=4, cex=detailsControl$cex, col=detailsControl$col)
        #points(X, Y, col=2)
        #points(xs, depths, col="magenta", pch=20)
        for (i in seq_along(xs)) {
            lines(c(xs[i], X[i]), c(depths[i], Y[i]), col=detailsControl$col, lwd=0.6*par("lwd"))
        }
    }
    mtext(title, side=1, cex=par("cex"))
}

#' Discretise the chain and wire portions of a mooring
#'
#' Break up `chain` and `wire` portions of a mooring into smaller chunks,
#' so that the deformation by a current can be traced more
#' accurately by [knockdown()].
#'
#' @template mTemplate
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
#' @aliases discretize
#'
#' @author Dan Kelley
discretise <- function(m, by=1)
{
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    if (by <= 0)
        stop("by must be a positive number")
    n <- length(m)
    rval <- list()
    group <- 1
    for (item in m) {
        isWire <- inherits(item, "wire")
        isChain <- inherits(item, "chain")
        if (isWire || isChain) {
            height <- item$height
            n <- max(1L, as.integer(round(height/by)))
            dheight <- height / n
            portion <- item
            portion$height <- dheight
            portion$area <- dheight * if (isWire) portion$diameter else portion$width
            portion$group <- group # so we can undo this later
            for (i in seq_len(n))
                rval[[1+length(rval)]] <- portion
            group <- group + 1
        } else {
            rval[[1+length(rval)]] <- item
        }
    }
    # Fix up the z and T values
    z <- -rval[[length(rval)]]$depth + cumsum(sapply(rval, function(x) x$height))
    T <- tension(rval, stagnant=TRUE)
    for (i in seq_along(rval)) {
        rval[[i]]$z <- z[i]
        rval[[i]]$T <- T[i]
    }
    class(rval) <- "mooring"
    attr(rval, "discretised") <- TRUE
    attr(rval, "waterDepth") <- attr(m, "waterDepth")
    rval
}                                      # discretise

#' Compute how a mooring is knocked down by a current
#'
#' The current may be a depth-independent or depth-dependent,
#' as specified by the `u` argument.  The returned result has
#' an attribute named `u` that holds the value of that
#' argument, and this is how a later call to [plot.mooring()]
#' is able to display a velocity profile; see
#' \dQuote{Examples} 2 and 3.
#'
#' @param m an object of the `"mooring"` class, usually created with
#' [discretise()].
#'
#' @template uTemplate
#'
#' @template debugTemplate
#'
#' @return a new `mooring` object representing the deformed mooring, with
#' `x` and `z` values updated (and original values saved as `x0` and `z0`).
#'
#' @examples
#' # Illustrate importance of drag on the wire.
#' library(mooring)
#' m <- mooring(anchor(depth=200), wire(length=180), float("HMB 20"))
#' md <- discretise(m)
#'
#' # Example 1: no current
#' plot(md)
#'
#' # Example 2: uniform 0.5 m/s (approx. 1 knot) current
#' par(mfrow=c(1, 2))
#' k1 <- knockdown(md, u=0.5)
#' plot(k1, which="velocity")
#' plot(k1)
#'
#' # Example 3: 0.5 m/s current at surface, decaying exponetially below
#' k2 <- knockdown(md, u=function(depth) 0.5*exp(-depth/100))
#' par(mfrow=c(1, 2))
#' plot(k2, which="velocity")
#' plot(k2)
#'
#' @importFrom graphics grid
#' @importFrom utils tail
#' @export
#' @author Dan Kelley
knockdown <- function(m, u=1, debug=0L)
{
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    if (!is.null(attr(m, "u")))
        stop("cannot apply knockdown() to the result of a previous call")
    if (is.null(attr(m, "discretised")))
        warning("accuracy is better if discretise() is used first\n")
    att <- attributes(m)

    debug <- as.integer(max(0, debug))
    if (is.function(u) && debug > 0L) {
        #z <- sapply(m, function(x) x$z)
        #mooringDebug(debug, z, overview=TRUE)
        warning("FIXME: u=function() case is not fully coded yet (no iteration is done)\n")
    }
    # rename x,z into x0,z0 for the stagnant (u=0) case
    for (i in seq_along(m)) {
        m[[i]]$x0 <- m[[i]]$x
        m[[i]]$z0 <- m[[i]]$z
    }
    # Trim the anchor, which is not used in this calculation
    morig <- m
    waterDepth <- anchorHeight <- 0
    # Remove the anchor, after saving waterDepth and anchorHeight
    if (inherits(m[[1]], "anchor")) {
        waterDepth <- m[[1]]$depth
        anchorHeight <- m[[1]]$height
        m <- tail(m, -1) # discard the anchor
        class(m) <- "mooring"
    } else {
        stop("the mooring must start with an object creatred with anchor()")
    }
    # reverse elements, to make it simpler to work from top down
    mrev <- rev(m)
    class(mrev) <- "mooring"
    attributes(mrev) <- att
    # Depth below surface (FIXME: how to have more water above?)
    depth <- cumsum(sapply(mrev, function(item) item$height))
    mooringDebug(debug, depth, overview=TRUE)
    B <- g * buoyancy(mrev) # note multiplication by g, to get Newtons from kg
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
    # Trim the angle, which makes wire/cable lay out on the bottom, if
    # there is insufficient buoyancy to support it.
    phi <- ifelse(phi > pi/2, pi/2, phi)
    # The first phi and T are NA, so trim them
    phi <- tail(phi, -1L)
    T <- tail(T, -1L)
    #> cat(oce::vectorShow(phi, showNA=TRUE))
    #> cat(oce::vectorShow(T, showNA=TRUE))
    mooringDebug(debug, "Original values:\n")
    mooringDebug(debug, phi*180/pi, overview=TRUE, round=1)
    mooringDebug(debug, T/g, overview=TRUE, round=1)
    # Find x and z by integrating from bottom up.
    phiRev <- rev(phi)
    heightRev <- rev(height)[-1]
    x <- rev(cumsum(heightRev*sin(phiRev)))
    z <- rev(cumsum(heightRev*cos(phiRev))) - waterDepth
    #> cat(oce::vectorShow(x, showNA=TRUE))
    #> cat(oce::vectorShow(z, showNA=TRUE))
    mooringDebug(debug, x, overview=TRUE, round=2)
    mooringDebug(debug, z, overview=TRUE, round=2)
    rval <- morig
    # Add top of anchor, and bottom of anchor.  Then reverse, to match original 'm'.
    rx <- rev(x)
    rz <- rev(z)
    xNew <- c(0, rx)
    zNew <- c(-waterDepth, rz)
    #> cat(oce::vectorShow(xNew, showNA=TRUE))
    #> cat(oce::vectorShow(zNew, showNA=TRUE))
    #> plot(xNew,zNew,type="l")
    #> points(xNew[1],zNew[1])
    #> mtext(sprintf("bot x=%f z=%f",xNew[1],zNew[1]))
    #> browser()
    x <- xNew # rev(c(x, sum(head(x,2))/2.0, 0))
    z <- zNew # rev(c(z, -waterDepth + morig[[1]]$height, -waterDepth))
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
    #> cat("before replace last point:\n")
    #> cat(oce::vectorShow(sapply(rval,\(x)x$x), showNA=TRUE))
    #> cat(oce::vectorShow(sapply(rval,\(x)x$z), showNA=TRUE))
    # last two are NA.  Let's just linearly extrapolate to find x and z, since
    # this is almost certainly a float, and the element below is therefore just 1 metre
    # of wire or chain, which won't be bending a whole lot in 1 metre.
    n <- length(rval)
    rval[[n]]$x <- 2*rval[[n-1]]$x - rval[[n-2]]$x
    rval[[n]]$z <- 2*rval[[n-1]]$z - rval[[n-2]]$z
    #> cat("finally:\n")
    #> cat(oce::vectorShow(sapply(rval,\(x)x$x), showNA=TRUE))
    #> cat(oce::vectorShow(sapply(rval,\(x)x$z), showNA=TRUE))
    class(rval) <- "mooring"
    attr(rval, "u") <- u
    rval
}                                      # knockdown()

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
#' This is the depth of the *top* of the element. See also
#' [z()], which is the negative of the result from `depth()`.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
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
depth <- function(m, stagnant=FALSE)
{
    -z(m, stagnant=stagnant)
}

#' Return horizontal coordinate, in m, of elements in mooring.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
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
x <- function(m, stagnant=FALSE)
{
    if (stagnant) {
        if ("x0" %in% names(m[[1]]))
            sapply(m, function(mi) mi$x0)
        else
            sapply(m, function(mi) mi$x)
    } else {
        sapply(m, function(mi) mi$x)
    }
}

#' Return vertical coordinate, in m, of elements in mooring.
#'
#' This is the z coordinate of the *top* of the element. See also
#' [depth()], which is the negative of the result from `z()`.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
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
z <- function(m, stagnant=FALSE)
{
    if (stagnant) {
        if ("z0" %in% names(m[[1]]))
            sapply(m, function(mi) mi$z0)
        else
            sapply(m, function(mi) mi$z)
    } else {
        sapply(m, function(mi) mi$z)
    }
}

#' Return tension, in kg, between elements in mooring.
#'
#' The first element (for the anchor) is repeated, so that the
#' length of the returned result matches the length of `m`.
#'
#' @template mTemplate
#'
#' @template stagnantTemplate
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

#' Get mooring/element area
#'
#' @template meTemplate
#'
#' @return `area` returns a numeric value of the area viewed from a horizontal, in m^2.
#'
#' @examples
#' library(mooring)
#' area(float())
#'
#' @export
#'
#' @author Dan Kelley
area <- function(m)
{
    if (isMooring(m)) {
        sapply(m, function(mi) mi$area)
    } else {
        if (length(class(m)) == 2) m$area else stop("area can only be computed for a mooring or an element")
    }
}

#' Get mooring/element buoyancy, expressed in kg
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
#' @template meTemplate
#'
#' @template debugTemplate
#'
#' @return `buoyancy` returns a numeric vector of buoyancy, expressed in kg.
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


#' Find a mooring element with a fuzzy search
#'
#' `findElement` does a fuzzy search for an element model, using
#' [agrep()].  The output (if any) is in the form of suggested calls
#' to element-creating functions
#' [anchor()], [chain()], [connector()], [float()], [instrument()], and [wire()].
#' The list is in alphabetical order, not the order of the closeness of the match.
#'
#' `findElement` is used by e.g. `float("?BUB")`.
#'
#' @param e character value to be used for the fuzzy match, passed on to [agrep()].
#'
#' @param ignore.case logical value, passed to [agrep()]. The default is to ignore case.
#'
#' @param max.distance numeric value, passed to [agrep()]. The default usually catches
#' relevant cases; see the documentation for [agrep()] for the (somewhat subtle) meaning
#' of this argumebnt.
#'
#' @param search character vector holding the categories to be searched for.
#'
#' @return `findElement` returns (silently) a list of the suggested function calls.
#'
#' @examples
#' library(mooring)
#' findElement("nylon")
#'
#' @export
#'
#' @author Dan Kelley
findElement <- function(e, search=c("anchor", "chain", "connector", "float", "instrument", "wire"), ignore.case=TRUE, max.distance=0.1)
{
    data("mooringElements", package="mooring", envir=environment())
    mooringElements <- get("mooringElements")
    rval <- NULL
    for (element in search) {
        names <- mooringElements[[paste0(element,"s")]]$name
        match <- try(agrep(e, names, ignore.case=ignore.case, max.distance=max.distance), silent=TRUE)
        if (!inherits(match, "try-error")) {
            for (i in seq_along(match)) {
                rval <- c(rval, paste0(element, "('", names[match], "')"))
            }
        }
    }
    if (is.null(rval)) {
        cat("Sorry, found no good matches for \"", e, "\".\n", sep="")
    } else {
        cat("Some possible matches:\n")
        rval <- sort(unique(rval))
        for (i in seq_along(rval))
            cat("    ", rval[i], "\n")
    }
    invisible(rval)
}


#' Summarize a mooring
#'
#' @param object a mooring object, created by [mooring()].
#'
#' @param ... ignored.
#'
#' @examples
#' library(mooring)
#' # Simple case
#' m <- mooring(anchor(depth=100), wire(length=80), float("HMB 20"))
#' summary(m)
#' # Illustrate how it collects wire subintervals
#' md <- discretise(m)
#' mdk <- knockdown(md, 0.5)
#' summary(mdk)
#'
#' @export
#'
#' @author Dan Kelley
summary.mooring <- function(object, ...)
{
    m <- object # use a more useful name
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    n <- length(m)
    lastWasWire <- FALSE
    wireLength <- 0
    iWireStart <- 0
    for (i in seq_len(n)) {
        mi <- m[[i]]
        if (inherits(mi, "wire")) {
            if (!lastWasWire)
                iWireStart <- i
            lastWasWire <- TRUE
            wireLength <- wireLength + mi$height
        } else {
            if (lastWasWire) {
                # fake an element (and blank out the location)
                W <- m[[iWireStart]]
                W$height <- wireLength
                #cat("iWireStart=", iWireStart, "\n")
                print(W)
            }
            lastWasWire <- FALSE
            wireLength <- 0
            print(mi)
        }
    }
}



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
#' @return an object of the `"mooring"` class, with `type` equal to `"anchor"`.
#' @family functions that create mooring objects
#' @export
#' @author Dan Kelley
anchor <- function(model="default_anchor")
{
    rval <- list(list(type="anchor", model=model, height=0.3)) # guess on height
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
    rval <- list(list(type="anchor", model=model, height=1.0)) # guess on length
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
    rval <- list(list(type="chain", model=model, buoyancy=buoyancy, height=height, width=width, CD=CD))
    class(rval) <- "mooring"
    rval
}


#' Create a float object
#'
#' Create a float object.
#'
#' The permitted `model` values are listed alphabetically below, as listed with `float("?")`:
#'\preformatted{
#' [1] "16in Viny"        "17 in glass"      "28in ORE"        
#' [4] "30in float"       "37in ORE"         "41in ORE"        
#' [7] "48in ORE"         "54cm Alum CAP3"   "54x30inClamShell"
#'[10] "61in ORE"         "ADCP FloatTech"   "BENTHOS 17in"    
#'[13] "Billings-12in"    "BPS Float"        "CDMS 1.2m sphere"
#'[16] "CDMS 2m sphere"   "Double 16inViny"  "Double 17in"     
#'[19] "FloatTech CF-12"  "Geodyn 8' Toroid" "Kiel SFS40in"    
#'[22] "Top of Tow Rope"  "Torp. flt, 3-28"  "Tripple FT CF-12"
#'[25] "Trpl 12in glass"  "Trpl 16 in Viny" 
#'}
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
    rval <- list(list(type="float", model=model, buoyancy=buoyancy, height=height, diameter=diameter, CD=CD))
    class(rval) <- "mooring"
    rval
}

#' Combine two mooring objects
#'
#' The first object is "placed" the second.
#' @param m1,m2 objects of the `"mooring"` class.
#' @family functions that create mooring objects
#' @examples
#' library(mooring)
#' m <- anchor() + wire(height=100) + float()
#' print(m)
#' plot(m)
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
#' @examples
#' library(mooring)
#' m <- anchor() + wire(height=100) + float()
#' print(m)
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
#' @param ... optional arguments.
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre wire, and a float.
#' library(mooring)
#' m <- anchor() + wire(height=100) + float()
#' plot(m)

#' @importFrom graphics axis box lines mtext par points rect text
#' @export
#' @author Dan Kelley
plot.mooring <- function(x, ...)
{
    l <- cumsum(sapply(x, function(xi) xi$height))
    bottom <- -max(l) # Kludge ... maybe we want a water() function
    z <- bottom + l
    plot(rep(0, length(l)), z, xlim=c(-0.5, 0.5), xlab="", ylab="z [m]", type="n", axes=FALSE)
    axis(2)
    box()
    usr <- par("usr")
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
}



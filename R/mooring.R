# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

###################
# 1. overall docs #
###################

#' mooring: A Package for Analysing Oceanographic Moorings
#'
#' The mooring package provides functions for working with
#' oceanographic moorings.  The example provides a good
#' starting point for learning, and the documentation
#' for the key function, [mooring()], provides the
#' next logical step in learning, after which the
#' vignettes should be consulted.
#'
#' @examples
#' library(mooring)
#' # Illustrate the deformation of a 100-m mooring in a 0.5 m/s
#' # (roughly 1 knot) current. Buoyancy is provided with a float
#' # of diameter 20 inches.
#' m <- mooring(anchor(depth=120), wire(length=100), float("HMB 25"))
#' par(mfrow=c(1, 3))
#' plot(m)
#' # Must discretise the wire portion to resolve the shape.
#' md <- discretise(m)
#' mdk <- knockdown(md, u=0.5)
#' plot(mdk)
#'
#' @docType package
#'
#' @name mooring-package
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
#' Designing and Analyzing Oceanographic Moorings." Marine Models, vol. 1, no. 1
#' (December 1, 1999): 103–57. https://doi.org/10.1016/S1369-9350(00)00002-X
#'
#' Dewey, Richard. "Mooring Design and Dynamics:
#' A Matlab Package for Designing and Testing
#' Oceanographic Moorings And Towed Bodies."
#' Accessed May 15, 2021.
#' http://canuck.seos.uvic.ca/rkd/mooring/mdd/mdd.php
#' http://canuck.seos.uvic.ca/rkd/mooring/moordyn.php
NULL

###################
# 2. setup        #
###################

g <- 9.81

#' Detect whether an object is a mooring
#'
#' An object is a mooring if it is a list that has more than one element, and
#' if each element inherits from the `"mooring"` class.  For example, the
#' output of [mooring()] is a mooring, but the output of [anchor()] is not.
#' This function is mainly designed for use within the package that that, e.g.
#' [knockdown()] will produce an error if its first argument is not a mooring.
#'
#' @param m an object to be tested
#'
#' @export
isMooring <- function(m=NULL) {
    is.list(m) && length(m) > 1 && all(sapply(m, function(mi) inherits(mi, "mooring")))
}

#' Create a mooring
#'
#' Assemble elementary components into a mooring string.
#' The first argument must be an anchor, created
#' with [anchor()].  These are stored in reverse order
#' in the return value (see Example 1).
#'
#' @param ... two or more elementary objects, as created by
#' [anchor()], [release()], [chain()],
#' [wire()], [connector()], [instrument()], [misc()] or [float()].
#'
#' @examples
#' # Example 1: most basic case: anchor, line, float.
#' library(mooring)
#' m <- mooring(anchor(depth=100), wire(length=80), float())
#' m                          # whole-mooring overview
#' head(m, 1)                 # float overview
#'
#' # Example 2: compare a simple mooring (with inadequate
#' # buoyancy) with a stiffer one that has extra buoyancy
#' # near an instrument.  Note that the stiffness is
#' # concentrated in the depth region that is being sampled
#' # with the instrument (a "microcat").
#' A <- anchor(depth=150)
#' W <- function(l) wire("3/8in wire/jack", length=l)
#' MC <- instrument("SBE37 microcat clamp-on style")
#' BUB <- float("BUB 2x17in glass")
#' top <- float("Trpl 16in Viny")
#' m1 <- mooring(A, W(45), W(5), MC, W(5), W(80), top)
#' m2 <- mooring(A, W(45), BUB, W(5), MC, W(5), BUB, W(80), top)
#' m1k <- m1 |> discretise() |> knockdown(u=0.5)
#' m2k <- m2 |> discretise() |> knockdown(u=0.5)
#' plot(m1k)
#' # Show stiffened mooring in red
#' lines(x(m2k), depth(m2k), col=2, lwd=2)
#' points(x(m2k, skipWire=TRUE), depth(m2k, skipWire=TRUE), col=2, pch=20)
#' type <- sapply(m2k, \(i) class(i)[2])[!isWire(m2k)]
#' typeAbbrev <- unname(sapply(type, \(t)
#'     switch(t, float="F", instrument="I", anchor="A")))
#' text(x(m2k, skipWire=TRUE), depth(m2k, skipWire=TRUE), typeAbbrev, col=2, pos=2)
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
        stop("these are the indices of elements that are not of class \"mooring\": ", paste(w, collapse=" "))
    w <- which(2 != sapply(dots, function(x) length(class(x))))
    if (length(w))
        stop("these are the indices of elements that are not elementary: ", paste(w, collapse=" "))
    # All checks seem OK, so reverse parameters and create the return value.
    rval <- rev(dots)
    depth <- rval[[n]]$depth # NOTE: only anchor() objects have this, but we know we have one
    height <- rev(cumsum(sapply(rev(rval), function(x) x$height)))
    # bookmark B1a: same as B1b and analogous t0 B1c {{{
    b <- buoyancy(rval)[-n]
    tau <- cumsum(b)
    tau <- c(tau, tau[n-1])                  # repeat tension across anchor (for plot labelling; not used for calculations)
    # }}}
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
        rval[[i]]$tau <- tau[i]
    }
    if (alongBottom)
        warning("insufficient mooring buoyancy; placed ", alongBottom, " elements on the bottom")
    class(rval) <- "mooring"
    attr(rval, "waterDepth") <- depth
    rval
}                                      # mooring()

#' Print a mooring
#'
#' @param x an object of the `"mooring"` class.
#'
#' @param ... optional arguments.  If this includes `debug`, and if that holds
#' a number greater than zero, then some debugging information is printed.
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
    debug <- if ("debug" %in% names(list(...))) list(...)$debug else 0L
    mooringDebug(debug, "print.mooring() {\n", sep="")
    elementary <- 2 == length(class(x))
    n <- if (elementary) 1L else length(x)
    if (elementary || n == 1L) {
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
    # The'lastWas* variables keep track of repeats, e.g. as created by discretise().
    # This scheme will not work if a mooring is contructed with wire or chain elements
    # that are not joined by a connector, but that should not happen if the mooring
    # reflects reality.  If this poses a problem, we could also look at the group
    #' part of the item.
    lastWasChain <- FALSE
    lastWasWire <- FALSE
    i <- 1L
    while (i <= n) {
        xi <- if (elementary) x else x[[i]]
        mooringDebug(debug, "i=", i, " class=", paste(class(xi), collapse=","), "\n", sep="")
        if (inherits(xi, "anchor")) {
            cat(sprintf("%s%d: \"%s\" anchor, %gkg, height %gm, in %gm water depth\n",
                prefix, i, xi$model, xi$buoyancy, xi$height, xi$depth), sep="")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "chain")) {
            # See if there are more chain elements following this.
            mooringDebug(debug, "a chain; n=", n, ", n-i=", n-i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!inherits(x[[i+count]], "chain"))
                    break
                count <- count + 1L
            }
            #> message("chain count: ", count)
            if (count == 1L) {
                cat(sprintf("%s%d: \"%s\" chain, %gkg, length %gm, area %gm^2\n",
                    prefix, i, xi$model,
                    xi$buoyancy,
                    xi$height,
                    xi$area), sep="")
            } else {
                cat(sprintf("%s%d-%d: \"%s\" chain, %gm, length %gm, width %gm\n",
                    prefix, i, i+count-1L, xi$model,
                    xi$buoyancy,
                    xi$height,
                    xi$area), sep="")
            }
            i <- i + count             # account for skipped-over elements
        } else if (inherits(xi, "connector")) {
            cat(sprintf("%s%d: \"%s\" connector, %gkg, height %gm, area %gm^2\n",
                        prefix, i, xi$model, xi$buoyancy, xi$height, xi$area), sep="")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "float")) {
            cat(sprintf("%s%d: \"%s\" float, %gkg, height %gm, area %gm^2\n",
                         prefix, i, xi$model, xi$buoyancy, xi$height, xi$area), sep="")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "instrument")) {
            cat(sprintf("%s%d: \"%s\" instrument, %gkg, area %gm^2\n",
                         prefix, i, xi$model, xi$buoyancy, xi$area), sep="")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "misc")) {
            cat(sprintf("%s%d: \"%s\" misc, %gkg, height %gm, area %gm^2\n",
                        prefix, i, xi$model, xi$buoyancy, xi$height, xi$area), sep="")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "release")) {
            cat(sprintf("%s%d: \"%s\" release, %gkg, height %gm, area %gm^2\n",
                        prefix, i, xi$model, xi$buoyancy, xi$height, xi$area), sep="")
            lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (inherits(xi, "wire")) {
            # See if there are more wire elements following this.
            mooringDebug(debug, "a wire; n=", n, ", n-i=", n-i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!inherits(x[[i+count]], "wire"))
                    break
                count <- count + 1L
            }
            #> message("wire count: ", wire)
            if (count == 1L) {
                cat(sprintf("%s%d: \"%s\" wire, %gkg, length %gm, area %gm^2\n",
                            prefix, i, xi$model,
                            xi$buoyancy,
                            xi$height,
                            xi$area), sep="")
            } else {
                cat(sprintf("%s%d-%d: \"%s\" wire, %gkg, length %gm, area %gm^2\n",
                            prefix, i, i+count-1L, xi$model,
                            xi$buoyancy,
                            xi$height,
                            xi$area), sep="")
            }
            i <- i + count             # account for skipped-over elements
        } else {
            stop("unknown class c(\"", paste(class(xi), collapse="\", \""), "\")")
        }
    }
    mooringDebug(debug, "} # print.mooring()\n", sep="")
    invisible(x)
}

#' Plot a mooring
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
    showInterfaces=TRUE, showDepths=FALSE, showLabels=TRUE, showDetails=FALSE,
    fancy=FALSE, title="",
    mar=c(1.5, 3.5, 3.5, 1), mgp=c(2, 0.7, 0),
    xlim=NULL,
    type="l",
    ...)
{
    m <- x # we only use x above to obey the R rules on generics.
    if (!isMooring(m))
        stop("only works for objects created by mooring()")
    # Handle showDetails, converting it to a logical if not, and creating a list if required
    if (is.list(showDetails)) {
        detailsControl <- list(cex=if ("cex" %in% names(showDetails)) showDetails$cex else 0.8,
                               col=if ("col" %in% names(showDetails)) showDetails$col else "darkblue")
        showDetails <- TRUE
    } else if (is.logical(showDetails)) {
        detailsControl <- list(cex=0.8, col="darkblue")
    }
    colWater <- "#ccdcff"
    colBottom <- "#e6bb98"
    colStagnant <- "darkgray"
    dots <- list(...)
    debug <- 0L
    if ("debug" %in% names(dots))
        debug <- as.integer(max(0L, dots$debug))
    nm <- length(m)
    xshape <- x(m)
    xtension <- tension(m)
    depth <- depth(m)
    waterDepth <- if (inherits(m[[nm]], "anchor")) m[[nm]]$depth
        else max(abs(depth))
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
    mooringDebug(debug, depth, overview=TRUE, round=2)
    ylim <- c(waterDepth, 0)
    # Determine depth scale by doing a sort of dry run of a shape plot
    #. message("xlim given? ", !is.null(xlim))
    if (is.null(xlim)) xlim <- extendrange(c(x, 0))
    plot.window(0, 0, xlim=xlim, ylim=ylim, asp=if (which=="shape") 1, log="")
    usrShape <- par("usr")
    #message(oce::vectorShow(xlim))
    #message("usrShape[3:4] is ", usrShape[3], " ", usrShape[4])
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
        #mooringLength <- sum(sapply(m, function(x) x$height))
        #lines(rep(0, 2), waterDepth - c(mooringLength, 0), col=colStagnant, lwd=1.4*par("lwd"))
        #points(0, waterDepth - mooringLength, pch=20, col=colStagnant)
        #browser()
        xx <- x(m, stagnant=TRUE)
        yy <- depth(m, stagnant=TRUE)
        lines(xx, yy, col=colStagnant)
        notWire <- !isWire(m)
        points(xx[notWire], yy[notWire], pch=20, col=colStagnant)
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
        Y <- seq(usr[4]-yspace, usr[3]+yspace, length.out=N)
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

#' Print a debugging message
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
#' m <- mooring(anchor(depth=120), wire(length=100), F)
#' m["buoyancy"]
#'
#' @export
#'
#' @author Dan Kelley
`[.mooring` <- function(m, i)
{
    known <- c("area", "buoyancy", "CD", "height")
    if (isMooring(m)) {
        if (is.numeric(i)) {
            #message("m[i] with i=",paste(i, collapse=" "))
            i <- subset(i, 0L < i & i <= length(m))
            # message(" >> i=",paste(i, collapse=" "))
            um <- unclass(m)
            rval <- lapply(i, function(mi) um[[mi]])
            class(rval) <- class(m)
            rval
        } else {
            #message("m char")
            if (i %in% known)
                sapply(m, function(mi) mi[[i]])
            else
                stop("\"", i, "\" not handled; try one of: \"", paste(known, collapse="\", \""), "\"")
        }
    } else {
        if (length(class(m)) != 2)
            stop("only works for a mooring, or an element of a mooring")
        if (is.numeric(i)) {
            stop("integer lookup is not permitted for elementary objects")
        } else {
            #message("e char")
            if (i %in% known)
                unclass(m)[[i]]
            else
                stop("'", i, "' not handled; try one of: '", paste(known, collapse="', '"), "'")
        }
    }
}

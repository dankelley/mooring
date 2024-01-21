#' @importFrom S7 new_class new_object S7_object
mooringS7 <- S7::new_class("mooringS7",
    package = "mooring",
    properties = list(
        elements = class_list, # holds mooringElement items
        waterDepth = class_numeric,
        u = class_any
    ),
    validator = function(self) {
        NULL
    },
    constructor = function(..., waterDepth = NA_real_) {
        # cat("in mooring constructor\n")
        elements <- unlist(list(...)[[1]]) # FIXME: why is this so complicated?
        # cat("elements[[1]] follows\n");print(elements[[1]])
        if (!is.anchor(elements[[1]])) stop("element 1 is not an anchor")
        if (is.na(waterDepth)) stop("must specify waterDepth")
        new_object(S7_object(), elements = elements, waterDepth = waterDepth, u = 0.0)
    }
)

mooringElementS7 <- S7::new_class("mooringElementS7",
    package = "mooring",
    properties = list(
        model = class_character,
        buoyancy = class_numeric,
        height = class_numeric,
        area = class_numeric,
        CD = class_numeric,
        source = new_property(class_character, default = ""),
        originalName = new_property(class_character, default = ""),
        x = new_property(class_numeric, default = 0.0),
        x0 = new_property(class_numeric, default = 0.0),
        z = new_property(class_numeric, default = 0.0),
        z0 = new_property(class_numeric, default = 0.0),
        phi = new_property(class_numeric, default = 0.0),
        tau = new_property(class_numeric, default = 0.0),
        group = new_property(class_numeric, default = 0)
    ),
    validator = function(self) {
        NULL
    }
)

# properties = list(waterDepth = class_numeric)
anchorS7 <- S7::new_class("anchorS7", parent = mooringElementS7, package = "mooring")
chainS7 <- S7::new_class("chainS7", parent = mooringElementS7, package = "mooring")
connectorS7 <- S7::new_class("connectorS7", parent = mooringElementS7, package = "mooring")
floatS7 <- S7::new_class("floatS7", parent = mooringElementS7, package = "mooring")
instrumentS7 <- S7::new_class("instrumentS7", parent = mooringElementS7, package = "mooring")
miscS7 <- S7::new_class("miscS7", parent = mooringElementS7, package = "mooring")
releaseS7 <- S7::new_class("releaseS7", parent = mooringElementS7, package = "mooring")
wireS7 <- S7::new_class("wireS7", parent = mooringElementS7, package = "mooring")


#' Draw a mooring diagram
#'
#' Plot a side-view diagram that indicates mooring geometry.
#'
#' @param x a `mooring` object, created with [mooring()].
#'
#' @param which character value indicating the desired plot, with
#' choices: `"shape"` (the default), `"knockdown"`, `"tension"`
#' and `"velocity"`.
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
#' @param mar numeric vector of length 4, used to set margins
#' outside axes. The default values reduce whitespace.
#' See [par()].
#'
#' @param mgp numeric vector of length 3, with a default that
#' reduces axis whitespace. See [par()].
#'
#' @param xaxs,yaxs character values that control axis ranges.
#' See [par()].
#'
#' @param xlim optional numeric vector of length 2 that can
#' be used to specify the limits of the horizontal axis.
#'
#' @param type character value indicating type of plot. The default, `"l"`,
#' means to draw lines, while e.g. `"p"` means to draw points.
#'
#' @template debugTemplate
#'
#' @examples
#' # Create, summarize, and plot a simple mooring comprising
#' # a bottom anchor, a 100-metre wire, and a float, in
#' # a current of 1 m/s.
#' library(mooring)
#' m <- mooring(anchor(), wire(length = 80), float("HMB 20"), waterDepth = 100) |>
#'     segmentize() |>
#'     knockdown(u = 1)
#' plot(m)
#' plot(m, which = "tension")
#'
#' @importFrom graphics abline axis box lines mtext par plot.window points polygon rect strwidth text
#' @importFrom grDevices extendrange
#'
#' @export
#'
#' @name plot
#'
#' @author Dan Kelley
S7::method(`plot`, mooring:::mooringS7) <- function(
    x,
    which = "shape",
    showInterfaces = TRUE,
    showDepths = FALSE,
    showLabels = TRUE,
    showDetails = FALSE,
    fancy = FALSE,
    title = "",
    mar = c(1.5, 3.5, 3.5, 1),
    mgp = c(2, 0.7, 0),
    xlim = NULL,
    xaxs = "r", yaxs = "r",
    type = "l",
    debug = 0) {
    m <- x # we only use x above to obey the R rules on generics.
    if (!is.mooring(m)) {
        stop("only works for objects created by mooring()")
    }
    mooringDebug(debug, "plot(..., which=\"", which, "\") {\n", sep = "")
    # Handle showDetails, converting it to a logical if not, creating a list if required
    if (is.list(showDetails)) {
        detailsControl <- list(
            cex = if ("cex" %in% names(showDetails)) showDetails$cex else 0.8,
            col = if ("col" %in% names(showDetails)) showDetails$col else "darkblue"
        )
        showDetails <- TRUE
    } else if (is.logical(showDetails)) {
        detailsControl <- list(cex = 0.8, col = "darkblue")
    }
    # message("plot 4")
    colWater <- "#ccdcff"
    colDragWarning <- "2"
    colBottom <- "#e6bb98"
    colStagnant <- "darkgray"
    depth <- depth(m)
    # message("plot 6; depth=", paste(depth, collapse = " "))
    waterDepth <- m@waterDepth
    par(mar = mar, mgp = mgp)
    # Determine depth scale by doing a sort of dry run of a shape plot
    xlimOrig <- xlim
    # message("plot 7 (xlimOrig = ", paste(xlimOrig), collapse = " ", ")")
    if (is.null(xlim)) xlim <- extendrange(c(x(m), 0))
    # message("plot 8 (xlim = ", paste(xlim, collapse = " "), ")")
    # set the window so we know what xlim ought to be. The problem is
    # a complicated (to me) interaction between limits and obeying asp=1.
    plot.window(0, 0, xlim = xlim, ylim = c(waterDepth, 0), asp = 1, log = "")
    # message("plot 9: did plot.window())")
    xlim <- xlimOrig
    usrShape <- par("usr")
    # Handle velocity, which does not involve mooring elements and is a special case
    # message("plot 10: getting ready to plot with which=\"", which, "\"")
    if (which == "velocity") {
        u <- m@u # FIXME: ought to be in the object
        if (length(u) == 0L) stop("no velocity has been defined yet; use knockdown()")
        d <- seq(waterDepth, 0, length.out = 200)
        velocityProfile <- if (is.function(u)) u(d) else rep(u, length(d))
        plot(velocityProfile, d,
            ylim = usrShape[3:4],
            ylab = "", xlab = "", type = type, axes = FALSE,
            xaxs = xaxs, yaxs = yaxs
        )
        box()
        grid()
        if (fancy) {
            usr <- par("usr")
            rect(usr[1], usr[3], usr[2], waterDepth, col = colBottom, border = NA)
            rect(usr[1], waterDepth, usr[2], 0, col = colWater, border = NA)
            grid(col = "white", lwd = 1.4)
        }
        if (showInterfaces && !fancy) {
            abline(h = 0, col = colWater, lwd = 2)
            abline(h = waterDepth, col = colBottom, lwd = 2)
        }
        if (type == "l") {
            lines(velocityProfile, d, lwd = 1.4 * par("lwd"))
        } else {
            points(velocityProfile, d)
        }
        axis(2)
        mtext("Depth [m]", side = 2, line = par("mgp")[1], cex = par("cex"))
        axis(3)
        mtext("Velocity [m/s]", side = 3, line = par("mgp")[1], cex = par("cex"))
        mtext(title, side = 1, cex = par("cex"))
        return(invisible(NULL))
    }
    x <- switch(which,
        "shape" = x(m),
        "knockdown" = depth(m) - depth(m, stagnant = TRUE),
        "tension" = tension(m)
    )
    if (is.null(x)) {
        stop("which must be \"shape\", \"knockdown\", \"tension\" or \"velocity\"")
    }
    # xstagnant <- if (which == "shape") rep(0, length(m)) else if (which == "tension") tension(m, stagnant = TRUE)
    mooringDebug(debug, x, overview = TRUE, round = 2)
    mooringDebug(debug, depth, overview = TRUE, round = 2)
    ylim <- c(waterDepth, 0)
    # Determine depth scale by doing a sort of dry run of a shape plot
    # . message("xlim given? ", !is.null(xlim))
    if (is.null(xlim)) {
        xlim <- if (which == "shape") {
            extendrange(c(x, 0))
        } else if (which == "tension") {
            extendrange(c(x, 9.81 * anchorWeight(m)))
        } else {
            extendrange(x)
        }
    }
    # message("plot 11: xlim=", paste(xlim, collapse=" "))
    plot.window(xlim, ylim,
        xlim = xlim, ylim = ylim,
        xaxs = xaxs, yaxs = yaxs, log = "",
        asp = if (which %in% c("shape")) 1 else NA
    )
    usrShape <- par("usr")
    # message("plot 12: usrShape=", paste(usrShape, collapse=" "))
    # message(oce::vectorShow(xlim))
    # message("usrShape[3:4] is ", usrShape[3], " ", usrShape[4])
    look <- if (which == "tension") seq(2L, length(m@elements) - 1L) else seq_along(m@elements)
    # message("plot 13: head(look)=", paste(head(look), collapse=" "))
    plot(x[look], depth[look],
        xlim = xlim, ylim = usrShape[3:4],
        xlab = "", ylab = "", type = type, axes = FALSE,
        xaxs = xaxs, yaxs = yaxs,
        asp = if (which %in% c("shape")) 1 else NA
    )
    # message("plot 14")
    xlab <- switch(which,
        "shape" = "Horizontal Coordinate [m]",
        "knockdown" = "Depth Increase [m]",
        "tension" = "Tension [N]",
        "velocity" = "Velocity [m/s]"
    )
    ylab <- "Depth [m]"
    box()
    axis(2)
    mtext(ylab, side = 2, line = par("mgp")[1], cex = par("cex"))
    axis(3)
    mtext(xlab, side = 3, line = par("mgp")[1], cex = par("cex"))
    if (fancy) {
        box()
        usr <- par("usr")
        rect(usr[1], waterDepth, usr[2], 0, col = colWater, border = NA)
        grid(col = "white")
        # abline(h = 0, col = colWater, lwd = 4, lty = 2)
    } else {
        grid()
        if (showInterfaces) {
            abline(h = 0, col = colWater, lwd = 2)
            abline(h = waterDepth, col = colBottom, lwd = 2)
        }
    }
    # message("plot 15")
    # draw anchor (only makes sense for shape diagrams)
    if (which == "shape") {
        waterDepth <- attr(m, "waterDepth")
        A <- waterDepth - max(depth(m))
        anchorSymbol <- list(x = sqrt(3.0 / 4.0) * c(-A, 0, A), y = waterDepth - c(0, A, 0))
        polygon(anchorSymbol, col = colStagnant)
    }
    # message("plot 16")
    # Redraw to cover grid
    if (type == "l") {
        lines(x[look], depth[look], lwd = 1.4 * par("lwd"), col = "magenta")
    } else {
        points(x[look], depth[look], lwd = 1.4 * par("lwd"))
    }
    # message("plot 17")
    # Draw conditions for u=0 case
    if (fancy) {
        rect(usr[1], usr[3], usr[2], waterDepth, col = colBottom, border = NA)
    }
    # message("plot 18")
    # Redraw in case line runs along bottom
    lines(x[look], depth[look], lwd = 1.4 * par("lwd"))
    # message("plot 19")
    if (which == "shape") {
        # mooringLength <- sum(sapply(m, \(x) x@height))
        # lines(rep(0, 2), waterDepth - c(mooringLength, 0), col=colStagnant, lwd=1.4*par("lwd"))
        # points(0, waterDepth - mooringLength, pch=20, col=colStagnant)
        # browser()
        # message("plot 20")
        xx <- x(m, stagnant = TRUE)
        # message("plot 21")
        yy <- depth(m, stagnant = TRUE)
        # message("plot 22")
        lines(xx, yy, col = colStagnant)
        # message("plot 23")
        notWire <- !is.wire(m)
        # message("plot 24")
        points(xx[notWire], yy[notWire], pch = 20, col = colStagnant)
        # message("plot 25")
    } else if (which == "tension") {
        look <- seq(2L, length(m) - 1L)
        lines(tension(m, stagnant = TRUE)[look], depth[look],
            col = colStagnant, lwd = 1.4 * par("lwd")
        )
        abline(v = 9.81 * anchorWeight(m), col = colDragWarning, lwd = 3, lty = 2)
    }
    cex <- if (showDetails) detailsControl$cex else 1
    pch <- if (showDetails) detailsControl$pch else 20
    col <- if (showDetails) detailsControl$col else 1
    for (i in seq_along(m@elements)) {
        type <- gsub("^mooring::(.*)S7", "\\1", class(m@elements[[i]])[1])
        mooringDebug(debug, "m@elements[[", i, "]] has type=\"", type, "\"\n", sep = "")
        # message("plot element ", i, " has type \"", type, "\"")
        xi <- x[i] # FIXME: has this been defined? If so, why not z also?
        zi <- m@elements[[i]]@z
        if (type == "anchor" && which != "tension") {
            if (debug) {
                cat("i=", i, " (anchor at xi=", xi, ", zi=", zi, ")\n")
            }
            points(xi, -zi, pch = pch, cex = cex, col = col)
            if (showLabels && !showDetails) {
                text(xi, -zi, "A", pos = 2)
            }
        } else if (type == "float" && which != "tension") {
            if (debug) {
                cat("i=", i, " (float at xi=", xi, ", zi=", zi, ")\n")
            }
            points(xi, -zi, pch = pch, cex = cex, col = col)
            if (showLabels && !showDetails) {
                text(xi, -zi, "F", pos = 4)
            }
            if (showDepths && !showDetails) {
                if (abs(zi) < 1) {
                    text(xi, -zi, sprintf("%.3fm", -zi), pos = 2)
                } else {
                    text(xi, -zi, sprintf("%.1fm", -zi), pos = 2)
                }
            }
        } else if (type == "instrument") {
            if (debug) {
                cat("i=", i, " (instrument at xi=", xi, ", zi=", zi, ")\n")
            }
            points(xi, -zi, pch = pch, cex = cex, col = col)
            if (showLabels && !showDetails) {
                text(xi, -zi, "I", pos = 4)
            }
            if (showDepths && !showDetails) {
                if (abs(zi) < 1) {
                    text(xi, -zi, sprintf("%.3fm", -zi), pos = 2)
                } else {
                    text(xi, -zi, sprintf("%.1fm", -zi), pos = 2)
                }
            }
        } # note that other types are skipped
    }
    if (showDetails) {
        labels <- NULL
        # depths <- depthsStagnant <- NULL
        depths <- NULL
        xs <- xsStagnant <- NULL
        #> message(oce::vectorShow(which))
        for (i in seq_along(m@elements)) {
            e <- m@elements[[i]]
            if (is.anchor(e) || is.connector(e) || is.float(e) || is.instrument(e) || is.release(e)) {
                #> message("anchor, release or float at i=", i)
                depths <- c(depths, -e@z)
                xs <- c(xs, if (which == "shape") e@x else e@z0 - e@z)
                xsStagnant <- c(xsStagnant, 0)
                labels <- c(labels, e@model)
            }
        }
        #> message(oce::vectorShow(xs))
        N <- length(depths)
        usr <- par("usr")
        fac <- 1 / 15
        cex <- 0.75
        xspace <- fac * (usr[2] - usr[1])
        yspace <- fac * (usr[4] - usr[3])
        X0 <- max(xs, na.rm = TRUE)
        #> message(oce::vectorShow(X0))
        X <- rep(X0 + xspace, N)
        Y <- seq(usr[4] - yspace, usr[3] + yspace, length.out = N)
        # widths <- strwidth(labels, cex = cex)
        text(X, Y, labels, pos = 4, cex = detailsControl$cex, col = detailsControl$col)
        # points(X, Y, col=2)
        # points(xs, depths, col="magenta", pch=20)
        for (i in seq_along(xs)) {
            lines(c(xs[i], X[i]), c(depths[i], Y[i]), col = detailsControl$col, lwd = 0.6 * par("lwd"))
        }
    }
    mtext(title, side = 1, cex = par("cex"))
    mooringDebug(debug, "} # plot()\n", sep = "")
}

#' Summarize a mooring or a mooringElement
#'
#' @param x either a `mooring` object, created with [mooring()],
#' or a `mooringElement` object, created with [anchor()], [wire()],
#' or any of the related functions that create mooring elements.
#'
#' @examples
#' library(mooring)
#' a <- anchor()
#' summary(a)
#'
#' m <- mooring(anchor(), wire(length = 80), float("HMB 20"), waterDepth = 100)
#' summary(m)
#'
#' @export
#'
#' @name summary
#'
#' @author Dan Kelley
S7::method(`summary`, mooring:::mooringS7) <- function(
    x) {
    debug <- 0
    #message("SUMMARY of whole mooring")
    mooringDebug(debug, "summary.mooring() {\n", sep = "")
    elementary <- is.mooringElement(x)
    if (elementary) {
        stop("internal programming error: how did an element call this?")
    }
    n <- length(x@elements)
    if (is.null(attr(x, "segmentized"))) {
        cat(sprintf(
            "Mooring in %gm of water that has %d elements, listed from the top down:\n",
            x@waterDepth, n
        ))
    } else {
        if (is.null(attr(x, "u"))) {
            cat("Segmentized mooring with", n, "elements, listed from the top down:\n")
        } else {
            cat("Segmentized a knocked-over mooring with", n, "elements, listed from the top down:\n")
        }
    }
    prefix <- "  "
    # The lastWas variables keep track of repeats, e.g. as created by segmentize().
    # This scheme will not work if a mooring is constructed with wire or chain elements
    # that are not joined by a connector, but that should not happen if the mooring
    # reflects reality.  If this poses a problem, we could also look at the group
    #' part of the item.
    # lastWasChain <- FALSE
    # lastWasWire <- FALSE
    i <- 1L
    while (i <= n) {
        xi <- x@elements[[i]]
        mooringDebug(debug, "i=", i, " class=", paste(class(xi), collapse = ","), "\n", sep = "")
        if (is.anchor(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" FIXME anchor, buoyancy %gkg, height %gm\n",
                prefix, i, xi@model, xi@buoyancy, xi@height
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.chain(xi)) {
            # See if there are more chain elements following this.
            mooringDebug(debug, "a chain; n=", n, ", n-i=", n - i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!is.chain(x@elements[[i + count]])) {
                    break
                }
                count <- count + 1L
            }
            #> message("chain count: ", count)
            if (count == 1L) {
                cat(sprintf(
                    "%s%d: \"%s\" chain, buoyancy %gkg, length %gm, area %gm^2\n",
                    prefix, i, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            } else {
                cat(sprintf(
                    "%s%d-%d: \"%s\" chain, buoyancy %gm, length %gm, width %gm\n",
                    prefix, i, i + count - 1L, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            }
            i <- i + count # account for skipped-over elements
        } else if (is.connector(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" connector, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.float(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" float, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.instrument(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" instrument, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.misc(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" misc, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.release(xi)) {
            cat(sprintf(
                "%s%d: \"%s\" release, buoyancy %gkg, height %gm, area %gm^2\n",
                prefix, i, xi@model, xi@buoyancy, xi@height, xi@area
            ), sep = "")
            # lastWasChain <- lastWasWire <- FALSE
            i <- i + 1L
        } else if (is.wire(xi)) {
            # See if there are more wire elements following this.
            mooringDebug(debug, "a wire; n=", n, ", n-i=", n - i, "\n")
            count <- 1L
            while (count <= (n - i)) {
                if (!is.wire(x@elements[[i + count]])) {
                    break
                }
                count <- count + 1L
            }
            #> message("wire count: ", wire)
            if (count == 1L) {
                cat(sprintf(
                    "%s%d: \"%s\" wire, buoyancy %gkg, length %gm, area %gm^2\n",
                    prefix, i, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            } else {
                cat(sprintf(
                    "%s%d-%d: \"%s\" wire, buoyancy %gkg, length %gm, area %gm^2\n",
                    prefix, i, i + count - 1L, xi@model,
                    xi@buoyancy,
                    xi@height,
                    xi@area
                ), sep = "")
            }
            i <- i + count # account for skipped-over elements
        } else {
            stop("unknown class c(\"", paste(class(xi), collapse = "\", \""), "\")")
        }
    }
    mooringDebug(debug, "} # summary.mooring()\n", sep = "")
    invisible(x)
}

#' Summarize a mooring element
#'
#' @param x either a `mooring` object, created with [mooring()],
#' or a `mooringElement` object, created with [anchor()], [wire()],
#' or any of the related functions that create mooring elements.
#'
#' @name summary
#'
#' @export
#'
#' @author Dan Kelley
S7::method(`summary`, mooring:::mooringElementS7) <- function(
    x) {
    debug <- 0
    #message("SUMMARY of element")
    mooringDebug(debug, "summary(mooringElement) {\n", sep = "")
    elementary <- is.mooringElement(x)
    if (!elementary) {
        stop("internal programming error: how did a mooring call this?")
    }
    if (is.anchor(x)) {
        cat(sprintf(
            "\"%s\" anchor, buoyancy %gkg, height %gm\n",
            x@model, x@buoyancy, x@height
        ), sep = "")
    } else if (is.chain(x)) {
        cat(sprintf(
            "\"%s\" chain, buoyancy %gkg, length %gm, area %gm^2\n",
            x@model, x@buoyancy, x@height, x@area
        ), sep = "")
    } else if (is.connector(x)) {
        cat(sprintf(
            "\"%s\" connector, buoyancy %gkg, height %gm, area %gm^2\n",
            x@model, x@buoyancy, x@height, x@area
        ), sep = "")
    } else if (is.float(x)) {
        cat(sprintf(
            "\"%s\" float, buoyancy %gkg, height %gm, area %gm^2\n",
            x@model, x@buoyancy, x@height, x@area
        ), sep = "")
    } else if (is.instrument(x)) {
        cat(sprintf(
            "\"%s\" instrument, buoyancy %gkg, height %gm, area %gm^2\n",
            x@model, x@buoyancy, x@height, x@area
        ), sep = "")
    } else if (is.misc(x)) {
        cat(sprintf(
            "\"%s\" misc, buoyancy %gkg, height %gm, area %gm^2\n",
            x@model, x@buoyancy, x@height, x@area
        ), sep = "")
    } else if (is.release(x)) {
        cat(sprintf(
            "\"%s\" release, buoyancy %gkg, height %gm, area %gm^2\n",
            x@model, x@buoyancy, x@height, x@area
        ), sep = "")
    } else if (is.wire(x)) {
        cat(sprintf(
            "\"%s\" wire, buoyancy %gkg, length %gm, area %gm^2\n",
            x@model, x@buoyancy, x@height, x@area
        ), sep = "")
    } else {
        stop("unknown element")
    }
    mooringDebug(debug, "} # summary(mooringElement)\n", sep = "")
    invisible(x)
}

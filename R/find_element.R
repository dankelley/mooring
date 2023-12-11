# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Fuzzy search for mooring elements
#'
#' `findElement` does a fuzzy search for an element model, using
#' [agrep()].  The output (if any) is in the form of suggested calls
#' to element-creating functions
#' [anchor()], [chain()], [connector()], [float()], [instrument()], [misc()]
#' and [wire()].
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
#' of this argument.
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
findElement <- function(
    e,
    search = c("anchor", "chain", "connector", "float", "instrument", "wire"),
    ignore.case = TRUE, max.distance = 0.1) {
    data("mooringElements", package = "mooring", envir = environment())
    mooringElements <- get("mooringElements")
    rval <- NULL
    for (element in search) {
        names <- mooringElements[[paste0(element, "s")]]$name
        match <- try(agrep(e, names, ignore.case = ignore.case, max.distance = max.distance), silent = TRUE)
        if (!inherits(match, "try-error")) {
            for (i in seq_along(match)) {
                rval <- c(rval, paste0(element, "(\"", names[match], "\")"))
            }
        }
    }
    if (is.null(rval)) {
        cat("Sorry, found no good matches for \"", e, "\".\n", sep = "")
    } else {
        cat("Some possible matches:\n")
        rval <- sort(unique(rval))
        for (i in seq_along(rval)) {
            cat("    ", rval[i], "\n")
        }
    }
    invisible(rval)
}

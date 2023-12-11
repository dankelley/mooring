# vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
mooringDebug <- function(debug, v, ..., overview = FALSE, round = FALSE) {
    debug <- as.integer(debug)
    if (debug > 0L) {
        if (overview) {
            msg <- paste(deparse(substitute(expr = v, env = environment())), " ", sep = "")
            if (is.numeric(round)) {
                v <- round(v, as.integer(round))
            }
            n <- length(v)
            if (n > 1) {
                msg <- paste(msg, "[1:", n, "] ", sep = "")
            }
            if (n < 7) {
                msg <- paste(msg, paste(v, collapse = " "), "\n", sep = "")
            } else {
                msg <- paste(msg, paste(head(v, 3), collapse = " "), " ... ", paste(tail(v, 3), collapse = " "), "\n", sep = "")
            }
            cat(msg)
        } else {
            cat(v, ...)
        }
    }
    invisible(NULL)
}

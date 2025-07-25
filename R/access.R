#<old feature, no longer needed> # vim:spell:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
#<old feature, no longer needed>
#<old feature, no longer needed> #' Access something in a mooring
#<old feature, no longer needed> #'
#<old feature, no longer needed> #' Retrieves values from (a) a mooring element, as created with
#<old feature, no longer needed> #' [float()] or a similar function, or (b) a whole mooring, as created
#<old feature, no longer needed> #' with [mooring()].
#<old feature, no longer needed> #'
#<old feature, no longer needed> #' @template mTemplate
#<old feature, no longer needed> #'
#<old feature, no longer needed> #' @param i either (a) an integer specifying index of item to be returned, or
#<old feature, no longer needed> #' (b) a character string. The first case is used to look up components of
#<old feature, no longer needed> #' a mooring. The second case requires `i` to be `"area"`, `"buoyancy"`,
#<old feature, no longer needed> #' `"CD"`, or `"height"`, and the result is a single value if `m` is
#<old feature, no longer needed> #' an elementary object (example 1) or a whole mooring (example 2).
#<old feature, no longer needed> #'
#<old feature, no longer needed> #' @examples
#<old feature, no longer needed> #' library(mooring)
#<old feature, no longer needed> #' F <- float("HMB 20")
#<old feature, no longer needed> #' F["buoyancy"]
#<old feature, no longer needed> #' m <- mooring(anchor(), wire(length = 100), F, waterDepth = 120)
#<old feature, no longer needed> #' m["buoyancy"]
#<old feature, no longer needed> #'
#<old feature, no longer needed> #' @export
#<old feature, no longer needed> #'
#<old feature, no longer needed> #' @author Dan Kelley
#<old feature, no longer needed> `[.mooring` <- function(m, i) {
#<old feature, no longer needed>     known <- c("area", "buoyancy", "CD", "height")
#<old feature, no longer needed>     if (is.mooring(m)) {
#<old feature, no longer needed>         if (is.numeric(i)) {
#<old feature, no longer needed>             # message("m[i] with i=",paste(i, collapse=" "))
#<old feature, no longer needed>             i <- subset(i, 0L < i & i <= length(m))
#<old feature, no longer needed>             # message(" >> i=",paste(i, collapse=" "))
#<old feature, no longer needed>             um <- unclass(m)
#<old feature, no longer needed>             rval <- lapply(i, \(mi) um[[mi]])
#<old feature, no longer needed>             class(rval) <- class(m)
#<old feature, no longer needed>             rval
#<old feature, no longer needed>         } else {
#<old feature, no longer needed>             # message("m char")
#<old feature, no longer needed>             if (i %in% known) {
#<old feature, no longer needed>                 sapply(m, \(mi) mi[[i]])
#<old feature, no longer needed>             } else {
#<old feature, no longer needed>                 stop("\"", i, "\" not handled; try one of: \"", paste(known, collapse = "\", \""), "\"")
#<old feature, no longer needed>             }
#<old feature, no longer needed>         }
#<old feature, no longer needed>     } else {
#<old feature, no longer needed>         if (length(class(m)) != 2) {
#<old feature, no longer needed>             stop("only works for a mooring, or an element of a mooring")
#<old feature, no longer needed>         }
#<old feature, no longer needed>         if (is.numeric(i)) {
#<old feature, no longer needed>             stop("integer lookup is not permitted for elementary objects")
#<old feature, no longer needed>         } else {
#<old feature, no longer needed>             # message("e char")
#<old feature, no longer needed>             if (i %in% known) {
#<old feature, no longer needed>                 unclass(m)[[i]]
#<old feature, no longer needed>             } else {
#<old feature, no longer needed>                 stop("'", i, "' not handled; try one of: '", paste(known, collapse = "', '"), "'")
#<old feature, no longer needed>             }
#<old feature, no longer needed>         }
#<old feature, no longer needed>     }
#<old feature, no longer needed> }

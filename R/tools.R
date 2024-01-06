#####################################
# 1. exported support functions     #
#####################################


#' Compute buoyancy of object given in-air properties
#'
#' @param weight numeric value giving the object's in-air weight, kg.
#'
#' @param rho object's density in kg/m^3. The default is a value for steel.
#'
#' @param rhoWater water density in kg/m^3. The default is a rough value
#' for seawater.
#'
#' @return `buoyancyCalculation` returns the in-seawater buoyancy of the object, in kg.
#'
#' @examples
#' # 8x8x16 inch cinder block weighs 16.3kg
#' in2m <- 0.0254
#' buoyancyCalculation(16.3, 2400) # -9.32 kg
#' # 8x8x16 inch concrete block weighs 23.1kg
#' buoyancyCalculation(23.1, 2400) # -13.2 kg
#'
#' @export
#'
#' @author Dan Kelley
buoyancyCalculation <- function(weight = 16.3, rho = 2.4e3, rhoWater = 1027) {
    weight * (-1 + rhoWater / rho)
}

#' Compute root-mean-square
#' @param x a vector, matrix, etc.
#' @export
#' @author Dan Kelley
RMS <- function(x) {
    x^2 |>
        mean(na.rm = TRUE) |>
        sqrt()
}

#####################################
# 2. non-exported support functions #
#####################################


pluralize <- function(singular = "item", plural = NULL, n = 0L) {
    if (is.null(plural)) {
        plural <- paste0(singular, "s")
    }
    if (n == 1) paste(n, singular) else paste(n, plural)
}

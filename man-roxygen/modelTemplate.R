#' @param model character value indicating the type of <%= subclass%>.
#' There are three possibilities for `model`.
#' (1) If this is `"?"`, then the function returns a vector of permitted
#' character values. (2) If this is `"?X"`, where X is a set of
#' characters, then [findElement()] is called to do a fuzzy search,
#' with the `search` argument set to `"<%= subclass%>"`.
#' (3) If it is recognized, i.e. if that type is
#' stored in `data(mooringElements)`, then the that stored value
#' is used, and all other arguments to this function are ignored.
#' (4) Otherwise, a new <%= subclass%> object is created, using
#' values specified in the other arguments (all of which must
#' be supplied).

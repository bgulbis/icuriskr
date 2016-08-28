# class.R
#
# get and set class types used in icuriskr
#

# default values ---------------------------------------

# constructor functions --------------------------------

#' Construct icuriskr data types
#'
#' Takes an R object and sets class to an icuriskr type.
#'
#' @param x object to set class \code{riskr}
#'
#' @name riskr_class
#' @keywords internal
riskr <- function(x) {
    cl <- class(x)
    if ("riskr" %in% cl) return (x)
    class(x) <- c("riskr", cl)
    x
}

#' @rdname riskr_class
#' @export
as.riskr <- function(x) {
    if (missing(x)) x <- character()
    if (is.riskr(x)) return(x)
    after <- match("riskr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "riskr", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.temp <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.temp(x)) return(x)
    # if (!is.riskr(x)) x <- as.riskr(x)

    after <- match("temp", class(x), nomatch = 0L)
    class(x) <- append(class(x), "temp", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.map <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.map(x)) return(x)
    after <- match("map", class(x), nomatch = 0L)
    class(x) <- append(class(x), "map", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.hr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hr(x)) return(x)
    after <- match("hr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hr", after = after)
    x
}

# class test functions ---------------------------------

#' Test icuriskr-related classes
#'
#' Takes an R object and checks for a riskr class type.
#'
#' @param x object which may have a riskr class type
#' @keywords internal
is.riskr <- function(x) inherits(x, "riskr")

#' @rdname is.riskr
#' @export
is.temp <- function(x) inherits(x, "temp")

#' @rdname is.riskr
#' @export
is.map <- function(x) inherits(x, "map")

#' @rdname is.riskr
#' @export
is.hr <- function(x) inherits(x, "hr")

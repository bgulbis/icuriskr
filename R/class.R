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
    if (missing(x)) x <- character()
    if (is.temp(x)) return(x)
    # if (!is.riskr(x)) x <- as.riskr(x)

    after <- match("temp", class(x), nomatch = 0L)
    class(df) <- append(class(x), "temp", after = after)
    df
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

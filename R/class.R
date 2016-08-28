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

#' @rdname riskr_class
#' @export
as.rr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.rr(x)) return(x)
    after <- match("rr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "rr", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.ph <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.ph(x)) return(x)
    after <- match("ph", class(x), nomatch = 0L)
    class(x) <- append(class(x), "ph", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.sodium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.sodium(x)) return(x)
    after <- match("sodium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "sodium", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.potassium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.potassium(x)) return(x)
    after <- match("potassium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "potassium", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.scr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.scr(x)) return(x)
    after <- match("scr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "scr", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.hct <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hct(x)) return(x)
    after <- match("hct", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hct", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.wbc <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.wbc(x)) return(x)
    after <- match("wbc", class(x), nomatch = 0L)
    class(x) <- append(class(x), "wbc", after = after)
    x
}

#' @rdname riskr_class
#' @export
as.gcs <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.gcs(x)) return(x)
    after <- match("gcs", class(x), nomatch = 0L)
    class(x) <- append(class(x), "gcs", after = after)
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

#' @rdname is.riskr
#' @export
is.rr <- function(x) inherits(x, "rr")

#' @rdname is.riskr
#' @export
is.ph <- function(x) inherits(x, "ph")

#' @rdname is.riskr
#' @export
is.sodium <- function(x) inherits(x, "sodium")

#' @rdname is.riskr
#' @export
is.potassium <- function(x) inherits(x, "potassium")

#' @rdname is.riskr
#' @export
is.scr <- function(x) inherits(x, "scr")

#' @rdname is.riskr
#' @export
is.hct <- function(x) inherits(x, "hct")

#' @rdname is.riskr
#' @export
is.wbc <- function(x) inherits(x, "wbc")

#' @rdname is.riskr
#' @export
is.gcs <- function(x) inherits(x, "gcs")

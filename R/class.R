# class.R
#
# get and set class types used in icuriskr
#

# default values ---------------------------------------

# constructor functions --------------------------------

#' Construct Acute Physiology Score data types for APACHE II
#'
#' Takes an R object and sets class to an aps type.
#'
#' @param x object to set class \code{riskr}
#'
#' @name aps
#' @keywords internal
aps <- function(x) {
    cl <- class(x)
    if ("aps" %in% cl) return (x)
    class(x) <- c("aps", cl)
    x
}

#' @rdname aps
#' @keywords internal
as.aps <- function(x) {
    if (missing(x)) x <- character()
    if (is.aps(x)) return(x)
    after <- match("aps", class(x), nomatch = 0L)
    class(x) <- append(class(x), "aps", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.temp <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.temp(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("temp", class(x), nomatch = 0L)
    class(x) <- append(class(x), "temp", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.map <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.map(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("map", class(x), nomatch = 0L)
    class(x) <- append(class(x), "map", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.hr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hr(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("hr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hr", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.rr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.rr(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("rr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "rr", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.ph <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.ph(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("ph", class(x), nomatch = 0L)
    class(x) <- append(class(x), "ph", after = after)
    x
}

#' @rdname aps
#' @export
as.sodium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.sodium(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("sodium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "sodium", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.potassium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.potassium(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("potassium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "potassium", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.scr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.scr(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("scr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "scr", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.hct <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hct(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("hct", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hct", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.wbc <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.wbc(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("wbc", class(x), nomatch = 0L)
    class(x) <- append(class(x), "wbc", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.gcs <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.gcs(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("gcs", class(x), nomatch = 0L)
    class(x) <- append(class(x), "gcs", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.hco3 <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hco3(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("hco3", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hco3", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.pao2 <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.pao2(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("pao2", class(x), nomatch = 0L)
    class(x) <- append(class(x), "pao2", after = after)
    x
}

#' @rdname aps
#' @keywords internal
as.aa_grad <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.aa_grad(x)) return(x)
    if (!is.aps(x)) x <- as.aps(x)
    after <- match("aa_grad", class(x), nomatch = 0L)
    class(x) <- append(class(x), "aa_grad", after = after)
    x
}

# class test functions ---------------------------------

#' Test icuriskr-related classes
#'
#' Takes an R object and checks for an aps class type.
#'
#' @param x object which may have a aps class type
#' @keywords internal
is.aps <- function(x) inherits(x, "aps")

#' @rdname is.aps
#' @keywords internal
is.temp <- function(x) inherits(x, "temp")

#' @rdname is.aps
#' @keywords internal
is.map <- function(x) inherits(x, "map")

#' @rdname is.aps
#' @keywords internal
is.hr <- function(x) inherits(x, "hr")

#' @rdname is.aps
#' @keywords internal
is.rr <- function(x) inherits(x, "rr")

#' @rdname is.aps
#' @keywords internal
is.ph <- function(x) inherits(x, "ph")

#' @rdname is.aps
#' @keywords internal
is.sodium <- function(x) inherits(x, "sodium")

#' @rdname is.aps
#' @keywords internal
is.potassium <- function(x) inherits(x, "potassium")

#' @rdname is.aps
#' @keywords internal
is.scr <- function(x) inherits(x, "scr")

#' @rdname is.aps
#' @keywords internal
is.hct <- function(x) inherits(x, "hct")

#' @rdname is.aps
#' @keywords internal
is.wbc <- function(x) inherits(x, "wbc")

#' @rdname is.aps
#' @keywords internal
is.gcs <- function(x) inherits(x, "gcs")

#' @rdname is.aps
#' @keywords internal
is.hco3 <- function(x) inherits(x, "hco3")

#' @rdname is.aps
#' @keywords internal
is.pao2 <- function(x) inherits(x, "pao2")

#' @rdname is.aps
#' @keywords internal
is.aa_grad <- function(x) inherits(x, "aa_grad")

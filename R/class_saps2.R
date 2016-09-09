# class.R
#
# get and set class types used in icuriskr
#

# default values ---------------------------------------

# constructor functions --------------------------------

#' Construct Acute Physiology Score data types for Ssaps II
#'
#' Takes an R object and sets class to a saps type.
#'
#' @param x object to set class \code{riskr}
#'
#' @name saps
#' @keywords internal
saps <- function(x) {
    cl <- class(x)
    if ("saps" %in% cl) return (x)
    class(x) <- c("saps", cl)
    x
}

#' @rdname saps
#' @keywords internal
as.saps <- function(x) {
    if (missing(x)) x <- character()
    if (is.saps(x)) return(x)
    after <- match("saps", class(x), nomatch = 0L)
    class(x) <- append(class(x), "saps", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.temp <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.temp(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("temp", class(x), nomatch = 0L)
    class(x) <- append(class(x), "temp", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.sbp <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.sbp(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("sbp", class(x), nomatch = 0L)
    class(x) <- append(class(x), "sbp", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.hr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hr(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("hr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hr", after = after)
    x
}

#' @rdname saps
#' @export
as.sodium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.sodium(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("sodium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "sodium", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.potassium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.potassium(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("potassium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "potassium", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.bun <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.bun(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("bun", class(x), nomatch = 0L)
    class(x) <- append(class(x), "bun", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.wbc <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.wbc(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("wbc", class(x), nomatch = 0L)
    class(x) <- append(class(x), "wbc", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.gcs <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.gcs(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("gcs", class(x), nomatch = 0L)
    class(x) <- append(class(x), "gcs", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.hco3 <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hco3(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("hco3", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hco3", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.pao2 <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.pao2(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("pao2", class(x), nomatch = 0L)
    class(x) <- append(class(x), "pao2", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.bili <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.bili(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("bili", class(x), nomatch = 0L)
    class(x) <- append(class(x), "bili", after = after)
    x
}

#' @rdname saps
#' @keywords internal
as.uop <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.uop(x)) return(x)
    if (!is.saps(x)) x <- as.saps(x)
    after <- match("uop", class(x), nomatch = 0L)
    class(x) <- append(class(x), "uop", after = after)
    x
}

# class test functions ---------------------------------

#' Test icuriskr-related classes
#'
#' Takes an R object and checks for an saps class type.
#'
#' @param x object which may have a saps class type
#' @keywords internal
is.saps <- function(x) inherits(x, "saps")

#' @rdname is.saps
#' @keywords internal
is.temp <- function(x) inherits(x, "temp")

#' @rdname is.saps
#' @keywords internal
is.sbp <- function(x) inherits(x, "sbp")

#' @rdname is.saps
#' @keywords internal
is.hr <- function(x) inherits(x, "hr")

#' @rdname is.saps
#' @keywords internal
is.sodium <- function(x) inherits(x, "sodium")

#' @rdname is.saps
#' @keywords internal
is.potassium <- function(x) inherits(x, "potassium")

#' @rdname is.saps
#' @keywords internal
is.bun <- function(x) inherits(x, "bun")

#' @rdname is.saps
#' @keywords internal
is.wbc <- function(x) inherits(x, "wbc")

#' @rdname is.saps
#' @keywords internal
is.gcs <- function(x) inherits(x, "gcs")

#' @rdname is.saps
#' @keywords internal
is.hco3 <- function(x) inherits(x, "hco3")

#' @rdname is.saps
#' @keywords internal
is.pao2 <- function(x) inherits(x, "pao2")

#' @rdname is.saps
#' @keywords internal
is.bili <- function(x) inherits(x, "bili")

#' @rdname is.saps
#' @keywords internal
is.uop <- function(x) inherits(x, "uop")

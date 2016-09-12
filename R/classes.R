# class.R
#
# get and set class types used in icuriskr
#

# constructor functions --------------------------------

#' Construct Acute Physiology Score data types for APACHE II
#'
#' Takes an R object and sets class to an aps type.
#'
#' @param x object to set class \code{riskr}
#'
#' @name aps2
#' @keywords internal
aps2 <- function(x) {
    cl <- class(x)
    if ("aps2" %in% cl) return (x)
    class(x) <- c("aps2", cl)
    x
}

#' @rdname aps2
#' @keywords internal
as.aps2 <- function(x) {
    if (missing(x)) x <- character()
    if (is.aps2(x)) return(x)
    after <- match("aps2", class(x), nomatch = 0L)
    class(x) <- append(class(x), "aps2", after = after)
    x
}

#' Construct Acute Physiology Score data types for APACHE III
#'
#' Takes an R object and sets class to an aps type.
#'
#' @param x object to set class \code{riskr}
#'
#' @name aps3
#' @keywords internal
aps3 <- function(x) {
    cl <- class(x)
    if ("aps3" %in% cl) return (x)
    class(x) <- c("aps3", cl)
    x
}

#' @rdname aps3
#' @keywords internal
as.aps3 <- function(x) {
    if (missing(x)) x <- character()
    if (is.aps3(x)) return(x)
    after <- match("aps3", class(x), nomatch = 0L)
    class(x) <- append(class(x), "aps3", after = after)
    x
}

#' Construct Acute Physiology Score data types for SAPS II
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


#' Construct data types for use in risk scores
#'
#' Takes an R object and sets class to a physiology score data type.
#'
#' @param x object to set class \code{riskr}
#'
#' @name physiol
#' @keywords internal
physiol <- function(x) {
    cl <- class(x)
    if ("physiol" %in% cl) return (x)
    class(x) <- c("physiol", cl)
    x
}

#' @rdname phyisol
#' @keywords internal
as.temp <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.temp(x)) return(x)
    after <- match("temp", class(x), nomatch = 0L)
    class(x) <- append(class(x), "temp", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.sbp <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.sbp(x)) return(x)
    after <- match("sbp", class(x), nomatch = 0L)
    class(x) <- append(class(x), "sbp", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.map <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.map(x)) return(x)
    after <- match("map", class(x), nomatch = 0L)
    class(x) <- append(class(x), "map", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.hr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hr(x)) return(x)
    after <- match("hr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hr", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.rr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.rr(x)) return(x)
    after <- match("rr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "rr", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.ph <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.ph(x)) return(x)
    after <- match("ph", class(x), nomatch = 0L)
    class(x) <- append(class(x), "ph", after = after)
    x
}

#' @rdname phyisol
#' @export
as.sodium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.sodium(x)) return(x)
    after <- match("sodium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "sodium", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.potassium <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.potassium(x)) return(x)
    after <- match("potassium", class(x), nomatch = 0L)
    class(x) <- append(class(x), "potassium", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.scr <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.scr(x)) return(x)
    after <- match("scr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "scr", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.bun <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.bun(x)) return(x)
    after <- match("bun", class(x), nomatch = 0L)
    class(x) <- append(class(x), "bun", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.hct <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hct(x)) return(x)
    after <- match("hct", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hct", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.wbc <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.wbc(x)) return(x)
    after <- match("wbc", class(x), nomatch = 0L)
    class(x) <- append(class(x), "wbc", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.gcs <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.gcs(x)) return(x)
    after <- match("gcs", class(x), nomatch = 0L)
    class(x) <- append(class(x), "gcs", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.hco3 <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.hco3(x)) return(x)
    after <- match("hco3", class(x), nomatch = 0L)
    class(x) <- append(class(x), "hco3", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.pao2 <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.pao2(x)) return(x)
    after <- match("pao2", class(x), nomatch = 0L)
    class(x) <- append(class(x), "pao2", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.aa_grad <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.aa_grad(x)) return(x)
    after <- match("aa_grad", class(x), nomatch = 0L)
    class(x) <- append(class(x), "aa_grad", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.bili <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.bili(x)) return(x)
    after <- match("bili", class(x), nomatch = 0L)
    class(x) <- append(class(x), "bili", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.uop <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.uop(x)) return(x)
    after <- match("uop", class(x), nomatch = 0L)
    class(x) <- append(class(x), "uop", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.age <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.age(x)) return(x)
    after <- match("age", class(x), nomatch = 0L)
    class(x) <- append(class(x), "age", after = after)
    x
}

#' @rdname phyisol
#' @keywords internal
as.admit <- function(x) {
    if (missing(x)) x <- numeric()
    if (is.admit(x)) return(x)
    after <- match("admit", class(x), nomatch = 0L)
    class(x) <- append(class(x), "admit", after = after)
    x
}

# class test functions ---------------------------------

#' Test icuriskr-related classes
#'
#' Takes an R object and checks for an aps2 class type (APACHE II).
#'
#' @param x object which may have a aps2 class type
#' @keywords internal
is.aps2 <- function(x) inherits(x, "aps2")

#' Test icuriskr-related classes
#'
#' Takes an R object and checks for an aps3 class type (APACHE III).
#'
#' @param x object which may have a aps3 class type
#' @keywords internal
is.aps3 <- function(x) inherits(x, "aps3")

#' Test icuriskr-related classes
#'
#' Takes an R object and checks for an saps class type.
#'
#' @param x object which may have a saps class type
#' @keywords internal
is.saps <- function(x) inherits(x, "saps")

#' Test icuriskr-related classes
#'
#' Takes an R object and checks for a physiology class type.
#'
#' @param x object which may have a physiology class type
#' @keywords internal
is.physiol <- function(x) inherits(x, "physiol")

#' @rdname is.physiol
#' @keywords internal
is.temp <- function(x) inherits(x, "temp")

#' @rdname is.physiol
#' @keywords internal
is.sbp <- function(x) inherits(x, "sbp")

#' @rdname is.physiol
#' @keywords internal
is.map <- function(x) inherits(x, "map")

#' @rdname is.physiol
#' @keywords internal
is.hr <- function(x) inherits(x, "hr")

#' @rdname is.physiol
#' @keywords internal
is.rr <- function(x) inherits(x, "rr")

#' @rdname is.physiol
#' @keywords internal
is.ph <- function(x) inherits(x, "ph")

#' @rdname is.physiol
#' @keywords internal
is.sodium <- function(x) inherits(x, "sodium")

#' @rdname is.physiol
#' @keywords internal
is.potassium <- function(x) inherits(x, "potassium")

#' @rdname is.physiol
#' @keywords internal
is.scr <- function(x) inherits(x, "scr")

#' @rdname is.physiol
#' @keywords internal
is.bun <- function(x) inherits(x, "bun")

#' @rdname is.physiol
#' @keywords internal
is.hct <- function(x) inherits(x, "hct")

#' @rdname is.physiol
#' @keywords internal
is.wbc <- function(x) inherits(x, "wbc")

#' @rdname is.physiol
#' @keywords internal
is.gcs <- function(x) inherits(x, "gcs")

#' @rdname is.physiol
#' @keywords internal
is.hco3 <- function(x) inherits(x, "hco3")

#' @rdname is.physiol
#' @keywords internal
is.pao2 <- function(x) inherits(x, "pao2")

#' @rdname is.physiol
#' @keywords internal
is.aa_grad <- function(x) inherits(x, "aa_grad")

#' @rdname is.physiol
#' @keywords internal
is.bili <- function(x) inherits(x, "bili")

#' @rdname is.physiol
#' @keywords internal
is.uop <- function(x) inherits(x, "uop")

#' @rdname is.physiol
#' @keywords internal
is.age <- function(x) inherits(x, "age")

#' @rdname is.physiol
#' @keywords internal
is.admit <- function(x) inherits(x, "admit")


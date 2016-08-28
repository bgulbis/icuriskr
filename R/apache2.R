# APACHE II

#' Calculate APACHE II score
#'
#' @return
#' @export
#'
#' @examples
apache2 <- function() {

}

#' Calculate APACHE II Acute Physiologic Score
#'
#' \code{aps_score} calculates the APS score for an APACHE II variable
#'
#' This is an S3 generic function for calculating the Acute Physicologic Score
#' (APS) for a variable based on the APACHE II scoring system.The function
#' invokes the appropriate method based on the type of data (i.e., temperature,
#' mean arterial pressure, etc.).
#'
#' @param x A numeric vector with an icuriskr class type
#' @param ... additional arguments passed on to individual methods
#'
#' @examples
#'
#' @export
aps_score <- function(x, ...) {
    UseMethod("aps_score")
}

#' @export
#' @rdname aps_score
aps_score.default <- function(x, ...) {
    warning("No method available for objects of this class")
    x
}

#' @export
#' @rdname aps_score
aps_score.temp <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 41 | y <= 29.9 ~ 4L,
            y >= 39 | y <= 31.9 ~ 3L,
            y <= 33.9 ~ 2L,
            y >= 38.5 | y <= 35.9 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.map <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 160 | y <= 49 ~ 4L,
            y >= 130 ~ 3L,
            y >= 110 | y <= 69 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.hr <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 180 | y <= 39 ~ 4L,
            y >= 140 | y <= 54 ~ 3L,
            y >= 110 | y <= 69 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.rr <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 50 | y <= 5 ~ 4L,
            y >= 35 ~ 3L,
            y <= 9 ~ 2L,
            y >= 25 | y <= 11 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.ph <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 7.7 | y < 7.15 ~ 4L,
            y >= 7.6 | y <= 7.24 ~ 3L,
            y <= 7.32 ~ 2L,
            y >= 7.5 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.sodium <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 180 | y <= 110 ~ 4L,
            y >= 160 | y <= 119 ~ 3L,
            y >= 155 | y <= 129 ~ 2L,
            y >= 150 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.potassium <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 7 | y < 2.5 ~ 4L,
            y >= 6 ~ 3L,
            y <= 2.9 ~ 2L,
            y >= 5.5 | y <= 3.4 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.scr <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 3.5 ~ 4L,
            y >= 2 ~ 3L,
            y >= 1.5 | y < 0.6 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.hct <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 60 | y < 20 ~ 4L,
            y >= 50 | y <= 29.9 ~ 2L,
            y >= 46 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.wbc <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 40 | y < 1 ~ 4L,
            y >= 20 | y <= 2.9 ~ 2L,
            y >= 15 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname aps_score
aps_score.gcs <- function(x, ...) {
    purrr::map_int(x, ~ 15 - .x)
}

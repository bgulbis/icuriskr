# APACHE III

#' Calculate APACHE III score
#'
#' @param df A data frame
#'
#' @return A data frame
#' @export
apache3 <- function(df) {
    params <- c("hr", "map", "temp", "rr", "pao2")

    purrr::unslice(df) %>%
        # if ventilated, calculate PaO2/FiO2 ratio
        dplyr::mutate_at("pao2", dplyr::funs(
            pao2 = dplyr::if_else(vent == TRUE, pao2 / (fio2 / 100),
                                  NA_real_, NA_real_))) %>%
        purrr::dmap_at(params, as.aps3) %>%
        purrr::dmap_at("hr", as.hr) %>%
        purrr::dmap_at("map", as.map) %>%
        purrr::dmap_at("temp", ~as.temp(F_to_C(.x))) %>%
        purrr::dmap_at("pao2", as.pao2) %>%
        purrr::dmap_at("hco3", as.hco3) %>%
        purrr::dmap_at("sodium", as.sodium) %>%
        purrr::dmap_at("potassium", as.potassium) %>%
        purrr::dmap_at("bun", as.bun) %>%
        purrr::dmap_at("bili", as.bili) %>%
        purrr::dmap_at("wbc", as.wbc) %>%
        purrr::dmap_at("gcs", as.gcs) %>%
        purrr::dmap_at("uop", as.uop) %>%
        purrr::dmap_at("age", as.age) %>%
        purrr::dmap_at("admit_type", as.admit) %>%
        purrr::dmap_if(is.saps, saps_score) %>%
        dplyr::select_if(function(x) is.integer(x) | is.character(x)) %>%
        dplyr::group_by_(.dots = list("pie.id")) %>%
        dplyr::summarize_if(is.numeric, dplyr::funs(max(., na.rm = TRUE))) %>%
        purrr::by_row(function(x) sum(x[, -1], na.rm = TRUE),
                      .collate = "rows",
                      .to = "apache3")
}

#' Calculate APACHE II Acute Physiologic Score
#'
#' \code{aps3_score} calculates the APS score for an APACHE II variable
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
#' @keywords internal
aps3_score <- function(x, ...) {
    UseMethod("aps3_score")
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.default <- function(x, ...) {
    warning("No method available for objects of this class")
    x
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.temp <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y <= 32.9 ~ 28L,
            y <= 33.4 ~ 16L,
            y <= 33.9 ~ 13L,
            y <= 34.9 ~ 8L,
            y >= 40 ~ 4L,
            y <= 35.9 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.map <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y <= 39 ~ 23L,
            y >= 140 ~ 16L,
            y <= 59 ~ 15L,
            y >= 120 | y <= 69 ~ 7L,
            y <= 79 ~ 6L,
            y >= 100 ~ 4L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.hr <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 155 ~ 17L,
            y <= 39 ~ 8L,
            y >= 120 ~ 7L,
            y >= 110 | y <= 49 ~ 5L,
            y >= 100 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.rr <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 50 ~ 18L,
            y <= 5 ~ 17L,
            y >= 40 ~ 11L,
            y >= 35 ~ 9L,
            y <= 11 ~ 8L,
            y <= 13 ~ 7L,
            y >= 25 ~ 6L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.ph <- function(x, ...) {
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

#' @keywords internal
#' @rdname aps3_score
aps3_score.sodium <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 155 ~ 4L,
            y <= 119 ~ 3L,
            y <= 134 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.scr <- function(x, ..., arf = FALSE) {
    score <- function(y) {
        if (arf == TRUE) {
            dplyr::case_when(
                y >= 1.5 ~ 10L,
                is.numeric(y) ~ 0L
            )
        } else {
            dplyr::case_when(
                y >= 1.95 ~ 7L,
                y >= 1.5 ~ 4L,
                y <= 0.4 ~ 3L,
                is.numeric(y) ~ 0L
            )
        }
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.uop <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y <= 399 ~ 15L,
            y <= 599 ~ 8L,
            y <= 899 ~ 7L,
            y <= 1499 ~ 5L,
            y <= 1999 ~ 4L,
            y >= 4000 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.bun <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 80 ~ 12L,
            y >= 40 ~ 11L,
            y >= 20 ~ 7L,
            y >= 17 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.hct <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 50 | y <= 40.9 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.wbc <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y < 1 ~ 19L,
            y >= 25 | y <= 2.9 ~ 5L,
            y >= 20 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.gcs <- function(x, ...) {
    purrr::map_int(x, ~ 15L - as.integer(.x))
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.hco3 <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 52 | y < 15 ~ 4L,
            y >= 41 | y <= 17.9 ~ 3L,
            y <= 21.9 ~ 2L,
            y >= 32 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.pao2 <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y <= 49 ~ 15L,
            y <= 69 ~ 5L,
            y <= 79 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.aa_grad <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 500 ~ 14L,
            y >= 350 ~ 11L,
            y >= 250 ~ 9L,
            y >= 100 ~ 7L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname aps3_score
aps3_score.age <- function(x) {
    score <- function(y) {
        dplyr::case_when(
            y >= 75 ~ 6L,
            y >= 65 ~ 5L,
            y >= 55 ~ 3L,
            y >= 45 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}


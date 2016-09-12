# SAPS II

#' Calculate Ssaps II score
#'
#' @param df A data frame
#'
#' @return A data frame
#' @export
saps2 <- function(df) {
    params <- c("temp", "sbp", "hr", "pao2", "hco3", "sodium", "potassium",
                "bun", "bili", "wbc", "gcs", "uop", "age", "admit_type")

    purrr::unslice(df) %>%
        # if ventilated, calculate PaO2/FiO2 ratio
        dplyr::mutate_at("pao2", dplyr::funs(
            pao2 = dplyr::if_else(vent == TRUE, pao2 / (fio2 / 100),
                                  NA_real_, NA_real_))) %>%
        purrr::dmap_at(params, as.saps) %>%
        purrr::dmap_at("temp", ~as.temp(F_to_C(.x))) %>%
        purrr::dmap_at("sbp", as.sbp) %>%
        purrr::dmap_at("hr", as.hr) %>%
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
                      .to = "saps2")
}

#' Calculate SAPS II Score
#'
#' \code{saps_score} calculates the saps score for an SAPS II variable
#'
#' This is an S3 generic function for calculating the Acute Physicologic Score
#' for a variable based on the SAPS II scoring system.The function invokes the
#' appropriate method based on the type of data (i.e., temperature, systolic
#' arterial pressure, etc.).
#'
#' @param x A numeric vector with an icuriskr class type
#' @param ... additional arguments passed on to individual methods
#'
#' @examples
#'
#' @keywords internal
saps_score <- function(x, ...) {
    UseMethod("saps_score")
}

#' @keywords internal
#' @rdname saps_score
saps_score.default <- function(x, ...) {
    warning("No method available for objects of this class")
    x
}

#' @keywords internal
#' @rdname saps_score
saps_score.temp <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 39 ~ 3L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.sbp <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 200 ~ 2L,
            y < 70 ~ 13L,
            y <= 99 ~ 5L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.hr <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 160 ~ 7L,
            y >= 120 ~ 4L,
            y < 40 ~ 11L,
            y <= 69 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.sodium <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 145 ~ 1L,
            y < 125 ~ 5L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.potassium <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 5 | y < 3 ~ 3L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.bun <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 84 ~ 10L,
            y >= 28 ~ 6L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.bili <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 6 ~ 9L,
            y >= 4 ~ 4L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.wbc <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 20 ~ 3L,
            y < 1 ~ 12L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.gcs <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y < 6 ~ 26L,
            y < 8 ~ 13L,
            y < 10 ~ 7L,
            y < 13 ~ 5L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.hco3 <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y < 15 ~ 6L,
            y < 19 ~ 3L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.pao2 <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y < 100 ~ 11L,
            y <= 199 ~ 9L,
            y >= 200 ~ 6L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.uop <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y < 0.5 ~ 11L,
            y < 1 ~ 4L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.age <- function(x) {
    score <- function(y) {
        dplyr::case_when(
            y >= 80 ~ 18L,
            y >= 75 ~ 16L,
            y >= 70 ~ 15L,
            y >= 60 ~ 12L,
            y >= 40 ~ 7L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @keywords internal
#' @rdname saps_score
saps_score.admit <- function(x) {
    score <- function(y) {
        dplyr::case_when(
            y == "nonoperative" ~ 6L,
            y == "emergency" ~ 8L,
            is.character(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}


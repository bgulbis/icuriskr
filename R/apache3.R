# APACHE III

#' Calculate APACHE III score
#'
#' @param df A data frame
#'
#' @return A data frame
#' @export
apache3 <- function(df) {
    params <- c("hr", "map", "temp", "rr", "pao2", "aa_grad", "hct", "wbc",
                "uop", "bun", "sodium", "albumin", "bili", "glucose", "age")

    # ARF: SCr >= 1.5 + UOP < 410 and no chronic HD
    # If on vent, no RR points for 6-12
    # If on vent and FiO2 >= 0.5, us Aa-gradient; else use PaO2

    purrr::unslice(df) %>%
        # if ventilated, calculate PaO2/FiO2 ratio
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(
                ~dplyr::if_else(vent == TRUE, pao2 / (fio2 / 100), NA_real_, NA_real_),
                ~dplyr::if_else(vent == TRUE & rr >=6 & rr <= 12, 18, rr, rr)),
            nm = list("pao2", "rr")
        )) %>%
        purrr::dmap_at(params, as.aps3) %>%
        purrr::dmap_at("hr", as.hr) %>%
        purrr::dmap_at("map", as.map) %>%
        purrr::dmap_at("temp", ~as.temp(F_to_C(.x))) %>%
        purrr::dmap_at("rr", as.rr) %>%
        purrr::dmap_at("pao2", as.pao2) %>%
        purrr::dmap_at("aa_grad", as.aa_grad) %>%
        purrr::dmap_at("hct", as.hct) %>%
        purrr::dmap_at("wbc", as.wbc) %>%
        purrr::dmap_at("uop", as.uop) %>%
        purrr::dmap_at("bun", as.bun) %>%
        purrr::dmap_at("sodium", as.sodium) %>%
        purrr::dmap_at("albumin", as.albumin) %>%
        purrr::dmap_at("bili", as.bili) %>%
        purrr::dmap_at("glucose", as.glucose) %>%
        purrr::dmap_at("age", as.age) %>%
        purrr::dmap_if(is.aps3, apache3_score) %>%
        purrr::dmap_at("scr", ~ apache3_score(as.scr(.x), arf = df$arf)) %>%
        purrr::dmap_at("ph", ~ apache3_score(as.ph(.x), pco2 = df$pco2)) %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~dplyr::if_else(fio2 >= 0.5 & vent == TRUE, aa_grad, pao2, pao2)),
            nm = list("pulm")
        )) %>%
        dplyr::select_(quote(-aa_grad), quote(-pao2)) %>%
        dplyr::select_if(function(x) is.integer(x) | is.character(x)) %>%
        dplyr::group_by_(.dots = list("pie.id")) %>%
        dplyr::summarize_if(is.numeric, dplyr::funs(max(., na.rm = TRUE))) %>%
        purrr::by_row(function(x) sum(x[, -1], na.rm = TRUE),
                      .collate = "rows",
                      .to = "apache3")
}

#' Calculate APACHE III Acute Physiologic Score
#'
#' \code{apache3_score} calculates the APS score for an APACHE III variable
#'
#' This is an S3 generic function for calculating the Acute Physicologic Score
#' (APS) for a variable based on the APACHE III scoring system.The function
#' invokes the appropriate method based on the type of data (i.e., temperature,
#' mean arterial pressure, etc.).
#'
#' @param x A numeric vector with an icuriskr class type
#' @param ... additional arguments passed on to individual methods
#'
#' @examples
#'
#' @export
apache3_score <- function(x, ...) {
    UseMethod("apache3_score")
}

#' @export
#' @rdname apache3_score
apache3_score.default <- function(x, ...) {
    warning(paste("No method available for objects of class:", class(x)))
    x
}

#' @export
#' @rdname apache3_score
apache3_score.temp <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.map <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.hr <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.rr <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.ph <- function(x, ..., pco2) {
    score <- function(y, z) {
        dplyr::case_when(
            y < 7.20 & z < 50 ~ 12L,
            y < 7.20 ~ 4L,
            y < 7.30 & z >= 30 & z < 35 ~ 6L,
            y < 7.30 & z >= 35 & z < 50 ~ 3L,
            y < 7.30 & z >= 50 ~ 2L,
            y < 7.35 & z < 30 ~ 9L,
            y < 7.45 & z >= 45 ~ 1L,
            y < 7.50 & z < 30 ~ 5L,
            y >= 7.45 & y < 7.50 & z >= 35 & z < 45 ~ 2L,
            y >= 7.45 & z >= 45 ~ 12L,
            y >= 7.50 & z >= 35 ~ 12L,
            y >= 7.50 & z >= 25 ~ 3L,
            y >= 7.50 & y < 7.65 & z < 25 ~ 3L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map2_int(x, pco2, score)
}

#' @export
#' @rdname apache3_score
apache3_score.sodium <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.scr <- function(x, ..., arf) {
    score <- function(y, z) {
        if (is.na(z) | z == TRUE) {
            dplyr::case_when(
                y >= 1.95 ~ 7L,
                y >= 1.5 ~ 4L,
                y <= 0.4 ~ 3L,
                is.numeric(y) ~ 0L
            )
        } else {
            dplyr::case_when(
                y >= 1.5 ~ 10L,
                is.numeric(y) ~ 0L
            )
        }
    }

    purrr::map2_int(x, arf, score)
}

#' @export
#' @rdname apache3_score
apache3_score.uop <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.bun <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.hct <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 50 | y <= 40.9 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname apache3_score
apache3_score.wbc <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.bili <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 8 ~ 16L,
            y >= 5 ~ 8L,
            y >= 3 ~ 6L,
            y >= 2 ~ 5L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname apache3_score
apache3_score.albumin <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y <= 1.9 ~ 11L,
            y <= 2.4 ~ 6L,
            y >= 4.5 ~ 4L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname apache3_score
apache3_score.glucose <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y <= 39 ~ 8L,
            y <= 59 ~ 9L,
            y >= 350 ~ 5L,
            y >= 200 ~ 3L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname apache3_score
apache3_score.pao2 <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.aa_grad <- function(x, ...) {
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

#' @export
#' @rdname apache3_score
apache3_score.age <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 85 ~ 24L,
            y >= 75 ~ 17L,
            y >= 70 ~ 16L,
            y >= 65 ~ 13L,
            y >= 60 ~ 11L,
            y >= 45 ~ 5L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' Calculate neurologic abnormality score for APACHE III
#'
#' Calculate neurologic abnormality score for APACHE III
#'
#' @param eye A numeric vector
#' @param motor A numeric vector
#' @param verbal A numeric vector
#'
#' @return An integer vector
#' @export
apache3_neuro <- function(eye, motor, verbal) {
    score <- function(eye, motor, verbal) {
        if (is.na(eye) | is.na(motor) | is.na(verbal)) {
            return(0L)
        }

        if (eye == 1) {
            if (motor >= 5) {
                dplyr::case_when(
                    verbal == 1 ~ 16L,
                    is.numeric(verbal) ~ 0L
                )
            } else if (motor >=2 & motor <= 4) {
                dplyr::case_when(
                    verbal == 1 ~ 33L,
                    verbal <= 4 ~ 24L,
                    is.numeric(verbal) ~ 0L
                )
            } else if (motor == 1) {
                dplyr::case_when(
                    verbal == 1 ~ 48L,
                    verbal <= 4 ~ 29L,
                    is.numeric(verbal) ~ 0L
                )
            } else {
                0L
            }
        } else if (eye > 1) {
            if (motor == 6) {
                dplyr::case_when(
                    verbal == 1 ~ 15L,
                    verbal <= 3 ~ 10L,
                    verbal == 4 ~ 3L,
                    is.numeric(verbal) ~ 0L
                )
            } else if (motor == 5) {
                dplyr::case_when(
                    verbal == 1 ~ 15L,
                    verbal <= 3 ~ 13L,
                    verbal == 4 ~ 8L,
                    verbal == 5 ~ 3L,
                    is.numeric(verbal) ~ 0L
                )
            } else if (motor >= 2 & motor <= 4) {
                dplyr::case_when(
                    verbal < 4 ~ 24L,
                    verbal == 4 ~ 13L,
                    verbal == 5 ~ 3L,
                    is.numeric(verbal) ~ 0L
                )
            } else if (motor == 1) {
                dplyr::case_when(
                    verbal < 4 ~ 29L,
                    verbal == 4 ~ 13L,
                    verbal == 5 ~ 3L,
                    is.numeric(verbal) ~ 0L
                )
            } else {
                0L
            }
        } else {
            0L
        }
    }

    vals <- list(eye = eye, motor = motor, verbal = verbal)
    purrr::pmap_int(vals, score)
}

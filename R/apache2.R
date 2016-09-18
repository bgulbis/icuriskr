# APACHE II

#' Calculate APACHE II score
#'
#' @param df A data frame
#'
#' @return A data frame
#' @export
apache2 <- function(df) {
    params <- c("hr", "map", "temp", "rr", "pao2", "aa_grad", "hct", "wbc",
                "scr", "hco3", "sodium", "potassium", "gcs", "ph", "age")

    purrr::unslice(df) %>%
        purrr::dmap_at(params, as.aps2) %>%
        purrr::dmap_at("hr", as.hr) %>%
        purrr::dmap_at("map", as.map) %>%
        purrr::dmap_at("temp", ~as.temp(F_to_C(.x))) %>%
        purrr::dmap_at("rr", as.rr) %>%
        purrr::dmap_at("pao2", as.pao2) %>%
        purrr::dmap_at("aa_grad", as.aa_grad) %>%
        purrr::dmap_at("ph", as.ph) %>%
        purrr::dmap_at("hco3", as.hco3) %>%
        purrr::dmap_at("hct", as.hct) %>%
        purrr::dmap_at("wbc", as.wbc) %>%
        purrr::dmap_at("scr", as.scr) %>%
        purrr::dmap_at("sodium", as.sodium) %>%
        purrr::dmap_at("potassium", as.potassium) %>%
        purrr::dmap_at("gcs", as.gcs) %>%
        purrr::dmap_at("age", as.age) %>%
        purrr::dmap_if(is.aps2, apache2_score) %>%
        purrr::dmap_at("admit", ~ apache2_score(as.admit(.x), comorbidity = df$comorbidity)) %>%
        # if FiO2 >= 0.5, use A-a gradient; otherwise use PaO2
        # use HCO3 points if missing ABG
        # double SCr points if ARF
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(
                ~dplyr::if_else(fio2 >= 0.5, aa_grad, pao2, pao2),
                ~dplyr::coalesce(ph, hco3),
                ~dplyr::if_else(arf == TRUE, scr * 2L, scr)
            ),
            nm = list("pulm", "ph", "scr")
        )) %>%
        dplyr::select_(quote(-hco3), quote(-aa_grad), quote(-pao2)) %>%
        dplyr::select_if(function(x) is.integer(x) | is.character(x)) %>%
        dplyr::group_by_(.dots = list("pie.id")) %>%
        dplyr::summarize_if(is.numeric, dplyr::funs(max(., na.rm = TRUE))) %>%
        purrr::by_row(function(x) sum(x[, -1], na.rm = TRUE),
                      .collate = "rows",
                      .to = "apache2")
}

#' Calculate APACHE II Acute Physiologic Score
#'
#' \code{apache2_score} calculates the APS score for an APACHE II variable
#'
#' This is an S3 generic function for calculating the Acute Physicologic Score
#' (APS) for a variable based on the APACHE II scoring system.The function
#' invokes the appropriate method based on the type of data (i.e., temperature,
#' mean arterial pressure, etc.).
#'
#' @param x A numeric vector with an icuriskr class type
#' @param ... additional arguments passed on to individual methods
#'
#' @export
apache2_score <- function(x, ...) {
    UseMethod("apache2_score")
}

#' @export
#' @rdname apache2_score
apache2_score.default <- function(x, ...) {
    warning("No method available for objects of this class")
    x
}

#' @export
#' @rdname apache2_score
apache2_score.temp <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.map <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.hr <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.rr <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.ph <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.sodium <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.potassium <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.scr <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.hct <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.wbc <- function(x, ...) {
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
#' @rdname apache2_score
apache2_score.gcs <- function(x, ...) {
    purrr::map_int(x, ~ 15L - as.integer(.x))
}

#' @export
#' @rdname apache2_score
apache2_score.hco3 <- function(x, ...) {
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

#' @export
#' @rdname apache2_score
apache2_score.pao2 <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y < 55 ~ 4L,
            y <= 60 ~ 3L,
            y <= 70 ~ 1L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname apache2_score
apache2_score.aa_grad <- function(x, ...) {
    score <- function(y) {
        dplyr::case_when(
            y >= 500 ~ 4L,
            y >= 350 ~ 3L,
            y >= 200 ~ 2L,
            is.numeric(y) ~ 0L
        )
    }

    purrr::map_int(x, score)
}

#' @export
#' @rdname apache2_score
apache2_score.age <- function(x, ...) {
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

#' @export
#' @rdname apache2_score
apache2_score.admit <- function(x, ..., comorbidity) {
    score <- function(y, z) {
        if (is.na(z) | z == FALSE) {
            0L
        } else {
            if (is.na(y) | y == "elective") {
                2L
            } else {
                5L
            }
        }
    }

    purrr::map2_int(x, comorbidity, score)
}


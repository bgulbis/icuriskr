# helper functions

#' Set class types
#'
#' This function takes physiologic variables in a data frame and sets them to
#' the appropriate class types.
#'
#' @param df A data frame
#' @param types A named list with column name and setter function (ex: list(hr =
#'   as.hr))
#'
#' @return A data frame
#' @keywords internal
set_types <- function(df, types) {
    purrr::map_df(types, ~ purrr::dmap_at(df, .at = names(.x), .f = .x))
}

# conversions

#' Convert from Farenheit to Celsius
#'
#' This function converts temperatures from degrees Farenheit to degrees
#' Celsius.
#'
#' @param degree A numeric, the temperature in degrees Farenheit
#'
#' @return A numeric, the temperature in degrees Celsius
#' @export
F_to_C <- function(degree) {
    (degree - 32) * 5/9
}

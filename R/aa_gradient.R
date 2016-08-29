# aa_gradient.R

#' Calculate A-a O2 Gradient
#'
#' This function calculates the Alveolar-arterial (A-a) gradient.
#'
#' @param pco2 A numeric, the partial pressure of carbon dioxide (CO2) in
#'   arterial blood
#' @param pao2 A numeric, the partial pressure of oxygen (O2) in arterial blood
#' @param fio2 A numeric, the Fraction of Inspired Oxygen; defaults to room air
#'   (21%)
#' @param temp A numeric, the patient temperature in degrees Celsius; defaults
#'   to normal temperature (37 degrees Celsius)
#' @param elev A numeric, the elevation above sea level in meters; defaults to
#'   sea level (0 meters)
#'
#' @return A numeric
#' @export
#'
#' @references
#' \enumerate{
#'   \item Kanber GJ, King FW, Eshchar YR, et. al. The alveolar-arterial oxygen
#'   gradient in young and elderly men during air and oxygen breathing. Am Rev
#'   Respir Dis. 1968 Mar;97(3):376-81.
#'   \item Mellemgaard K. The alveolar-arterial oxygen difference: its size and
#'   components in normal man. Acta Physiol Scand. 1966 May;67(1):10-20.
#' }
aa_gradient <- function(pco2, pao2, fio2 = 21, temp = 37, elev = 0) {
    # Aa DO2 = (FIO2 * (Patm - PH2O) - (PaCO2 / 0.8)) - PaO2
    # Patm = 760 * exp(Elevation / -7000); Houston elevation = 43 feet (13.106 m)
    # PH2O = 47 * exp((Temp - 37) / 18.4)

    patm <- 760 * exp(elev / -7000)
    ph2o <- 47 * exp((temp - 37) / 18.4)

    if (fio2 > 1) {
        fio2 <- fio2 / 100
    }

    (fio2 * (patm - ph2o) - (pco2 / 0.8)) - pao2
}

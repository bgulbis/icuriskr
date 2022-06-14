# internal_data

#library(purrr)
#library(dplyr)
#library(tidyr)
#library(icd)


#install.packages("devtools")
#install_github("jackwasey/icd")

my_packages <- c("purrr","dplyr","tidyr","icd","purrrlyr")

package.check <-  lapply(
    my_packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)


comorbid_icd9 <- readr::read_csv("data-raw/comorbidities_icd9cm.csv")

tmp <- comorbid_icd9 %>%
    select(-drg) %>%
    purrrlyr::dmap_at("icd9cm", icd::decimal_to_short) %>%   # icd_decimal_to_short
    purrrlyr::slice_rows("comorbidity") %>%
    purrrlyr::by_slice(map_at, .at = "icd9cm", .f = c)

tmp2 <- flatten(tmp$.out)
names(tmp2) <- tmp$comorbidity

tmp3 <- tmp2 %>%
    map(as.icd9) %>%
    map(as.icd_short_diag)

tmp3$tuberculosis <- icd::expand_range("010", "018")  # icd_expand_range("010", "018")
tmp3$pvd <- icd9_map_elix$PVD
tmp3$pulmonary <- icd9_map_elix$Pulmonary
tmp3$cancer_mets <- icd9_map_elix$Mets
tmp3$lymphoma <- icd9_map_elix$Lymphoma

comorbidity_map_icd9 <- icd::as.comorbidity_map(tmp3) # as.icd_comorbidity_map(tmp3)

comorbidity_drg <- comorbid_icd9 %>%
    distinct(comorbidity, drg) %>%
    filter(!is.na(drg))

usethis::use_data(comorbidity_map_icd9, comorbidity_drg, overwrite = TRUE)  # devtools::use_data(comorbidity_map_icd9, comorbidity_drg, overwrite = TRUE)


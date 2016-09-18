# internal_data

library(purrr)
library(dplyr)
library(tidyr)
library(icd)

comorbid_icd9 <- readr::read_csv("data-raw/comorbidities_icd9cm.csv")

tmp <- comorbid_icd9 %>%
    select(-drg) %>%
    dmap_at("icd9cm", icd_decimal_to_short) %>%
    slice_rows("comorbidity") %>%
    by_slice(map_at, .at = "icd9cm", .f = c)

tmp2 <- flatten(tmp$.out)
names(tmp2) <- tmp$comorbidity

tmp3 <- tmp2 %>%
    map(as.icd9) %>%
    map(as.icd_short_diag)

tmp3$tuberculosis <- icd_expand_range("010", "018")
tmp3$pvd <- icd9_map_elix$PVD
tmp3$pulmonary <- icd9_map_elix$Pulmonary
tmp3$cancer_mets <- icd9_map_elix$Mets
tmp3$lymphoma <- icd9_map_elix$Lymphoma

comorbidity_map_icd9 <- as.icd_comorbidity_map(tmp3)

devtools::use_data(comorbidity_map_icd9, overwrite = TRUE)

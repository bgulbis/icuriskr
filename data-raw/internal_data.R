# internal_data

library(tidyverse)
library(icd)

comorbid_icd9 <- readr::read_csv("data-raw/comorbidities_icd9cm.csv")

tmp <- comorbid_icd9 %>%
    select(-drg) %>%
    dmap_at("icd9cm", icd_decimal_to_short)

icd9 <- tmp %>%
    slice_rows("comorbidity") %>%
    by_slice(map_at, .at = "icd9cm", .f = c)

tmp2 <- flatten(icd9$.out)
names(tmp2) <- icd9$comorbidity

tmp3 <- tmp2 %>%
    map(as.icd9) %>%
    map(as.icd_short_diag)

tmp3$tuberculosis <- icd_expand_range("010", "018")
tmp3$pvd <- icd9_map_elix$PVD
tmp3$pulmonary <- icd9_map_elix$Pulmonary
tmp3$cancer_mets <- icd9_map_elix$Mets
tmp3$lymphoma <- icd9_map_elix$Lymphoma

comorbidity_map_icd9 <- as.icd_comorbidity_map(tmp3)

comorbidity_drg <- comorbid_icd9 %>%
    distinct(comorbidity, drg) %>%
    filter(!is.na(drg))

cols <- fwf_empty("data-raw/2016_I9gem.txt", col_names = c("icd9", "icd10", "other"))
icd10 <- read_fwf("data-raw/2016_I9gem.txt", cols) %>%
    filter(icd10 != "NoDx")

tmp4 <- inner_join(tmp, icd10, c("icd9cm" = "icd9")) %>%
    select(-icd9cm, -other) %>%
    slice_rows("comorbidity") %>%
    by_slice(map_at, .at = "icd10", .f = c)

tmp5 <- flatten(tmp4$.out)
names(tmp5) <- tmp4$comorbidity

devtools::use_data(comorbidity_map_icd9, comorbidity_drg, overwrite = TRUE)


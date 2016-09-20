# internal_data

library(tidyverse)
library(icd)

# get list of manually compiled ICD-9-CM codes for comorbidities
comorbid_icd9 <- readr::read_csv("data-raw/comorbidities_icd9cm.csv")

manual_icd9 <- comorbid_icd9 %>%
    select(-drg) %>%
    dmap_at("icd9cm", icd_decimal_to_short)

icd9_df <- manual_icd9 %>%
    slice_rows("comorbidity") %>%
    by_slice(map_at, .at = "icd9cm", .f = c)

icd9_list <- flatten(icd9_df$.out)
names(icd9_list) <- icd9_df$comorbidity

icd9_list <- icd9_list %>%
    map(as.icd9) %>%
    map(as.icd_short_diag)

icd9_list$tuberculosis <- icd_expand_range("010", "018")
icd9_list$pvd <- icd9_map_elix$PVD
icd9_list$pulmonary <- icd9_map_elix$Pulmonary
icd9_list$cancer_mets <- icd9_map_elix$Mets
icd9_list$lymphoma <- icd9_map_elix$Lymphoma
icd9_list$hiv_elix <- icd9_map_elix$HIV
icd9_list$chf_elix <- icd9_map_elix$CHF

comorbidity_map_icd9 <- as.icd_comorbidity_map(icd9_list)

comorbidity_drg <- comorbid_icd9 %>%
    distinct(comorbidity, drg) %>%
    filter(!is.na(drg))

cols <- fwf_empty("data-raw/2016_I9gem.txt", col_names = c("icd9", "icd10", "other"))
icd10_gem <- read_fwf("data-raw/2016_I9gem.txt", cols) %>%
    filter(icd10 != "NoDx")

icd10_df <- inner_join(manual_icd9, icd10_gem, c("icd9cm" = "icd9")) %>%
    select(-icd9cm, -other) %>%
    slice_rows("comorbidity") %>%
    by_slice(map_at, .at = "icd10", .f = c)

icd10_list <- flatten(icd10_df$.out)
names(icd10_list) <- icd10_df$comorbidity

icd10_list <- icd10_list %>%
    map(as.icd10) %>%
    map(as.icd_short_diag)

icd10_tb <- icd10 %>%
    filter(icd9 %in% icd9_list$tuberculosis) %>%
    distinct(icd10)

icd10_list$tuberculosis <- icd10_tb$icd10
icd10_list$pvd <- icd10_map_elix$PVD
icd10_list$pulmonary <- icd10_map_elix$Pulmonary
icd10_list$cancer_mets <- icd10_map_elix$Mets
icd10_list$lymphoma <- icd10_map_elix$Lymphoma
icd10_list$hiv_elix <- icd10_map_elix$HIV
icd10_list$chf_elix <- icd10_map_elix$CHF

comorbidity_map_icd10 <- as.icd_comorbidity_map(icd10_list)

devtools::use_data(comorbidity_map_icd9, comorbidity_map_icd10, comorbidity_drg,
                   overwrite = TRUE)

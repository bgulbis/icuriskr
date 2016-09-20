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

# manually compiled list
comorbidity_map_icd9_ek <- as.icd_comorbidity_map(icd9_list)

# list relying on original Elixhauser
icd9_elix <- icd9_list
icd9_elix$hiv <- icd9_map_elix$HIV
icd9_elix$chf <- icd9_map_elix$CHF
icd9_elix$pulm_htn <- icd9_map_elix$PHTN
comorbidity_map_icd9_elix <- as.icd_comorbidity_map(icd9_elix)

# list using Quan-modified Elixhauser
icd9_quan <- icd9_list
icd9_quan$pvd <- icd9_map_quan_elix$PVD
icd9_quan$pulmonary <- icd9_map_quan_elix$Pulmonary
icd9_quan$cancer_mets <- icd9_map_quan_elix$Mets
icd9_quan$lymphoma <- icd9_map_quan_elix$Lymphoma
icd9_quan$hiv <- icd9_map_quan_elix$HIV
icd9_quan$chf <- icd9_map_quan_elix$CHF
icd9_quan$pulm_htn <- icd9_map_quan_elix$PHTN
comorbidity_map_icd9_quan <- as.icd_comorbidity_map(icd9_quan)

# list using AHRQ
icd9_ahrq <- icd9_list
icd9_ahrq$pvd <- icd9_map_ahrq$PVD
icd9_ahrq$pulmonary <- icd9_map_ahrq$Pulmonary
icd9_ahrq$cancer_mets <- icd9_map_ahrq$Mets
icd9_ahrq$lymphoma <- icd9_map_ahrq$Lymphoma
icd9_ahrq$hiv <- icd9_map_ahrq$HIV
icd9_ahrq$chf <- icd9_map_ahrq$CHF
icd9_ahrq$pulm_htn <- icd9_map_ahrq$PHTN
comorbidity_map_icd9_ahrq <- as.icd_comorbidity_map(icd9_ahrq)

# get list of DRG MDC codes for each comorbidity
comorbidity_drg <- comorbid_icd9 %>%
    distinct(comorbidity, drg) %>%
    filter(!is.na(drg))

# get ICD-9-CM to ICD-10-CM crosswalk
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

icd10_tb <- icd10_gem %>%
    filter(icd9 %in% icd9_list$tuberculosis) %>%
    distinct(icd10)

icd10_list$tuberculosis <- icd10_tb$icd10
icd10_list$pvd <- icd10_map_elix$PVD
icd10_list$pulmonary <- icd10_map_elix$Pulmonary
icd10_list$cancer_mets <- icd10_map_elix$Mets
icd10_list$lymphoma <- icd10_map_elix$Lymphoma

# manually compiled list
comorbidity_map_icd10_ek <- as.icd_comorbidity_map(icd10_list)

# list relying on original Elixhauser
icd10_elix <- icd10_list
icd10_elix$hiv <- icd10_map_elix$HIV
icd10_elix$chf <- icd10_map_elix$CHF
icd10_elix$pulm_htn <- icd10_map_elix$PHTN
comorbidity_map_icd10_elix <- as.icd_comorbidity_map(icd10_elix)

# list using Quan-modified Elixhauser
icd10_quan <- icd10_list
icd10_quan$pvd <- icd10_map_quan_elix$PVD
icd10_quan$pulmonary <- icd10_map_quan_elix$Pulmonary
icd10_quan$cancer_mets <- icd10_map_quan_elix$Mets
icd10_quan$lymphoma <- icd10_map_quan_elix$Lymphoma
icd10_quan$hiv <- icd10_map_quan_elix$HIV
icd10_quan$chf <- icd10_map_quan_elix$CHF
icd10_quan$pulm_htn <- icd10_map_quan_elix$PHTN
comorbidity_map_icd10_quan <- as.icd_comorbidity_map(icd10_quan)

# list using AHRQ
icd10_ahrq <- icd10_list
icd10_ahrq$pvd <- icd10_map_ahrq$PVD
icd10_ahrq$pulmonary <- icd10_map_ahrq$Pulmonary
icd10_ahrq$cancer_mets <- icd10_map_ahrq$Mets
icd10_ahrq$lymphoma <- icd10_map_ahrq$Lymphoma
icd10_ahrq$hiv <- icd10_map_ahrq$HIV
icd10_ahrq$chf <- icd10_map_ahrq$CHF
icd10_ahrq$pulm_htn <- icd10_map_ahrq$PHTN
comorbidity_map_icd10_ahrq <- as.icd_comorbidity_map(icd10_ahrq)

devtools::use_data(comorbidity_map_icd9_ek,
                   comorbidity_map_icd9_elix,
                   comorbidity_map_icd9_quan,
                   comorbidity_map_icd9_ahrq,
                   comorbidity_map_icd10_ek,
                   comorbidity_map_icd10_elix,
                   comorbidity_map_icd10_quan,
                   comorbidity_map_icd10_ahrq,
                   comorbidity_drg,
                   overwrite = TRUE)

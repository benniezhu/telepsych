library(tidyverse)
library(haven)

#load data

Medical_Conditions_dfs <- list()

for (year in 2020:2021){
  file_name <- paste0('F:/data/MEPS/Medical_Conditions/MedicalConditions', year, '.dta')
  df <- read_dta(file_name)
  Medical_Conditions_dfs[[year]] <- df
}

Medical_Conditions_dfs <- compact(Medical_Conditions_dfs)

for (i in seq_along(Medical_Conditions_dfs)){
  year <- 2020 + i - 1
  Medical_Conditions_dfs[[i]]$year <- year
}

Medical_Conditions_dfs <- bind_rows(Medical_Conditions_dfs)

# ICD-10 codes for Charlson comorbidities
icd10_charlson <- list(
  myocardial_infarction = c('I21', 'I22', 'I25'),
  congestive_heart_failure = c('I50', 'I09', 'I11', 'I13', 'I42', 'I43', 'P29'),
  peripheral_vascular_disease = c('I70', 'I71', 'I73', 'I77', 'K55', 'Z95'),
  cerebrovascular_disease = c('I60', 'I61', 'I62', 'I63', 'I64', 'I65', 'I66', 'I67', 'I68',
                              'I69', 'G45', 'G46', 'H34'),
  dementia = c('F01', 'F02', 'F03', 'F05', 'G30', 'G31'),
  chronic_pulmonary_disease = c('J40', 'J41', 'J42', 'J43', 'J44', 'J45', 'J46', 'J47',
                                'J60', 'J61', 'J62', 'J63', 'J64', 'J65', 'J66', 'J67',
                                'J70','I27'),
  rheumatic_disease = c('M34', 'M32', 'M33', 'M05', 'M06', 'M35', 'M36'),
  peptic_ulcer_disease = c('K25', 'K26', 'K27', 'K28'),
  mild_liver_disease = c('B18', 'K70', 'K71', 'K73', 'K74', 'Z94'),
  moderate_or_severe_liver_disease = c('K72', 'I85', 'I86', 'I98', 'K76'),
  hemiplegia_or_paraplegia = c('G04', 'G11', 'G80', 'G81', 'G82', 'G83'),
  renal_disease = c('N18', 'N19', 'N18.1', 'N18.2', 'N18.3', 'N18.4', 'N18.5', 'N18.6', 'N18.9'),
  any_malignancy = c('C00', 'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 
                                                     'C08', 'C09', 'C10', 'C11', 'C12', 'C13', 'C14', 'C15', 
                                                     'C16', 'C17', 'C18', 'C19', 'C20', 'C21', 'C22', 'C23', 
                                                     'C24', 'C25', 'C26', 'C27', 'C28', 'C30', 'C31', 'C32', 
                                                     'C33', 'C34', 'C37', 'C38', 'C39', 'C40', 'C41', 'C43', 
                                                     'C44', 'C45', 'C46', 'C47', 'C48', 'C49', 'C50', 'C51', 
                                                     'C52', 'C53', 'C54', 'C55', 'C56', 'C57', 'C58', 'C59', 
                                                     'C60', 'C61', 'C62', 'C63', 'C64', 'C65', 'C66', 'C67', 
                                                     'C68', 'C69', 'C70', 'C71', 'C72', 'C73', 'C74', 'C75', 
                                                     'C76', 'C77', 'C78', 'C79', 'C80', 'C81', 'C82', 'C83', 
                                                     'C84', 'C85', 'C86', 'C87', 'C88', 'C89', 'C90', 'C91', 
                                                     'C92', 'C93', 'C94', 'C95', 'C96', 'C97', 'D45', 'D46', 'D47', 'D48', 'D49'),
  metastatic_tumor = c('C77', 'C78', 'C79', 'C7A', 'C7B', 'C7C', 'C80'),
  hiv = c('B20', 'B21', 'B22')
)

for (condition in names(icd10_charlson)){
  icd10_condition <- icd10_charlson[[condition]]
  
  Medical_Conditions_dfs[paste0(condition)] <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% icd10_condition, 1, 0)
  
}

charlson_weights <- c(
  myocardial_infarction = 1,
  congestive_heart_failure = 1,
  peripheral_vascular_disease = 1,
  cerebrovascular_disease = 1,
  dementia = 1,
  chronic_pulmonary_disease = 1,
  rheumatic_disease = 1,
  peptic_ulcer_disease = 1,
  mild_liver_disease = 1,
  hemiplegia_or_paraplegia = 2,
  renal_disease = 2,
  any_malignancy = 2,
  moderate_or_severe_liver_disease = 3,
  metastatic_tumor = 6
)



#coalesce Panel
# Medical_Conditions_dfs <- Medical_Conditions_dfs %>% 
#   mutate(panel = coalesce(PANEL03, PANEL04, PANEL))

Medical_Conditions_dfs_subset <- Medical_Conditions_dfs %>% 
  select(DUPERSID, year, matches("^[a-z_]+$", ignore.case = FALSE)) %>% 
  group_by(DUPERSID, year) %>% 
  summarize_all(max, na.rm = FALSE)


Medical_Conditions_dfs_subset$renal_disease[Medical_Conditions_dfs_subset$renal_disease == 1] <- 2
Medical_Conditions_dfs_subset$any_malignancy[Medical_Conditions_dfs_subset$any_malignancy == 1] <- 2
Medical_Conditions_dfs_subset$moderate_or_severe_liver_disease[Medical_Conditions_dfs_subset$moderate_or_severe_liver_disease == 1] <- 3
Medical_Conditions_dfs_subset$metastatic_tumor[Medical_Conditions_dfs_subset$metastatic_tumor == 1] <- 6
Medical_Conditions_dfs_subset$hemiplegia_or_paraplegia[Medical_Conditions_dfs_subset$hemiplegia_or_paraplegia == 1] <- 2
# 
# #calculate score
# charlson_score <- rowSums(Medical_Conditions_dfs_subset[, paste0(names(charlson_weights))] * charlson_weights)



Medical_Conditions_dfs_subset$charlson_score <- Medical_Conditions_dfs_subset$myocardial_infarction + Medical_Conditions_dfs_subset$congestive_heart_failure +
  Medical_Conditions_dfs_subset$peripheral_vascular_disease + Medical_Conditions_dfs_subset$dementia + Medical_Conditions_dfs_subset$chronic_pulmonary_disease + Medical_Conditions_dfs_subset$rheumatic_disease +
  Medical_Conditions_dfs_subset$peptic_ulcer_disease + Medical_Conditions_dfs_subset$mild_liver_disease + Medical_Conditions_dfs_subset$moderate_or_severe_liver_disease + Medical_Conditions_dfs_subset$hemiplegia_or_paraplegia +
  Medical_Conditions_dfs_subset$renal_disease + Medical_Conditions_dfs_subset$any_malignancy + Medical_Conditions_dfs_subset$metastatic_tumor

table(Medical_Conditions_dfs_subset$year)

write_dta(Medical_Conditions_dfs_subset, 'F:/projects/telepsych/data/ml_ready/MEPS_2020_2021_charlson_score.dta')

#write_tsv(Medical_Conditions_dfs_subset_panel, 'F:/projects/antiobesitymeds/data/ml_ready/confounding_medical_conditions_panel.tsv')


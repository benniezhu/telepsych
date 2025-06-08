library(tidyverse)
library(haven)
#load data

Medical_Conditions_dfs <- list()

for (year in 2020:2022){
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

table(Medical_Conditions_dfs$year)


# ICD codes for various conditions
#conditions not well suited; intellecttual diability and autisum
#thought to include personality disorders too

autism_icd <- 'F84' #actually is pervasive developmental disorders, includes retts, aspergers, etc
intellectual_icd <- c('F70', 'F71', 'F72', 'F73', 'F74', 'F75', 'F76', 'F77', 'F78', 'F79')
personality_icd <- c('F60')
adhd_icd <- 'F90'

manic_episode_icd <- 'F30'
bipolar_disorder_icd <- 'F31'
depressive_episode_icd <- 'F32'
mdd_icd <- 'F33'
persistent_mood_disorder_icd <- 'F34'


eating_disorder_icd <- 'F50'
schizophrenia_icd <- 'F20'
stress_adjstment_icd <- 'F43' #inc PTSD, Acute stress, adjustment
ocd_icd <- 'F42'

alcohol_icd <- 'F10'
opioid_icd <- 'F11'
cannabis_icd <- 'F12'
cocaine_icd <- 'F14'
other_stimulant <- 'F15'

# Create diagnosis columns based on ICD codes
Medical_Conditions_dfs$autism_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% autism_icd, 1, 0)
Medical_Conditions_dfs$intellectual_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% intellectual_icd, 1, 0)
Medical_Conditions_dfs$personality_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% personality_icd, 1, 0)
Medical_Conditions_dfs$manic_episode_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% manic_episode_icd, 1, 0)
Medical_Conditions_dfs$bipolar_disorder_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% bipolar_disorder_icd, 1, 0)
Medical_Conditions_dfs$depressive_episode_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% depressive_episode_icd, 1, 0)
Medical_Conditions_dfs$mdd_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% mdd_icd, 1, 0)
Medical_Conditions_dfs$persistent_mood_disorder_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% persistent_mood_disorder_icd, 1, 0)
Medical_Conditions_dfs$adhd_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% adhd_icd, 1, 0)

Medical_Conditions_dfs$eating_disorder_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% eating_disorder_icd, 1, 0)
Medical_Conditions_dfs$schizophrenia_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% schizophrenia_icd, 1, 0)
Medical_Conditions_dfs$stress_adjstment_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% stress_adjstment_icd, 1, 0)
Medical_Conditions_dfs$ocd_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% ocd_icd, 1, 0)

Medical_Conditions_dfs$alcohol_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% alcohol_icd, 1, 0)
Medical_Conditions_dfs$opioid_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% opioid_icd, 1, 0)
Medical_Conditions_dfs$cannabis_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% cannabis_icd, 1, 0)
Medical_Conditions_dfs$cocaine_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% cocaine_icd, 1, 0)
Medical_Conditions_dfs$other_stimulant_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% other_stimulant, 1, 0)


Medical_Conditions_dfs_subset <- Medical_Conditions_dfs %>%
  select(DUPERSID, year, ends_with("_dx")) %>%
  group_by(DUPERSID, year) %>%
  summarize(across(ends_with("_dx"), ~max(.x, na.rm = FALSE)))


table(Medical_Conditions_dfs_subset$personality_dx)

            
# List of all _dx variables
dx_variables <- grep("_dx$", names(Medical_Conditions_dfs_subset), value = TRUE)



# Loop through each _dx variable and print the table
for (dx in dx_variables) {
  cat("Frequency table for", dx, ":\n")
  print(table(Medical_Conditions_dfs_subset[[dx]]))
  cat("\n")  # Adds a line break between tables
}

write_dta(Medical_Conditions_dfs_subset, "F:/projects/telepsych/data/medical_conditions_summary.dta")


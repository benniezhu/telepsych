#clink join with the medical condition visits and pull out psych visits with condition codes

#this is for the telepsych project!!
#load and do basic cleaning on outpatient and office based files
library(tidyverse)
library(haven)

office_visits <- list()
outpatient_visits <- list()
for (year in 2020:2022){
  file_name <- paste0('F:/data/MEPS/Outpatient/Outpatient', year, '.dta')
  df <- read_dta(file_name)
  outpatient_visits[[year]] <- df
  
  
}


for (year in 2020:2022){
  file_name <- paste0('F:/data/MEPS/OfficeBased/OfficeBased', year, '.dta')
  df <- read_dta(file_name)
  office_visits[[year]] <- df
  
  
}


outpatient_visits <- compact(outpatient_visits)
office_visits <- compact(office_visits)

for (i in seq_along(outpatient_visits)){
  year <- 2020 + i - 1
  
  outpatient_visits[[i]]$year <- year
}


for (i in seq_along(office_visits)){
  year <- 2020 + i - 1
  
  office_visits[[i]]$year <- year
}




outpatient_visits <- bind_rows(outpatient_visits)
office_visits <- bind_rows(office_visits)

outpatient_visits$outpatient_visit <- 1
outpatient_visits$office_visit <- 0

office_visits$outpatient_visits <- 0
office_visits$office_visit <- 1

#load conditions file
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

source('F:/projects/telepsych/code/mental_health_medical_condition_icd_codes.R')

psych_conditions <- Medical_Conditions_dfs %>% 
  filter( ICD10CDX %in% mental_icd10_codes)

rm(Medical_Conditions_dfs)

#load clink files
clink_files <- list()
for (year in 2020:2022){
  file_name <- paste0('F:/data/MEPS/clink/clink', year, '.dta')
  df <- read_dta(file_name)
  clink_files[[year]] <- df
  
  
}

clink_files <- compact(clink_files)
for (i in seq_along(clink_files)){
  year <- 2020 + i - 1
  clink_files[[i]]$year <- year
}


clink_files <- bind_rows(clink_files)

outpt_clink <- clink_files %>% filter(EVENTYPE == 2)

office_clink <- clink_files %>% filter(EVENTYPE == 1)



#join them separately since you dont have enough ram you broke bitch
#MEPS documentation below
# 2 is outpatient
#1 is office based
#1 = MVIS - office-based medical provider visit event contained on MEPS release HC-220G
#2 = OPAT - outpatient department visit event contained on MEPS release HC-220

#join psych conditions with clink file
psych_office_clink = inner_join(psych_conditions, office_clink ,
  by = c('DUPERSID', 'CONDIDX', 'year', 'PANEL')) %>% distinct(DUPERSID, EVNTIDX, EVENTYPE, year)
  

psych_outpatient_clink = inner_join(psych_conditions, outpt_clink,
  by = c('DUPERSID', 'CONDIDX', 'year', 'PANEL')) %>% distinct(DUPERSID, EVNTIDX, EVENTYPE, year)

mental_health_outpatient_visits <- inner_join(outpatient_visits, psych_outpatient_clink, 
                                      by = c('EVNTIDX', 'year')) %>% 
  mutate(psych_outpatient_visit = 1) %>% 
  select(-DUPERSID.y ) %>% rename(DUPERSID = DUPERSID.x)

mental_health_office_visits <- inner_join(office_visits, psych_office_clink, 
                                  by = c('year', 'EVNTIDX')) %>% 
  mutate(psych_office_visit = 1) %>% 
  select(-DUPERSID.y ) %>% rename(DUPERSID = DUPERSID.x)

mental_health_visits <- bind_rows(mental_health_office_visits, mental_health_outpatient_visits)

write_dta(mental_health_visits, 'F:/projects/telepsych/data/mental_health_visits.dta')

# 
# names(psych_office_visits)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# op_office_visits <- bind_rows(outpatient_visits, office_visits)

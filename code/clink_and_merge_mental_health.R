library(tidyverse)
library(haven)

clink <- list()
for (year in 2020:2022){
  file_name <- paste0('F:/data/MEPS/clink/clink', year, '.dta')
  df <- read_dta(file_name)
  clink[[year]] <- df
  
  
}

clink <- compact(clink)

for(i in seq_along(clink)){
  year <- 2020 + i - 1
  clink[[i]]$year <- year 
}

clink <- bind_rows(clink)

# Filter CLNK file to only office-based visits --------------------------------
#  EVENTYPE:
#   1 = "Office-based"
#   2 = "Outpatient" 
#   3 = "Emergency room"
#   4 = "Inpatient stay"
#   7 = "Home health"
#   8 = "Prescribed medicine" 

clink_filtered <- clink %>% 
  filter(EVENTYPE == 1 | EVENTYPE == 2)




# 
# psych_visits <- read_dta("F:/projects/telepsych/data/psych_visits.dta") %>% 
#   mutate(count = 1, 
#          domain = 1)

#summarized_psych_visits <- read_dta("F:/projects/telepsych/data/summarized_psych_visits.dta")

summarized_mh_visits <- read_dta("F:/projects/telepsych/data/summarized_mental_health_visits.dta")

summarized_medical_conditions <- read_dta("F:/projects/telepsych/data/medical_conditions_summary.dta")

charlson_score <- read_dta("F:/projects/telepsych/data/ml_ready/MEPS_2020_2022_charlson_score.dta")

#you need to unselect the variables to save psych_fyc; aka the not summary dataframe but still need to think 
#
#FYC_dfs <- read_dta("F:/projects/telepsych/data/FYC_dfs.dta") %>% 
#  select(-VARSTR, -VARPSU, -PERWT20F, -PERWT21F, -PERWT22F, -perwt, -DUID, -PID, -PANEL)
FYC_dfs <- read_dta("F:/projects/telepsych/data/FYC_dfs.dta") 

#merge datasets 

#this is the dataset that has all of the visits joined with the FYC
psych_FYC <- full_join(psych_visits, FYC_dfs, by = c('DUPERSID', 'year'))

#this dataset has summary information on visits and medical conditions joined with FYC
mental_health_summary_FYCs <- left_join(FYC_dfs, summarized_mh_visits, by = c('DUPERSID', 'year')) %>% 
 left_join(charlson_score, by = c('DUPERSID', 'year')) %>% 
  left_join(summarized_medical_conditions) %>% 
  filter(!is.na(total_mental_health_visits))


write_dta(mental_health_summary_FYCs, 'F:/projects/telepsych/data/ml_ready/mental_health_summary_FYC.dta')

#left_join(summarized_medical_conditions, by = c('DUPERSID' , 'year')) %>% 
write_dta(psych_FYC, "F:/projects/telepsych/data/ml_ready/psych_FYC.dta")


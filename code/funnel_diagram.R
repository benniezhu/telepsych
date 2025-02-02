library(tidyverse)
library(haven)

FYC_dfs <- list()

for (year in 2020:2022){
  file_name <- paste0("F:/data/MEPS/FullYearConsolidated/FYC_", year, ".dta")
  
  df <- read_dta(file_name) %>% 
    select(DUPERSID)
  
  FYC_dfs[[year]] <- df
  
}


FYC_dfs <- compact(FYC_dfs)

#create year variable
for (i in seq_along(FYC_dfs)){
  year <- 2020 + i - 1
  
  FYC_dfs[[i]]$year <- year
  
}

FYC_dfs <- bind_rows(FYC_dfs)

MEPS_unique_pts <- FYC_dfs %>% 
  group_by(year) %>% 
  summarise(count = n_distinct(DUPERSID)) %>% 
  rename(total = count)

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

table(office_visits$DRSPLTY_M18)

table(outpatient_visits$DRSPLTY_M18)

all_visits <- bind_rows(office_visits, outpatient_visits)

derm_visits <- all_visits %>% 
  filter(DRSPLTY_M18 == 4)

table(all_visits$year)
table(all_visits$DRSPLTY_M18)
  

#now i need to calculaten number of unique people to these visits 
all_visits %>% distinct(year, DUPERSID) 

all_visits_unique_pts <- all_visits %>% 
  group_by(year) %>% 
  summarise(count = n_distinct(DUPERSID)) %>% 
  rename(total_with_visit = count)

derm_visits_unique_pts <- derm_visits %>% 
  group_by(year) %>% 
  summarise(count = n_distinct(DUPERSID)) %>% 
  rename(total_with_derm_visit = count)

funnel_diagram <- left_join(MEPS_unique_pts, all_visits_unique_pts, by = 'year') %>% 
  left_join(derm_visits_unique_pts , by = 'year')

write_csv(funnel_diagram, 'F:/projects/telederm/output/funnel_diagram.csv')

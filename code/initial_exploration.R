library(tidyverse)
library(haven)

office_visits <- list()
outpatient_visits <- list()
for (year in 2020:2021){
  file_name <- paste0('F:/data/MEPS/Outpatient/Outpatient', year, '.dta')
  df <- read_dta(file_name)
  outpatient_visits[[year]] <- df
  
  
}

for (year in 2020:2021){
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

outpatient_visits_derm <- outpatient_visits %>% 
  filter(DRSPLTY_M18 == 4)

office_visits_derm <- office_visits %>% 
  filter(DRSPLTY_M18 == 4)


#proportion raw, of telehealth visits

table(office_visits_derm$TELEHEALTHFLAG)
table(outpatient_visits_derm$TELEHEALTHFLAG)


table(office_visits_derm$year, office_visits_derm$TELEHEALTHFLAG)
table(outpatient_visits_derm$year, outpatient_visits_derm$TELEHEALTHFLAG)

derm_visits <- bind_rows(office_visits_derm, outpatient_visits_derm)

derm_visits <- derm_visits %>% 
  mutate(perwt = ifelse(!is.na(PERWT20F), PERWT20F, PERWT21F))

derm_visits <- derm_visits %>% 
  mutate(outpatient_sf_20 = OPFSF20X + OPDSF20X,
         outpatient_sf_21 = OPFSF21X + OPDSF21X,
         outpatient_xp_20 = OPFXP20X + OPDXP20X,
         outpatient_xp_21 = OPFXP21X + OPDXP21X)

derm_visits <- derm_visits %>% 
  mutate(sf = coalesce(OBSF20X, OBSF21X, outpatient_sf_20, outpatient_sf_21),
         xp = coalesce(OBXP20X, OBXP21X, outpatient_xp_20, outpatient_xp_21))


write_dta(derm_visits, "F:/projects/telederm/data/initial_exploration.dta")

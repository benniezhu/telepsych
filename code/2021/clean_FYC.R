library(tidyverse)
library(haven)

FYC_dfs <- list()
#load the data in
for (year in 2020:2021){
  file_name <- paste0("F:/data/MEPS/FullYearConsolidated/FYC_", year, ".dta")
  
  df <- read_dta(file_name)
  
  FYC_dfs[[year]] <- df
  
}


FYC_dfs <- compact(FYC_dfs)

#create year variable
for (i in seq_along(FYC_dfs)){
  year <- 2020 + i - 1
  
  FYC_dfs[[i]]$year <- year
  
}

# Insurance 
year0320 <- c("20", "21")

# this loop creats a bunch of insurance variables
for (i in seq_along(FYC_dfs)){
  FYC_dfs[[i]]$private_ever <- ifelse(FYC_dfs[[i]][,grep("^PRVEV", 
                                                                   names(FYC_dfs[[i]]))] == 1, 1, 0)
  
  
  FYC_dfs[[i]]$medicaid_ever <- ifelse(rowSums(FYC_dfs[[i]][, grep("^MCD(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE).*X$", 
                                                                             names(FYC_dfs[[i]]))] == 1) > 0, 1, 0)
  
  
  FYC_dfs[[i]]$medicare_ever <- ifelse(rowSums(FYC_dfs[[i]][, grep("^MCR(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE).*X$", 
                                                                             names(FYC_dfs[[i]]))] == 1) > 0, 1, 0)
  
  FYC_dfs[[i]]$public_ever <- ifelse(rowSums(FYC_dfs[[i]][, grep("^PUB(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE).*X$", 
                                                                           names(FYC_dfs[[i]]))] == 1) > 0, 1, 0)
  
  FYC_dfs[[i]]$uninsured_ever <- ifelse(rowSums(FYC_dfs[[i]][, grep("^INS(JA|FE|MA|AP|MY|JU|JL|AU|SE|OC|NO|DE).*X$", 
                                                                              names(FYC_dfs[[i]]))] == 2) > 0, 1, 0)
  
  FYC_dfs[[i]]$oth_public_ever <- FYC_dfs[[i]]$public_ever
  
  FYC_dfs[[i]]$oth_public_ever[FYC_dfs[[i]]$medicaid_ever == 1 & 
                                      FYC_dfs[[i]]$medicare_ever == 1] <- 0
}



#make disease variables that are easy here along with diabetes
disease_vars <- c('HIBPDX', 'HIBPDX53', 'MIDX53', 'MIDX', 'OHRTDX53', 'OHRTDX',
                  'CHOLDX', 'CHOLDX53', 'STRKDX53', 'STRKDX', 'EMPHDX53', 'EMPHDX',
                  'ARTHDX53', 'ARTHDX', 'JTPAIN53', 'JTPAIN31', 'JTPAIN31_M18', 'JTPAIN53_M18',
                  'DIABDX53', 'DIABDX', 'DIABDX_M18', 'CHBMIX42', 'ADBMI42', 'BMINDX53')
#this loop is 3 loops that first loops through the FYC dfs and 
#then the disease vars and generates variables 
#where everything is NA
for (i in seq_along(FYC_dfs)) {
  for (var in disease_vars) {
    if (!(var %in% names(FYC_dfs[[i]]))) {
      FYC_dfs[[i]][[var]] <- NA
    }
  }
}

#this one populates each of the variables and makes them 0/1 if person has condition
for (i in seq_along(FYC_dfs)) {
  #high blood pressure
  FYC_dfs[[i]]$hbp_dx <- 0
  FYC_dfs[[i]]$hbp_dx[FYC_dfs[[i]]$HIBPDX == 1 | FYC_dfs[[i]]$HIBPDX53 == 1] <- 1 
  
  # heart disease 
  FYC_dfs[[i]]$heart_disease_dx <- 0
  FYC_dfs[[i]]$heart_disease_dx[FYC_dfs[[i]]$MIDX53 == 1 | FYC_dfs[[i]]$MIDX == 1 |
                                  FYC_dfs[[i]]$OHRTDX53 == 1 | FYC_dfs[[i]]$OHRTDX == 1] <- 1
  
  # high cholesterol aka hyperlipidemia
  FYC_dfs[[i]]$hld_dx <- 0
  FYC_dfs[[i]]$hld_dx[FYC_dfs[[i]]$CHOLDX == 1 | FYC_dfs[[i]]$CHOLDX53 == 1] <- 1
  
  #stroke
  FYC_dfs[[i]]$stroke_dx <- 0
  FYC_dfs[[i]]$stroke_dx[FYC_dfs[[i]]$STRKDX == 1 | FYC_dfs[[i]]$STRKDX53 == 1] <- 1
  
  #emphysema
  
  FYC_dfs[[i]]$emphy_dx <- 0
  FYC_dfs[[i]]$emphy_dx[FYC_dfs[[i]]$EMPHDX == 1 | FYC_dfs[[i]]$EMPHDX53 == 1] <- 1
  
  # arthritis
  FYC_dfs[[i]]$arth_dx <- 0
  FYC_dfs[[i]]$arth_dx[FYC_dfs[[i]]$ARTHDX == 1 | FYC_dfs[[i]]$ARTHDX53 == 1] <- 1 
  
  #joint pain 
  FYC_dfs[[i]]$joint_dx <- 0
  FYC_dfs[[i]]$joint_dx[FYC_dfs[[i]]$JTPAIN31_M18 == 1 |FYC_dfs[[i]]$JTPAIN53==1 | 
                          (FYC_dfs[[i]]$JTPAIN31==1 & FYC_dfs[[i]]$year==2017) | 
                          (FYC_dfs[[i]]$ARTHDX==1 & FYC_dfs[[i]]$year>2017)] <- 1
  #diabetes
  FYC_dfs[[i]]$diabetes_dx <- 0
  FYC_dfs[[i]]$diabetes_dx[FYC_dfs[[i]]$DIABDX_M18 == 1 | FYC_dfs[[i]]$DIABDX == 1 |
                             FYC_dfs[[i]]$DIABDX53 == 1] <- 1 
  
  #light obesity 27+ 
  FYC_dfs[[i]]$obesity_27 <- 0
  FYC_dfs[[i]]$obesity_27[FYC_dfs[[i]]$CHBMIX42 >= 27 | FYC_dfs[[i]]$ADBMI42 >= 27 | FYC_dfs[[i]]$BMINDX53 >= 27] <- 1
  
  
  #obesity 30+
  FYC_dfs[[i]]$obesity_30 <- 0
  FYC_dfs[[i]]$obesity_30[FYC_dfs[[i]]$CHBMIX42 >= 30 | FYC_dfs[[i]]$ADBMI42 >= 30 | FYC_dfs[[i]]$BMINDX53 >= 30] <- 1
}
#bind to single dataframe 
FYC_dfs <- bind_rows(FYC_dfs)



#generate race variable

FYC_dfs <- FYC_dfs %>% 
  mutate(race = case_when(
    RACEV1X == 1 ~ 1,
    RACEV1X == 2 ~ 2, 
    HISPANX == 1 ~ 3,
    RACEV1X > 2 & HISPANX == 2 ~ 4
  ))


FYC_dfs <- FYC_dfs %>% 
  mutate(POVCAT = coalesce(POVCAT20, POVCAT21))

#make 4 catagory poverty catagory 

FYC_dfs <- FYC_dfs %>% 
  mutate(poverty = case_when(
    POVCAT %in% c(1,2) ~ 1,
    POVCAT == 3 ~ 2,
    POVCAT == 4 ~ 3,
    POVCAT == 5 ~ 4))


#age 
FYC_dfs <- FYC_dfs %>% 
  mutate(age = coalesce(AGE20X, AGE21X))

FYC_dfs <- FYC_dfs %>% 
  mutate(u18 = ifelse(age <18, 1, 0))
#make education variable

#MEPS Variables:
#HIDEG is every year except 2013/2014 and 2003-2004

FYC_dfs <- FYC_dfs %>% 
  mutate(edu = case_when(
    HIDEG == 1 ~ 1, # no deg
    HIDEG == 2 | HIDEG == 3 ~ 2, # HS/ GED
    HIDEG >= 4 & HIDEG <= 7 ~ 3, # College +
    HIDEG == 8 ~ 1, # under 16
    HIDEG < 0 ~ 1 # refused, don't know, or can't compute
  ))

#coalease region variables

FYC_dfs <- FYC_dfs %>% 
  mutate(region = coalesce(REGION20, REGION21))

FYC_dfs$region[FYC_dfs$region == -1] <- NA
#insurance

#coalease needed insurances
FYC_dfs <- FYC_dfs %>% 
  mutate(PRVEV = coalesce(PRVEV20, PRVEV21))

insurance_prefixes <- c('MCD','PUB', 'INS')
month_prefixes <- c('JA', 'FE', 'MA', 'AP', 'MY', 'JU', 'JL', 'AU', 'SE', 'OC', 'NO', 'DE')
year_suffix <- c(20, 21)

for (insurance in insurance_prefixes){
  for (month in month_prefixes){
    variable_name <- paste0(insurance, month)
    new_variable_name <- paste0(insurance, month, 'X')
    combined <- paste0(variable_name, sprintf("%02d", year_suffix), 'X')
    
    print(variable_name)
    print(combined)
    
    FYC_dfs <- FYC_dfs %>% 
      mutate(!!new_variable_name := coalesce(!!!syms(combined)))
    
  }
}
# #don't really understand why i needed to break this out but i got some weird  error below when it was in the above loop
# Error in .shallow(x, cols = cols, retain.key = TRUE): attempt to set index 0/0 in SET_VECTOR_ELT
insurance_prefixes3 <- 'MCR'
for (insurance in insurance_prefixes3){
  for (month in month_prefixes){
    variable_name <- paste0(insurance, month)
    new_variable_name <- paste0(insurance, month, 'X')
    combined <- paste0(variable_name, sprintf("%02d", year_suffix), 'X')
    
    print(variable_name)
    print(combined)
    
    FYC_dfs <- FYC_dfs %>% 
      mutate(!!new_variable_name := coalesce(!!!syms(combined)))
    
  }
}

insurance_prefixes2 <- 'PRI'

for (insurance in insurance_prefixes2){
  for (month in month_prefixes){
    variable_name <- paste0(insurance, month)
    combined <- paste0(variable_name, sprintf("%02d", year_suffix))
    
    print(variable_name)
    print(combined)
    
    FYC_dfs <- FYC_dfs %>% 
      mutate(!!variable_name := coalesce(!!!syms(combined)))
    
  }
}




FYC_dfs <- FYC_dfs %>% 
  mutate(MCDEV = coalesce(MCDEV20, MCDEV21))

#finish making insurance variable
#paper used following insurance catagories
#private, medicare, medicaid, medicare+private|medicaid|medicare advantage, uninsured, others
#medicare advantage is MCRPHO

#in part1_datasetup_list you make private_ever, medicaid_ever, medicare_ever, public_ever, oth_public_ever

#coalease medicare advantage aka medicare managed care (MCRPHO only available 06-present)

FYC_dfs <- FYC_dfs %>% 
  mutate(MCRPHO = coalesce(MCRPHO20, MCRPHO21))

FYC_dfs <- FYC_dfs %>% 
  mutate(UNINS = coalesce(UNINS20, UNINS21))


FYC_dfs <- FYC_dfs %>% 
  mutate(INSCOV = coalesce(INSCOV20, INSCOV21))

FYC_dfs$uninsured <- ifelse(FYC_dfs$UNINS == 1, 1, 0)

FYC_dfs$medicare_advantage <- ifelse(FYC_dfs$MCRPHO == 1, 1, 0)

FYC_dfs$medicare_supplemental <- ifelse(FYC_dfs$medicare_ever == 1 & (FYC_dfs$private_ever == 1 |
                                                                                  FYC_dfs$medicaid_ever == 1 |
                                                                                  FYC_dfs$medicare_advantage == 1), 1, 0)

table(FYC_dfs$insurance, useNA = 'ifany')

FYC_dfs$insurance <- 0
FYC_dfs <- FYC_dfs %>% 
  mutate(insurance = if_else(private_ever == 1, 1, insurance),
         insurance = if_else(medicaid_ever == 1, 2, insurance),
         insurance = if_else(medicare_ever == 1, 3, insurance),
         insurance = if_else((medicare_advantage == 1 | medicare_supplemental == 1) & medicare_ever == 1 & year >= 2006, 4, insurance),
         insurance = if_else(uninsured_ever == 1, 5, insurance),
         insurance = if_else(insurance == 0, 6, insurance)) 


FYC_dfs <- FYC_dfs %>% 
  mutate(perwt = coalesce(PERWT20F, PERWT21F))

write_dta(FYC_dfs, "F:/projects/telederm/data/FYC_dfs.dta")


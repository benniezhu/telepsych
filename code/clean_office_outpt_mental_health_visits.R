#takes the dataframe of mental health visits and makes the total mental health spending
#in a year for each DUPERSID


mental_health_visits <- read_dta('F:/projects/telepsych/data/mental_health_visits.dta')



mental_health_visits <- mental_health_visits %>% 
  mutate(perwt = ifelse(!is.na(PERWT20F), PERWT20F, PERWT21F))

mental_health_visits <- mental_health_visits %>%
  mutate(
    outpatient_sf_20 = OPFSF20X + OPDSF20X,
    outpatient_sf_21 = OPFSF21X + OPDSF21X,
    outpatient_sf_22 = OPFSF22X + OPDSF22X,
  
    outpatient_xp_20 = OPFXP20X + OPDXP20X,
    outpatient_xp_21 = OPFXP21X + OPDXP21X,
    outpatient_xp_22 = OPFXP22X + OPDXP22X
  )


mental_health_visits <- mental_health_visits %>%
  mutate(
    mental_health_sf = coalesce(
      OBSF20X, OBSF21X, OBSF22X,
      outpatient_sf_20, outpatient_sf_21, outpatient_sf_22
    ),
    mental_health_xp = coalesce(
      OBXP20X, OBXP21X, OBXP22X,
      outpatient_xp_20, outpatient_xp_21, outpatient_xp_22
    )
  )

summary(mental_health_visits$mental_health_sf)
summary(mental_health_visits$mental_health_xp)

mental_health_visits <- mental_health_visits %>%
  mutate(
    mental_health_sf = case_when(
      year == 2020 ~ mental_health_sf * 1.053342224,
      year == 2021 ~ mental_health_sf * 1.040508228,
      TRUE ~ mental_health_sf  # assumes 2022
    ),
    mental_health_xp = case_when(
      year == 2020 ~ mental_health_xp * 1.053342224,
      year == 2021 ~ mental_health_xp * 1.040508228,
      TRUE ~ mental_health_xp  # assumes 2022
    )
  )

summary(mental_health_visits$mental_health_sf)
summary(mental_health_visits$mental_health_xp)


mental_health_visits <- mental_health_visits %>% 
  mutate(tele_mental_health_sf = ifelse(mental_health_visits$TELEHEALTHFLAG == 1, mental_health_visits$mental_health_sf, 0),
         tele_mental_health_xp = ifelse(mental_health_visits$TELEHEALTHFLAG == 1, mental_health_visits$mental_health_xp, 0))

mental_health_visits$mental_health_visit <- 1

mental_health_visits$mental_telehealth <- ifelse(mental_health_visits$TELEHEALTHFLAG == 1, 1, 0)


mental_health_visits_subset <- mental_health_visits %>% 
  select(DUPERSID, year, mental_health_sf, mental_health_xp, mental_health_visit, mental_telehealth, 
         tele_mental_health_sf, tele_mental_health_xp) %>% 
  group_by(DUPERSID, year) %>% 
  summarize(total_mental_health_visits = sum(mental_health_visit),
            total_mental_telehealth_visits = sum(mental_telehealth),
            oop_mental_health_visits = sum(mental_health_sf),
            total_spend_mental_health_visits = sum(mental_health_xp),
            tele_oop_mental_health_visits = sum(tele_mental_health_sf),
            tele_total_spend_mh_visits = sum(tele_mental_health_xp))

write_dta(mental_health_visits_subset, "F:/projects/telepsych/data/summarized_mental_health_visits.dta")


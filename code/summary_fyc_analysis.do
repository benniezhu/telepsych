log using "F:/projects/telepsych/output/summary_fyc_analysis.log", replace 

use "F:/projects/telepsych/data/ml_ready/psych_summary_FYC.dta", clear 

keep if total_visits != .

*drop unknown ages 3027 --> 3015 obs 
drop if age < 0 
svyset VARPSU [pweight = perwt], strata(VARSTR) vce(linearized) singleunit(certainty)


*make variable labels

label define insurance_lbl 1 "private" 2 "medicaid" 3 "medicare" 4 "medicare advantage" 5 "uninsured 1 month" 6 "other"
label define sex_lbl 1 "female" 2 "male" 
label define poverty_lbl 1 "<100% FPL" 2 "100%-200% FPL" 3 "200%-400% FPL" 4 ">400% FPL"
label define region_lbl 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label define race_lbl 1 "White" 2 "Black" 3 "Hispanic" 4 "Other"
label define edu_lbl 1 "<HS" 2 "HS Diploma" 3 "Bachelor or more"


label values insurance insurance_lbl
label values SEX sex_lbl
label values poverty poverty_lbl
label values region region_lbl 
label values edu edu_lbl
label values race race_lbl

recode age (0/18 = 1 "0-18") (19/36 = 2 "19-36") (37/55 = 3 "37-55") (56/65 = 4 "56-65") (66/80 = 5 "66-80") (81/max = 6 "80+"), gen(age_cat)

gen telemed = 0
replace telemed = 1 if total_telehealth_visits > 0

gen age2 = age * age 

global id_vars "age age2 i.year i.SEX i.race i.edu i.region i.poverty i.insurance charlson_score autism_dx intellectual_dx personality_dx manic_episode_dx bipolar_disorder_dx depressive_episode_dx mdd_dx persistent_mood_disorder_dx adhd_dx eating_disorder_dx schizophrenia_dx stress_adjstment_dx ocd_dx alcohol_dx opioid_dx cannabis_dx cocaine_dx other_stimulant_dx"


*autism_dx intellectual_dx personality_dx manic_episode_dx bipolar_disorder_dx mdd_dx persistent_mood_disorder_dx adhd_dx

*svy: mean i.SEX i.race i.insurance i.edu i.region i.poverty charlson_score 

svy: mean  total_visits total_telehealth_visits oop total_spend age_cat $id_vars 
 
svy: logit telemed i.age_cat i.year i.SEX i.race i.edu i.region i.poverty i.insurance charlson_score autism_dx intellectual_dx personality_dx manic_episode_dx bipolar_disorder_dx mdd_dx persistent_mood_disorder_dx adhd_dx, or

svy: logit telemed $id_vars , or

svy: twopm tele_oop $id_vars ,  f(probit) s(glm, link(log) family(gamma))

margins insurance edu poverty race SEX
margins, dydx( insurance edu poverty race SEX)

svy: twopm tele_total_spend $id_vars ,  f(probit) s(glm, link(log) family(gamma))
margins insurance edu poverty race
margins, dydx( insurance edu poverty race SEX)

svy: glm tele_oop $id_vars , family(gamma) link(log)

margins insurance edu poverty race SEX
margins, dydx( insurance edu poverty race SEX)


svy: glm tele_total_spend $id_vars , family(gamma) link(log)

margins insurance edu poverty race SEX
margins, dydx( insurance edu poverty race SEX)

log close 

*table 1 code 

global vars "total_visits total_telehealth_visits oop total_spend tele_oop tele_total_spend age i.SEX i.race i.insurance i.edu i.region i.poverty charlson_score "

eststo: svy: mean $vars 

eststo: svy: mean $vars if telemed == 0

eststo: svy: mean $vars if telemed == 1

esttab using "F:\projects\telepsych\output\table1.rtf" , b(2) ci(2) label replace nostar 


eststo: svy: mean $percentage_vars 

eststo: svy: mean $percentage_vars if telemed == 0

eststo: svy: mean $percentage_vars if telemed == 1

global percentage_vars "i.SEX i.race i.insurance i.edu i.region i.poverty"



/*
margins insurance edu poverty race, vce(uncond) noestimcheck
margins, dydx(race insurance edu poverty) vce(uncond) noestimcheck
margins insurance edu poverty race, vce(uncond) noestimcheck
margins, dydx(race insurance edu poverty) vce(uncond) noestimcheck
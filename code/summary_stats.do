log using "F:\projects\lizeth_project\output\summary_stats.log", replace 


use "F:\projects\lizeth_project\data\Data_weight_merged_demo.dta" , clear 
*combine weights WTMECPRP is unique to 2017-2020
gen wtmec_combined = .
replace wtmec_combined = WTMEC2YR_nhanes
replace wtmec_combined = WTMECPRP if wtmec_combined == .

svyset [w= wtmec_combined], psu(SDMVPSU) strata(SDMVSTRA)

destring AGE_GROUPS, replace
drop if AGE_GROUPS == .

*define labels 
label define year_labels 1 "1999_2000" 2 "2001_2002" 3 "2003_2004" 4 "2005_2006" 5 "2007_2008" 6 "2009_2010" 7 "2011_2012" 8 "2013_2014" 9 "2015_2016" 10 "2017_2018" 11 "2017_2020"
label values year2 year_labels

label define edu_labels 1 "Less Than 9th Grade" 2 "9-11th grade" 3 "High School Grad/GED" 4 "Some College or AA" 5 "College Graduate or Above" 6 "Refused/DK"
label values Education_level edu_labels

label define race_labels 1 "Mexican-American" 2 "Other-Hispanic" 3 "Non-Hispanic white" 4 "Non-Hispanic Black" 5 "Other Race"
label values RaceHispanic race_labels 

label define age_group_labels 1 "21-39" 2 "40-59" 3 "60+"
label values AGE_GROUPS age_group_labels

label define gender_labels 1 "Female" 2 "Male"
label values GENDER gender_labels

svy: mean i.year2 i.Education_level i.RaceHispanic i.AGE_GROUPS AGE i.GENDER OBESITY SEVERE_OBESITY BMI

log close 
use "F:/projects/telederm/data/ml_ready/derm_FYC.dta"

svyset VARPSU [pweight = perwt], strata(VARSTR) vce(linearized) singleunit(certainty)

*make variable labels

label define insurance_lbl 1 "private" 2 "medicaid" 3 "medicare" 4 "medicare advantage" 5 "uninsured 1 month" 6 "other"
label define sex_lbl 1 "female" 2 "male" 
label define poverty_lbl 1 "<100% FPL" 2 "100%-200% FPL" 3 "200%-400% FPL" 4 ">400% FPL"
label define region_lbl 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label define race_lbl 1 "White" 2 "Black" 3 "Hispanic" 4 "Other"
label define edu_lbl 1 "<HS" 2 "HS Diploma" 3 "Bachelor or more"
label define time_period_lbl 1 "<2007" 2 "2008-2011" 3 "2012-2015" 4 "2016+"
label define time_period2_lbl 1 "<2006" 2 "2007-2010" 3 "2011-2013" 4 "2014-2016" 5 "2017+"

label values insurance insurance_lbl
label values SEX sex_lbl
label values poverty poverty_lbl
label values region region_lbl 
label values edu edu_lbl
label values time_period time_period_lbl
label values time_period2 time_period2_lbl
label values race race_lbl


gen telemed = 0 if TELEHEALTHFLAG != .
replace telemed = 1 if TELEHEALTHFLAG == 1

gen age2 = age * age 

svy: mean telemed age i.year i.race i.insurance i.region i.poverty hbp_dx heart_disease_dx hld_dx stroke_dx emphy_dx arth_dx joint_dx diabetes_dx if TELEHEALTHFLAG != .


svy: logit telemed age age2 i.year i.race i.insurance i.region i.poverty hbp_dx heart_disease_dx hld_dx stroke_dx emphy_dx arth_dx joint_dx diabetes_dx, or


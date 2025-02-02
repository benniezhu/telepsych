library(tidyverse)
library(haven)
library(MEPS)
#load data

Medical_Conditions_dfs <- list()

for (year in 2020:2021){
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

#common inflamatory skin dxs 
# We defined the study population as those with at least one common inflammatory skin disease diagnosis,
#which we defined using the International Classification of Diseases, Tenth Revision (ICD-10) codes 
#for the following conditions from the Medical Conditions file: 
#L21 Seborrheic Dermatitis, L23 Allergic Contact Dermatitis, L29 Pruritus, L30 Other Unspecified Dermatitis,
#L40 Psoriasis, L50 Urticaria, L60 Nail Disorders, L65 Other Non-Scarring Hair Loss, L70 Acne, L71 Rosacea, 
#L90 Atrophic disorder of skin, L81 Pigmentation, L91 Hypertrophic Disorder of Skin,
#R21 Rash and Other Nonspecific Skin Eruption, 
# C43 Malignant Melanoma of the Skin, and C44 Other and Unspecified Malignant Neoplasm of the Skin.


#Regarding clinical scenarios that are best for video visits, follow up visits for acne, 
#eczema and psoriasis 
#are well suited from both a patient and dermatologist perspective. Skin exams, 
#examination of pigmented lesions and hair loss are conditions that are less well suited clinically
#for teledermatology.

#icd codes

#706 is technically diseases of sebacious glands
acne_icd <- c('L70', '706')

#691 and 692 are atopic and contact/other eczema 
eczema_icd <- c('L20', '691', '692')

psoriasis_icd <- c('L40' , '696')

#includes deiseases of hair follicles,  alopecia areata; androgenic alopecia; 
#other non scarring hair loss; 
#scarring hair loss
hair_loss_icd <- c('L63', 'L64', 'L65', 'L66', '704')

seb_dermatitis_icd <- c('L21')

allergic_dermatitis <- c('L23')

pruritus_icd <- c('L29')

unspecified_dermatitis_icd <- c('L30')

uticaria_icd <- c('L50')

nail_icd <- c('L60')

rosacea_icd <- c('L71')
pigmentation_icd <- c('L81')
atrophic_icd <- c('L90')
hypertrophic_icd <- c('L91')
rash_icd <- c('R21')
skin_cancer_icd <- c('C43', 'C44') #contains malignant melanoma and other/unspecfieid 


Medical_Conditions_dfs$acne_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% acne_icd, 1, 0)
Medical_Conditions_dfs$eczema_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% eczema_icd, 1, 0)
Medical_Conditions_dfs$psoriasis_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% psoriasis_icd, 1, 0)


# ICD codes for various conditions
acne_icd <- c('L70', '706')
eczema_icd <- c('L20', '691', '692')
psoriasis_icd <- c('L40', '696')
hair_loss_icd <- c('L63', 'L64', 'L65', 'L66', '704')
seb_dermatitis_icd <- c('L21')
allergic_dermatitis_icd <- c('L23')
pruritus_icd <- c('L29')
unspecified_dermatitis_icd <- c('L30')
uticaria_icd <- c('L50')
nail_icd <- c('L60')
rosacea_icd <- c('L71')
pigmentation_icd <- c('L81')
atrophic_icd <- c('L90')
hypertrophic_icd <- c('L91')
rash_icd <- c('R21')
skin_cancer_icd <- c('C43', 'C44')  # malignant melanoma and other/unspecified

# Create diagnosis columns based on ICD codes
Medical_Conditions_dfs$acne_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% acne_icd, 1, 0)
Medical_Conditions_dfs$eczema_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% eczema_icd, 1, 0)
Medical_Conditions_dfs$psoriasis_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% psoriasis_icd, 1, 0)
Medical_Conditions_dfs$hair_loss_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% hair_loss_icd, 1, 0)
Medical_Conditions_dfs$seb_dermatitis_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% seb_dermatitis_icd, 1, 0)
Medical_Conditions_dfs$allergic_dermatitis_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% 
                                                          allergic_dermatitis_icd, 1, 0)
Medical_Conditions_dfs$pruritus_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% pruritus_icd, 1, 0)
Medical_Conditions_dfs$unspecified_dermatitis_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% 
                                                             unspecified_dermatitis_icd, 1, 0)
Medical_Conditions_dfs$uticaria_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% uticaria_icd, 1, 0)
Medical_Conditions_dfs$nail_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% nail_icd, 1, 0)
Medical_Conditions_dfs$rosacea_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% rosacea_icd, 1, 0)
Medical_Conditions_dfs$pigmentation_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% pigmentation_icd, 1, 0)
Medical_Conditions_dfs$atrophic_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% atrophic_icd, 1, 0)
Medical_Conditions_dfs$hypertrophic_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% hypertrophic_icd, 1, 0)
Medical_Conditions_dfs$rash_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% rash_icd, 1, 0)
Medical_Conditions_dfs$skin_cancer_dx <- ifelse(Medical_Conditions_dfs$ICD10CDX %in% skin_cancer_icd, 1, 0)


Medical_Conditions_dfs_subset <- Medical_Conditions_dfs %>% 
  select(DUPERSID, year, ends_with("_dx")) %>% 
  group_by(DUPERSID, year) %>% 
  summarize(acne_dx = max(acne_dx, na.rm = FALSE),
            eczema_dx = max(eczema_dx, na.rm = FALSE),
            psoriasis_dx = max(psoriasis_dx, na.rm = FALSE),
            hair_loss_dx = max(hair_loss_dx, na.rm = TRUE),
            seb_dermatitis_dx = max(seb_dermatitis_dx, na.rm = TRUE),
            allergic_dermatitis_dx = max(allergic_dermatitis_dx, na.rm = TRUE),
            pruritus_dx = max(pruritus_dx, na.rm = TRUE),
            unspecified_dermatitis_dx = max(unspecified_dermatitis_dx, na.rm = TRUE),
            uticaria_dx = max(uticaria_dx, na.rm = TRUE),
            nail_dx = max(nail_dx, na.rm = TRUE),
            rosacea_dx = max(rosacea_dx, na.rm = TRUE),
            pigmentation_dx = max(pigmentation_dx, na.rm = TRUE),
            atrophic_dx = max(atrophic_dx, na.rm = TRUE),
            hypertrophic_dx = max(hypertrophic_dx, na.rm = TRUE),
            rash_dx = max(rash_dx, na.rm = TRUE),
            skin_cancer_dx = max(skin_cancer_dx, na.rm = TRUE))

# List of all _dx variables
dx_variables <- c("acne_dx", "eczema_dx", "psoriasis_dx", "hair_loss_dx", "seb_dermatitis_dx", 
                  "allergic_dermatitis_dx", "pruritus_dx", "unspecified_dermatitis_dx", 
                  "uticaria_dx", "nail_dx", "rosacea_dx", "pigmentation_dx", 
                  "atrophic_dx", "hypertrophic_dx", "rash_dx", "skin_cancer_dx")

# Loop through each _dx variable and print the table
for (dx in dx_variables) {
  cat("Frequency table for", dx, ":\n")
  print(table(Medical_Conditions_dfs_subset[[dx]]))
  cat("\n")  # Adds a line break between tables
}

write_dta(Medical_Conditions_dfs_subset, "F:/projects/telederm/data/medical_conditions_summary.dta")

table(Medical_Conditions_dfs_subset$acne_dx)


test <- Medical_Conditions_dfs %>% 
  select(DUPERSID, year, ends_with("_dx")) %>% 
  group_by(DUPERSID, year) %>% 
  summarize(acne_dx = sum(acne_dx, na.rm = FALSE),
            eczema_dx = sum(eczema_dx, na.rm = FALSE),
            psoriasis_dx = sum(psoriasis_dx, na.rm = FALSE))



icd_10_fx <- function()
{
  
  L20  Atopic dermatitis
  L21  Seborrheic dermatitis
  L22  Diaper dermatitis
  L23  Allergic contact dermatitis
  L24  Irritant contact dermatitis
  L25  Unspecified contact dermatitis
  L26  Exfoliative dermatitis
  L27  Dermatitis due to substances taken internally
  L28  Lichen simplex chronicus and prurigo
  L29  Pruritus
  L30  Other and unspecified dermatitis
  
  
  L40  Psoriasis
  L41  Parapsoriasis
  L42  Pityriasis rosea
  L43  Lichen planus
  L44  Other papulosquamous disorders
  L45  Papulosquamous disorders in diseases classified elsewhere
  
  L63  Alopecia areata
  L64  Androgenic alopecia
  L65  Other nonscarring hair loss
  L66  Cicatricial alopecia [scarring hair loss]
  L67  Hair color and hair shaft abnormalities
  L68  Hypertrichosis
  L70  Acne
  
}

icd_9_fx <- function()
{
  690 Erythematosquamous dermatosis
  691 Atopic dermatitis and related conditions
  692 Contact dermatitis and other eczema
  693 Dermatitis due to substances taken internally
  694 Bullous dermatoses
  695 Erythematous conditions
  696 Psoriasis and similar disorders
  697 Lichen
  698 Pruritus and related conditions
  
  700 Corns and callosities
  701 Other hypertrophic and atrophic conditions of skin
  702 Other dermatoses
  703 Diseases of nail
  704 Diseases of hair and hair follicles
  705 Disorders of sweat glands
  706 Diseases of sebaceous glands
  707 Chronic ulcer of skin
  708 Urticaria
  709 Other disorders of skin and subcutaneous tissue
  
}




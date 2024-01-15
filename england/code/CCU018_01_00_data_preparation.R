################################################################################
## TITLE: Data preparation of England dataset for CCU018 
##
## Description: Formatting and adding derived variables
##
## By : Elena Raffetti
##
################################################################################

rm(list = ls())

# install.packages -------------------------------------------------------------
install.packages(odbc)
install.packages(DBI)
install.packages(dbplyr)
install.packages(dplyr)
install.packages(lubridate)

# load required packages -------------------------------------------------------
library(odbc)
library(DBI)
library(dbplyr)
library(dplyr)
library(lubridate)

# load data --------------------------------------------------------------------
con <- DBI::dbConnect(odbc:::odbc(), 'databricks')

# cohort
ccu018_cohort_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu018_01_out_cohort"))
cohort <- ccu018_cohort_tbl %>%
  collect()

# covariates
ccu018_covariates_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu018_01_out_covariates"))
covariates <- ccu018_covariates_tbl %>%
  collect()

# covid exposure
ccu018_covid_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu018_01_out_exposures"))
exposure <- ccu018_covid_tbl %>%
  collect()

# outcomes during pregnancy
ccu018_outcomes_dur_preg_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu018_01_out_outcomes_dur_preg"))
outcomes_during_preg <- ccu018_outcomes_dur_preg_tbl %>%
  collect()

# outcomes post pregnancy
ccu018_outcomes_post_preg_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu018_01_out_outcomes_post_preg"))
outcomes_post_preg <- ccu018_outcomes_post_preg_tbl %>%
  collect()

# outcomes at birth
ccu018_outcomes_at_birth_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu018_01_out_outcomes_at_birth"))
outcomes_birth <- ccu018_outcomes_at_birth_tbl %>%
  collect()

# skinny table
ccu018_skinny_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu018_01_out_skinny"))
skinny <- ccu018_skinny_tbl %>%
  collect()

# prepare cohort table ---------------------------------------------------------
head(cohort)
names(cohort) <- toupper(names(cohort))
ls(cohort)

# format dates -----------------------------------------------------------------
cohort$PREG_START_DATE <- as.Date(cohort$PREG_START_DATE, "%d/%m/%Y") 
cohort$DELIVERY_DATE <- as.Date(cohort$DELIVERY_DATE, "%d/%m/%Y")
cohort$FU_END_DATE <- as.Date(cohort$FU_END_DATE, "%d/%m/%Y")

# define pregnancy duration ----------------------------------------------------
cohort$FUP_PREG<-as.numeric(difftime(cohort$DELIVERY_DATE, cohort$PREG_START_DATE, unit="days"))
summary(cohort$FUP_PREG)

# prepare cohort table for merging with skinny ---------------------------------
cohort_skinny <- cohort %>% select(PERSON_ID,PREG_START_DATE)
nrow(cohort)

# prepare skinny table ---------------------------------------------------------
head(skinny)
ls(skinny)

# link skinny with cohort table ------------------------------------------------
skinny <- merge(cohort_skinny,skinny, by=c("PERSON_ID"), all=TRUE)


# prepare skinny table ---------------------------------------------------------
skinny<-skinny %>%
  rename(date_DOB = `_date_DOB`,
         date_DOD = `_date_DOD`,
         date_ETHNIC = `_date_ETHNIC`,
         date_LSOA = `_date_LSOA`,
         date_SEX = `_date_SEX`,
         source_DOB  = `_source_DOB`,
         source_DOD = `_source_DOD`,
         source_ETHNIC = `_source_ETHNIC`,
         source_LSOA = `_source_LSOA`,
         source_SEX = `_source_SEX`,
         tie_DOB = `_tie_DOB`,
         tie_DOD = `_tie_DOD`,
         tie_ETHNIC= `_tie_ETHNIC`,
         tie_LSOA = `_tie_LSOA`,
         tie_SEX = `_tie_SEX`)

skinny$DOB <- as.Date(skinny$DOB, "%d/%m/%Y")
skinny$COV_AGE<- as.integer((as.numeric(difftime(skinny$PREG_START_DATE, skinny$DOB, unit="days")))/365.25)
hist(skinny$COV_AGE)

summary(skinny$COV_AGE)

# define age groups ------------------------------------------------------------
agebreaks <- c(0, 30, 40, 500)
agelabels <- c("<30", "30-39", ">=40")
library(data.table)
skinny <- setDT(skinny)[ , agegroup := cut(COV_AGE, breaks = agebreaks, right = FALSE, labels = agelabels)]

skinny<-skinny %>%
  rename(COV_ETHNICITY = ETHNIC_CAT)

# define ethnicity with 3 categories -------------------------------------------
skinny <- skinny %>%
  mutate     (COV_ETHNICITY_3lev = case_when(skinny$COV_ETHNICITY == "Mixed" | skinny$COV_ETHNICITY == "Asian or Asian British" | skinny$COV_ETHNICITY == "Black or Black British"  ~  "Other",
                                             skinny$COV_ETHNICITY == "Unknown" |  skinny$COV_ETHNICITY == NA ~  "Unknown",
                                             skinny$COV_ETHNICITY == "White" ~  "White"
  )
  )

# rename deprivation -----------------------------------------------------------
skinny<-skinny %>%
  rename(COV_DEPRIVATION = IMD_2019_QUINTILES)

skinny <- skinny %>% select(-PREG_START_DATE)


# prepare covariates -----------------------------------------------------------
head(covariates)
ls(covariates)

# diabetes
covariates$cov_hx_diabetes_drugs_flag[is.na(covariates$cov_hx_diabetes_drugs_flag)] <- 0
covariates$cov_hx_diabetes_flag[is.na(covariates$cov_hx_diabetes_flag)] <- 0
covariates$cov_hx_hypertension_drugs_flag[is.na(covariates$cov_hx_hypertension_drugs_flag)] <- 0
covariates$cov_hx_hypertension_flag[is.na(covariates$cov_hx_hypertension_flag)] <- 0

table(covariates$cov_hx_diabetes_drugs_flag, covariates$cov_hx_diabetes_flag)
table(covariates$cov_hx_hypertension_drugs_flag, covariates$cov_hx_hypertension_flag)

covariates <- covariates %>%
  mutate     (COV_HX_HYPERTENSION = case_when(covariates$cov_hx_hypertension_drugs_flag == 0 & covariates$cov_hx_hypertension_flag==0  ~  0,
                                              covariates$cov_hx_hypertension_drugs_flag == 1 | covariates$cov_hx_hypertension_flag==1   ~ 1
  )
  )

covariates <- covariates %>%
  mutate     (COV_HX_DIABETES = case_when(covariates$cov_hx_diabetes_drugs_flag == 0 & covariates$cov_hx_diabetes_flag==0  ~  0,
                                          covariates$cov_hx_diabetes_drugs_flag == 1 | covariates$cov_hx_diabetes_flag==1   ~ 1
  )
  )

covariates$cov_hx_gest_diabetes_flag[is.na(covariates$cov_hx_gest_diabetes_flag)] <- 0
covariates$cov_hx_gest_hypertension_flag[is.na(covariates$cov_hx_gest_hypertension_flag)] <- 0
covariates$cov_hx_preeclampsia_flag[is.na(covariates$cov_hx_preeclampsia_flag)] <- 0
covariates$cov_hx_pregnancy_flag[is.na(covariates$cov_hx_pregnancy_flag)] <- 0
covariates$cov_hx_arterial_flag[is.na(covariates$cov_hx_arterial_flag)] <- 0
covariates$cov_hx_venous_flag[is.na(covariates$cov_hx_venous_flag)] <- 0
covariates$cov_hx_other_cvd_flag[is.na(covariates$cov_hx_other_cvd_flag)] <- 0
covariates$cov_hx_haematological_flag[is.na(covariates$cov_hx_haematological_flag)] <- 0
covariates$cov_hx_dementia_flag[is.na(covariates$cov_hx_dementia_flag)] <- 0
covariates$cov_hx_pcos_flag[is.na(covariates$cov_hx_pcos_flag)] <- 0
covariates$cov_hx_ckd_flag[is.na(covariates$cov_hx_ckd_flag)] <- 0
covariates$cov_hx_prostate_cancer_flag[is.na(covariates$cov_hx_prostate_cancer_flag)] <- 0
covariates$cov_hx_covid19_flag[is.na(covariates$cov_hx_covid19_flag)] <- 0
covariates$cov_hx_bmi_obesity_flag[is.na(covariates$cov_hx_bmi_obesity_flag)] <- 0
covariates$cov_hx_copd_flag[is.na(covariates$cov_hx_copd_flag)] <- 0
covariates$cov_hx_cancer_flag[is.na(covariates$cov_hx_cancer_flag)] <- 0
covariates$cov_hx_depression_flag[is.na(covariates$cov_hx_depression_flag)] <- 0
covariates$cov_hx_liver_disease_flag[is.na(covariates$cov_hx_liver_disease_flag)] <- 0
covariates$cov_surg_last_yr_flag[is.na(covariates$cov_surg_last_yr_flag)] <- 0


covariates <- covariates %>%
  mutate     (COV_HX_OTHER_DIS = case_when(covariates$cov_hx_copd_flag == 0 & covariates$cov_surg_last_yr_flag==0 & covariates$cov_hx_liver_disease_flag == 0 & covariates$cov_hx_ckd_flag == 0 & covariates$cov_hx_cancer_flag == 0  ~  0,
                                           covariates$cov_hx_copd_flag == 1 | covariates$cov_surg_last_yr_flag==1 | covariates$cov_hx_liver_disease_flag == 1 | covariates$cov_hx_ckd_flag == 1 | covariates$cov_hx_cancer_flag == 1   ~ 1
  )
  )
table(covariates$COV_HX_OTHER_DIS)

covariates <- covariates %>%
  mutate     (COV_HX_CVD_HEM = case_when(covariates$cov_hx_haematological_flag == 0 & covariates$cov_hx_other_cvd_flag == 0 & covariates$cov_hx_venous_flag == 0  & covariates$cov_hx_arterial_flag == 0  ~  0,
                                         covariates$cov_hx_haematological_flag == 1 | covariates$cov_hx_other_cvd_flag == 1 | covariates$cov_hx_venous_flag == 1 | covariates$cov_hx_arterial_flag == 1   ~ 1
  )
  )
table(covariates$COV_HX_CVD_HEM)

covariates <- covariates %>%
  mutate     (COV_HX_DIABETES_DIS = case_when(covariates$cov_hx_gest_diabetes_flag==0 & covariates$COV_HX_DIABETES==0 ~  0,
                                              covariates$cov_hx_gest_diabetes_flag==1 | covariates$COV_HX_DIABETES==1 ~  1
  )
  )


covariates <- covariates %>%
  mutate     (COV_HX_HYPERTENSIVE_DIS = case_when(covariates$cov_hx_gest_hypertension_flag==0 & covariates$cov_hx_preeclampsia_flag==0 & covariates$COV_HX_HYPERTENSION==0 ~  0,
                                                  covariates$cov_hx_gest_hypertension_flag==1 | covariates$cov_hx_preeclampsia_flag==1 | covariates$COV_HX_HYPERTENSION==1 ~  1
  )
  )

covariates <- covariates %>%
  mutate     (COV_HX_HEALTH_CONDITION = case_when(covariates$COV_HX_HYPERTENSIVE_DIS==0 & covariates$COV_HX_CVD_HEM==0 & covariates$COV_HX_DIABETES_DIS==0 & covariates$COV_HX_OTHER_DIS==0 ~  0,
                                                  covariates$COV_HX_HYPERTENSIVE_DIS==1 | covariates$COV_HX_CVD_HEM==1 | covariates$COV_HX_DIABETES_DIS==1 | covariates$COV_HX_OTHER_DIS==1 ~  1
  )
  )


table(is.na(covariates$cov_smoking_status))

ls(covariates)

covariates <-covariates %>%
  rename(COV_HX_PREGNANCY_MAX = cov_hx_pregnancy_flag
  )

covariates_selection <- covariates %>% select(PERSON_ID, COV_HX_HEALTH_CONDITION, COV_HX_CVD_HEM, COV_HX_OTHER_DIS, cov_hx_depression_flag, COV_HX_DIABETES_DIS, COV_HX_PREGNANCY_MAX, COV_HX_HYPERTENSIVE_DIS,
                                              cov_hx_bmi_obesity_flag, cov_hx_pcos_flag, cov_smoking_status)
names(covariates_selection) <- toupper(names(covariates_selection))
ls(covariates_selection)

table(covariates_selection$COV_HX_PCOS_FLAG)


# prepare exposure -------------------------------------------------------------
head(exposure)

names(exposure) <- toupper(names(exposure))

exposure$EXP_PRE_COVID_1ST_PHENOTYPE[is.na(exposure$EXP_PRE_COVID_1ST_PHENOTYPE)] <- 0
exp_pre_phen <-table(exposure$EXP_PRE_COVID_1ST_PHENOTYPE)
cbind(exp_pre_phen,prop.table(exp_pre_phen))
rm("exp_pre_phen")

exposure$EXP_DUR_COVID_1ST_PHENOTYPE[is.na(exposure$EXP_DUR_COVID_1ST_PHENOTYPE)] <- 0
exp_dur_phen <-table(exposure$EXP_DUR_COVID_1ST_PHENOTYPE)
cbind(exp_dur_phen,prop.table(exp_dur_phen))
rm("exp_dur_phen")

exposure$EXP_PRE_COVID_ADM_DAYS_LE_28[is.na(exposure$EXP_PRE_COVID_ADM_DAYS_LE_28)] <- 0
exp_pre_severity <-table(exposure$EXP_PRE_COVID_ADM_DAYS_LE_28)
cbind(exp_pre_severity,prop.table(exp_pre_severity))
rm("exp_pre_severity")

exposure$EXP_DUR_COVID_ADM_DAYS_LE_28[is.na(exposure$EXP_DUR_COVID_ADM_DAYS_LE_28)] <- 0
exp_dur_severity <-table(exposure$EXP_DUR_COVID_ADM_DAYS_LE_28)
cbind(exp_dur_severity,prop.table(exp_dur_severity))
rm("exp_dur_severity")

exposure$VACC_1ST_FLAG[is.na(exposure$VACC_1ST_FLAG)] <- 0
first_vaccination <-table(exposure$VACC_1ST_FLAG)
cbind(first_vaccination,prop.table(first_vaccination))
rm("first_vaccination")

# merge with pregnancy cohort information --------------------------------------
cohort_exp <- cohort %>% select (PERSON_ID, PREG_START_DATE, DELIVERY_DATE)
exposures <- merge(cohort_exp, exposure, by=c("PERSON_ID"), all= TRUE)

# define format dates ----------------------------------------------------------
exposures$EXP_PRE_COVID_1ST_DATE <- as.Date(exposures$EXP_PRE_COVID_1ST_DATE, "%d/%m/%Y") 
exposures$EXP_DUR_COVID_1ST_DATE <- as.Date(exposures$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y")
exposures$VACC_1ST_DATE <- as.Date(exposures$VACC_1ST_DATE, "%d/%m/%Y") 
exposures$PREG_START_DATE <- as.Date(exposures$PREG_START_DATE, "%d/%m/%Y") 
exposures$DELIVERY_DATE <- as.Date(exposures$DELIVERY_DATE, "%d/%m/%Y")
head(exposures)

# check COVID-19 infection during pregnancy ------------------------------------
exposures$EXP_COVID_BIN <- ifelse(exposures$EXP_DUR_COVID_1ST_DATE>=exposures$PREG_START_DATE & exposures$EXP_DUR_COVID_1ST_DATE<=exposures$DELIVERY_DATE,1,0)
exposures$EXP_COVID_BIN[is.na(exposures$EXP_DUR_COVID_1ST_DATE)]=0

covid_bin <-table(exposures$EXP_COVID_BIN)
cbind(covid_bin,prop.table(covid_bin))
rm("covid_bin")

# check COVID-19 infection before pregnancy ------------------------------------
exposures$EXP_COVID_PRE_BIN <- ifelse(exposures$EXP_PRE_COVID_1ST_DATE<exposures$PREG_START_DATE,1,0)
exposures$EXP_COVID_PRE_BIN[is.na(exposures$EXP_PRE_COVID_1ST_DATE)]=0

covid_pre_bin <-table(exposures$EXP_COVID_PRE_BIN)
cbind(covid_pre_bin,prop.table(covid_pre_bin))
rm("covid_pre_bin")

table(exposures$EXP_COVID_BIN, exposures$EXP_COVID_PRE_BIN)

# define hosp for COVID19 during pregnancy -------------------------------------
severity <-table(exposures$EXP_DUR_COVID_ADM_DAYS_LE_28)
cbind(severity,prop.table(severity))
rm("severity")

exposures<-exposures %>%
  rename(EXP_DUR_HOSP= EXP_DUR_COVID_ADM_DAYS_LE_28)

covid_hosp <-table(exposures$EXP_DUR_HOSP)
cbind(covid_hosp,prop.table(covid_hosp))
rm("covid_hosp")

# define 1st and 2nd vaccination -----------------------------------------------
exposures$VACC_1ST_DUR_FLAG[is.na(exposures$VACC_1ST_DUR_FLAG)] <- 0
exposures$VACC_2ND_DUR_FLAG[is.na(exposures$VACC_2ND_DUR_FLAG)] <- 0

# set vaccination exposures as binary ------------------------------------------
table(exposures$VACC_1ST_DUR_FLAG)
table(exposures$VACC_1ST_DUR_FLAG, exposures$EXP_COVID_BIN)

# define trimester of infection ------------------------------------------------
exposures$EXP_TIME_TO_COVID<-as.numeric(difftime(exposures$EXP_DUR_COVID_1ST_DATE, exposures$PREG_START_DATE, unit="days"))
hist(exposures$EXP_TIME_TO_COVID)
exposures$EXP_TIME_TO_COVID[exposures$EXP_COVID_BIN==0]= NA
sum(!is.na(exposures$EXP_TIME_TO_COVID))

exposures <- exposures %>%
  mutate     (EXP_TRIMESTER = case_when(exposures$EXP_COVID_BIN==0 ~  0,
                                        exposures$EXP_TIME_TO_COVID <= 84 ~  1,
                                        exposures$EXP_TIME_TO_COVID > 84 & exposures$EXP_TIME_TO_COVID <= 182   ~ 2,
                                        exposures$EXP_TIME_TO_COVID > 182 & exposures$EXP_DUR_COVID_1ST_DATE <= exposures$DELIVERY_DATE  ~  3
  )
  )

table(exposures$EXP_TRIMESTER, exposures$EXP_COVID_BIN)

trim <-table(exposures$EXP_TRIMESTER)
cbind(trim,prop.table(trim))
rm("trim")

# define trimester of 1st vaccination ------------------------------------------
exposures$EXP_TIME_TO_VACCINATION<-as.numeric(difftime(exposures$VACC_1ST_DUR_DATE, exposures$PREG_START_DATE, unit="days"))
hist(exposures$EXP_TIME_TO_VACCINATION)
exposures$EXP_TIME_TO_VACCINATION[exposures$VACC_1ST_DUR_FLAG==0]= NA
hist(exposures$EXP_TIME_TO_VACCINATION)
sum(!is.na(exposures$EXP_TIME_TO_VACCINATION))

exposures <- exposures %>%
  mutate     (EXP_VACC_TRIMESTER = case_when(exposures$VACC_1ST_DUR_FLAG==0 ~  0,
                                        exposures$EXP_TIME_TO_VACCINATION <= 84 ~  1,
                                        exposures$EXP_TIME_TO_VACCINATION > 84 & exposures$EXP_TIME_TO_VACCINATION <= 182   ~ 2,
                                        exposures$EXP_TIME_TO_VACCINATION > 182 & exposures$VACC_1ST_DUR_DATE <= exposures$DELIVERY_DATE  ~  3
  )
  )

table(exposures$EXP_VACC_TRIMESTER, exposures$VACC_1ST_DUR_FLAG)

trim_vaccination <-table(exposures$EXP_VACC_TRIMESTER)
cbind(trim_vaccination,prop.table(trim_vaccination))
rm("trim_vaccination")

# define trimester of 2nd vaccination ------------------------------------------
exposures$EXP_TIME_TO_2ND_VACCINATION<-as.numeric(difftime(exposures$VACC_2ND_DUR_DATE, exposures$PREG_START_DATE, unit="days"))
hist(exposures$EXP_TIME_TO_2ND_VACCINATION)
exposures$EXP_TIME_TO_2ND_VACCINATION[exposures$VACC_2ND_DUR_DATE==0]= NA
hist(exposures$EXP_TIME_TO_2ND_VACCINATION)
sum(!is.na(exposures$EXP_TIME_TO_2ND_VACCINATION))

exposures <- exposures %>%
  mutate     (EXP_2ND_VACC_TRIMESTER = case_when(exposures$VACC_2ND_DUR_DATE==0 ~  0,
                                             exposures$EXP_TIME_TO_2ND_VACCINATION <= 84 ~  1,
                                             exposures$EXP_TIME_TO_2ND_VACCINATION > 84 & exposures$EXP_TIME_TO_2ND_VACCINATION <= 182   ~ 2,
                                             exposures$EXP_TIME_TO_2ND_VACCINATION > 182 & exposures$VACC_2ND_DUR_DATE <= exposures$DELIVERY_DATE  ~  3
  )
  )

table(exposures$EXP_2ND_VACC_TRIMESTER, exposures$VACC_2ND_DUR_DATE)

# outcomes at birth ------------------------------------------------------------
head(outcomes_birth)
ls(outcomes_birth)
names(outcomes_birth) <- toupper(names(outcomes_birth))
ls(outcomes_birth)

table(outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE)
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_GENCODE_HES)
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_GENCODE_GDPPR)

outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX[is.na(outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX)] <- 0
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX)

outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR)] <- 0
outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE[is.na(outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE)] <- 0 
outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GDPPR[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GDPPR)] <- 0 
outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)] <- 0 

table(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR)
table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GDPPR)
table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE)

table(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR, outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GDPPR)
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE, outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)

outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX)] <- 0 
table(outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX) 

still_delvar <-table(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELCODE)
cbind(still_delvar,prop.table(still_delvar))
rm("still_delvar")

still_hes <-table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)
cbind(still_hes,prop.table(still_hes))
rm("still_hes")

still_gp <-table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GDPPR)
cbind(still_gp,prop.table(still_gp))
rm("still_gp")

fup <- cohort %>% select(PERSON_ID,FUP_PREG)
nrow(outcomes_birth)
outcomes_birth <- merge(outcomes_birth,fup, by=c("PERSON_ID"))
nrow(outcomes_birth)

# set minimum days from estimated pregnancy start date to define stillbirth ----
outcomes_birth <- outcomes_birth %>%
  mutate     (OUT_BIRTH_STILLBIRTH_MAX = case_when(outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX==0  ~  0,
                                                   outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX==1 & outcomes_birth$FUP_PREG < 240 ~  0,
                                                   outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX==1 & outcomes_birth$FUP_PREG >= 240 ~  1
  )
  )

outcomes_birth$OUT_BIRTH_IND_LAB[is.na(outcomes_birth$OUT_BIRTH_IND_LAB)] <- 0 

table(outcomes_birth$OUT_BIRTH_PRETERM, outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX)

outcomes_birth$OUT_BIRTH_PRETERM[is.na(outcomes_birth$OUT_BIRTH_PRETERM)] <- 0
outcomes_birth$OUT_BIRTH_VERY_PRETERM[is.na(outcomes_birth$OUT_BIRTH_VERY_PRETERM)] <- 0
outcomes_birth$OUT_BIRTH_EXT_PRETERM[is.na(outcomes_birth$OUT_BIRTH_EXT_PRETERM)] <- 0
outcomes_birth$OUT_BIRTH_RESUS[is.na(outcomes_birth$OUT_BIRTH_RESUS)] <- 0
outcomes_birth$OUT_BIRTH_SMALL_GEST_AGE[is.na(outcomes_birth$OUT_BIRTH_SMALL_GEST_AGE)] <- 0

# outcomes during pregnancy ----------------------------------------------------
head(outcomes_during_preg)
ls(outcomes_during_preg)
names(outcomes_during_preg) <- toupper(names(outcomes_during_preg))
head(outcomes_during_preg)

outcomes_during_preg$OUT_DUR_GEST_HYPERTENSION_FLAG[is.na(outcomes_during_preg$OUT_DUR_GEST_HYPERTENSION_FLAG)] <- 0 
outcomes_during_preg$OUT_DUR_PREECLAMPSIA_FLAG[is.na(outcomes_during_preg$OUT_DUR_PREECLAMPSIA_FLAG)] <- 0 
outcomes_during_preg$OUT_DUR_GEST_DIABETES_FLAG[is.na(outcomes_during_preg$OUT_DUR_GEST_DIABETES_FLAG)] <- 0 
outcomes_during_preg$OUT_DUR_ARTERIAL_FLAG[is.na(outcomes_during_preg$OUT_DUR_ARTERIAL_FLAG)] <- 0 
outcomes_during_preg$OUT_DUR_VENOUS_FLAG[is.na(outcomes_during_preg$OUT_DUR_VENOUS_FLAG)] <- 0 
outcomes_during_preg$OUT_DUR_HAEMATOLOGICAL_FLAG[is.na(outcomes_during_preg$OUT_DUR_HAEMATOLOGICAL_FLAG)] <- 0 

#rename outcomes during pregnancy ----------------------------------------------
out_dur_preg <- outcomes_during_preg %>% select(PERSON_ID,
                                                OUT_DUR_HAEMATOLOGICAL_FLAG, OUT_DUR_HAEMATOLOGICAL_DATE,
                                                OUT_DUR_VENOUS_FLAG,  OUT_DUR_VENOUS_DATE,
                                                OUT_DUR_ARTERIAL_FLAG, OUT_DUR_ARTERIAL_DATE,
                                                OUT_DUR_GEST_DIABETES_FLAG, OUT_DUR_GEST_DIABETES_DATE,
                                                OUT_DUR_PREECLAMPSIA_FLAG,  OUT_DUR_PREECLAMPSIA_DATE,
                                                OUT_DUR_GEST_HYPERTENSION_FLAG,  OUT_DUR_GEST_HYPERTENSION_DATE) 

out_dur_preg<-out_dur_preg %>%
  rename(OUT_BIN_DUR_HAEMOTOLOGICAL= OUT_DUR_HAEMATOLOGICAL_FLAG,
         OUT_BIN_DUR_VENOUS = OUT_DUR_VENOUS_FLAG,
         OUT_BIN_DUR_ARTERIAL = OUT_DUR_ARTERIAL_FLAG,
         OUT_BIN_GEST_DIABETES = OUT_DUR_GEST_DIABETES_FLAG,
         OUT_BIN_PREECLAMPSIA  = OUT_DUR_PREECLAMPSIA_FLAG,
         OUT_BIN_GEST_HYPERTENSION  = OUT_DUR_GEST_HYPERTENSION_FLAG)

# select variable from out_birth -----------------------------------------------
out_post_preg <- outcomes_post_preg %>% select(PERSON_ID,  
                                               OUT_POST_HAEMATOLOGICAL_FLAG, OUT_POST_HAEMATOLOGICAL_DATE,
                                               OUT_POST_VENOUS_FLAG,  OUT_POST_VENOUS_DATE,
                                               OUT_POST_ARTERIAL_FLAG, OUT_POST_ARTERIAL_DATE)

# select variables from out_post_pregnancy -------------------------------------
out_post_preg<-out_post_preg %>%
  rename(OUT_BIN_POST_HAEMATOLOGICAL= OUT_POST_HAEMATOLOGICAL_FLAG,
         OUT_BIN_POST_VENOUS = OUT_POST_VENOUS_FLAG,
         OUT_BIN_POST_ARTERIAL = OUT_POST_ARTERIAL_FLAG)

# select variables from out_birth ----------------------------------------------
out_birth <- outcomes_birth %>% select(PERSON_ID, OUT_BIRTH_PRETERM,OUT_BIRTH_STILLBIRTH_MAX, OUT_BIRTH_MULTI_GEST_MAX, 
                                       OUT_BIRTH_VERY_PRETERM,  OUT_BIRTH_EXT_PRETERM,
                                       OUT_BIRTH_RESUS, OUT_BIRTH_SMALL_GEST_AGE, OUT_BIRTH_IND_LAB)

# rename out_birth names -------------------------------------------------------
out_birth<-out_birth %>%
  mutate(across(.names = 'OUT_BIN_{sub("OUT_", "", .col)}'))

# combine dataframes ------------------------------------------------------------
data_temp1 <- merge(cohort,skinny, by=c("PERSON_ID"), all=TRUE)
exposures <- subset(exposures, select = -c(PREG_START_DATE, DELIVERY_DATE), all=TRUE)
data_temp2 <- merge(data_temp1,exposures, by=c("PERSON_ID"), all=TRUE)
outcomes <- merge(out_birth, out_post_preg, by=c("PERSON_ID"), all=TRUE)
outcomes <- merge(outcomes, out_dur_preg, by=c("PERSON_ID"), all=TRUE)
data_temp3 <- merge(data_temp2,outcomes, by=c("PERSON_ID"), all=TRUE)
data_temp4 <- merge(data_temp3,covariates_selection, by=c("PERSON_ID"), all=TRUE)
rm("data_temp1","data_temp2","data_temp3")

# set inclusion criteria for infection analysis --------------------------------
ls(data_temp4)

nrow(data_temp4)
data_temp4 <- data_temp4[which(data_temp4$FUP_PREG>=84),]
data_temp4 <- data_temp4[which(data_temp4$OUT_BIN_BIRTH_MULTI_GEST_MAX==0),]
nrow(data_temp4)

data_temp4 <- data_temp4[which(data_temp4$COV_AGE>=18 & data_temp4$COV_AGE<=49),]
nrow(data_temp4)

# set inclusion criteria for vaccination analysis ------------------------------
data_temp5 <- data_temp4
ls(data_temp5)
nrow(data_temp5)

# format vaccination date
data_temp5$PREG_START_DATE <- as.Date(data_temp5$PREG_START_DATE, "%d/%m/%Y")

# include women with estimated pregnancy start date in vaccination era
data_temp5 <- data_temp5[which(data_temp5$PREG_START_DATE >= '2020-12-08'),]
nrow(data_temp5)

# exclude women vaccinated before pregnancy
data_temp5<- data_temp5 %>%
  mutate     (VACC_PRE_PREG = case_when(data_temp5$VACC_1ST_FLAG == 0 ~  0,
                                        data_temp5$VACC_1ST_FLAG==1  & (data_temp5$VACC_1ST_DATE == data_temp5$VACC_1ST_DUR_DATE) ~ 0,
                                        data_temp5$VACC_1ST_FLAG==1  & (data_temp5$VACC_1ST_DATE > data_temp5$DELIVERY_DATE) ~ 0,
                                        data_temp5$VACC_1ST_FLAG==1  & (data_temp5$VACC_1ST_DATE < data_temp5$VACC_1ST_DUR_DATE) ~ 1
  )
  )

data_temp5 <- data_temp5[which(data_temp5$VACC_PRE_PREG ==0),]
nrow(data_temp5)


data_temp6 <- data_temp5 %>%
  mutate     (END_FOLLOW_1ST_VACC = data_temp5$DELIVERY_DATE
  )

#check
sum(data_temp6$VACC_2ND_DUR_DATE< data_temp6$VACC_1ST_DUR_DATE)

data_temp7 <- data_temp5[which(data_temp5$VACC_1ST_DUR_FLAG ==1),]
nrow(data_temp7)

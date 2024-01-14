################################################################################
## TITLE: Data preparation of Wales dataset for CCU018_01
##
## Description: Formatting and adding derived variables
##
## By : Elena Raffetti
##
################################################################################

rm(list = ls())

# Install.packages -------------------------------------------------------------
#install.packages("DBI")
#install.packages("odbc")
#install.packages("dbplyr")
#install.packages("lubridate")
#install.packages("rstudioapi")
#install.packages("RODBC")
#install.packages("ggplot2")
#install.packages("incidence")
#install.packages("tidyverse")
#install.packages("epiR")
#install.packages("scales")
#install.packages("survival")
#install.packages("survminer")
#install.packages("gtsummary")
#install.packages("reshape2")
#install.packages("date")
#install.packages("lubridate")
#install.packages("stringr")

# load required packages -------------------------------------------------------
library(DBI)
library(odbc)
library(dbplyr)
library(dplyr)
library(rstudioapi)
library(lubridate)
library(RODBC)
library(stringr)
library(ggplot2)
library(incidence)
library(tidyverse)
library(epiR)
library(scales)
library(survival)
library(survminer)
library(gtsummary)
library(reshape2)
library(date)
library(lubridate)

# load data --------------------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(),
                      "PR_SAIL",
                      uid = askForPassword("Enter Username"),
                      password = askForPassword("Enter Password"))

CODELIST <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_CODELIST")) 
CODELIST <- CODELIST %>%
  collect()
summary(CODELIST)
data.table::fwrite(CODELIST,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_CODELIST.csv")


OUT_COHORT <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_OUT_COHORT")) 
OUT_COHORT <- OUT_COHORT %>%
          collect()
summary(OUT_COHORT)
data.table::fwrite(OUT_COHORT,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_COHORT.csv")

SKINNY <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_OUT_SKINNY")) 
SKINNY <- SKINNY %>%
  collect()
summary(SKINNY)
data.table::fwrite(SKINNY,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_SKINNY.csv")

OUT_COVARIATES <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_OUT_COVARIATES")) 
OUT_COVARIATES <- OUT_COVARIATES %>%
  collect()
summary(OUT_COVARIATES)
data.table::fwrite(OUT_COVARIATES,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_COVARIATES.csv")

OUT_EXPOSURE <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_OUT_EXPOSURE")) 
OUT_EXPOSURE <- OUT_EXPOSURE %>%
  collect()
summary(OUT_EXPOSURE)
data.table::fwrite(OUT_EXPOSURE,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_EXPOSURE.csv")

OUTCOMES_AT_BIRTH <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_OUT_OUTCOMES_AT_BIRTH")) 
OUTCOMES_AT_BIRTH <- OUTCOMES_AT_BIRTH %>%
  collect()
summary(OUTCOMES_AT_BIRTH)
data.table::fwrite(OUTCOMES_AT_BIRTH,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_OUTCOMES_AT_BIRTH.csv")

OUTCOMES_DUR_PREG <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_OUT_OUTCOMES_DUR_PREG")) 
OUTCOMES_DUR_PREG <- OUTCOMES_DUR_PREG %>%
  collect()
summary(OUTCOMES_DUR_PREG)
data.table::fwrite(OUTCOMES_DUR_PREG,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_OUTCOMES_DUR_PREG.csv")

OUTCOMES_POST_PREG <- tbl(con, in_schema("SAILWWMCCV", "CCU018_01_OUT_OUTCOMES_POST_PREG")) 
OUTCOMES_POST_PREG <- OUTCOMES_POST_PREG %>%
  collect()
summary(OUTCOMES_POST_PREG)
data.table::fwrite(OUTCOMES_POST_PREG,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_OUTCOMES_POST_PREG.csv")

codelist <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_CODELIST.csv",
                            data.table = FALSE)
head(codelist)
codelist_pregnancy <- codelist[which(codelist$NAME=="PREGNANCY"),]
write.csv(codelist_pregnancy, "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/codelist_pregnancy.csv")

codelist_stillbirth <- codelist[which(codelist$NAME=="STILLBIRTH"),]
write.csv(codelist_stillbirth, "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/codelist_stillbirth.csv")

codelist_preterm <- codelist[which(codelist$NAME=="PRETERM"),]
write.csv(codelist_preterm, "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/codelist_preterm.csv")

codelist_multiple_gest <- codelist[which(codelist$NAME=="MULTIPLE_GESTATION"),]
write.csv(codelist_multiple_gest, "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/codelist_multiple_gest.csv")

cohort <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_COHORT.csv",
                        data.table = FALSE)
head(cohort)

skinny <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_SKINNY.csv",
                            data.table = FALSE)
head(skinny)

covariates <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_COVARIATES.csv",
                            data.table = FALSE)
head(covariates)

exposure <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_EXPOSURE.csv",
                            data.table = FALSE)
head(exposure)

outcomes_birth <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_OUTCOMES_AT_BIRTH.csv",
                              data.table = FALSE)
head(outcomes_birth)

outcomes_during_preg <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_OUTCOMES_DUR_PREG.csv",
                                    data.table = FALSE)
head(outcomes_during_preg)

outcomes_post_preg <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_01_OUT_OUTCOMES_POST_PREG.csv",
                                          data.table = FALSE)
head(outcomes_post_preg)

# prepare cohort table ---------------------------------------------------------
head(cohort)
ls(cohort)

# format dates -----------------------------------------------------------------
cohort$PREG_START_DATE <- as.Date(cohort$PREG_START_DATE, "%d/%m/%Y") 
cohort$DELIVERY_DATE <- as.Date(cohort$DELIVERY_DATE, "%d/%m/%Y")
cohort$FU_END_DATE <- as.Date(cohort$FU_END_DATE, "%d/%m/%Y")

preg_id <-table(cohort$PREG_ID)
cbind(preg_id,prop.table(preg_id))
rm("preg_id")

preg_start_est <-table(cohort$PREG_START_DATE_EST)
cbind(preg_start_est,prop.table(preg_start_est))
rm("preg_start_est")

cohort$FUP_PREG<-as.numeric(difftime(cohort$DELIVERY_DATE, cohort$PREG_START_DATE, unit="days"))
summary(cohort$FUP_PREG)

# rename estimated pregnancy start date ----------------------------------------
cohort<- cohort%>%
  rename(PREG_START_DATE_ESTIMATED= PREG_START_DATE_EST)

# prepare skinny table ---------------------------------------------------------
head(skinny)
ls(skinny)

sex <-table(skinny$SEX)
cbind(sex,prop.table(sex))
rm("sex")

skinny<-skinny %>%
  rename(COV_AGE=AGE)
    
# define agegroups -------------------------------------------------------------
agebreaks <- c(0, 30, 40, 500)
agelabels <- c("<30", "30-39", ">=40")
library(data.table)
skinny <- setDT(skinny)[ , agegroup := cut(COV_AGE, breaks = agebreaks, right = FALSE, labels = agelabels)]

agegroup <-table(skinny$agegroup)
cbind(agegroup,prop.table(agegroup))
rm("agegroup")

summary(skinny$COV_AGE)
hist(skinny$COV_AGE)

ethnicity <-table(skinny$ETHNIC_CAT)
table(is.na(skinny$ETHNIC_CAT))
cbind(ethnicity,prop.table(ethnicity))
rm("ethnicity")

ethnicityb <-table(skinny$ETHNIC)
cbind(ethnicityb,prop.table(ethnicityb))
rm("ethnicityb")

# define ethnicity with 3 categories -------------------------------------------
skinny <- skinny %>%
  mutate     (COV_ETHNICITY_3lev = case_when(skinny$ETHNIC_CAT >= 1 & skinny$ETHNIC_CAT <= 4  ~  "Other",
                                             skinny$ETHNIC_CAT == 5 |  skinny$ETHNIC_CAT == NA ~  "Unknown",
                                             skinny$ETHNIC_CAT == 0 ~  "White"
  )
  )

table(is.na(skinny$COV_ETHNICITY_3lev))
skinny$COV_ETHNICITY_3lev[is.na(skinny$COV_ETHNICITY_3lev)]="Unknown"
table(is.na(skinny$COV_ETHNICITY_3lev))

ethnicity_3lev <-table(skinny$COV_ETHNICITY_3lev)
cbind(ethnicity_3lev,prop.table(ethnicity_3lev))
rm("ethnicity_3lev")

# rename deprivation -----------------------------------------------------------
table(skinny$WIMD_2019_QUINTILES)
skinny<-skinny %>%
  rename(COV_DEPRIVATION = WIMD_2019_QUINTILES)

deprivation <-table(skinny$COV_DEPRIVATION)
table(is.na(skinny$COV_DEPRIVATION))
cbind(deprivation,prop.table(deprivation))
rm("deprivation")

# prepare covariates -----------------------------------------------------------
head(covariates)
ls(covariates)

covariates$COV_HX_DIABETES_DRUGS_FLAG[is.na(covariates$COV_HX_DIABETES_DRUGS_FLAG)] <- 0
covariates$COV_HX_DIABETES_FLAG[is.na(covariates$COV_HX_DIABETES_FLAG)] <- 0
covariates$COV_HX_HYPERTENSION_DRUGS_FLAG[is.na(covariates$COV_HX_HYPERTENSION_DRUGS_FLAG)] <- 0
covariates$COV_HX_HYPERTENSION_FLAG[is.na(covariates$COV_HX_HYPERTENSION_FLAG)] <- 0

table(covariates$COV_HX_DIABETES_DRUGS_FLAG, covariates$COV_HX_DIABETES_FLAG)
table(covariates$COV_HX_HYPERTENSION_DRUGS_FLAG, covariates$COV_HX_HYPERTENSION_FLAG)

covariates <- covariates %>%
  mutate     (COV_HX_HYPERTENSION = case_when(covariates$COV_HX_HYPERTENSION_DRUGS_FLAG == 0 & covariates$COV_HX_HYPERTENSION_FLAG==0  ~  0,
                                              covariates$COV_HX_HYPERTENSION_DRUGS_FLAG == 1 | covariates$COV_HX_HYPERTENSION_FLAG==1   ~ 1
  )
  )

covariates <- covariates %>%
  mutate     (COV_HX_DIABETES = case_when(covariates$COV_HX_DIABETES_DRUGS_FLAG == 0 & covariates$COV_HX_DIABETES_FLAG==0  ~  0,
                                          covariates$COV_HX_DIABETES_DRUGS_FLAG == 1 | covariates$COV_HX_DIABETES_FLAG==1   ~ 1
  )
  )

covariates$COV_HX_GEST_DIABETES_FLAG[is.na(covariates$COV_HX_GEST_DIABETES_FLAG)] <- 0
covariates$COV_HX_PREECLAMPSIA_FLAG[is.na(covariates$COV_HX_PREECLAMPSIA_FLAG)] <- 0
covariates$COV_HX_PREGNANCY_FLAG[is.na(covariates$COV_HX_PREGNANCY_FLAG)] <- 0

hx_pregnancy <-table(covariates$COV_HX_PREGNANCY_FLAG)
cbind(hx_pregnancy ,prop.table(hx_pregnancy))
rm("hx_pregnancy")

hx_preeclampsia <-table(covariates$COV_HX_PREECLAMPSIA_FLAG)
cbind(hx_preeclampsia ,prop.table(hx_preeclampsia))
rm("hx_preeclampsia")

table(covariates$COV_HX_PREECLAMPSIA_FLAG, covariates$COV_HX_PREGNANCY_FLAG)

covariates$COV_BMI_VALUE_OBESE[is.na(covariates$COV_BMI_VALUE_OBESE)] <- 0
hx_bmi_value_obesity <-table(covariates$COV_BMI_VALUE_OBESE)
cbind(hx_bmi_value_obesity,prop.table(hx_bmi_value_obesity))
rm("hx_bmi_value_obesity")

covariates$COV_HX_BMI_OBESITY_FLAG[is.na(covariates$COV_HX_BMI_OBESITY_FLAG)] <- 0
hx_bmi_obesity <-table(covariates$COV_HX_BMI_OBESITY_FLAG)
cbind(hx_bmi_obesity,prop.table(hx_bmi_obesity))
rm("hx_bmi_obesity") 

covariates$COV_HX_PREGNANCY_INIT_ASSESS_VAR[is.na(covariates$COV_HX_PREGNANCY_INIT_ASSESS_VAR)] <- 0
hx_pregnancy_intial <-table(covariates$COV_HX_PREGNANCY_INIT_ASSESS_VAR)
cbind(hx_pregnancy_intial,prop.table(hx_pregnancy_intial))
rm("hx_pregnancy_intial")

covariates$COV_HX_PREGNANCY_INIT_ASSESS_VAR[is.na(covariates$COV_HX_PREGNANCY_INIT_ASSESS_VAR)] <- 0
hx_pregnancy_delivery <-table(covariates$COV_HX_PREGNANCY_DELRECS)
cbind(hx_pregnancy_delivery,prop.table(hx_pregnancy_delivery))
rm("hx_pregnancy_delivery") 

table(covariates$COV_HX_PREGNANCY_INIT_ASSESS_VAR, covariates$COV_HX_PREGNANCY_DELRECS)

table(covariates$COV_HX_PREGNANCY_MAX, covariates$COV_HX_ICVT_PREGNANCY_FLAG)
table(covariates$COV_HX_PREGNANCY_MAX, covariates$COV_HX_DVT_PREGNANCY_FLAG)
table(covariates$COV_HX_PREGNANCY_MAX, covariates$COV_HX_PREECLAMPSIA_FLAG)

table(covariates$COV_HX_MULTI_GEST_DELCODE, covariates$COV_HX_MULTI_GEST_MULTI_ID)

hx_multi_gest <-table(covariates$COV_HX_MULTI_GEST_MAX)
cbind(hx_multi_gest,prop.table(hx_multi_gest))
rm("hx_multi_gest")

covariates$COV_HX_ARTERIAL_FLAG[is.na(covariates$COV_HX_ARTERIAL_FLAG)] <- 0
hx_arterial <-table(covariates$COV_HX_ARTERIAL_FLAG)
cbind(hx_arterial,prop.table(hx_arterial))
rm("hx_arterial")

covariates$COV_HX_AMI_FLAG[is.na(covariates$COV_HX_AMI_FLAG)] <- 0
hx_ami <-table(covariates$COV_HX_AMI_FLAG)
cbind(hx_ami,prop.table(hx_ami))
rm("hx_ami")

covariates$COV_HX_HAEMOTOLOGICAL_FLAG[is.na(covariates$COV_HX_HAEMOTOLOGICAL_FLAG)] <- 0
hx_haemotological <-table(covariates$COV_HX_HAEMOTOLOGICAL_FLAG)
cbind(hx_haemotological,prop.table(hx_haemotological))
rm("hx_haemotological")

covariates$COV_HX_VENOUS_FLAG[is.na(covariates$COV_HX_VENOUS_FLAG)] <- 0
hx_venous <-table(covariates$COV_HX_VENOUS_FLAG)
cbind(hx_venous,prop.table(hx_venous))
rm("hx_venous")

hx_dtv_pregnancy <-table(covariates$COV_HX_DVT_PREGNANCY_FLAG)
cbind(hx_dtv_pregnancy,prop.table(hx_dtv_pregnancy))
rm("hx_dtv_pregnancy")

covariates$COV_HX_DEPRESSION_FLAG[is.na(covariates$COV_HX_DEPRESSION_FLAG)] <- 0
hx_depression <-table(covariates$COV_HX_DEPRESSION_FLAG)
cbind(hx_depression,prop.table(hx_depression))
rm("hx_depression")

covariates$COV_HX_CANCER_FLAG[is.na(covariates$COV_HX_CANCER_FLAG)] <- 0
hx_cancer <-table(covariates$COV_HX_CANCER_FLAG)
cbind(hx_cancer,prop.table(hx_cancer))
rm("hx_cancer")

covariates$COV_HX_COPD_FLAG[is.na(covariates$COV_HX_COPD_FLAG)] <- 0
hx_copd <-table(covariates$COV_HX_COPD_FLAG)
cbind(hx_copd,prop.table(hx_copd))
rm("hx_copd")

covariates$COV_SURG_LAST_YR_FLAG[is.na(covariates$COV_SURG_LAST_YR_FLAG)] <- 0
hx_surg_last_yr <-table(covariates$COV_SURG_LAST_YR_FLAG)
cbind(hx_surg_last_yr,prop.table(hx_surg_last_yr))
rm("hx_surg_last_yr")

covariates$COV_HX_CKD_FLAG[is.na(covariates$COV_HX_CKD_FLAG)] <- 0
hx_ckd <-table(covariates$COV_HX_CKD_FLAG)
cbind(hx_ckd,prop.table(hx_ckd))
rm("hx_ckd")

covariates$COV_HX_LIVER_DISEASE_FLAG[is.na(covariates$COV_HX_LIVER_DISEASE_FLAG)] <- 0
hx_liver <-table(covariates$COV_HX_LIVER_DISEASE_FLAG)
cbind(hx_liver,prop.table(hx_liver))
rm("hx_liver")

table(covariates$COV_SMOKING_MIDS_BIRTH_FLAG, covariates$COV_SMOKING_MIDS_INIT_FLAG)
table(covariates$COV_SMOKING_MIDS_BIRTH_FLAG, covariates$COV_SMOKING_STATUS)
table(covariates$COV_SMOKING_MIDS_INIT_FLAG, covariates$COV_SMOKING_STATUS)

covariates$COV_SMOKING_STATUS[covariates$COV_SMOKING_STATUS==""] <- "Never"
smoking <-table(covariates$COV_SMOKING_STATUS)
cbind(smoking,prop.table(smoking))
rm("smoking")

smoking_init <-table(covariates$COV_SMOKING_MIDS_INIT_FLAG)
cbind(smoking_init,prop.table(smoking_init))
rm("smoking_init")

smoking_birth <-table(covariates$COV_SMOKING_MIDS_BIRTH_FLAG)
cbind(smoking_birth,prop.table(smoking_birth))
rm("smoking_birth")

table(covariates$COV_SMOKING_MIDS_BIRTH_FLAG, covariates$COV_SMOKING_MIDS_INIT_FLAG)

covariates <- covariates %>%
  mutate     (COV_HX_CVD_HEM = case_when(covariates$COV_HX_ARTERIAL_FLAG==1 | covariates$COV_HX_OTHER_CVD_FLAG==1 | covariates$COV_HX_VENOUS_FLAG==1  | covariates$COV_HX_HAEMOTOLOGICAL_FLAG==1 ~ 1,
                                            covariates$COV_HX_ARTERIAL_FLAG==0 & covariates$COV_HX_OTHER_CVD_FLAG==0 & covariates$COV_HX_VENOUS_FLAG==0 & covariates$COV_HX_HAEMOTOLOGICAL_FLAG==0 ~ 0
  )
  )

table(covariates$COV_HX_CVD_HEM)

covariates <- covariates %>%
  mutate     (COV_HX_OTHER_DIS = case_when(covariates$COV_HX_COPD_FLAG == 0 & covariates$COV_SURG_LAST_YR_FLAG==0 & covariates$COV_HX_LIVER_DISEASE_FLAG == 0 & covariates$COV_HX_CKD_FLAG == 0 & covariates$COV_HX_CANCER_FLAG == 0  ~  0,
                                           covariates$COV_HX_COPD_FLAG == 1 | covariates$COV_SURG_LAST_YR_FLAG==1 | covariates$COV_HX_LIVER_DISEASE_FLAG == 1 | covariates$COV_HX_CKD_FLAG == 1 | covariates$COV_HX_CANCER_FLAG == 1   ~ 1
  )
  )
table(covariates$COV_HX_OTHER_DIS)


covariates <- covariates %>%
  mutate     (COV_HX_DIABETES_DIS = case_when(covariates$COV_HX_GEST_DIABETES_FLAG==0 & covariates$COV_HX_DIABETES==0 ~  0,
                                              covariates$COV_HX_GEST_DIABETES_FLAG==1 | covariates$COV_HX_DIABETES==1 ~  1
  )
  )

table(covariates$COV_HX_DIABETES_DIS)

covariates <- covariates %>%
  mutate     (COV_HX_HYPERTENSIVE_DIS = case_when(covariates$COV_HX_PREECLAMPSIA_FLAG==0 & covariates$COV_HX_HYPERTENSION==0 ~  0,
                                                  covariates$COV_HX_PREECLAMPSIA_FLAG==1 | covariates$COV_HX_HYPERTENSION==1 ~  1
  )
  )

covariates$COV_HX_PREGNANCY_MAX[is.na(covariates$COV_HX_PREGNANCY_MAX)] <- 0

covariates_selection <- covariates %>% select(PERSON_ID,PREG_ID,COV_HX_CVD_HEM, COV_HX_OTHER_DIS, COV_HX_DEPRESSION_FLAG, 
                                              COV_HX_DIABETES_DIS, COV_HX_PREGNANCY_MAX, COV_HX_HYPERTENSIVE_DIS,
                                              COV_HX_BMI_OBESITY_FLAG, COV_SMOKING_STATUS)
ls(covariates_selection)

# prepare exposure -------------------------------------------------------------
head(exposure)
ls(exposure)

exposure$EXP_PRE_COVID_SOURCE[is.na(exposure$EXP_PRE_COVID_SOURCE)] <- 0
exp_pre_source <-table(exposure$EXP_PRE_COVID_SOURCE)
cbind(exp_pre_source,prop.table(exp_pre_source))
rm("exp_pre_source")

exposure$EXP_DUR_COVID_SOURCE[is.na(exposure$EXP_DUR_COVID_SOURCE)] <- 0
exp_dur_source <-table(exposure$EXP_DUR_COVID_SOURCE)
cbind(exp_dur_source,prop.table(exp_dur_source))
rm("exp_dur_source")

exposure$EXP_PRE_COVID_PHENOTYPE[is.na(exposure$EXP_PRE_COVID_PHENOTYPE)] <- 0
exp_pre_phen <-table(exposure$EXP_PRE_COVID_PHENOTYPE)
cbind(exp_pre_phen,prop.table(exp_pre_phen))
rm("exp_pre_phen")

exposure$EXP_DUR_COVID_PHENOTYPE[is.na(exposure$EXP_DUR_COVID_PHENOTYPE)] <- 0
exp_dur_phen <-table(exposure$EXP_DUR_COVID_PHENOTYPE)
cbind(exp_dur_phen,prop.table(exp_dur_phen))
rm("exp_dur_phen")

exposure$EXP_PRE_COVID_SEVERITY[is.na(exposure$EXP_PRE_COVID_SEVERITY)] <- 0
exp_pre_severity <-table(exposure$EXP_PRE_COVID_SEVERITY)
cbind(exp_pre_severity,prop.table(exp_pre_severity))
rm("exp_pre_severity")

exposure$EXP_DUR_COVID_SEVERITY[is.na(exposure$EXP_DUR_COVID_SEVERITY)] <- 0
exp_dur_severity <-table(exposure$EXP_DUR_COVID_SEVERITY)
cbind(exp_dur_severity,prop.table(exp_dur_severity))
rm("exp_dur_severity")

exposure$VACC_1ST_FLAG[is.na(exposure$VACC_1ST_FLAG)] <- 0
first_vaccination <-table(exposure$VACC_1ST_FLAG)
cbind(first_vaccination,prop.table(first_vaccination))
rm("first_vaccination")

# merge with pregnancy cohort information --------------------------------------
cohort_exp<- cohort %>% select (PREG_START_DATE, DELIVERY_DATE, PERSON_ID, PREG_ID)
exposures<- merge(cohort_exp, exposure, by=c("PERSON_ID", "PREG_ID"))
nrow(exposures)
rm("cohort_exp")

# define format dates ----------------------------------------------------------
exposures$EXP_PRE_COVID_RECORD_DATE <- as.Date(exposures$EXP_PRE_COVID_RECORD_DATE, "%d/%m/%Y") 
exposures$EXP_DUR_COVID_RECORD_DATE <- as.Date(exposures$EXP_DUR_COVID_RECORD_DATE, "%d/%m/%Y")
exposures$VACC_1ST_DATE <- as.Date(exposures$VACC_1ST_DATE, "%d/%m/%Y") 
exposures$PREG_START_DATE <- as.Date(exposures$PREG_START_DATE, "%d/%m/%Y") 
exposures$DELIVERY_DATE <- as.Date(exposures$DELIVERY_DATE, "%d/%m/%Y")

# check COVID-19 infection during pregnancy ------------------------------------
exposures$EXP_COVID_BIN <- ifelse(exposures$EXP_DUR_COVID_RECORD_DATE>=exposures$PREG_START_DATE & exposures$EXP_DUR_COVID_RECORD_DATE<=exposures$DELIVERY_DATE,1,0)
exposures$EXP_COVID_BIN[is.na(exposures$EXP_DUR_COVID_RECORD_DATE)]=0

covid_bin <-table(exposures$EXP_COVID_BIN)
cbind(covid_bin,prop.table(covid_bin))
rm("covid_bin")

# check COVID-19 infection before pregnancy ------------------------------------
exposures$EXP_COVID_PRE_BIN <- ifelse(exposures$EXP_PRE_COVID_RECORD_DATE<exposures$PREG_START_DATE,1,0)
exposures$EXP_COVID_PRE_BIN[is.na(exposures$EXP_PRE_COVID_RECORD_DATE)]=0

covid_pre_bin <-table(exposures$EXP_COVID_PRE_BIN)
cbind(covid_pre_bin,prop.table(covid_pre_bin))
rm("covid_pre_bin")

table(exposures$EXP_COVID_BIN, exposures$EXP_COVID_PRE_BIN)

# define hosp for COVID19 during pregnancy -------------------------------------
severity <-table(exposures$EXP_DUR_COVID_SEVERITY)
cbind(severity,prop.table(severity))
rm("severity")

exposures<-exposures %>% 
  mutate(EXP_DUR_HOSP = case_when(EXP_DUR_COVID_SEVERITY == "Hospitalised"  ~  1,
                                  EXP_DUR_COVID_SEVERITY == "Non-Hospitalised" ~ 0))

covid_hosp <-table(exposures$EXP_DUR_HOSP)
cbind(covid_hosp,prop.table(covid_hosp))
rm("covid_hosp")

# set vaccination exposures as binary ------------------------------------------
exposures$VACC_1ST_DUR_FLAG[is.na(exposures$VACC_1ST_DUR_FLAG)] <- 0
exposures$VACC_2ND_DUR_FLAG[is.na(exposures$VACC_2ND_DUR_FLAG)] <- 0

table(exposures$VACC_1ST_DUR_FLAG)
table(exposures$VACC_1ST_DUR_FLAG, exposures$EXP_COVID_BIN)

# define trimester of infection ------------------------------------------------
exposures$EXP_TIME_TO_COVID<-as.numeric(difftime(exposures$EXP_DUR_COVID_RECORD_DATE, exposures$PREG_START_DATE, unit="days"))
hist(exposures$EXP_TIME_TO_COVID)
exposures$EXP_TIME_TO_COVID[exposures$EXP_COVID_BIN==0]= NA
sum(!is.na(exposures$EXP_TIME_TO_COVID))

exposures <- exposures %>%
  mutate     (EXP_TRIMESTER = case_when(exposures$EXP_TIME_TO_COVID <= 84 ~  1,
                                        exposures$EXP_TIME_TO_COVID > 84 & exposures$EXP_TIME_TO_COVID <= 182   ~ 2,
                                        exposures$EXP_TIME_TO_COVID > 182 & exposures$EXP_DUR_COVID_RECORD_DATE <= exposures$DELIVERY_DATE  ~  3
                                        )
             )

trim <-table(exposures$EXP_TRIMESTER)
cbind(trim,prop.table(trim))
rm("trim")

exposures <- exposures %>% 
rename(EXP_PRE_COVID_1ST_DATE = EXP_PRE_COVID_RECORD_DATE,
       EXP_DUR_COVID_1ST_DATE = EXP_DUR_COVID_RECORD_DATE
)

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

# select exposure variables ----------------------------------------------------
exposures <- exposures %>% select (-c(EXP_PRE_COVID_SOURCE,
                                    EXP_DUR_COVID_SOURCE,
                                    EXP_DUR_COVID_PHENOTYPE,
                                    EXP_PRE_COVID_PHENOTYPE,
                                    EXP_DUR_COVID_SEVERITY,
                                    EXP_PRE_COVID_SEVERITY,
                                    EXP_TIME_TO_COVID,
                                    EXP_TIME_TO_VACCINATION))

# outcomes at birth ------------------------------------------------------------
head(outcomes_birth)
ls(outcomes_birth)

table(outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE)
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_GENCODE_HES)
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_GENCODE_GP)

outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX[is.na(outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX)] <- 0
table(outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX)

outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR)] <- 0
outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE[is.na(outcomes_birth$OUT_BIRTH_MULTI_GEST_DELCODE)] <- 0 
outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GP[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GP)] <- 0 
outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)] <- 0 

table(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR)
table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GP)
table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)

table(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR, outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GP)
table(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR, outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)

outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX[is.na(outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX)] <- 0 
table(outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX) 

still_delvar <-table(outcomes_birth$OUT_BIRTH_STILLBIRTH_DELVAR)
cbind(still_delvar,prop.table(still_delvar))
rm("still_delvar")

still_hes <-table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_HES)
cbind(still_hes,prop.table(still_hes))
rm("still_hes")

still_gp <-table(outcomes_birth$OUT_BIRTH_STILLBIRTH_GENCODE_GP)
cbind(still_gp,prop.table(still_gp))
rm("still_gp")

fup <- cohort %>% select(PERSON_ID, PREG_ID, FUP_PREG)
nrow(outcomes_birth)
outcomes_birth <- merge(outcomes_birth,fup, by=c("PERSON_ID", "PREG_ID"))
nrow(outcomes_birth)

# set minimum days from estimated pregnancy start date to define stillbirth ----
outcomes_birth <- outcomes_birth %>%
  mutate     (OUT_BIRTH_STILLBIRTH_MAX = case_when(outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX==0  ~  0,
                                                   outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX==1 & outcomes_birth$FUP_PREG < 240 ~  0,
                                                   outcomes_birth$OUT_BIRTH_STILLBIRTH_MAX==1 & outcomes_birth$FUP_PREG >= 240 ~  1
  )
  )

table(outcomes_birth$OUT_BIRTH_PRETERM, outcomes_birth$OUT_BIRTH_MULTI_GEST_MAX)

sum(is.na(outcomes_birth$OUT_BIRTH_PRETERM)) 
outcomes_birth$OUT_BIRTH_PRETERM[is.na(outcomes_birth$OUT_BIRTH_PRETERM)] <- 0
preterm <-table(outcomes_birth$OUT_BIRTH_PRETERM)
cbind(preterm,prop.table(preterm))
rm("preterm")

sum(is.na(outcomes_birth$OUT_BIRTH_VERY_PRETERM)) 
outcomes_birth$OUT_BIRTH_VERY_PRETERM[is.na(outcomes_birth$OUT_BIRTH_VERY_PRETERM)] <- 0
verypreterm <-table(outcomes_birth$OUT_BIRTH_VERY_PRETERM)
cbind(verypreterm,prop.table(verypreterm))
rm("verypreterm")

sum(is.na(outcomes_birth$OUT_BIRTH_EXT_PRETERM)) 
outcomes_birth$OUT_BIRTH_EXT_PRETERM[is.na(outcomes_birth$OUT_BIRTH_EXT_PRETERM)] <- 0
extpreterm <-table(outcomes_birth$OUT_BIRTH_EXT_PRETERM)
cbind(extpreterm,prop.table(extpreterm))
rm("extpreterm")

sum(is.na(outcomes_birth$OUT_BIRTH_ABNORMAL_APGAR)) 
outcomes_birth$OUT_BIRTH_ABNORMAL_APGAR[is.na(outcomes_birth$OUT_BIRTH_ABNORMAL_APGAR)] <- 0
abnormal_apgar <-table(outcomes_birth$OUT_BIRTH_ABNORMAL_APGAR)
cbind(abnormal_apgar,prop.table(abnormal_apgar))
rm("abnormal_apgar")

outcomes_birth$OUT_BIRTH_PRES_LAB[is.na(outcomes_birth$OUT_BIRTH_PRES_LAB)] <- 0 
pres_lab <-table(outcomes_birth$OUT_BIRTH_PRES_LAB)
cbind(pres_lab,prop.table(pres_lab))
rm("pres_lab")

outcomes_birth$OUT_BIRTH_IND_LAB[is.na(outcomes_birth$OUT_BIRTH_IND_LAB)] <- 0 
sum(is.na(outcomes_birth$OUT_BIRTH_IND_LAB)) 
ind_lab <-table(outcomes_birth$OUT_BIRTH_IND_LAB)
cbind(ind_lab,prop.table(ind_lab))
rm("ind_lab")

outcomes_birth$OUT_BIRTH_MODE_DEL[is.na(outcomes_birth$OUT_BIRTH_MODE_DEL)] <- 0 
mode_del <-table(outcomes_birth$OUT_BIRTH_MODE_DEL)
cbind(mode_del,prop.table(mode_del))
rm("mode_del")

outcomes_birth$OUT_BIRTH_SMALL_GEST_AGE[is.na(outcomes_birth$OUT_BIRTH_SMALL_GEST_AGE)] <- 0
small_gest_age <-table(outcomes_birth$OUT_BIRTH_SMALL_GEST_AGE)
cbind(small_gest_age,prop.table(small_gest_age))
rm("small_gest_age")

# outcomes during pregnancy ----------------------------------------------------
head(outcomes_during_preg)
ls(outcomes_during_preg)

outcomes_during_preg$OUT_DUR_GEST_HYPERTENSION_FLAG[is.na(outcomes_during_preg$OUT_DUR_GEST_HYPERTENSION_FLAG)] <- 0 
gest_hyper <-table(outcomes_during_preg$OUT_DUR_GEST_HYPERTENSION_FLAG)
cbind(gest_hyper,prop.table(gest_hyper))
rm("gest_hyper")

outcomes_during_preg$OUT_DUR_PREECLAMPSIA_FLAG[is.na(outcomes_during_preg$OUT_DUR_PREECLAMPSIA_FLAG)] <- 0 
preeclampsia <-table(outcomes_during_preg$OUT_DUR_PREECLAMPSIA_FLAG)
cbind(preeclampsia,prop.table(preeclampsia))
rm("preeclampsia")

table (outcomes_during_preg$OUT_DUR_GEST_HYPERTENSION_FLAG, outcomes_during_preg$OUT_DUR_PREECLAMPSIA_FLAG)

outcomes_during_preg$OUT_DUR_GEST_DIABETES_FLAG[is.na(outcomes_during_preg$OUT_DUR_GEST_DIABETES_FLAG)] <- 0 
gest_diab <-table(outcomes_during_preg$OUT_DUR_GEST_DIABETES_FLAG)
cbind(gest_diab,prop.table(gest_diab))
rm("gest_diab")

outcomes_during_preg$OUT_DUR_ARTERIAL_FLAG[is.na(outcomes_during_preg$OUT_DUR_ARTERIAL_FLAG)] <- 0 
out_dur_arterial <-table(outcomes_during_preg$OUT_DUR_ARTERIAL_FLAG)
cbind(out_dur_arterial,prop.table(out_dur_arterial))
rm("out_dur_arterial")

outcomes_during_preg$OUT_DUR_VENOUS_FLAG[is.na(outcomes_during_preg$OUT_DUR_VENOUS_FLAG)] <- 0 
out_dur_venous <-table(outcomes_during_preg$OUT_DUR_VENOUS_FLAG)
cbind(out_dur_venous,prop.table(out_dur_venous))
rm("out_dur_venous")

outcomes_during_preg$OUT_DUR_HAEMOTOLOGICAL_FLAG[is.na(outcomes_during_preg$OUT_DUR_HAEMOTOLOGICAL_FLAG)] <- 0 
out_dur_hem <-table(outcomes_during_preg$OUT_DUR_HAEMOTOLOGICAL_FLAG)
cbind(out_dur_hem,prop.table(out_dur_hem))
rm("out_dur_hem")

# outcomes post pregnancy
outcomes_post_preg$OUT_POST_ARTERIAL_FLAG[is.na(outcomes_post_preg$OUT_POST_ARTERIAL_FLAG)] <- 0 
out_post_arterial <-table(outcomes_post_preg$OUT_POST_ARTERIAL_FLAG)
cbind(out_post_arterial,prop.table(out_post_arterial))
rm("out_post_arterial")

table(outcomes_during_preg$OUT_DUR_ARTERIAL_FLAG, outcomes_post_preg$OUT_POST_ARTERIAL_FLAG)

outcomes_post_preg$OUT_POST_VENOUS_FLAG[is.na(outcomes_post_preg$OUT_POST_VENOUS_FLAG)] <- 0 
out_post_venous <-table(outcomes_post_preg$OUT_POST_VENOUS_FLAG)
cbind(out_post_venous,prop.table(out_post_venous))
rm("out_post_venous")

table(outcomes_during_preg$OUT_DUR_VENOUS_FLAG, outcomes_post_preg$OUT_POST_VENOUS_FLAG)

outcomes_post_preg$OUT_POST_HAEMOTOLOGICAL_FLAG[is.na(outcomes_post_preg$OUT_POST_HAEMOTOLOGICAL_FLAG)] <- 0 
out_post_hem <-table(outcomes_post_preg$OUT_POST_HAEMOTOLOGICAL_FLAG)
cbind(out_post_hem,prop.table(out_post_hem))
rm("out_post_hem")

table(outcomes_during_preg$OUT_DUR_HAEMOTOLOGICAL_FLAG, outcomes_post_preg$OUT_POST_HAEMOTOLOGICAL_FLAG)

#rename outcomes during pregnancy for consistency with SDE ---------------------
outcomes_during_preg<-outcomes_during_preg %>%
  rename(OUT_DUR_HAEMATOLOGICAL_FLAG= OUT_DUR_HAEMOTOLOGICAL_FLAG,
         OUT_DUR_HAEMATOLOGICAL_RECORD_DATE= OUT_DUR_HAEMOTOLOGICAL_RECORD_DATE)

#select outcomes during pregnancy ----------------------------------------------
out_dur_preg <- outcomes_during_preg %>% select(PERSON_ID, PREG_ID,
                                                OUT_DUR_HAEMATOLOGICAL_FLAG, OUT_DUR_HAEMATOLOGICAL_RECORD_DATE,
                                                OUT_DUR_VENOUS_FLAG,  OUT_DUR_VENOUS_RECORD_DATE,
                                                OUT_DUR_ARTERIAL_FLAG, OUT_DUR_ARTERIAL_RECORD_DATE,
                                                OUT_DUR_GEST_DIABETES_FLAG, OUT_DUR_GEST_DIABETES_RECORD_DATE,
                                                OUT_DUR_PREECLAMPSIA_FLAG,  OUT_DUR_PREECLAMPSIA_RECORD_DATE,
                                                OUT_DUR_GEST_HYPERTENSION_FLAG,  OUT_DUR_GEST_HYPERTENSION_RECORD_DATE) 

#rename outcomes during pregnancy ----------------------------------------------
out_dur_preg<-out_dur_preg %>%
  rename(OUT_BIN_DUR_HAEMOTOLOGICAL= OUT_DUR_HAEMATOLOGICAL_FLAG,
         OUT_BIN_DUR_VENOUS = OUT_DUR_VENOUS_FLAG,
         OUT_BIN_DUR_ARTERIAL = OUT_DUR_ARTERIAL_FLAG,
         OUT_BIN_GEST_DIABETES = OUT_DUR_GEST_DIABETES_FLAG,
         OUT_BIN_PREECLAMPSIA  = OUT_DUR_PREECLAMPSIA_FLAG,
         OUT_BIN_GEST_HYPERTENSION  = OUT_DUR_GEST_HYPERTENSION_FLAG,
         OUT_DUR_HAEMATOLOGICAL_DATE = OUT_DUR_HAEMATOLOGICAL_RECORD_DATE,
         OUT_DUR_VENOUS_DATE = OUT_DUR_VENOUS_RECORD_DATE,
         OUT_DUR_ARTERIAL_DATE = OUT_DUR_ARTERIAL_RECORD_DATE,
         OUT_DUR_GEST_DIABETES_DATE= OUT_DUR_GEST_DIABETES_RECORD_DATE,
         OUT_DUR_PREECLAMPSIA_DATE = OUT_DUR_PREECLAMPSIA_RECORD_DATE,
         OUT_DUR_GEST_HYPERTENSION_DATE= OUT_DUR_GEST_HYPERTENSION_RECORD_DATE)

head(out_dur_preg)

# select variables from out_post_pregnancy -------------------------------------
ls(outcomes_post_preg)
out_post_preg <- outcomes_post_preg %>% select(PERSON_ID, PREG_ID,
                                               OUT_POST_HAEMOTOLOGICAL_FLAG, 
                                               OUT_POST_HAEMOTOLOGICAL_RECORD_DATE,
                                               OUT_POST_VENOUS_FLAG,  
                                               OUT_POST_VENOUS_RECORD_DATE,
                                               OUT_POST_ARTERIAL_FLAG, 
                                               OUT_POST_ARTERIAL_RECORD_DATE)

out_post_preg<-out_post_preg %>%
  rename(OUT_BIN_POST_HAEMATOLOGICAL= OUT_POST_HAEMOTOLOGICAL_FLAG,
         OUT_POST_HAEMATOLOGICAL_DATE= OUT_POST_HAEMOTOLOGICAL_RECORD_DATE,
         OUT_BIN_POST_VENOUS = OUT_POST_VENOUS_FLAG,
         OUT_POST_VENOUS_DATE =OUT_POST_VENOUS_RECORD_DATE,
         OUT_BIN_POST_ARTERIAL = OUT_POST_ARTERIAL_FLAG,
         OUT_POST_ARTERIAL_DATE =OUT_POST_ARTERIAL_RECORD_DATE)

head(out_post_preg)

# select variables from out_birth ----------------------------------------------
ls(outcomes_birth)
out_birth <- outcomes_birth %>% select(PERSON_ID, PREG_ID, OUT_BIRTH_PRETERM,OUT_BIRTH_STILLBIRTH_MAX, OUT_BIRTH_MULTI_GEST_MAX, 
                                       OUT_BIRTH_VERY_PRETERM, OUT_BIRTH_EXT_PRETERM,
                                       OUT_BIRTH_ABNORMAL_APGAR, OUT_BIRTH_SMALL_GEST_AGE)

# rename out_birth names -------------------------------------------------------
out_birth<-out_birth %>%
  mutate(across(.names = 'OUT_BIN_{sub("OUT_", "", .col)}'))
out_birth <- out_birth %>% select(-c(starts_with("OUT_BIRTH")))
ls(out_birth)                               
head(out_birth)

# combine dataframes -----------------------------------------------------------
data_temp1 <- merge(cohort,skinny, by=c("PERSON_ID", "PREG_ID"), all=TRUE)
exposures <- subset(exposures, select = -c(PREG_START_DATE, DELIVERY_DATE))
data_temp2 <- merge(data_temp1,exposures, by=c("PERSON_ID", "PREG_ID"), all=TRUE)
outcomes <- merge(out_birth, out_post_preg, by=c("PERSON_ID", "PREG_ID"), all=TRUE)
outcomes <- merge(outcomes, out_dur_preg, by=c("PERSON_ID", "PREG_ID"), all=TRUE)
data_temp3 <- merge(data_temp2,outcomes, by=c("PERSON_ID", "PREG_ID"), all=TRUE)
data_temp4 <- merge(data_temp3,covariates_selection, by=c("PERSON_ID", "PREG_ID"), all=TRUE)
rm("data_temp1","data_temp2","data_temp3")

# set inclusion criteria for infection analysis --------------------------------
data_temp4 <- data_temp4[which(data_temp4$PREG_ID==1),]
data_temp4 <- data_temp4[which(data_temp4$FUP_PREG>=84),]
data_temp4 <- data_temp4[which(data_temp4$OUT_BIN_BIRTH_MULTI_GEST_MAX==0),]

data_temp4 <- data_temp4[which(data_temp4$COV_AGE>=18 & data_temp4$COV_AGE<=49),]
nrow(data_temp4)

data.table::fwrite(data_temp4,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis.csv")


# set inclusion criteria for vaccination analysis ------------------------------
data_temp5 <- data_temp4
ls(data_temp5)
nrow(data_temp5)
data_temp5 <- data_temp5[which(data_temp5$PREG_START_DATE >= '2020-12-08'),]
nrow(data_temp5)

data_temp5 <- data_temp5 %>%
  mutate (VACC_PRE_PREG = case_when(data_temp5$VACC_1ST_FLAG == 0   ~  0,
                                    data_temp5$VACC_1ST_FLAG == 1 & (data_temp5$VACC_1ST_DATE == data_temp5$VACC_1ST_DUR_DATE) ~  0,
                                    data_temp5$VACC_1ST_FLAG == 1 & (data_temp5$VACC_1ST_DATE > data_temp5$DELIVERY_DATE) ~  0,
                                    data_temp5$VACC_1ST_FLAG == 1 & (data_temp5$VACC_1ST_DATE < data_temp5$VACC_1ST_DUR_DATE) ~  1,                                    
                                    )
                                   )

data_temp5 <- data_temp5[which(data_temp5$VACC_PRE_PREG ==0),]
nrow(data_temp5)

data_temp5$PREG_START_DATE <- as.Date(data_temp5$PREG_START_DATE, "%d/%m/%Y")
data_temp5$DELIVERY_DATE <- as.Date(data_temp5$DELIVERY_DATE, "%d/%m/%Y")
data_temp5$VACC_2ND_DUR_DATE <- as.Date(data_temp5$VACC_2ND_DUR_DATE, "%d/%m/%Y")

data_temp6 <- data_temp5 %>%
  mutate(END_FOLLOW_1ST_VACC = case_when(is.na(data_temp5$VACC_2ND_DUR_DATE) ~ data_temp5$DELIVERY_DATE,
                                          data_temp5$DELIVERY_DATE > data_temp5$VACC_2ND_DUR_DATE ~ data_temp5$VACC_2ND_DUR_DATE
                                        )
                                        )
data.table::fwrite(data_temp6,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis_vaccination.csv")

################################################################################
## TITLE: Data preparation of England dataset for CCU018_01
##
## Description: Compare results with propensity score adjustment and covariates
##
## By : Elena Raffetti
##
################################################################################

################################################################################
#############    CHECK PROPENSITY SCORE  - gestational diabetes    ############# 
################################################################################

# Install.packages -------------------------------------------------------------
#install.packages("stringr")
#install.packages("ggplot2")
# install.packages("incidence")
#install.packages("tidyverse")
#install.packages("epiR")
#install.packages("scales")
#install.packages("dplyr")
#install.packages("survival")
#install.packages("survminer")
#install.packages("gtsummary")
#install.packages("reshape2")
#install.packages("date")
#install.packages("lubridate")
#install.packages("splines")
#install.packages("purrr")
#install.packages("ggpubr")

# load required packages -------------------------------------------------------
library(stringr)
library(ggplot2)
library(incidence)
library(tidyverse)
library(epiR)
library(scales)
library(dplyr)
library(survival)
library(survminer)
library(gtsummary)
library(reshape2)
library(date)
library(lubridate)
library(splines)
library(purrr)
library(ggpubr)

# inclusion criteria -----------------------------------------------------------
data <- data_temp4
data<-data %>%  rename(ID=PERSON_ID)

# prepare propensity scores ----------------------------------------------------
data_ps <- data

data_ps$exposure  <- data_ps$EXP_COVID_BIN 

vars <- names(data_ps) %in% c("ID", "exposure", "COV_HX_OTHER_DIS", "COV_HX_CVD_HEM", "COV_HX_DEPRESSION_FLAG", "COV_ETHNICITY_3lev", 
                              "COV_HX_BMI_OBESITY_FLAG", "COV_HX_HYPERTENSIVE_DIS", "COV_HX_PCOS_FLAG", "COV_HX_DIABETES_DIS",
                              "COV_HX_PREGNANCY_MAX", "COV_SMOKING_STATUS", "PREG_START_DATE_ESTIMATED")

data_ps <- data_ps[vars]
data_ps <- data_ps[complete.cases(data_ps),]

# include only women with known estimated pregnancy start date for outcomes at birth
data_ps_birth <- data_ps[which(data_ps$PREG_START_DATE_ESTIMATED==0),]

prop_model_full <- glm(exposure ~  COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                       + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps, family=binomial(link="logit"))

summary(prop_model_full)
exp(cbind("Odds ratio" = coef(prop_model_full), confint.default(prop_model_full, level=0.95)))

data_ps$pr_score_full <- predict(prop_model_full,type="response")

ggplot(data_ps ,aes (x = pr_score_full)) +
  geom_histogram(color = "white") +
  facet_wrap(~ exposure) +
  xlab("Probability of being infected with COVID") +
  theme_bw()

prop_model_birth <- glm(exposure ~   COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                       + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps_birth, family=binomial(link="logit"))

data_ps_birth$pr_score_birth <- predict(prop_model_birth,type="response")

ggplot(data_ps_birth,aes (x = pr_score_birth)) +
  geom_histogram(color = "white") +
  facet_wrap(~ exposure) +
  xlab("Probability of being infected with COVID") +
  theme_bw()

# select information from propensity score analyses ----------------------------
data_ps <- data_ps %>% select (ID, pr_score_full)
data_ps_birth <- data_ps_birth %>% select (ID,pr_score_birth) 

# combine propensity score with main data --------------------------------------
data %>% count(EXP_COVID_BIN)
nrow(data)
data <- merge(data,data_ps, by=c("ID"), all = TRUE)
data <- merge(data,data_ps_birth, by=c("ID"), all = TRUE)
data %>% count(EXP_COVID_BIN)
nrow(data)

# define date ------------------------------------------------------------------
data$EXP_DUR_COVID_RECORD_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y") 
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")

data<-data %>%
  rename(
         OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES
        )


data$OUT_DUR_GEST_DIABETES_RECORD_DATE <- as.Date(data$OUT_DUR_GEST_DIABETES_DATE, "%d/%m/%Y") 
data$OUT_DUR_GEST_DIABETES_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_DIABETES_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_GEST_DIABETES_RECORD_DATE)

# define calendar period variable-----------------------------------------------
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
date_start <- ymd("2019/08/01")
data$calendar_month <- ((year(data$PREG_START_DATE) - year(date_start)) * 12) + month(data$PREG_START_DATE) - month(date_start)

# prepare for data transform to long format ------------------------------------
data$fup<-as.numeric(difftime(data$DELIVERY_DATE, data$PREG_START_DATE, unit="days"))
ls(data)

# set event---------------------------------------------------------------------
data$overall_event<-data$OUT_DUR_GEST_DIABETES

# set event date ---------------------------------------------------------------
data$overall_pregn_event_date<-data$OUT_DUR_GEST_DIABETES_RECORD_DATE
nrow(data[!is.na(data$overall_pregn_event_date),])


data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, "%d/%m/%Y")
data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$PREG_START_DATE, unit="days"))
data$age_sq <- data$COV_AGE^2

# choose exposure --------------------------------------------------------------
data$covid_inf <- data$EXP_COVID_BIN
data$covid_inf_date <- data$EXP_DUR_COVID_RECORD_DATE

# split rows in those who were infected and those who were not infected --------
infected_cases <- data[which(data$covid_inf==1),]
infected_cases$delta_cov_inf<-as.numeric(difftime(infected_cases$covid_inf_date, infected_cases$PREG_START_DATE, unit="days"))
infected_cases$delta_cov_inf

# variable list for long format transformation ---------------------------------
vars<-c("ID", "region","calendar_month", "PREG_START_DATE", "DELIVERY_DATE", "pr_score_full", "COV_AGE", "age_sq", "COV_DEPRIVATION",  
        "COV_HX_OTHER_DIS", "COV_HX_CVD_HEM", "COV_HX_DEPRESSION_FLAG", "COV_ETHNICITY_3lev", 
        "COV_HX_BMI_OBESITY_FLAG", "COV_HX_HYPERTENSIVE_DIS", "COV_HX_PCOS_FLAG", "COV_HX_DIABETES_DIS",
        "COV_HX_PREGNANCY_MAX", "COV_SMOKING_STATUS")


# data transform to long format for becoming infected --------------------------
td_data <-
  tmerge(
    data1 = infected_cases %>% select(all_of(vars), covid_inf_date, overall_event_time), 
    data2 = infected_cases %>% select(all_of(vars), overall_event, fup, delta_cov_inf),
    id = ID, 
    overall_event = event(fup,  overall_event), 
    covid_inf = tdc(delta_cov_inf)
  )

without_expo <- data[which(data$covid_inf==0),]
without_expo$tstart<- c(0)
without_expo$tstop <- ifelse(without_expo$fup ==0,  without_expo$fup + 0.001, without_expo$fup) # right now this isn't doing anything because I excluded those with 0 fup.
without_expo$covid_inf<- c(0)
without_expo$last_step <- c(1)
without_expo$hospitalised <- c(1)
without_expo_noncases <- without_expo[which(without_expo$overall_event==0),]
noncase_ids <- unique(without_expo_noncases$ID)

# combine uninfected and infected exposure dataframes --------------------------
with_expo_cols <- colnames(td_data)
with_expo_cols
without_expo <- without_expo %>% dplyr::select(all_of(with_expo_cols))
data_surv <-rbind(td_data, without_expo)
rm(list=c("td_data", "without_expo"))

# final dataframe --------------------------------------------------------------
survival_data <- data_surv

# run models--------------------------------------------------------------------
model_ps_adj <- coxph(Surv(time = tstart, time2 = tstop, event = overall_event) ~ covid_inf + COV_AGE + age_sq + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month) + region, survival_data, id = ID)
summary(model_ps_adj)

model_full <- coxph(Surv(time = tstart, time2 = tstop, event = overall_event) ~ covid_inf + COV_AGE + age_sq +
                           COV_DEPRIVATION + COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + 
                           COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG + COV_ETHNICITY_3lev + COV_SMOKING_STATUS + bs(calendar_month) + region, survival_data, id = ID)
summary(model_full)


################################################################################
#############   CHECK PROPENSITY SCORE  - preterm                  ############# 
################################################################################

# Install.packages -------------------------------------------------------------
#install.packages("stringr")
#install.packages("ggplot2")
# install.packages("incidence")
#install.packages("tidyverse")
#install.packages("epiR")
#install.packages("scales")
#install.packages("dplyr")
#install.packages("survival")
#install.packages("survminer")
#install.packages("gtsummary")
#install.packages("reshape2")
#install.packages("date")
#install.packages("lubridate")
#install.packages("splines")
#install.packages("purrr")
#install.packages("ggpubr")

# load required packages -------------------------------------------------------
library(stringr)
library(ggplot2)
library(incidence)
library(tidyverse)
library(epiR)
library(scales)
library(dplyr)
library(survival)
library(survminer)
library(gtsummary)
library(reshape2)
library(date)
library(lubridate)
library(splines)
library(purrr)
library(ggpubr)

# inclusion criteria -----------------------------------------------------------
data <- data_temp4
data<-data %>%  rename(ID=PERSON_ID)

# prepare propensity scores ----------------------------------------------------
data_ps <- data
ls(data_ps)

data_ps$exposure  <- data_ps$EXP_COVID_BIN 

vars <- names(data_ps) %in% c("ID", "exposure", "COV_HX_OTHER_DIS", "COV_HX_CVD_HEM", "COV_HX_DEPRESSION_FLAG", "COV_ETHNICITY_3lev", 
                              "COV_HX_BMI_OBESITY_FLAG", "COV_HX_HYPERTENSIVE_DIS", "COV_HX_PCOS_FLAG", "COV_HX_DIABETES_DIS",
                              "COV_HX_PREGNANCY_MAX", "COV_SMOKING_STATUS", "PREG_START_DATE_ESTIMATED")

data_ps <- data_ps[vars]
data_ps <- data_ps[complete.cases(data_ps),]

# include only women with known estimated pregnancy start date for outcomes at birth
data_ps_birth <- data_ps[which(data_ps$PREG_START_DATE_ESTIMATED==0),]

prop_model_full <- glm(exposure ~  COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                       + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps, family=binomial(link="logit"))

summary(prop_model_full)
exp(cbind("Odds ratio" = coef(prop_model_full), confint.default(prop_model_full, level=0.95)))

data_ps$pr_score_full <- predict(prop_model_full,type="response")

ggplot(data_ps ,aes (x = pr_score_full)) +
  geom_histogram(color = "white") +
  facet_wrap(~ exposure) +
  xlab("Probability of being infected with COVID") +
  theme_bw()

prop_model_birth <- glm(exposure ~   COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                        + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps_birth, family=binomial(link="logit"))

data_ps_birth$pr_score_birth <- predict(prop_model_birth,type="response")

ggplot(data_ps_birth,aes (x = pr_score_birth)) +
  geom_histogram(color = "white") +
  facet_wrap(~ exposure) +
  xlab("Probability of being infected with COVID") +
  theme_bw()

# select information from propensity score analyses ----------------------------
data_ps <- data_ps %>% select (ID, pr_score_full)

data_ps_birth <- data_ps_birth %>% select (ID,pr_score_birth) 

# combine propensity score with main data --------------------------------------
data %>% count(EXP_COVID_BIN)
nrow(data)
data <- merge(data,data_ps, by=c("ID"), all = TRUE)
data <- merge(data,data_ps_birth, by=c("ID"), all = TRUE)
data %>% count(EXP_COVID_BIN)
nrow(data)

# define date ------------------------------------------------------------------
data$EXP_DUR_COVID_RECORD_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y") 
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")

data<-data %>%
  rename(
    OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES
  )

data$OUT_BIRTH_PRETERM_RECORD_DATE<-  as.Date(data$DELIVERY_DATE, "%d/%m/%Y")  
data$OUT_BIRTH_PRETERM_RECORD_DATE[data$OUT_BIN_BIRTH_PRETERM==0] <- NA
data$OUT_BIRTH_PRETERM_RECORD_DATE <- ifelse(data$OUT_BIRTH_PRETERM_RECORD_DATE=='1970/01/01', NA, data$OUT_BIRTH_PRETERM_RECORD_DATE)

# define calendar period variable-----------------------------------------------
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
date_start <- ymd("2019/08/01")
data$calendar_month <- ((year(data$PREG_START_DATE) - year(date_start)) * 12) + month(data$PREG_START_DATE) - month(date_start)

# prepare for data transform to long format ------------------------------------

data$fup<-as.numeric(difftime(data$DELIVERY_DATE, data$PREG_START_DATE, unit="days"))

data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
data$pr_score_full <- data$pr_score_birth

# set event---------------------------------------------------------------------
data$overall_event<-data$OUT_BIN_BIRTH_PRETERM

# set event date ---------------------------------------------------------------
data$overall_pregn_event_date<-data$OUT_BIRTH_PRETERM_RECORD_DATE
nrow(data[!is.na(data$overall_pregn_event_date),])

data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, "%d/%m/%Y")
data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$PREG_START_DATE, unit="days"))
data$age_sq <- data$COV_AGE^2

# choose exposure --------------------------------------------------------------
data$covid_inf <- data$EXP_COVID_BIN
data$covid_inf_date <- data$EXP_DUR_COVID_RECORD_DATE

# split rows in those who were infected and those who were not infected --------
infected_cases <- data[which(data$covid_inf==1),]
infected_cases$delta_cov_inf<-as.numeric(difftime(infected_cases$covid_inf_date, infected_cases$PREG_START_DATE, unit="days"))
infected_cases$delta_cov_inf

# variable list for long format transformation ---------------------------------
vars<-c("ID", "region","calendar_month", "PREG_START_DATE", "DELIVERY_DATE", "pr_score_full", "COV_AGE", "age_sq", "COV_DEPRIVATION",  
        "COV_HX_OTHER_DIS", "COV_HX_CVD_HEM", "COV_HX_DEPRESSION_FLAG", "COV_ETHNICITY_3lev", 
        "COV_HX_BMI_OBESITY_FLAG", "COV_HX_HYPERTENSIVE_DIS", "COV_HX_PCOS_FLAG", "COV_HX_DIABETES_DIS",
        "COV_HX_PREGNANCY_MAX", "COV_SMOKING_STATUS")


# data transform to long format for becoming infected --------------------------
td_data <-
  tmerge(
    data1 = infected_cases %>% select(all_of(vars), covid_inf_date, overall_event_time), 
    data2 = infected_cases %>% select(all_of(vars), overall_event, fup, delta_cov_inf),
    id = ID, 
    overall_event = event(fup,  overall_event), 
    covid_inf = tdc(delta_cov_inf)
  )

without_expo <- data[which(data$covid_inf==0),]
without_expo$tstart<- c(0)
without_expo$tstop <- ifelse(without_expo$fup ==0,  without_expo$fup + 0.001, without_expo$fup) # right now this isn't doing anything because I excluded those with 0 fup.
without_expo$covid_inf<- c(0)
without_expo$last_step <- c(1)
without_expo$hospitalised <- c(1)
without_expo_noncases <- without_expo[which(without_expo$overall_event==0),]
noncase_ids <- unique(without_expo_noncases$ID)

# combine uninfected and infected exposure dataframes --------------------------
with_expo_cols <- colnames(td_data)
with_expo_cols
without_expo <- without_expo %>% dplyr::select(all_of(with_expo_cols))
data_surv <-rbind(td_data, without_expo)
rm(list=c("td_data", "without_expo"))

# final dataframe --------------------------------------------------------------
survival_data <- data_surv

# run models--------------------------------------------------------------------
model_ps_adj <- coxph(Surv(time = tstart, time2 = tstop, event = overall_event) ~ covid_inf + COV_AGE + age_sq + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month) + region, survival_data, id = ID)
summary(model_ps_adj)

model_full <- coxph(Surv(time = tstart, time2 = tstop, event = overall_event) ~ covid_inf + COV_AGE + age_sq +
                      COV_DEPRIVATION + COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + 
                      COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG + COV_ETHNICITY_3lev + COV_SMOKING_STATUS + bs(calendar_month) + region, survival_data, id = ID)
summary(model_full)


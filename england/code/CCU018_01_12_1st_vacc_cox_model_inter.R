################################################################################
## TITLE: Data preparation of England dataset for CCU018_01
##
## Description: Cox models to examine the associations between vaccination
## against COVID-19 (1st dose) and adverse pregnancy outcomes - interaction 
## analysis
##
## By : Elena Raffetti
##
################################################################################
writeLines(c("event model adj exp estimate conf.low conf.high stad.error p.value n.event subgroup"), paste0('C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv'))

# install.packages -------------------------------------------------------------
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("incidence")
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

dev.off()

# inclusion criteria -----------------------------------------------------------
  data <- data_temp6
  data<-data %>%  rename(ID=PERSON_ID)
  table(data$PREG_START_DATE_ESTIMATED)
  nrow(data)
  
  # format dates -----------------------------------------------------------------
  data$COHORT_END_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$OUT_DUR_PREECLAMPSIA_DATE <- as.Date(data$OUT_DUR_PREECLAMPSIA_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_DIABETES_DATE <- as.Date(data$OUT_DUR_GEST_DIABETES_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_HYPERTENSION_DATE <- as.Date(data$OUT_DUR_GEST_HYPERTENSION_DATE, "%d/%m/%Y")
  data$OUT_DUR_VENOUS_DATE <- as.Date(data$OUT_DUR_VENOUS_DATE, "%d/%m/%Y")
  
  # prepare propensity scores ----------------------------------------------------
  data_ps <- data

  data_ps$exposure  <- data_ps$VACC_1ST_DUR_FLAG
  
  vars <- names(data_ps) %in% c("ID", "exposure", "COV_HX_OTHER_DIS", "COV_HX_CVD_HEM", "COV_HX_DEPRESSION_FLAG", "COV_ETHNICITY_3lev", 
                                "COV_HX_BMI_OBESITY_FLAG", "COV_HX_HYPERTENSIVE_DIS", "COV_HX_PCOS_FLAG", "COV_HX_DIABETES_DIS",
                                "COV_HX_PREGNANCY_MAX", "COV_SMOKING_STATUS", "PREG_START_DATE_ESTIMATED")
  
  data_ps <- data_ps[vars]
  data_ps <- data_ps[complete.cases(data_ps),]
  
  data_ps_birth <- data_ps[which(data_ps$PREG_START_DATE_ESTIMATED==0),]

  prop_model_full <- glm(exposure ~  COV_HX_PREGNANCY_MAX  + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                         + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps, family=binomial(link="logit"))
  
  
  summary(prop_model_full)
  exp(cbind("Odds ratio" = coef(prop_model_full), confint.default(prop_model_full, level=0.95)))
  
  data_ps$pr_score_full <- predict(prop_model_full,type="response")
  
  ggplot(data_ps ,aes (x = pr_score_full)) +
    geom_histogram(color = "white") +
    facet_wrap(~ exposure) +
    xlab("Probability of being infected with COVID") +
    theme_bw()
  
  prop_model_birth <- glm(exposure ~  COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                          + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps_birth, family=binomial(link="logit"))
  
  data_ps_birth$pr_score_birth <- predict(prop_model_birth,type="response")
  
  # prepare propensity scores for analysis stratified for hx of pregnancy-------
  prop_model_no_hx_preg <- glm(exposure ~  OUT_BIN_BIRTH_MULTI_GEST_MAX + COV_HX_HYPERTENSIVE_DIS  + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                               + COV_ETHNICITY_3lev + COV_SMOKING_STATUS , data= data_ps, family=binomial(link="logit"))
  
  
  summary(prop_model_no_hx_preg)
  exp(cbind("Odds ratio" = coef(prop_model_no_hx_preg), confint.default(prop_model_no_hx_preg, level=0.95)))
  
  data_ps$pr_score_no_hx_preg <- predict(prop_model_no_hx_preg,type="response")
  
  ggplot(data_ps ,aes(x = pr_score_no_hx_preg)) +
    geom_histogram(color = "white") +
    facet_wrap(~ exposure) +
    xlab("Probability of being infected with COVID") +
    theme_bw()
  
  prop_model_no_hx_preg_birth <- glm(exposure ~   COV_HX_HYPERTENSIVE_DIS  + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                                     + COV_ETHNICITY_3lev + COV_SMOKING_STATUS , data= data_ps_birth, family=binomial(link="logit"))
  
  data_ps_birth$pr_score_no_hx_preg_birth <- predict(prop_model_no_hx_preg_birth,type="response")
  
  # prepare propensity scores for analysis stratified for ethnicity ------------
  prop_model_no_ethn <-  glm(exposure ~   OUT_BIN_BIRTH_MULTI_GEST_MAX +  COV_HX_PREGNANCY_MAX  + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                             + COV_SMOKING_STATUS , data= data_ps, family=binomial(link="logit"))
  
  summary(prop_model_no_ethn)
  exp(cbind("Odds ratio" = coef(prop_model_no_ethn), confint.default(prop_model_no_ethn, level=0.95)))
  
  data_ps$pr_score_no_ethn <- predict(prop_model_no_ethn,type="response")
  
  ggplot(data_ps ,aes(x = pr_score_no_ethn)) +
    geom_histogram(color = "white") +
    facet_wrap(~ exposure) +
    xlab("Probability of being infected with COVID") +
    theme_bw()
  
  prop_model_no_ethn_birth <- glm(exposure ~  COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                                  + COV_SMOKING_STATUS, data= data_ps_birth, family=binomial(link="logit"))
  
  data_ps_birth$pr_score_no_ethn_birth <- predict(prop_model_no_ethn_birth,type="response")
  
  # prepare propensity scores for analysis stratified for health conditions ----
  prop_model_no_health_cond <-  glm(exposure ~   OUT_BIN_BIRTH_MULTI_GEST_MAX +  COV_HX_PREGNANCY_MAX + COV_HX_BMI_OBESITY_FLAG  + COV_HX_DEPRESSION_FLAG
                                    + COV_ETHNICITY_3lev + COV_SMOKING_STATUS , data= data_ps, family=binomial(link="logit"))
  
  summary(prop_model_no_health_cond)
  exp(cbind("Odds ratio" = coef(prop_model_no_health_cond), confint.default(prop_model_no_health_cond, level=0.95)))
  
  data_ps$pr_score_no_health_cond <- predict(prop_model_no_health_cond,type="response")
  
  ggplot(data_ps ,aes(x = pr_score_no_health_cond)) +
    geom_histogram(color = "white") +
    facet_wrap(~ exposure) +
    xlab("Probability of being infected with COVID") +
    theme_bw()
  
  prop_model_no_health_cond_birth <-  glm(exposure ~   OUT_BIN_BIRTH_MULTI_GEST_MAX +  COV_HX_PREGNANCY_MAX + COV_HX_BMI_OBESITY_FLAG  + COV_HX_DEPRESSION_FLAG
                                          + COV_ETHNICITY_3lev + COV_SMOKING_STATUS , data= data_ps_birth, family=binomial(link="logit"))
  
  
  data_ps_birth$pr_score_no_health_cond_birth <- predict(prop_model_no_health_cond_birth,type="response")
  
  # select information from propensity score analyses --------------------------
  data_ps <- data_ps %>% select (ID, pr_score_full, pr_score_no_hx_preg, pr_score_no_ethn, pr_score_no_health_cond)
  
  data_ps_birth <- data_ps_birth %>% select (ID, pr_score_birth, pr_score_no_hx_preg_birth, 
                                             pr_score_no_ethn_birth, pr_score_no_health_cond_birth
  ) 
  
  # combine propensity score with main data ------------------------------------
  data %>% count(VACC_1ST_DUR_FLAG)
  data <- merge(data,data_ps, by=c("ID"), all = TRUE)
  data <- merge(data,data_ps_birth, by=c("ID"), all = TRUE)
  data %>% count(VACC_1ST_DUR_FLAG)
  
  # define date ----------------------------------------------------------------
  data$EXP_DUR_COVID_RECORD_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y") 
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
  data$COHORT_END_DATE <- as.Date(data$COHORT_END_DATE, "%d/%m/%Y")
  
  data<-data %>%
    rename(OUT_DUR_VENOUS = OUT_BIN_DUR_VENOUS,
           OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES,
           OUT_DUR_PREECLAMPSIA  = OUT_BIN_PREECLAMPSIA,
           OUT_DUR_GEST_HYPERTENSION  = OUT_BIN_GEST_HYPERTENSION)
  
  data$OUT_BIRTH_PRETERM_RECORD_DATE<-  as.Date(data$DELIVERY_DATE, "%d/%m/%Y")  
  data$OUT_BIRTH_VERY_PRETERM_RECORD_DATE<-  as.Date(data$DELIVERY_DATE, "%d/%m/%Y")  
  data$OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE<-  as.Date(data$DELIVERY_DATE, "%d/%m/%Y")  
  data$OUT_BIRTH_STILLBIRTH_RECORD_DATE<-  as.Date(data$DELIVERY_DATE, "%d/%m/%Y")  
  
  data$OUT_BIRTH_PRETERM_RECORD_DATE[data$OUT_BIN_BIRTH_PRETERM==0] <- NA
  data$OUT_BIRTH_VERY_PRETERM_RECORD_DATE[data$OUT_BIN_BIRTH_VERY_PRETERM==0] <- NA
  data$OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE[data$OUT_BIN_BIRTH_SMALL_GEST_AGE==0] <- NA
  data$OUT_BIRTH_STILLBIRTH_RECORD_DATE[data$OUT_BIN_BIRTH_STILLBIRTH_MAX==0] <- NA
  
  data$OUT_BIRTH_PRETERM_RECORD_DATE <- ifelse(data$OUT_BIRTH_PRETERM_RECORD_DATE=='1970/01/01', NA, data$OUT_BIRTH_PRETERM_RECORD_DATE)
  data$OUT_BIRTH_VERY_PRETERM_RECORD_DATE <- ifelse(data$OUT_BIRTH_VERY_PRETERM_RECORD_DATE=='1970-01-01', NA, data$OUT_BIRTH_VERY_PRETERM_RECORD_DATE)
  data$OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE <- ifelse(data$OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE=='1970-01-01', NA, data$OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE)
  data$OUT_BIRTH_STILLBIRTH_RECORD_DATE <- ifelse(data$OUT_BIRTH_STILLBIRTH_RECORD_DATE=='1970-01-01', NA, data$OUT_BIRTH_STILLBIRTH_RECORD_DATE)
  
  data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE <- as.Date(data$OUT_DUR_GEST_HYPERTENSION_DATE, "%d/%m/%Y") 
  data$OUT_DUR_GEST_DIABETES_RECORD_DATE <- as.Date(data$OUT_DUR_GEST_DIABETES_DATE, "%d/%m/%Y") 
  data$OUT_DUR_PREECLAMPSIA_RECORD_DATE <- as.Date(data$OUT_DUR_PREECLAMPSIA_DATE, "%d/%m/%Y")
  data$OUT_DUR_VENOUS_RECORD_DATE <- as.Date(data$OUT_DUR_VENOUS_DATE, "%d/%m/%Y") 
  
  data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE=='1970/01/01', NA, data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE)
  data$OUT_DUR_GEST_DIABETES_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_DIABETES_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_GEST_DIABETES_RECORD_DATE)
  data$OUT_DUR_PREECLAMPSIA_RECORD_DATE <- ifelse(data$OUT_DUR_PREECLAMPSIA_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_PREECLAMPSIA_RECORD_DATE)
  data$OUT_DUR_VENOUS_RECORD_DATE <- ifelse(data$OUT_DUR_VENOUS_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_VENOUS_RECORD_DATE)
  
  # define calendar period variable-----------------------------------------------
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
  date_start <- ymd("2019/08/01")
  data$calendar_month <- ((year(data$PREG_START_DATE) - year(date_start)) * 12) + month(data$PREG_START_DATE) - month(date_start)
  
# cox model function overall follow-up period ----------------------------------
cox_model_full <- function(k, y, z) {

  # define dataframe for outcomes at birth -------------------------------------
  if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
    data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
    data <- data[which(data$OUT_BIN_BIRTH_MULTI_GEST_MAX==0),]
    data$pr_score_full <- data$pr_score_birth
    data$pr_score_no_hx_preg <- data$pr_score_no_hx_preg_birth
    data$pr_score_no_ethn <- data$pr_score_no_ethn_birth
    data$pr_score_no_health_cond <- data$pr_score_no_health_cond_birth
  }
  
  # prepare for data transform to long format ----------------------------------
  data$fup<-as.numeric(difftime(data$COHORT_END_DATE, data$PREG_START_DATE, unit="days"))
  

  # set event ------------------------------------------------------------------
  data$overall_event<-data[[k]]
  ls(data)
  
  # set event date--------------------------------------------------------------
  data$overall_pregn_event_date<-data[[y]]
  nrow(data[!is.na(data$overall_pregn_event_date),])
  data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
  
  table(data$overall_event)
  data <- data %>%
    mutate(overall_event = case_when(data$overall_event==0 ~ 0,
                                     data$overall_event==1 & data$PREG_START_DATE<=data$overall_pregn_event_date & data$overall_pregn_event_date<=data$COHORT_END_DATE ~ 1,
                                     data$overall_event==1 & (data$PREG_START_DATE>data$overall_pregn_event_date | data$overall_pregn_event_date>data$COHORT_END_DATE) ~ 0
                                     
    )
    )
  table(data$overall_event)
  
  data$overall_pregn_event_date[data$overall_event==0] <- NA
  
  data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$PREG_START_DATE, unit="days"))
  
  data$age_sq <- data$COV_AGE^2
  
  # stratification -------------------------------------------------------------
  data$parity_label <-ifelse(data$COV_HX_PREGNANCY_MAX==1, "hx_of_preg", "no_hx_of_preg")
  data$parity_label <- factor(data$parity_label, levels = c("hx_of_preg", "no_hx_of_preg"))
  data$parity_label = relevel(data$parity_label, ref = "no_hx_of_preg")
  
  table(data$parity_label)
  
  data <- data %>%
    mutate(COV_DEP_BIN = case_when(data$COV_DEPRIVATION<= 2 ~ 0,
                                   data$COV_DEPRIVATION >= 3 ~  1
    )
    )
  
  data$dep_label <- ifelse(data$COV_DEP_BIN==1, "high_dep", "low_dep")
  data$dep_label <- factor(data$dep_label, levels = c( "high_dep", "low_dep"))
  data$dep_label = relevel(data$dep_label, ref = "low_dep")
  
  table(data$dep_label)
  
  data$dep_covid_pre <- ifelse(data$EXP_COVID_PRE_BIN==1, "yes", "no")
  data$dep_covid_pre <- factor(data$dep_covid_pre, levels = c( "yes", "no"))
  data$dep_covid_pre = relevel(data$dep_covid_pre, ref = "no")
  
  table(data$dep_covid_pre)
  
  data$health_condition_label <-ifelse(data$COV_HX_HEALTH_CONDITION==1, "hx_of_health_cond", "no_hx_of_health_cond")
  data$health_condition_label <- factor(data$health_condition_label, levels = c("hx_of_health_cond", "no_hx_of_health_cond"))
  data$health_condition_label = relevel(data$health_condition_label, ref = "no_hx_of_health_cond")
  
  table(data$health_condition_label)
  
  # choose exposure  -----------------------------------------------------------
  data$vacc_covid <- data$VACC_1ST_DUR_FLAG
  
  data$VACC_1ST_DUR_DATE<- as.Date(data$VACC_1ST_DUR_DATE, "%d/%m/%Y")
  data$vacc_covid_date <- data$VACC_1ST_DUR_DATE
  
  #split rows in those who were vaccinated and those who were not vaccinated ---
  vacc_cases <- data[which(data$vacc_covid==1),]
  vacc_cases$delta_vacc_covid<-as.numeric(difftime(vacc_cases$vacc_covid_date, vacc_cases$PREG_START_DATE, unit="days"))
  vacc_cases$delta_vacc_covid
  
  #variable list for long format transformation---------------------------------
  vars<-c("ID",
          "calendar_month",
          "region",
          "pr_score_full",
          "pr_score_no_hx_preg",
          "pr_score_no_ethn",
          "pr_score_no_health_cond",
          "COV_AGE",
          "age_sq",
          "COV_DEPRIVATION",
          "PREG_START_DATE",
          "COHORT_END_DATE",
          "COV_HX_PREGNANCY_MAX",
          "agegroup", 
          "parity_label", 
          "dep_label",
          "COV_ETHNICITY_3lev",
          "COV_ETHNICITY",
          "health_condition_label",
          "dep_covid_pre",
          "COV_HX_HEALTH_CONDITION",
          "EXP_COVID_PRE_BIN",
          "COV_DEP_BIN"
  )

  # data transform to long format for time from vaccination for those vaccination
  td_data <-
    tmerge(
      data1 = vacc_cases %>% select(all_of(vars), vacc_covid_date, overall_event_time), 
      data2 = vacc_cases %>% select(all_of(vars), overall_event, fup, delta_vacc_covid),
      id = ID, 
      overall_event = event(fup,  overall_event), 
      vacc_covid = tdc(delta_vacc_covid)
    )
  
  without_expo <- data[which(data$vacc_covid==0),]
  without_expo$tstart<- c(0)
  without_expo$tstop <- ifelse(without_expo$fup ==0,  without_expo$fup + 0.001, without_expo$fup) # right now this isn't doing anything because I excluded those with 0 fup.
  without_expo$vacc_covid<- c(0)
  without_expo$last_step <- c(1)
  without_expo$hospitalised <- c(1)
  without_expo_noncases <- without_expo[which(without_expo$overall_event==0),]
  noncase_ids <- unique(without_expo_noncases$ID)
  
  # combine unvaccinated and vaccinated exposure dataframes --------------------
  # td_data <- subset(td_data, select = -c(id))
  with_expo_cols <- colnames(td_data)
  with_expo_cols
  without_expo <- without_expo %>% dplyr::select(all_of(with_expo_cols))
  data_surv <-rbind(td_data, without_expo)
  rm(list=c("td_data", "without_expo"))
  
  # final dataframe ------------------------------------------------------------
  survival_data <- data_surv
  
  # set parameters---------------------------------------------------------------- 
  event_model <- c(paste0(z))
  adjustment <-  c("full_adjusted")
  analysis <-  c("full")
  
  # run models------------------------------------------------------------------
  cov <-  c("int_parity")
  cov_model <-  c("vacc_covid:COV_HX_PREGNANCY_MAX + vacc_covid + COV_HX_PREGNANCY_MAX + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_no_hx_preg) + bs(calendar_month) + region")

  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    k <- s$coefficients[20,,drop=F]
    cat(paste0(substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(k, 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", round((x[5]), 5),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv"), append = TRUE)
    invisible(model)
  })

  cov <-  c("int_ethn_3lev")
  cov_model <-  c("vacc_covid:COV_ETHNICITY_3lev + vacc_covid + COV_ETHNICITY_3lev + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_no_ethn) + bs(calendar_month) + region")

  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    cat(paste0(substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(s$coefficients[21:22,], 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", round((x[5]), 5),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv"), append = TRUE)
    invisible(model)
  })
  
  cov <-  c("int_ethn")
  cov_model <-  c("vacc_covid:COV_ETHNICITY + vacc_covid + COV_ETHNICITY + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_no_ethn) + bs(calendar_month) + region")
  
  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    cat(paste0(substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(s$coefficients[24:28,], 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", round((x[5]), 5),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv"), append = TRUE)
    invisible(model)
  })

  cov <-  c("int_dep")
  cov_model <-  c("vacc_covid:COV_DEP_BIN + vacc_covid + COV_DEP_BIN + COV_AGE  +  age_sq + bs(pr_score_full)  + bs(calendar_month) + region")
  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    k <- s$coefficients[19,,drop=F]
    cat(paste0(substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(k, 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", round((x[5]), 5),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv"), append = TRUE)
    invisible(model)
  })

  cov <-  c("int_age")
  cov_model <-  c("vacc_covid:as.factor(agegroup) + vacc_covid + agegroup + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month)  + region")

  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    cat(paste0(substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(s$coefficients[19:20,], 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", round((x[5]), 5),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv"), append = TRUE)
    invisible(model)
  })

  cov <-  c("int_pre_covid")
  cov_model <-  c("vacc_covid:EXP_COVID_PRE_BIN + vacc_covid + EXP_COVID_PRE_BIN + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month) + region")
  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    k <- s$coefficients[20,,drop=F]
    cat(paste0(substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(k, 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", round((x[5]), 5),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv"), append = TRUE)
    invisible(model)
  })

  cov <-  c("int_health_cond")
  cov_model <-  c("vacc_covid:COV_HX_HEALTH_CONDITION + vacc_covid + COV_HX_HEALTH_CONDITION + COV_AGE + age_sq + COV_DEPRIVATION + bs(pr_score_no_health_cond) + bs(calendar_month) + region")
  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    k <- s$coefficients[20,,drop=F]
    cat(paste0(substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(k, 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", round((x[5]), 5),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv"), append = TRUE)
    invisible(model)
  })

}
cox_model_full("OUT_DUR_GEST_DIABETES", "OUT_DUR_GEST_DIABETES_RECORD_DATE", "GEST_DIABETES")
cox_model_full("OUT_DUR_GEST_HYPERTENSION", "OUT_DUR_GEST_HYPERTENSION_RECORD_DATE", "GEST_HYPERTENSION")
cox_model_full("OUT_DUR_PREECLAMPSIA", "OUT_DUR_PREECLAMPSIA_RECORD_DATE", "PREECLAMPSIA")
cox_model_full("OUT_DUR_VENOUS", "OUT_DUR_VENOUS_RECORD_DATE", "VENOUS")
cox_model_full("OUT_BIN_BIRTH_PRETERM", "OUT_BIRTH_PRETERM_RECORD_DATE", "PRETERM")
cox_model_full("OUT_BIN_BIRTH_VERY_PRETERM", "OUT_BIRTH_VERY_PRETERM_RECORD_DATE", "VERY_PRETERM")
cox_model_full("OUT_BIN_BIRTH_SMALL_GEST_AGE", "OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE", "SMALL_GEST_AGE")
cox_model_full("OUT_BIN_BIRTH_STILLBIRTH_MAX", "OUT_BIRTH_STILLBIRTH_RECORD_DATE", "STILLBIRTH")

#include model with more than 10 events-----------------------------------------
library(readr)
library(plyr)

out <- read_table2("C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv")
nrow(out)
out <- out[which(out$n.event > 10),]
out$n.event <- round_any(out$n.event, 5)
nrow(out)

nrow(out)
data.table::fwrite(out,"C:/ProgramData/UserDataFolders/S-1-5-21-1756195885-2368276971-2238036799-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_out_HRs_full_period_inter.csv")
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
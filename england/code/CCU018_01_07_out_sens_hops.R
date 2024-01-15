################################################################################
## TITLE: Data preparation of England dataset for CCU018_01
##
## Description: Cox models to examine the associations between COVID-19 infection
## and adverse pregnancy outcomes stratified for hospitalization
##
## By : Elena Raffetti
##
################################################################################

writeLines(c("sens hosp event model adj exp estimate conf.low conf.high stad.error n.event subgroup"), paste0('D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/out_HRs_sens_hosp.csv'))

# Install.packages -------------------------------------------------------------
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("scales")
#install.packages("dplyr")
#install.packages("survival")
#install.packages("survminer")
#install.packages("gtsummary")
#install.packages("reshape2")
#install.packages("lubridate")
#install.packages("splines")
#install.packages("purrr")
#install.packages("ggpubr")

# load required packages -------------------------------------------------------
library(stringr)
library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)
library(survival)
library(survminer)
library(gtsummary)
library(reshape2)
library(lubridate)
library(splines)
library(purrr)
library(ggpubr)

dev.off()

# start function ---------------------------------------------------------------
sensitivity_analyses <- function(q) {
  
  data <- data_temp4
  ls(data)
  
  # define cohort end date -----------------------------------------------------
  data$COHORT_END_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  
  # sensitivity analyses inclusion criteria ------------------------------------
  if (q == "pre_vaccination"){
    
    data$COHORT_END_DATE <- as.Date(data$COHORT_END_DATE, "%d/%m/%Y")
    data <- data[which(data$COHORT_END_DATE < '2020-12-08'),]
  }
  
  if (q == "post_vaccination"){
    data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
    data <- data[which(data$PREG_START_DATE >= '2020-12-08'),]
    
    data$COHORT_END_DATE <- as.Date(data$COHORT_END_DATE, "%d/%m/%Y")
  }  
  
  sens <- c(paste0(q))
  
  hosp_strata <- function(hosp) {

  if (hosp == "sens_hosp_no"){
    data <- data %>%
      mutate     (EXP_COVID_BIN  = case_when(data$EXP_COVID_BIN ==0 ~ 0 ,
                                             data$EXP_COVID_BIN ==1 & data$EXP_DUR_HOSP==1 ~ 0,
                                             data$EXP_COVID_BIN ==1 & data$EXP_DUR_HOSP==0 ~ 1,
      )
      )
    
    
    data$EXP_DUR_COVID_1ST_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y")
    data <- data %>%
    mutate     (COHORT_END_DATE = case_when((data$EXP_DUR_HOSP==0  & data$EXP_COVID_BIN ==0) |  (data$EXP_DUR_HOSP==0  & data$EXP_COVID_BIN ==1) ~  data$COHORT_END_DATE,
                                            (data$EXP_DUR_HOSP==1  & data$EXP_COVID_BIN ==1) ~   data$EXP_DUR_COVID_1ST_DATE
    )
    )
  }
  
  if (hosp == "sens_hosp_yes"){
    data <- data %>%
      mutate     (EXP_COVID_BIN  = case_when(data$EXP_COVID_BIN ==0 ~ 0 ,
                                             data$EXP_COVID_BIN ==1 & data$EXP_DUR_HOSP==0 ~ 0,
                                             data$EXP_COVID_BIN ==1 & data$EXP_DUR_HOSP==1 ~ 1,
      )
      )
    
    
    data$EXP_DUR_COVID_1ST_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y")
    data <- data %>%
      mutate     (COHORT_END_DATE = case_when((data$EXP_DUR_HOSP==0  & data$EXP_COVID_BIN ==0) |  (data$EXP_DUR_HOSP==1  & data$EXP_COVID_BIN ==1) ~  data$COHORT_END_DATE,
                                              (data$EXP_DUR_HOSP==0  & data$EXP_COVID_BIN ==1) ~   data$EXP_DUR_COVID_1ST_DATE
      )
      )
  }
 
    hosp <- c(paste0(hosp))

  data<-data %>%  rename(ID=PERSON_ID)
  table(data$PREG_START_DATE_ESTIMATED)
  nrow(data)
  
  # format dates ---------------------------------------------------------------
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$OUT_DUR_PREECLAMPSIA_DATE <- as.Date(data$OUT_DUR_PREECLAMPSIA_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_DIABETES_DATE <- as.Date(data$OUT_DUR_GEST_DIABETES_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_HYPERTENSION_DATE <- as.Date(data$OUT_DUR_GEST_HYPERTENSION_DATE, "%d/%m/%Y")
  data$OUT_DUR_VENOUS_DATE <- as.Date(data$OUT_DUR_VENOUS_DATE, "%d/%m/%Y")
  
  # prepare propensity scores --------------------------------------------------
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
  data_ps$pr_score_full <- predict(prop_model_full,type="response")
  
  ggplot(data_ps ,aes (x = pr_score_full)) +
    geom_histogram(color = "white") +
    facet_wrap(~ exposure) +
    xlab("Probability of being infected with COVID") +
    theme_bw()
  
  prop_model_birth <- glm(exposure ~  COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                          + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps_birth, family=binomial(link="logit"))
  data_ps_birth$pr_score_birth <- predict(prop_model_birth,type="response")
  
  # prepare propensity scores for analysis stratified for history of pregnancy--
  prop_model_no_hx_preg <- glm(exposure ~  COV_HX_HYPERTENSIVE_DIS  + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                               + COV_ETHNICITY_3lev + COV_SMOKING_STATUS , data= data_ps, family=binomial(link="logit"))
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
  prop_model_no_ethn <-  glm(exposure ~   COV_HX_PREGNANCY_MAX  + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                             + COV_SMOKING_STATUS , data= data_ps, family=binomial(link="logit"))
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
  prop_model_no_health_cond <-  glm(exposure ~   COV_HX_PREGNANCY_MAX + COV_HX_BMI_OBESITY_FLAG  + COV_HX_DEPRESSION_FLAG
                                    + COV_ETHNICITY_3lev + COV_SMOKING_STATUS , data= data_ps, family=binomial(link="logit"))
  
  data_ps$pr_score_no_health_cond <- predict(prop_model_no_health_cond,type="response")
  
  ggplot(data_ps ,aes(x = pr_score_no_health_cond)) +
    geom_histogram(color = "white") +
    facet_wrap(~ exposure) +
    xlab("Probability of being infected with COVID") +
    theme_bw()
  
  prop_model_no_health_cond_birth <-  glm(exposure ~   COV_HX_PREGNANCY_MAX + COV_HX_BMI_OBESITY_FLAG  + COV_HX_DEPRESSION_FLAG
                                          + COV_ETHNICITY_3lev + COV_SMOKING_STATUS , data= data_ps_birth, family=binomial(link="logit"))
  
  
  data_ps_birth$pr_score_no_health_cond_birth <- predict(prop_model_no_health_cond_birth,type="response")
  
  # select information from propensity score analyses --------------------------
  data_ps <- data_ps %>% select (ID, pr_score_full, pr_score_no_hx_preg, pr_score_no_ethn, pr_score_no_health_cond)
  
  data_ps_birth <- data_ps_birth %>% select (ID,pr_score_birth, pr_score_no_hx_preg_birth, 
                                             pr_score_no_ethn_birth, pr_score_no_health_cond_birth
  ) 
  
  # combine propensity score with main data ------------------------------------
  data %>% count(EXP_COVID_BIN)
  nrow(data)
  data <- merge(data,data_ps, by=c("ID"), all = TRUE)
  data <- merge(data,data_ps_birth, by=c("ID"), all = TRUE)
  data %>% count(EXP_COVID_BIN)
  nrow(data)
  
  # define date ----------------------------------------------------------------
  data$EXP_DUR_COVID_RECORD_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y") 
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
  data$COHORT_END_DATE <- as.Date(data$COHORT_END_DATE, "%d/%m/%Y")
  
  # rename outcome names -------------------------------------------------------
  data<-data %>%
    rename(OUT_DUR_VENOUS = OUT_BIN_DUR_VENOUS,
           OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES,
           OUT_DUR_PREECLAMPSIA  = OUT_BIN_PREECLAMPSIA,
           OUT_DUR_GEST_HYPERTENSION  = OUT_BIN_GEST_HYPERTENSION)
  
  # define date ----------------------------------------------------------------
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
  
  # define calendar period variable---------------------------------------------
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
  date_start <- ymd("2019/08/01")
  data$calendar_month <- ((year(data$PREG_START_DATE) - year(date_start)) * 12) + month(data$PREG_START_DATE) - month(date_start)
  
  # cox model function for stratified exposure ---------------------------------
  cox_model_2week <- function(k, y, z) {
    
    # set dataframe for outcomes at birth --------------------------------------
    if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
      data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
      data$pr_score_full <- data$pr_score_birth
      data$pr_score_no_hx_preg <- data$pr_score_no_hx_preg_birth
      data$pr_score_no_hx_preg <- data$pr_score_no_hx_preg_birth
      data$pr_score_no_ethn <- data$pr_score_no_ethn_birth
      data$pr_score_no_health_cond <- data$pr_score_no_health_cond_birth
    }

    # prepare for data transform to long format --------------------------------
    ls(data)

    # set event-----------------------------------------------------------------
    data$overall_event<-data[[k]]
    table(data$overall_event)

    # set event date -----------------------------------------------------------
    data$overall_pregn_event_date<-data[[y]]
    nrow(data[!is.na(data$overall_pregn_event_date),])
    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
    
    # include events that occurs during the follow-up time for each sensitivity analysis
    table(data$overall_event)
    data <- data %>%
      mutate(overall_event = case_when(data$overall_event==0 ~ 0,
                                       data$overall_event==1 & data$PREG_START_DATE<=data$overall_pregn_event_date & data$overall_pregn_event_date<=data$COHORT_END_DATE ~ 1,
                                       data$overall_event==1 & (data$PREG_START_DATE>data$overall_pregn_event_date | data$overall_pregn_event_date>data$COHORT_END_DATE) ~ 0

      )
      )
    table(data$overall_event)

    data$overall_pregn_event_date[data$overall_event==0] <- NA

    # duration of follow-up ----------------------------------------------------
    data$fup<-as.numeric(difftime(data$COHORT_END_DATE, data$PREG_START_DATE, unit="days"))

    data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$PREG_START_DATE, unit="days"))
    data$age_sq <- data$COV_AGE^2

    # stratification -----------------------------------------------------------
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

    # choose exposure ----------------------------------------------------------
    data$covid_inf <- data$EXP_COVID_BIN
    data$covid_inf_date <- data$EXP_DUR_COVID_RECORD_DATE

    # split rows in those who were infected and those who were not infected ----
    infected_cases <- data[which(data$covid_inf==1),]
    infected_cases$delta_cov_inf<-as.numeric(difftime(infected_cases$covid_inf_date, infected_cases$PREG_START_DATE, unit="days"))
    infected_cases$delta_cov_inf

    # variable list for long format transformation -----------------------------
    vars<-c("ID",
            "region",
            "calendar_month",
            "pr_score_full",
            "pr_score_no_hx_preg",
            "pr_score_no_ethn",
            "pr_score_no_health_cond",
            "COV_AGE",
            "age_sq",
            "COV_DEPRIVATION",
            "PREG_START_DATE",
            "COHORT_END_DATE",
            "agegroup",
            "parity_label",
            "dep_label",
            "COV_ETHNICITY_3lev",
            "health_condition_label",
            "dep_covid_pre",
            "COV_HX_HEALTH_CONDITION",
            "EXP_COVID_PRE_BIN"
    )

    # data transform to long format for becoming infected ----------------------
    td_data <-
      tmerge(
        data1 = infected_cases %>% select(all_of(vars), covid_inf_date, overall_event_time),
        data2 = infected_cases %>% select(all_of(vars), overall_event, fup, delta_cov_inf),
        id = ID,
        overall_event = event(fup,  overall_event),
        covid_inf = tdc(delta_cov_inf)
      )

    # split long format dataframe into pre and post infection ------------------
    with_expo_postexpo <- td_data %>% filter(covid_inf==1)
    with_expo_preexpo <- td_data %>% filter(covid_inf==0)
    with_expo_postexpo <- with_expo_postexpo %>% rename(t0=tstart, t=tstop) %>% mutate(tstart=0, tstop=t-t0)

    cuts_weeks_since_expo <- c(2)

    #  data transform to long format for time from infection for those infected-
    with_expo_postexpo<-survSplit(Surv(tstop, overall_event) ~., with_expo_postexpo,
                                  cut=cuts_weeks_since_expo*7,
                                  episode ="Weeks_category")

    # prepare dataframe for those  with infection exposure ---------------------
    with_expo_postexpo <- with_expo_postexpo %>% mutate(tstart=tstart+t0, tstop=tstop+t0) %>% dplyr::select(-c(t0,t))
    with_expo_preexpo$Weeks_category <- 0
    ls_with_expo <- list(with_expo_preexpo, with_expo_postexpo)
    with_expo <- do.call(rbind, lapply(ls_with_expo, function(x) x[match(names(ls_with_expo[[1]]), names(x))]))
    rm(list=c("ls_with_expo", "with_expo_preexpo", "with_expo_postexpo"))
    with_expo  <- with_expo %>%
      group_by(ID) %>% arrange(Weeks_category) %>% mutate(last_step = ifelse(row_number()==n(),1,0))
    with_expo$overall_event  <- with_expo$overall_event * with_expo$last_step

    # prepare dataframe for those  without infection ---------------------------
    without_expo <- data[which(data$covid_inf==0),]
    without_expo$tstart<- c(0)
    without_expo$tstop <- ifelse(without_expo$fup ==0,  without_expo$fup + 0.001, without_expo$fup)
    without_expo$covid_inf<- c(0)
    without_expo$Weeks_category <- c(0)
    without_expo$last_step <- c(1)
    without_expo$hospitalised <- c(1)
    without_expo_noncases <- without_expo[which(without_expo$overall_event==0),]
    noncase_ids <- unique(without_expo_noncases$ID)

    # combine uninfected and infected exposure dataframes ----------------------
    with_expo_cols <- colnames(with_expo)
    with_expo_cols
    without_expo <- without_expo %>% dplyr::select(all_of(with_expo_cols))
    data_surv <-rbind(with_expo, without_expo)
    rm(list=c("with_expo", "without_expo"))

    data_surv$days_to_expo<-as.numeric(difftime(data_surv$covid_inf_date, data_surv$PREG_START_DATE, unit="days"))

    # Pivot Wide for time intervals (exposure) ---------------------------------
    number_period <- c(1, 2)
    interval_names <- mapply(function(x, y) ifelse(x == y, paste0("E", x), paste0("E", x, "_", y)),
                             lag(number_period, default = 0)+1,
                             number_period,
                             SIMPLIFY = FALSE)
    cat("...... interval_names...... \n")
    print(interval_names)
    intervals <- mapply(c, lag(number_period, default = 0)+1, number_period, SIMPLIFY = F)


    i<-0
    for (ls in mapply(list, interval_names, intervals, SIMPLIFY = F)){
      i <- i+1
      print(paste(c(ls, i), collapse="..."))
      data_surv[[ls[[1]]]] <- if_else(data_surv$Weeks_category==i, 1, 0)
    }

    data_surv %>% filter(tstart>0) %>% arrange(ID)%>% head(10) %>% print(n = Inf)
    data_surv %>% filter(days_to_expo==overall_event_time) %>% arrange(ID)%>% head(10) %>% print(n = Inf) #replaced overall_event_date with overall_event_time

    # final dataframe ----------------------------------------------------------
    survival_data <- data_surv
    
    # set parameters -----------------------------------------------------------
    event_model <- c(paste0(z))
    hosp <- c(paste0(hosp))
    adjustment <-  c("non_adjusted", "adjusted", "full_adjusted")
    analysis <-  c("week2")

    cov <-  c("E1", "E2")

    cov_model <-  c("E1 + E2",
                    "E1 + E2 + COV_AGE  +  age_sq  + COV_DEPRIVATION ",
                    "E1 + E2 + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month) + region")


    # run models----------------------------------------------------------------
    map2(cov_model, adjustment, function(x,i) {
      f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
      model <- coxph(f, survival_data, id = ID)
      model$call$formula <- f
      s <- summary(model)
      cat(paste0(sens,' ',hosp,' ',substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(s$coefficients[1:2,], 1,
                                                                                         function(x) {
                                                                                           paste0(" ", round(exp(x[1]), 3),
                                                                                                  ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                                  ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                                  " ", round((x[3]), 4),
                                                                                                  " ", summary(model)$nevent)}),
                 collapse = '\n'), '\n', sep = '', file = paste0("D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/out_HRs_sens_hosp.csv"), append = TRUE)
      invisible(model)
    })

  }
  cox_model_2week("OUT_DUR_GEST_DIABETES", "OUT_DUR_GEST_DIABETES_RECORD_DATE", "GEST_DIABETES")
  cox_model_2week("OUT_DUR_GEST_HYPERTENSION", "OUT_DUR_GEST_HYPERTENSION_RECORD_DATE", "GEST_HYPERTENSION")
  cox_model_2week("OUT_DUR_PREECLAMPSIA", "OUT_DUR_PREECLAMPSIA_RECORD_DATE", "PREECLAMPSIA")
  cox_model_2week("OUT_DUR_VENOUS", "OUT_DUR_VENOUS_RECORD_DATE", "VENOUS")
  cox_model_2week("OUT_BIN_BIRTH_PRETERM", "OUT_BIRTH_PRETERM_RECORD_DATE", "PRETERM")
  cox_model_2week("OUT_BIN_BIRTH_VERY_PRETERM", "OUT_BIRTH_VERY_PRETERM_RECORD_DATE", "VERY_PRETERM")
  cox_model_2week("OUT_BIN_BIRTH_SMALL_GEST_AGE", "OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE", "SMALL_GEST_AGE")
  cox_model_2week("OUT_BIN_BIRTH_STILLBIRTH_MAX", "OUT_BIRTH_STILLBIRTH_RECORD_DATE", "STILLBIRTH")
  
  # Cox model function overall follow-up period --------------------------------
  cox_model_full <- function(k, y, z) {
    ls(data)
    
    # define dataframe for outcomes at birth -----------------------------------
    if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
      data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
      data$pr_score_full <- data$pr_score_birth
      data$pr_score_no_hx_preg <- data$pr_score_no_hx_preg_birth
      data$pr_score_no_ethn <- data$pr_score_no_ethn_birth
      data$pr_score_no_health_cond <- data$pr_score_no_health_cond_birth
    }

    # prepare for data transform to long format ---------------------------------

    # set event ----------------------------------------------------------------
    table(data$overall_event)
    data$overall_event<-data[[k]]

    # set event date------------------------------------------------------------
    data$overall_pregn_event_date<-data[[y]]
    nrow(data[!is.na(data$overall_pregn_event_date),])
    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')

    # include events that occurs during the follow-up time for each sensitivity analysis
    table(data$overall_event)
    data <- data %>%
      mutate(overall_event = case_when(data$overall_event==0 ~ 0,
                                       data$overall_event==1 & data$PREG_START_DATE<=data$overall_pregn_event_date & data$overall_pregn_event_date<=data$COHORT_END_DATE ~ 1,
                                       data$overall_event==1 & (data$PREG_START_DATE>data$overall_pregn_event_date | data$overall_pregn_event_date>data$COHORT_END_DATE) ~ 0

      )
      )
    table(data$overall_event)

    data$overall_pregn_event_date[data$overall_event==0] <- NA

    # duration of follow-up ----------------------------------------------------
    data$fup<-as.numeric(difftime(data$COHORT_END_DATE, data$PREG_START_DATE, unit="days"))

    data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$PREG_START_DATE, unit="days"))
    data$age_sq <- data$COV_AGE^2

    # stratification -----------------------------------------------------------
    ls(data)

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

    data$dep_covid_pre <- ifelse(data$EXP_COVID_PRE_BIN==1, "yes", "no")
    data$dep_covid_pre <- factor(data$dep_covid_pre, levels = c( "yes", "no"))
    data$dep_covid_pre = relevel(data$dep_covid_pre, ref = "no")

    data$health_condition_label <-ifelse(data$COV_HX_HEALTH_CONDITION==1, "hx_of_health_cond", "no_hx_of_health_cond")
    data$health_condition_label <- factor(data$health_condition_label, levels = c("hx_of_health_cond", "no_hx_of_health_cond"))
    data$health_condition_label = relevel(data$health_condition_label, ref = "no_hx_of_health_cond")

    # choose exposure  ---------------------------------------------------------
    data$covid_inf <- data$EXP_COVID_BIN
    data$covid_inf_date <- data$EXP_DUR_COVID_RECORD_DATE

    #split rows in those who were infected and those who were not infected -----
    infected_cases <- data[which(data$covid_inf==1),]
    infected_cases$delta_cov_inf<-as.numeric(difftime(infected_cases$covid_inf_date, infected_cases$PREG_START_DATE, unit="days"))
    infected_cases$delta_cov_inf

    #variable list for long format transformation-------------------------------
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
            "agegroup",
            "parity_label",
            "dep_label",
            "COV_ETHNICITY_3lev",
            "health_condition_label",
            "dep_covid_pre",
            "COV_HX_HEALTH_CONDITION",
            "EXP_COVID_PRE_BIN"
    )

    #data transform to long format for becoming infected -----------------------
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
    without_expo$tstop <- ifelse(without_expo$fup ==0,  without_expo$fup + 0.001, without_expo$fup)
    without_expo$covid_inf<- c(0)
    without_expo$last_step <- c(1)
    without_expo$hospitalised <- c(1)
    without_expo_noncases <- without_expo[which(without_expo$overall_event==0),]
    noncase_ids <- unique(without_expo_noncases$ID)

    # combine uninfected and infected exposure dataframes ----------------------
    with_expo_cols <- colnames(td_data)
    with_expo_cols
    without_expo <- without_expo %>% dplyr::select(all_of(with_expo_cols))
    data_surv <-rbind(td_data, without_expo)
    rm(list=c("td_data", "without_expo"))

    # final dataframe ----------------------------------------------------------
    survival_data <- data_surv
    
    # set parameters------------------------------------------------------------
    event_model <- c(paste0(z))
    hosp <- c(paste0(hosp))
    adjustment <-  c("non_adjusted", "adjusted", "full_adjusted")
    analysis <-  c("full")

    cov <-  c("covid_inf")

    cov_model <-  c("covid_inf",
                    "covid_inf + COV_AGE  +  age_sq  + COV_DEPRIVATION ",
                    "covid_inf + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month) + region")

    # run models----------------------------------------------------------------
    map2(cov_model, adjustment, function(x,i) {
      f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
      model <- coxph(f, survival_data, id = ID)
      model$call$formula <- f
      s <- summary(model)
      k <- s$coefficients[1,,drop=F]
      cat(paste0(sens,' ',hosp,' ',substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(k, 1,
                                                                                         function(x) {
                                                                                           paste0(" ", round(exp(x[1]), 3),
                                                                                                  ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                                  ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                                  " ", round((x[3]), 4),
                                                                                                  " ", summary(model)$nevent)}),
                 collapse = '\n'), '\n', sep = '', file = paste0("D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/out_HRs_sens_hosp.csv"), append = TRUE)
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

  } 
  hosp_strata("sens_hosp_no")
  hosp_strata("sens_hosp_yes")
  
}
sensitivity_analyses("total")
sensitivity_analyses("pre_vaccination")
sensitivity_analyses("post_vaccination")

#include models with more than 10 events----------------------------------------
library(readr)
library(plyr)
out <- read_table2("D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/out_HRs_sens_hosp.csv")
nrow(out)
out <- out[which(out$n.event > 10),]
nrow(out)
out$n.event <- round_any(out$n.event, 5)

data.table::fwrite(out,"D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/out_HRs_sens_hosp.csv")
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)

################################################################################
## TITLE: Data preparation of England dataset for CCU018_01
##
## Description: Cox models to examine the associations between vaccination
## against COVID-19 (2nd dose) and adverse pregnancy outcomes
##
## By : Elena Raffetti
##
################################################################################

writeLines(c("sens event model adj exp estimate conf.low conf.high stad.error n.event subgroup"), paste0('C:/ProgramData/UserDataFolders/S-1-5-21-3444344212-1812383397-2900750023-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/2nd_vaccination_out_HRs.csv'))

# install.packages -------------------------------------------------------------
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

# sensitivity analyses inclusion criteria --------------------------------------
sensitivity_analyses <- function(q) {
  
  # load data ------------------------------------------------------------------
  data <- data_temp7
  data<-data %>%  rename(ID=PERSON_ID)

  # format dates --------------------------------------------------------------- 
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  
  data$follow_up_time<-as.numeric(difftime(data$DELIVERY_DATE, data$PREG_START_DATE, unit="days"))
  summary(data$follow_up_time)
  (sum(data$follow_up_time))/365.25
  
  data$VACC_2ND_DUR_DATE <- as.Date( data$VACC_2ND_DUR_DATE, "%d/%m/%Y")
  data$VACC_1ST_DUR_DATE <- as.Date( data$VACC_1ST_DUR_DATE, "%d/%m/%Y")
  data$COHORT_END_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  
if (q == "total"){
  }

if (q == "sens"){
  table(data$PREG_START_DATE_ESTIMATED)
  data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]

}

if (q == "sens_covid"){

  data <- data[which(data$EXP_COVID_PRE_BIN==0),]

  # define cohort end date -----------------------------------------------------
  data$VACC_2ND_DUR_DATE <- as.Date( data$VACC_2ND_DUR_DATE, "%d/%m/%Y")
  data$EXP_DUR_COVID_1ST_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y")

  data <- data %>%
    mutate     (VACC_2ND_DUR_FLAG  = case_when(data$VACC_2ND_DUR_FLAG ==0  ~ 0 ,
                                               data$VACC_2ND_DUR_FLAG ==1 & data$EXP_COVID_BIN ==0 ~ 1 ,
                                               data$VACC_2ND_DUR_FLAG ==1 & data$EXP_COVID_BIN ==1 & (data$EXP_DUR_COVID_1ST_DATE < data$VACC_2ND_DUR_DATE) ~ 0 ,
                                               data$VACC_2ND_DUR_FLAG ==1 & data$EXP_COVID_BIN ==1 & (data$EXP_DUR_COVID_1ST_DATE >= data$VACC_2ND_DUR_DATE) ~ 1 ,
    )
    )


  data <- data %>%
    mutate     (COHORT_END_DATE = case_when(data$EXP_COVID_BIN ==0 ~ data$COHORT_END_DATE,
                                            data$EXP_COVID_BIN ==1 ~ data$EXP_DUR_COVID_1ST_DATE
    )
    )

  data$VACC_1ST_DUR_DATE[data$VACC_1ST_DUR_FLAG==0] <- NA

  data_check <- data%>% select (VACC_1ST_DUR_DATE, COHORT_END_DATE, EXP_COVID_BIN, EXP_DUR_COVID_1ST_DATE,
                                VACC_2ND_DUR_FLAG, VACC_2ND_DUR_DATE
  )

}

if (q == "sens_no_early_preg"){
  nrow(data)
  table(data$VACC_1ST_DUR_FLAG)

  data$VACC_1ST_DUR_DATE <- as.Date(data$VACC_1ST_DUR_DATE, "%d/%m/%Y")

  data <- data[which(data$VACC_1ST_DUR_DATE >'2021-06-18'),]
  nrow(data)
}

# define vector with sensitivity analysis information to include in the output, call in map2 functions
sens <- c(paste0(q))

# define dates -----------------------------------------------------------------
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
data$VACC_1ST_DUR_DATE <- as.Date(data$VACC_1ST_DUR_DATE, "%d/%m/%Y")
data$GEST_WEEK_1ST_VACC<-(as.numeric(difftime(data$VACC_1ST_DUR_DATE, data$PREG_START_DATE, unit="days")))/7
data$DAYS_FROM_PREG_START<-(as.numeric(difftime(data$VACC_1ST_DUR_DATE, data$PREG_START_DATE, unit="days")))

# set start risk period from 1st vaccination -----------------------------------
data$COHORT_START_DATE <- data$VACC_1ST_DUR_DATE

# prepare propensity scores ----------------------------------------------------
data_ps <- data
ls(data_ps)

data_ps$exposure  <- data_ps$VACC_2ND_DUR_FLAG 

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
  xlab("Probability of being vaccinated") +
  theme_bw()

prop_model_birth <- glm(exposure ~  COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                        + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps_birth, family=binomial(link="logit"))

data_ps_birth$pr_score_birth <- predict(prop_model_birth,type="response")

# select information from propensity score analyses ----------------------------
data_ps <- data_ps %>% select (ID, pr_score_full)

data_ps_birth <- data_ps_birth %>% select (ID,pr_score_birth) 

# combine propensity score with main data -------------------------------------

data %>% count(VACC_2ND_DUR_FLAG)
data <- merge(data,data_ps, by=c("ID"), all = TRUE)
data <- merge(data,data_ps_birth, by=c("ID"), all = TRUE)
data %>% count(VACC_2ND_DUR_FLAG)

# define dates -----------------------------------------------------------------
data$EXP_DUR_COVID_RECORD_DATE <- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y") 
data$COHORT_START_DATE <- as.Date(data$COHORT_START_DATE, "%d/%m/%Y") 
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

#replace with NA if date is 1 Jan 1970
data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE=='1970/01/01', NA, data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE)
data$OUT_DUR_GEST_DIABETES_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_DIABETES_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_GEST_DIABETES_RECORD_DATE)
data$OUT_DUR_PREECLAMPSIA_RECORD_DATE <- ifelse(data$OUT_DUR_PREECLAMPSIA_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_PREECLAMPSIA_RECORD_DATE)
data$OUT_DUR_VENOUS_RECORD_DATE <- ifelse(data$OUT_DUR_VENOUS_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_VENOUS_RECORD_DATE)

# define calendar period variable-----------------------------------------------
data$COHORT_START_DATE <- as.Date(data$COHORT_START_DATE, "%d/%m/%Y") 
date_start <- ymd("2019/08/01")
data$calendar_month <- ((year(data$COHORT_START_DATE) - year(date_start)) * 12) + month(data$COHORT_START_DATE) - month(date_start)


# cox model function for stratified exposure -----------------------------------
cox_model_2week <- function(k, y, z) {
  
  # define dataframe for outcomes at birth -------------------------------------
  if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
    data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
    data$pr_score_full <- data$pr_score_birth
    data$pr_score_no_hx_preg <- data$pr_score_no_hx_preg_birth
    data$pr_score_no_ethn <- data$pr_score_no_ethn_birth
    data$pr_score_no_health_cond <- data$pr_score_no_health_cond_birth
  }
  
  # prepare for data transform to long format ----------------------------------
  data$fup<-as.numeric(difftime(data$COHORT_END_DATE, data$COHORT_START_DATE, unit="days"))
  
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
                                     data$overall_event==1 & data$COHORT_START_DATE<=data$overall_pregn_event_date & data$overall_pregn_event_date<=data$COHORT_END_DATE ~ 1,
                                     data$overall_event==1 & (data$COHORT_START_DATE>data$overall_pregn_event_date | data$overall_pregn_event_date>data$COHORT_END_DATE) ~ 0
                                     
    )
    )
  table(data$overall_event)
  
  data$overall_pregn_event_date[data$overall_event==0] <- NA

  data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$COHORT_START_DATE, unit="days"))
  
  data$age_sq <- data$COV_AGE^2
  
  
  # choose exposure  -----------------------------------------------------------
  data$vacc_covid <- data$VACC_2ND_DUR_FLAG 
  data$vacc_covid_date <- data$VACC_2ND_DUR_DATE
  
  # split rows in those who were vaccinated and those who were not vaccinated --
  vaccinated_cases <- data[which(data$vacc_covid==1),]
  vaccinated_cases$delta_vacc_covid<-as.numeric(difftime(vaccinated_cases$vacc_covid_date, vaccinated_cases$COHORT_START_DATE, unit="days"))
  vaccinated_cases$delta_vacc_covid
  
  # variable list for long format transformation--------------------------------
  vars<-c("ID",
          "GEST_WEEK_1ST_VACC",
          "calendar_month",
          "region",
          "pr_score_full",
          "COV_AGE",
          "age_sq",
          "COV_DEPRIVATION",
          "COHORT_START_DATE",
          "COHORT_END_DATE"
  )
  
  # data transform to long format for becoming vaccinated ----------------------
  td_data <-
    tmerge(
      data1 = vaccinated_cases %>% select(all_of(vars), vacc_covid_date, overall_event_time), 
      data2 = vaccinated_cases %>% select(all_of(vars), overall_event, fup, delta_vacc_covid),
      id = ID, 
      overall_event = event(fup,  overall_event), 
      vacc_covid = tdc(delta_vacc_covid)
    )
  
  # split long format dataframe into pre and post vaccination ------------------
  with_expo_postexpo <- td_data %>% filter(vacc_covid==1)
  with_expo_preexpo <- td_data %>% filter(vacc_covid==0)
  with_expo_postexpo <- with_expo_postexpo %>% rename(t0=tstart, t=tstop) %>% mutate(tstart=0, tstop=t-t0)
  
  cuts_weeks_since_expo <- c(2)
  
  # data transform to long format for time from vaccination for those vaccinated-
  with_expo_postexpo<-survSplit(Surv(tstop, overall_event) ~., with_expo_postexpo,
                                cut=cuts_weeks_since_expo*7,
                                episode ="Weeks_category")
  
  
  # prepare dataframe for those  with vaccination exposure ---------------------
  with_expo_postexpo <- with_expo_postexpo %>% mutate(tstart=tstart+t0, tstop=tstop+t0) %>% dplyr::select(-c(t0,t))
  with_expo_preexpo$Weeks_category <- 0
  ls_with_expo <- list(with_expo_preexpo, with_expo_postexpo)
  with_expo <- do.call(rbind, lapply(ls_with_expo, function(x) x[match(names(ls_with_expo[[1]]), names(x))]))
  rm(list=c("ls_with_expo", "with_expo_preexpo", "with_expo_postexpo"))
  with_expo  <- with_expo %>%
    group_by(ID) %>% arrange(Weeks_category) %>% mutate(last_step = ifelse(row_number()==n(),1,0))
  with_expo$overall_event  <- with_expo$overall_event * with_expo$last_step
  
  # prepare dataframe for those  without infection -----------------------------
  without_expo <- data[which(data$vacc_covid==0),]
  without_expo$tstart<- c(0)
  without_expo$tstop <- ifelse(without_expo$fup ==0,  without_expo$fup + 0.001, without_expo$fup) # right now this isn't doing anything because I excluded those with 0 fup.
  without_expo$vacc_covid<- c(0)
  without_expo$Weeks_category <- c(0)
  without_expo$last_step <- c(1)
  without_expo$hospitalised <- c(1)
  without_expo_noncases <- without_expo[which(without_expo$overall_event==0),]
  noncase_ids <- unique(without_expo_noncases$ID)
  
  # combine unvaccinated and vaccinated exposure dataframes --------------------
  with_expo_cols <- colnames(with_expo)
  with_expo_cols
  without_expo <- without_expo %>% dplyr::select(all_of(with_expo_cols))
  data_surv <-rbind(with_expo, without_expo)
  rm(list=c("with_expo", "without_expo"))
  
  data_surv$days_to_expo<-as.numeric(difftime(data_surv$vacc_covid_date, data_surv$COHORT_START_DATE, unit="days"))
  
  # Pivot Wide for time intervals (exposure) -----------------------------------
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
  
  # final dataframe ------------------------------------------------------------
  survival_data <- data_surv 
  
  # set parameters--------------------------------------------------------------
  event_model <- c(paste0(z))
  adjustment <-  c("non_adjusted", "adjusted", "full_adjusted")
  analysis <-  c("week2")
  
  cov <-  c("E1", "E2")
  
  cov_model <-  c("E1 + E2 + GEST_WEEK_1ST_VACC",
                  "E1 + E2 + GEST_WEEK_1ST_VACC + COV_AGE  +  age_sq  + COV_DEPRIVATION ",
                  "E1 + E2 + GEST_WEEK_1ST_VACC + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month) + region")
  
  # run models------------------------------------------------------------------
  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    cat(paste0(sens,' ',substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(s$coefficients[1:2,], 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-3444344212-1812383397-2900750023-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/2nd_vaccination_out_HRs.csv"), append = TRUE)
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

# cox model function overall follow-up period ----------------------------------
cox_model_full <- function(k, y, z) {
  
  data$fup<-as.numeric(difftime(data$COHORT_END_DATE, data$COHORT_START_DATE, unit="days"))
  ls(data)
  
  # define dataframe for outcomes at birth -------------------------------------
  if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
    data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
    data$pr_score_full <- data$pr_score_birth
    data$pr_score_no_hx_preg <- data$pr_score_no_hx_preg_birth
    data$pr_score_no_ethn <- data$pr_score_no_ethn_birth
    data$pr_score_no_health_cond <- data$pr_score_no_health_cond_birth
  }
  
  # prepare for data transform to long format -----------------------------------
  data$fup<-as.numeric(difftime(data$COHORT_END_DATE, data$COHORT_START_DATE, unit="days"))
  
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
                                     data$overall_event==1 & data$COHORT_START_DATE<=data$overall_pregn_event_date & data$overall_pregn_event_date<=data$COHORT_END_DATE ~ 1,
                                     data$overall_event==1 & (data$COHORT_START_DATE>data$overall_pregn_event_date | data$overall_pregn_event_date>data$COHORT_END_DATE) ~ 0
                                     
    )
    )
  table(data$overall_event)
  
  data$overall_pregn_event_date[data$overall_event==0] <- NA
  
  data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$COHORT_START_DATE, unit="days"))
  
  data$age_sq <- data$COV_AGE^2
  
  # choose exposure ------------------------------------------------------------
  data$vacc_covid <- data$VACC_2ND_DUR_FLAG 
  
  data$VACC_2ND_DUR_DATE<- as.Date(data$VACC_2ND_DUR_DATE, "%d/%m/%Y")
  data$vacc_covid_date <- data$VACC_2ND_DUR_DATE
  
  # split rows in those who were vaccinated and those who were not vaccinated --
  vacc_cases <- data[which(data$vacc_covid==1),]
  vacc_cases$delta_vacc_covid<-as.numeric(difftime(vacc_cases$vacc_covid_date, vacc_cases$COHORT_START_DATE, unit="days"))
  vacc_cases$delta_vacc_covid
  
  # variable list for long format transformation--------------------------------
  vars<-c("ID",
          "GEST_WEEK_1ST_VACC",
          "region",
          "calendar_month",
          "pr_score_full",
          "COV_AGE",
          "age_sq",
          "COV_DEPRIVATION",
          "COHORT_START_DATE",
          "COHORT_END_DATE"
  )
  
  # data transform to long format for time from vaccination for those vaccinated-
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
  with_expo_cols <- colnames(td_data)
  with_expo_cols
  without_expo <- without_expo %>% dplyr::select(all_of(with_expo_cols))
  data_surv <-rbind(td_data, without_expo)
  rm(list=c("td_data", "without_expo"))
  
  # final dataframe ------------------------------------------------------------
  survival_data <- data_surv
  
  # set parameters--------------------------------------------------------------
  event_model <- c(paste0(z))
  adjustment <-  c("non_adjusted", "adjusted", "full_adjusted")
  analysis <-  c("full")
  
  cov <-  c("vacc_covid")
  
  cov_model <-  c("vacc_covid + GEST_WEEK_1ST_VACC",
                  "vacc_covid + GEST_WEEK_1ST_VACC + COV_AGE  +  age_sq  + COV_DEPRIVATION ",
                  "vacc_covid + GEST_WEEK_1ST_VACC + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month) + region")
  
  # run models------------------------------------------------------------------
  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    k <- s$coefficients[1,,drop=F]
    cat(paste0(sens,' ',substring(event_model,1,10),' ',analysis,' ', i,' ',cov, apply(k, 1,
                                                                              function(x) {
                                                                                paste0(" ", round(exp(x[1]), 3),
                                                                                       ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                                       ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                                       " ", round((x[3]), 4),
                                                                                       " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-3444344212-1812383397-2900750023-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/2nd_vaccination_out_HRs.csv"), append = TRUE)
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
sensitivity_analyses("total")
sensitivity_analyses("sens")
sensitivity_analyses("sens_covid")
sensitivity_analyses("sens_no_early_preg")

# include model with more than 10 events----------------------------------------
library(readr)
library(plyr)

out <- read_table2("C:/ProgramData/UserDataFolders/S-1-5-21-3444344212-1812383397-2900750023-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/2nd_vaccination_out_HRs.csv")
nrow(out)
out <- out[which(out$n.event > 10),]
out$n.event <- round_any(out$n.event, 5)
nrow(out)
data.table::fwrite(out,"C:/ProgramData/UserDataFolders/S-1-5-21-3444344212-1812383397-2900750023-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/2nd_vaccination_out_HRs.csv")
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
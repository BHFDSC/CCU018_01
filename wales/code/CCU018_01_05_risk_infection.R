################################################################################
## TITLE: Data preparation of Wales dataset for CCU018_01
##
## Description: Cox models to examine the associations between COVID-19 vaccination
## (1st dose) and infection
##
## By : Elena Raffetti
##
################################################################################

# load data --------------------------------------------------------------------
rm(list = ls())
data <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis_vaccination.csv",
                          data.table = FALSE)

# install.packages -------------------------------------------------------------
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
#install.packages("plyr")

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

# format dates -----------------------------------------------------------------
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")

data$follow_up_time<-as.numeric(difftime(data$DELIVERY_DATE, data$PREG_START_DATE, unit="days"))
summary(data$follow_up_time)
(sum(data$follow_up_time))/365.25

data<-data %>%  rename(ID=PERSON_ID)


writeLines(c("analysis event adj week estimate conf.low conf.high stad.error n.event subgroup"), paste0('S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/out_risk_covid_infection.csv'))

# start function ---------------------------------------------------------------
risk_covid_analysis <- function(q) {

if (q == "vaccination_era"){
    data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
    data <- data[which(data$PREG_START_DATE >= '2020-12-08'),]
  }

if (q == "vaccination_era_noprevcovid"){
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data <- data[which(data$PREG_START_DATE >= '2020-12-08'),]
  data <- data[which(data$EXP_COVID_PRE_BIN == 0),]
}

  # define vector with analysis information to include in the output, call in map2 function
sens <- c(paste0(q))

# date format ------------------------------------------------------------------
data$EXP_PRE_COVID_1ST_DATE <- as.Date(data$EXP_PRE_COVID_1ST_DATE, "%d/%m/%Y")

################## calendar period variable ###################
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
date_start <- ymd("2019/08/01")
data$calendar_month <- ((year(data$PREG_START_DATE) - year(date_start)) * 12) + month(data$PREG_START_DATE) - month(date_start)


# prepare propensity scores ----------------------------------------------------
data_ps <- data
ls(data_ps)

data_ps$exposure  <- data_ps$VACC_1ST_DUR_FLAG

vars <- names(data_ps) %in% c("ID", "exposure", "COV_HX_OTHER_DIS", "COV_HX_CVD_HEM", "COV_HX_DEPRESSION_FLAG", "COV_ETHNICITY_3lev", 
                              "COV_HX_BMI_OBESITY_FLAG", "COV_HX_HYPERTENSIVE_DIS", "COV_HX_PCOS_FLAG", "COV_HX_DIABETES_DIS",
                              "COV_HX_PREGNANCY_MAX", "COV_SMOKING_STATUS")

table(is.na(data_ps$COV_SMOKING_STATUS))
table(is.na(data_ps$COV_HX_PREGNANCY_MAX))
table(is.na(data_ps$COV_HX_DIABETES_DIS))
table(is.na(data_ps$COV_HX_PCOS_FLAG))
table(is.na(data_ps$COV_HX_HYPERTENSIVE_DIS))
table(is.na(data_ps$COV_HX_BMI_OBESITY_FLAG))
table(is.na(data_ps$COV_ETHNICITY_3lev))
table(is.na(data_ps$COV_HX_DEPRESSION_FLAG))
table(is.na(data_ps$COV_HX_CVD_HEM))
table(is.na(data_ps$COV_HX_OTHER_DIS))
table(is.na(data_ps$exposure))

data_ps <- data_ps[vars]
data_ps <- data_ps[complete.cases(data_ps),]

prop_model_full <- glm(exposure ~  COV_HX_PREGNANCY_MAX + COV_HX_HYPERTENSIVE_DIS + COV_HX_DIABETES_DIS + COV_HX_OTHER_DIS  + COV_HX_BMI_OBESITY_FLAG + COV_HX_CVD_HEM  + COV_HX_DEPRESSION_FLAG
                       + COV_ETHNICITY_3lev + COV_SMOKING_STATUS, data= data_ps, family=binomial(link="logit"))


summary(prop_model_full)
exp(cbind("Odds ratio" = coef(prop_model_full), confint.default(prop_model_full, level=0.95)))

data_ps$pr_score_full <- predict(prop_model_full,type="response")

#this plots the distribution of the estimated propensity score
ggplot(data_ps ,aes (x = pr_score_full)) +
  geom_histogram(color = "white") +
  facet_wrap(~ exposure) +
  xlab("Probability of being vaccinated") +
  theme_bw()

# combine propensity score with main data --------------------------------------
data_ps <- data_ps %>% select (ID, pr_score_full)
data %>% count(VACC_1ST_DUR_FLAG)
data <- merge(data,data_ps, by=c("ID"), all = TRUE)
data %>% count(VACC_1ST_DUR_FLAG)

# define date ------------------------------------------------------------------
data$EXP_RECORD_DATE <- as.Date(data$VACC_1ST_DUR_DATE, "%d/%m/%Y") 
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")

data$EXP_DUR_COVID_1ST_DATE<- as.Date(data$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y") 
data$EXP_DUR_COVID_1ST_DATE <- ifelse(data$EXP_DUR_COVID_1ST_DATE=='1970/01/01', NA, data$EXP_DUR_COVID_1ST_DATE)

# define calendar period variable-----------------------------------------------
data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
date_start <- ymd("2019/08/01")
data$calendar_month <- ((year(data$PREG_START_DATE) - year(date_start)) * 12) + month(data$PREG_START_DATE) - month(date_start)

# cox model function -----------------------------------------------------------
 risk_covid <- function(k, y, z) {
  
  # prepare for data transform to long format ----------------------------------
  data$fup<-as.numeric(difftime(data$DELIVERY_DATE, data$PREG_START_DATE, unit="days"))
  ls(data)
  
  # set event ------------------------------------------------------------------
  data$overall_event<-data[[k]]
  
  # set event date--------------------------------------------------------------
  data$overall_pregn_event_date<-data[[y]]
  nrow(data[!is.na(data$overall_pregn_event_date),])
  
  data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
  data$overall_event_time<-as.numeric(difftime(data$overall_pregn_event_date, data$PREG_START_DATE, unit="days"))
  data$age_sq <- data$COV_AGE^2
  
  # choose exposure  -----------------------------------------------------------
  data$covid_vacc <- data$VACC_1ST_DUR_FLAG
  data$covid_vacc_date <- data$VACC_1ST_DUR_DATE
  
  # split rows in those who were vaccinated and those who were not vaccinated --
  vaccination_cases <- data[which(data$covid_vacc==1),]
  vaccination_cases$delta_cov_vacc<-as.numeric(difftime(vaccination_cases$covid_vacc_date, vaccination_cases$PREG_START_DATE, unit="days"))
  vaccination_cases$delta_cov_vacc
  vaccination_cases$delta_cov_vacc[vaccination_cases$delta_cov_vacc<0] <- 0
  
  # variable list for long format transformation--------------------------------
  vars<-c("calendar_month",
          "ID",
          "pr_score_full",
          "COV_AGE",
          "age_sq",
          "COV_DEPRIVATION",
          "PREG_START_DATE",
          "DELIVERY_DATE",
          "agegroup"
  )

  # data transform to long format for becoming vaccinated ----------------------
  td_data <-
    tmerge(
      data1 = vaccination_cases %>% select(all_of(vars), covid_vacc_date, overall_event_time), 
      data2 = vaccination_cases %>% select(all_of(vars), overall_event, fup, delta_cov_vacc),
      id = ID, 
      overall_event = event(fup,  overall_event), 
      covid_vacc = tdc(delta_cov_vacc)
    )
  
  # split long format dataframe into pre and post vaccination ------------------
  with_expo_postexpo <- td_data %>% filter(covid_vacc==1)
  with_expo_preexpo <- td_data %>% filter(covid_vacc==0)
  with_expo_postexpo <- with_expo_postexpo %>% rename(t0=tstart, t=tstop) %>% mutate(tstart=0, tstop=t-t0)
  
  cuts_weeks_since_expo <- c(4, 8, 50)
  cuts_weeks_since_expo
  
  # data transform to long format for time from vaccination for those vaccinated
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
  
  # prepare dataframe for those  without vaccination ---------------------------
  without_expo <- data[which(data$covid_vacc==0),]
  without_expo$tstart<- c(0)
  without_expo$tstop <- ifelse(without_expo$fup ==0,  without_expo$fup + 0.001, without_expo$fup)
  without_expo$covid_vacc<- c(0)
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
  
  data_surv$days_to_expo<-as.numeric(difftime(data_surv$covid_vacc_date, data_surv$PREG_START_DATE, unit="days"))
  
  # pivot Wide for time intervals (exposure) -----------------------------------
  interval_names <- mapply(function(x, y) ifelse(x == y, paste0("week", x), paste0("week", x, "_", y)),
                           lag(cuts_weeks_since_expo, default = 0)+1,
                           cuts_weeks_since_expo,
                           SIMPLIFY = FALSE)
  cat("...... interval_names...... \n")
  print(interval_names)
  intervals <- mapply(c, lag(cuts_weeks_since_expo, default = 0)+1, cuts_weeks_since_expo, SIMPLIFY = F)
  
  
  i<-0
  for (ls in mapply(list, interval_names, intervals, SIMPLIFY = F)){
    i <- i+1
    print(paste(c(ls, i), collapse="..."))
    data_surv[[ls[[1]]]] <- if_else(data_surv$Weeks_category==i, 1, 0)
  }
  
  data_surv %>% filter(tstart>0) %>% arrange(ID)%>% head(10) %>% print(n = Inf) 
  data_surv %>% filter(days_to_expo==overall_event_time) %>% arrange(ID)%>% head(10) %>% print(n = Inf) #replaced overall_event_date with overall_event_time
 
  nrow(data_surv)
  data_surv<- data_surv[which(data_surv$Weeks_category!=4),]
  nrow(data_surv)
  
  # final dataframe ------------------------------------------------------------
  survival_data <- data_surv 
  survival_data <- survival_data[which(survival_data$Weeks_category!=4),]

  # set parameters--------------------------------------------------------------
  event_model <- c(paste0(z))
  adjustment <-  c("non_adjusted", "adjusted", "full_adjusted")
  
  cov <-  c("week1_4", "week5_8", "week9_50")
  
  cov_model <-  c("week1_4 + week5_8 + week9_50",
                  "week1_4 + week5_8 + week9_50 + COV_AGE  +  age_sq  + COV_DEPRIVATION ",
                  "week1_4 + week5_8 + week9_50 + COV_AGE  +  age_sq  + COV_DEPRIVATION + bs(pr_score_full) + bs(calendar_month)")
  
  # run models------------------------------------------------------------------
  map2(cov_model, adjustment, function(x,i) {
    f <- as.formula(paste("Surv(time = tstart, time2 = tstop, event = overall_event) ~ ",  x))
    model <- coxph(f, survival_data, id = ID)
    model$call$formula <- f
    s <- summary(model)
    cat(paste0(sens,' ',substring(event_model,1,10),' ', i,' ',cov, apply(s$coefficients[1:3,], 1,
                                                                 function(x) {
                                                                   paste0(" ", round(exp(x[1]), 3),
                                                                          ' ', round(exp(x[1] - 1.96 * x[3]), 3),
                                                                          ' ', round(exp(x[1] + 1.96 * x[3]), 3),
                                                                          " ", round((x[3]), 4),
                                                                          " ", summary(model)$nevent)}),
               collapse = '\n'), '\n', sep = '', file = paste0("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/out_risk_covid_infection.csv"), append = TRUE)
    invisible(model)
  })
  
 }  
   risk_covid ("EXP_COVID_BIN", "EXP_DUR_COVID_1ST_DATE", "COVID_INFECTION")

}
risk_covid_analysis("vaccination_era")
risk_covid_analysis("vaccination_era_noprevcovid")

#include model with more than 10 events-----------------------------------------
library(readr)
library(plyr)
out <- read_table2("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/out_risk_covid_infection.csv")
nrow(out)
View(out)
out <- out[which(out$n.event > 10),]
out$n.event <- round_any(out$n.event, 5)
nrow(out)
data.table::fwrite(out,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/out_risk_covid_infection_to_export.csv")
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
  
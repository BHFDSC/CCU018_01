################################################################################
## TITLE: Data preparation of Wales dataset for CCU018_01
##
## Description: Prepare summary outcomes
##
## By : Elena Raffetti
##
################################################################################

# install.packages -------------------------------------------------------------
#install.packages("dplyr")
#install.packages("data.table")
#install.packages("purrr")

# load required packages -------------------------------------------------------
library(dplyr)
library(data.table)
library(purrr)

dev.off()

# load data --------------------------------------------------------------------
data <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis.csv",
                          data.table = FALSE)

# infection analyses -----------------------------------------------------------
data<-data %>%  rename(ID=PERSON_ID)

writeLines(c("analysis event n fu n_event IR"), paste0('S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/summary_outcomes.csv'))

# function for infection analyses ----------------------------------------------
analyses <- function(q) {
  
  # define format dates --------------------------------------------------------
  if (q == "infection_analysis_pre_vaccination"){
    data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
    data <- data[which(data$DELIVERY_DATE < '2020-12-08'),]
  }
  
  if (q == "infection_analysis_vaccination"){
    data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
    data <- data[which(data$PREG_START_DATE >= '2020-12-08'),]
    data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  }
  
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
  data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")

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
  
  data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE=='1970/01/01', NA, data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE)
  data$OUT_DUR_GEST_DIABETES_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_DIABETES_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_GEST_DIABETES_RECORD_DATE)
  data$OUT_DUR_PREECLAMPSIA_RECORD_DATE <- ifelse(data$OUT_DUR_PREECLAMPSIA_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_PREECLAMPSIA_RECORD_DATE)

  # rename outcomes ------------------------------------------------------------
  data<-data %>%
    rename(OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES,
           OUT_DUR_PREECLAMPSIA  = OUT_BIN_PREECLAMPSIA,
           OUT_DUR_GEST_HYPERTENSION  = OUT_BIN_GEST_HYPERTENSION)
  
  # start table function for 1st vaccination analysis --------------------------
  table_outcome <- function(k, y, z) {

    if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE"){
      data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
    }
    
    # set event ----------------------------------------------------------------
    data$overall_event<-data[[k]]
    table(data$overall_event)
    
    # set event date -----------------------------------------------------------
    data$overall_pregn_event_date<-data[[y]]
    nrow(data[!is.na(data$overall_pregn_event_date),])

    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, "%d/%m/%Y")
    
    data <- data %>%
      mutate (COHORT_END_DATE = case_when(is.na(data$overall_event) | data$overall_event==0 ~ data$DELIVERY_DATE,
                                          data$overall_event==1 ~ data$overall_pregn_event_date
                                                 )
              )
    
    data$follow_up<-as.numeric(difftime(data$DELIVERY_DATE, data$PREG_START_DATE, unit="days"))

    # count function -----------------------------------------------------------
    overview <- data %>%
      summarise (
        N = n(),
        person_time_follow_up = sum(data$follow_up/365.25),
        N_events = sum(data$overall_event),
        IR_per_1000 = (N_events/person_time_follow_up) *1000
      )

    # include parameters in the output -----------------------------------------
    event_model <- c(paste0(z))
    analysis <- c(paste0(q))

    cat(paste0(analysis,' ',substring(event_model,1,10),' ',overview$N,' ',round(overview$person_time_follow_up,2),' ',overview$N_events,' ',round(overview$IR_per_1000,2),
               collapse = '\n'), '\n', sep = '', file = paste0("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/summary_outcomes.csv"), append = TRUE)

  }
  table_outcome("OUT_DUR_GEST_DIABETES", "OUT_DUR_GEST_DIABETES_RECORD_DATE", "GEST_DIABETES")
  table_outcome("OUT_DUR_GEST_HYPERTENSION", "OUT_DUR_GEST_HYPERTENSION_RECORD_DATE", "GEST_HYPERTENSION")
  table_outcome("OUT_DUR_PREECLAMPSIA", "OUT_DUR_PREECLAMPSIA_RECORD_DATE", "PREECLAMPSIA")
  table_outcome("OUT_BIN_BIRTH_PRETERM", "OUT_BIRTH_PRETERM_RECORD_DATE", "PRETERM")
  table_outcome("OUT_BIN_BIRTH_VERY_PRETERM", "OUT_BIRTH_VERY_PRETERM_RECORD_DATE", "VERY_PRETERM")
  table_outcome("OUT_BIN_BIRTH_SMALL_GEST_AGE", "OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE", "SMALL_GEST_AGE")
  
}
analyses("infection_analysis_total")
analyses("infection_analysis_pre_vaccination")
analyses("infection_analysis_vaccination")


# load data --------------------------------------------------------------------
data <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis_vaccination.csv",
                          data.table = FALSE)

# 1st vaccination analysis -----------------------------------------------------
data<-data %>%  rename(ID=PERSON_ID)
  
  # define format dates ---------------------------------------------------------
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y") 
  data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  
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
  
  #replace with NA if date is 1 Jan 1970
  data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE=='1970/01/01', NA, data$OUT_DUR_GEST_HYPERTENSION_RECORD_DATE)
  data$OUT_DUR_GEST_DIABETES_RECORD_DATE <- ifelse(data$OUT_DUR_GEST_DIABETES_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_GEST_DIABETES_RECORD_DATE)
  data$OUT_DUR_PREECLAMPSIA_RECORD_DATE <- ifelse(data$OUT_DUR_PREECLAMPSIA_RECORD_DATE=='1970-01-01', NA, data$OUT_DUR_PREECLAMPSIA_RECORD_DATE)
  
  # rename outcomes ------------------------------------------------------------
  data<-data %>%
    rename(OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES,
           OUT_DUR_PREECLAMPSIA  = OUT_BIN_PREECLAMPSIA,
           OUT_DUR_GEST_HYPERTENSION  = OUT_BIN_GEST_HYPERTENSION)

  # start table function for vaccination analysis ------------------------------
  table_outcome <- function(k, y, z) {
    
    ls(data)
    
    # set dataframe for outcomes at birth
    if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE"){
      data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
    }
    
    # set event ----------------------------------------------------------------
    data$overall_event<-data[[k]]
    table(data$overall_event)
    
    data$overall_pregn_event_date<-data[[y]]
    nrow(data[!is.na(data$overall_pregn_event_date),])
    
    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, "%d/%m/%Y")
    
    data <- data %>%
      mutate (COHORT_END_DATE = case_when(is.na(data$overall_event) | data$overall_event==0 ~ data$DELIVERY_DATE,
                                          data$overall_event==1 ~ data$overall_pregn_event_date
      )
      )
    
    data$follow_up<-as.numeric(difftime(data$DELIVERY_DATE, data$PREG_START_DATE, unit="days"))
    
    # count function -----------------------------------------------------------
    overview <- data %>%
      summarise (
        N = n(),
        person_time_follow_up = sum(data$follow_up/365.25),
        N_events = sum(data$overall_event),
        IR_per_1000 = (N_events/person_time_follow_up) *1000
      )

    # include parameters in the output -----------------------------------------
    event_model <- c(paste0(z))
    analysis <- c(paste0("vaccination_analysis"))
    
    cat(paste0(analysis,' ',substring(event_model,1,10),' ',overview$N,' ',round(overview$person_time_follow_up, 2),' ',overview$N_events,' ',round(overview$IR_per_1000,2),
               collapse = '\n'), '\n', sep = '', file = paste0("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/summary_outcomes.csv"), append = TRUE)

  }
  table_outcome("OUT_DUR_GEST_DIABETES", "OUT_DUR_GEST_DIABETES_RECORD_DATE", "GEST_DIABETES")
  table_outcome("OUT_DUR_GEST_HYPERTENSION", "OUT_DUR_GEST_HYPERTENSION_RECORD_DATE", "GEST_HYPERTENSION")
  table_outcome("OUT_DUR_PREECLAMPSIA", "OUT_DUR_PREECLAMPSIA_RECORD_DATE", "PREECLAMPSIA")
  table_outcome("OUT_BIN_BIRTH_PRETERM", "OUT_BIRTH_PRETERM_RECORD_DATE", "PRETERM")
  table_outcome("OUT_BIN_BIRTH_VERY_PRETERM", "OUT_BIRTH_VERY_PRETERM_RECORD_DATE", "VERY_PRETERM")
  table_outcome("OUT_BIN_BIRTH_SMALL_GEST_AGE", "OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE", "SMALL_GEST_AGE")
  
# prepare data to export -------------------------------------------------------
library(readr)
library(plyr)
out <- read_table2("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/summary_outcomes.csv")
nrow(out)
out <- out[which(out$n_event > 10),]
out$n_event <- round_any(out$n_event, 5)
nrow(out)
data.table::fwrite(out,"S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/summary_outcomes.csv")
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
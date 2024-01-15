################################################################################
## TITLE: Data preparation of England dataset for CCU018_01
##
## Description: Prepare summary outcomes
##
## By : Elena Raffetti
##
################################################################################

writeLines(c("analysis event n fu n_event IR"), paste0('C:/ProgramData/UserDataFolders/S-1-5-21-125356058-1516156398-375133408-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/summary_outcomes.csv'))

# install.packages -------------------------------------------------------------
#install.packages("dplyr")
#install.packages("data.table")

# load required packages -------------------------------------------------------
library(dplyr)
library(data.table)

dev.off()

# function to round to the nearest 5 -------------------------------------------
round_to_5 <- function(x) {
  round(as.numeric(x) / 5) * 5
}

# infection analyses -----------------------------------------------------------
data <- data_temp4
data<-data %>%  rename(ID=PERSON_ID)

# function for infection analyses ----------------------------------------------
analyses <- function(q) {
  
  data$COHORT_END_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")

  
  if (q == "infection_analysis_pre_vaccination"){
    
    data$COHORT_END_DATE <- as.Date(data$COHORT_END_DATE, "%d/%m/%Y")
    data <- data[which(data$COHORT_END_DATE < '2020-12-08'),]
  }
  
  if (q == "infection_analysis_vaccination"){
    data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
    data <- data[which(data$PREG_START_DATE >= '2020-12-08'),]
    
    data$COHORT_END_DATE <- as.Date(data$COHORT_END_DATE, "%d/%m/%Y")
  }
  
  # define format dates --------------------------------------------------------
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$OUT_DUR_PREECLAMPSIA_DATE <- as.Date(data$OUT_DUR_PREECLAMPSIA_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_DIABETES_DATE <- as.Date(data$OUT_DUR_GEST_DIABETES_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_HYPERTENSION_DATE <- as.Date(data$OUT_DUR_GEST_HYPERTENSION_DATE, "%d/%m/%Y")
  data$OUT_DUR_VENOUS_DATE <- as.Date(data$OUT_DUR_VENOUS_DATE, "%d/%m/%Y")
  
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
  
  # rename outcomes --------------------------------------------------------------
  data<-data %>%
    rename(OUT_DUR_VENOUS = OUT_BIN_DUR_VENOUS,
           OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES,
           OUT_DUR_PREECLAMPSIA  = OUT_BIN_PREECLAMPSIA,
           OUT_DUR_GEST_HYPERTENSION  = OUT_BIN_GEST_HYPERTENSION)
  
  # start table function for infection analysis ----------------------------------
  table_outcome <- function(k, y, z) {
    
  if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
    data <- data[which(data$PREG_START_DATE_ESTIMATED==0),]
  }

    # set event ----------------------------------------------------------------
    data$overall_event<-data[[k]]
    table(data$overall_event)

    # set event date ----------------------------------------------------------------
    data$overall_pregn_event_date<-data[[y]]
    nrow(data[!is.na(data$overall_pregn_event_date),])
    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, origin='1970-01-01')
    
    
    data$overall_pregn_event_date <- as.Date(data$overall_pregn_event_date, "%d/%m/%Y")
    
    data <- data %>%
      mutate     (COHORT_END_DATE = case_when(  is.na(data$overall_event) |  data$overall_event==0 ~  data$COHORT_END_DATE,
                                                data$overall_event==1 ~   data$overall_pregn_event_date
      )
      )
    
    data$follow_up<-as.numeric(difftime(data$COHORT_END_DATE, data$PREG_START_DATE, unit="days"))
    head(data$follow_up)

    # count function ----------------------------------------------------------
    overview <- data %>% 
      summarise(
        N = n(),
        N = round_to_5(N),
        person_time_follow_up = sum(data$follow_up/365.25),
        N_events = sum(data$overall_event),
        N_events = round_to_5(N_events),
        IR_per_1000 = (N_events / person_time_follow_up) * 1000
      )

    # include parameters in the output -----------------------------------------
    event_model <- c(paste0(z))
    analysis <- c(paste0(q))

cat(paste0(analysis,' ',substring(event_model,1,10),' ',overview$N,' ',overview$person_time_follow_up,' ',overview$N_events,' ',overview$IR_per_1000, collapse = '\n'), '\n', sep = '', 
  file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-125356058-1516156398-375133408-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/summary_outcomes.csv"), append = TRUE)

}
  table_outcome("OUT_DUR_GEST_DIABETES", "OUT_DUR_GEST_DIABETES_RECORD_DATE", "GEST_DIABETES")
  table_outcome("OUT_DUR_GEST_HYPERTENSION", "OUT_DUR_GEST_HYPERTENSION_RECORD_DATE", "GEST_HYPERTENSION")
  table_outcome("OUT_DUR_PREECLAMPSIA", "OUT_DUR_PREECLAMPSIA_RECORD_DATE", "PREECLAMPSIA")
  table_outcome("OUT_DUR_VENOUS", "OUT_DUR_VENOUS_RECORD_DATE", "VENOUS")
  table_outcome("OUT_BIN_BIRTH_PRETERM", "OUT_BIRTH_PRETERM_RECORD_DATE", "PRETERM")
  table_outcome("OUT_BIN_BIRTH_VERY_PRETERM", "OUT_BIRTH_VERY_PRETERM_RECORD_DATE", "VERY_PRETERM")
  table_outcome("OUT_BIN_BIRTH_SMALL_GEST_AGE", "OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE", "SMALL_GEST_AGE")
  table_outcome("OUT_BIN_BIRTH_STILLBIRTH_MAX", "OUT_BIRTH_STILLBIRTH_RECORD_DATE", "STILLBIRTH")
  
}
analyses("infection_analysis_total")
analyses("infection_analysis_pre_vaccination")
analyses("infection_analysis_vaccination")


# 1 st vaccination analysis ----------------------------------------------------
data <- data_temp6
data<-data %>%  rename(ID=PERSON_ID)
  
  # define format dates --------------------------------------------------------
  data$COHORT_END_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$OUT_DUR_PREECLAMPSIA_DATE <- as.Date(data$OUT_DUR_PREECLAMPSIA_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_DIABETES_DATE <- as.Date(data$OUT_DUR_GEST_DIABETES_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_HYPERTENSION_DATE <- as.Date(data$OUT_DUR_GEST_HYPERTENSION_DATE, "%d/%m/%Y")
  data$OUT_DUR_VENOUS_DATE <- as.Date(data$OUT_DUR_VENOUS_DATE, "%d/%m/%Y")
  
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
  
  # rename outcomes ------------------------------------------------------------
  data<-data %>%
    rename(OUT_DUR_VENOUS = OUT_BIN_DUR_VENOUS,
           OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES,
           OUT_DUR_PREECLAMPSIA  = OUT_BIN_PREECLAMPSIA,
           OUT_DUR_GEST_HYPERTENSION  = OUT_BIN_GEST_HYPERTENSION)
  
  # start table function for 1st vaccination analysis --------------------------
  table_outcome <- function(k, y, z) {
    
    if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
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
      mutate     (COHORT_END_DATE = case_when(  is.na(data$overall_event) |  data$overall_event==0 ~  data$COHORT_END_DATE,
                                                data$overall_event==1 ~   data$overall_pregn_event_date
      )
      )
    
    data$follow_up<-as.numeric(difftime(data$COHORT_END_DATE, data$PREG_START_DATE, unit="days"))
    head(data$follow_up)
    
    # count function -----------------------------------------------------------
    overview <- data %>% 
      summarise(
        N = n(),
        N = round_to_5(N),
        person_time_follow_up = sum(data$follow_up/365.25),
        N_events = sum(data$overall_event),
        N_events = round_to_5(N_events),
        IR_per_1000 = (N_events / person_time_follow_up) * 1000
      )
    
    # include parameters in the output -----------------------------------------
    event_model <- c(paste0(z))
    analysis <- c(paste0("vaccination_analysis"))
    
    cat(paste0(analysis,' ',substring(event_model,1,10),' ',overview$N,' ',overview$person_time_follow_up,' ',overview$N_events,' ',overview$IR_per_1000, collapse = '\n'), '\n', sep = '', 
        file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-125356058-1516156398-375133408-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/summary_outcomes.csv"), append = TRUE)
    
  }
  table_outcome("OUT_DUR_GEST_DIABETES", "OUT_DUR_GEST_DIABETES_RECORD_DATE", "GEST_DIABETES")
  table_outcome("OUT_DUR_GEST_HYPERTENSION", "OUT_DUR_GEST_HYPERTENSION_RECORD_DATE", "GEST_HYPERTENSION")
  table_outcome("OUT_DUR_PREECLAMPSIA", "OUT_DUR_PREECLAMPSIA_RECORD_DATE", "PREECLAMPSIA")
  table_outcome("OUT_DUR_VENOUS", "OUT_DUR_VENOUS_RECORD_DATE", "VENOUS")
  table_outcome("OUT_BIN_BIRTH_PRETERM", "OUT_BIRTH_PRETERM_RECORD_DATE", "PRETERM")
  table_outcome("OUT_BIN_BIRTH_VERY_PRETERM", "OUT_BIRTH_VERY_PRETERM_RECORD_DATE", "VERY_PRETERM")
  table_outcome("OUT_BIN_BIRTH_SMALL_GEST_AGE", "OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE", "SMALL_GEST_AGE")
  table_outcome("OUT_BIN_BIRTH_STILLBIRTH_MAX", "OUT_BIRTH_STILLBIRTH_RECORD_DATE", "STILLBIRTH")
  
  
  # 2 nd vaccination analysis ----------------------------------------------------
  data <- data_temp7
  data<-data %>%  rename(ID=PERSON_ID)
  
  # define format dates ---------------------------------------------------------
  data$COHORT_END_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")#
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$DELIVERY_DATE <- as.Date(data$DELIVERY_DATE, "%d/%m/%Y")
  
  data$PREG_START_DATE <- as.Date(data$PREG_START_DATE, "%d/%m/%Y")
  data$OUT_DUR_PREECLAMPSIA_DATE <- as.Date(data$OUT_DUR_PREECLAMPSIA_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_DIABETES_DATE <- as.Date(data$OUT_DUR_GEST_DIABETES_DATE, "%d/%m/%Y")
  data$OUT_DUR_GEST_HYPERTENSION_DATE <- as.Date(data$OUT_DUR_GEST_HYPERTENSION_DATE, "%d/%m/%Y")
  data$OUT_DUR_VENOUS_DATE <- as.Date(data$OUT_DUR_VENOUS_DATE, "%d/%m/%Y")
  
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
  
  # rename outcomes ------------------------------------------------------------
  data<-data %>%
    rename(OUT_DUR_VENOUS = OUT_BIN_DUR_VENOUS,
           OUT_DUR_GEST_DIABETES = OUT_BIN_GEST_DIABETES,
           OUT_DUR_PREECLAMPSIA  = OUT_BIN_PREECLAMPSIA,
           OUT_DUR_GEST_HYPERTENSION  = OUT_BIN_GEST_HYPERTENSION)
  
  # start table function for 2nd vaccination analysis --------------------------
  table_outcome <- function(k, y, z) {
    
    if (z == "PRETERM" | z == "VERY_PRETERM" | z == "SMALL_GEST_AGE" | z == "STILLBIRTH"){
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
      mutate     (COHORT_END_DATE = case_when(  is.na(data$overall_event) |  data$overall_event==0 ~  data$COHORT_END_DATE,
                                                data$overall_event==1 ~   data$overall_pregn_event_date
      )
      )
    
    data$follow_up<-as.numeric(difftime(data$COHORT_END_DATE, data$PREG_START_DATE, unit="days"))
    head(data$follow_up)
    
    # count function -----------------------------------------------------------
    overview <- data %>% 
      summarise(
        N = n(),
        N = round_to_5(N),
        person_time_follow_up = sum(data$follow_up/365.25),
        N_events = sum(data$overall_event),
        N_events = round_to_5(N_events),
        IR_per_1000 = (N_events / person_time_follow_up) * 1000
      )
    
    # include parameters in the output -----------------------------------------
    event_model <- c(paste0(z))
    analysis <- c(paste0("vaccination_2dose_analysis"))
    
    cat(paste0(analysis,' ',substring(event_model,1,10),' ',overview$N,' ',overview$person_time_follow_up,' ',overview$N_events,' ',overview$IR_per_1000, collapse = '\n'), '\n', sep = '', 
        file = paste0("C:/ProgramData/UserDataFolders/S-1-5-21-125356058-1516156398-375133408-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/summary_outcomes.csv"), append = TRUE)
    
  }
  table_outcome("OUT_DUR_GEST_DIABETES", "OUT_DUR_GEST_DIABETES_RECORD_DATE", "GEST_DIABETES")
  table_outcome("OUT_DUR_GEST_HYPERTENSION", "OUT_DUR_GEST_HYPERTENSION_RECORD_DATE", "GEST_HYPERTENSION")
  table_outcome("OUT_DUR_PREECLAMPSIA", "OUT_DUR_PREECLAMPSIA_RECORD_DATE", "PREECLAMPSIA")
  table_outcome("OUT_DUR_VENOUS", "OUT_DUR_VENOUS_RECORD_DATE", "VENOUS")
  table_outcome("OUT_BIN_BIRTH_PRETERM", "OUT_BIRTH_PRETERM_RECORD_DATE", "PRETERM")
  table_outcome("OUT_BIN_BIRTH_VERY_PRETERM", "OUT_BIRTH_VERY_PRETERM_RECORD_DATE", "VERY_PRETERM")
  table_outcome("OUT_BIN_BIRTH_SMALL_GEST_AGE", "OUT_BIRTH_SMALL_GEST_AGE_RECORD_DATE", "SMALL_GEST_AGE")
  table_outcome("OUT_BIN_BIRTH_STILLBIRTH_MAX", "OUT_BIRTH_STILLBIRTH_RECORD_DATE", "STILLBIRTH")
  
# prepare data to export -------------------------------------------------------
library(readr)
library(plyr)
out <- read_table("C:/ProgramData/UserDataFolders/S-1-5-21-125356058-1516156398-375133408-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/summary_outcomes.csv")
View(out)
nrow(out)
out <- out[which(out$n_event > 10),]
nrow(out)
data.table::fwrite(out,"C:/ProgramData/UserDataFolders/S-1-5-21-125356058-1516156398-375133408-1012/My Files/Home Folder/collab/CCU018/CCU018_01/output/summary_outcomes.csv")
detach("package:plyr", unload = TRUE)
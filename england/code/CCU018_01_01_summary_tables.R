################################################################################
## TITLE: Data preparation of England dataset for CCU018_01
##
## Description: Prepare summary tables
##
## By : Elena Raffetti
##
################################################################################

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

# define dataframes that include sensitivity analyses ---------------------------
data_temp4$COHORT_END_DATE <- as.Date(data_temp4$DELIVERY_DATE, "%d/%m/%Y")

data_total <- data_temp4

data_pre_vacc <- data_temp4
nrow(data_pre_vacc)
data_pre_vacc$COHORT_END_DATE <- as.Date(data_pre_vacc$COHORT_END_DATE, "%d/%m/%Y")
data_pre_vacc <- data_pre_vacc[which(data_pre_vacc$COHORT_END_DATE < '2020-12-08'),]

data_missing_pre_vacc <- data_pre_vacc[which(data_pre_vacc$PREG_START_DATE_ESTIMATED==0),]

data_vacc <- data_temp4
data_vacc$PREG_START_DATE <- as.Date(data_vacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc <- data_vacc[which(data_vacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc$COHORT_END_DATE <- as.Date(data_vacc$COHORT_END_DATE, "%d/%m/%Y")
data_missing_post_vacc <- data_vacc[which(data_vacc$PREG_START_DATE_ESTIMATED==0),]

data <- data_temp6
data<-data %>%  rename(ID=PERSON_ID)
data <- data[which(data$VACC_1ST_DUR_FLAG==1),]

data_1st_vacc <- data_temp6
data_2nd_vacc <- data_temp7

data_missing <- data_temp4[which(data_temp4$PREG_START_DATE_ESTIMATED==0),]

data_hosp <-  data_temp4 %>%
  mutate     (EXP_COVID_BIN  = case_when(data_temp4$EXP_COVID_BIN ==0 ~ 0 ,
                                         data_temp4$EXP_COVID_BIN ==1 & data_temp4$EXP_DUR_HOSP==1 ~ 0,
                                         data_temp4$EXP_COVID_BIN ==1 & data_temp4$EXP_DUR_HOSP==0 ~ 1,
  )
  )


data_hosp$EXP_DUR_COVID_1ST_DATE <- as.Date(data_hosp$EXP_DUR_COVID_1ST_DATE, "%d/%m/%Y")
data_hosp <- data_hosp %>%
  mutate     (COHORT_END_DATE = case_when(  is.na(data_hosp$EXP_DUR_HOSP) |  data_hosp$EXP_DUR_HOSP==0 ~  data_hosp$COHORT_END_DATE,
                                            data_hosp$EXP_DUR_HOSP==1 ~   data_hosp$EXP_DUR_COVID_1ST_DATE
  )
  )

data_pand_init <- data_temp4
data_pand_init$PREG_START_DATE <- as.Date(data_pand_init$PREG_START_DATE, "%d/%m/%Y")
data_pand_init <- data_pand_init[which(data_pand_init$PREG_START_DATE >= '2020-03-11'),]
data_pand_init$COHORT_END_DATE <- as.Date(data_pand_init$COHORT_END_DATE, "%d/%m/%Y")

data_before_pand_init <- data_temp4
data_before_pand_init$PREG_START_DATE <- as.Date(data_before_pand_init$PREG_START_DATE, "%d/%m/%Y")
data_before_pand_init <- data_before_pand_init[which(data_before_pand_init$PREG_START_DATE < '2020-03-11'),]
data_before_pand_init$COHORT_END_DATE <- as.Date(data_before_pand_init$COHORT_END_DATE, "%d/%m/%Y")

data_vacc_anytime <- data_temp4
data_vacc_anytime$PREG_START_DATE <- as.Date(data_vacc_anytime$PREG_START_DATE, "%d/%m/%Y")
data_vacc_anytime <- data_vacc_anytime[which(data_vacc_anytime$PREG_START_DATE >= '2020-12-08'),]
data_vacc_anytime$COHORT_END_DATE <- as.Date(data_vacc_anytime$COHORT_END_DATE, "%d/%m/%Y")
data_vacc_anytime <- data_vacc_anytime[which(data_vacc_anytime$VACC_1ST_FLAG == 1),]

data_vacc_anytime_noearly_vacc <- data_temp4
data_vacc_anytime_noearly_vacc$PREG_START_DATE <- as.Date(data_vacc_anytime_noearly_vacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_vacc <- data_vacc_anytime_noearly_vacc[which(data_vacc_anytime_noearly_vacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc_anytime_noearly_vacc$COHORT_END_DATE <- as.Date(data_vacc_anytime_noearly_vacc$COHORT_END_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_vacc <- data_vacc_anytime_noearly_vacc[which(data_vacc_anytime_noearly_vacc$VACC_1ST_FLAG == 1),]
data_vacc_anytime_noearly_vacc$VACC_1ST_DATE <- as.Date(data_vacc_anytime_noearly_vacc$VACC_1ST_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_vacc <- data_vacc_anytime_noearly_vacc[which(data_vacc_anytime_noearly_vacc$VACC_1ST_DATE > '2021-06-18'),]

data_vacc_anytime_nonvacc <- data_temp4
data_vacc_anytime_nonvacc$PREG_START_DATE <- as.Date(data_vacc_anytime_nonvacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc_anytime_nonvacc <- data_vacc_anytime_nonvacc[which(data_vacc_anytime_nonvacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc_anytime_nonvacc <- data[which(data_vacc_anytime_nonvacc$VACC_1ST_FLAG == 0),]

data_vacc_anytime_1st_vacc <- data_temp6
data_vacc_anytime_1st_vacc$PREG_START_DATE <- as.Date(data_vacc_anytime_1st_vacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc_anytime_1st_vacc <- data_vacc_anytime_1st_vacc[which(data_vacc_anytime_1st_vacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc_anytime_1st_vacc$COHORT_END_DATE <- as.Date(data_vacc_anytime_1st_vacc$DELIVERY_DATE, "%d/%m/%Y")
data_vacc_anytime_1st_vacc <- data_vacc_anytime_1st_vacc[which(data_vacc_anytime_1st_vacc$VACC_1ST_FLAG == 1),]

table(data_temp6$EXP_COVID_BIN)

data_vacc_anytime_2nd_vacc <- data_temp7
data_vacc_anytime_2nd_vacc$PREG_START_DATE <- as.Date(data_vacc_anytime_2nd_vacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc_anytime_2nd_vacc <- data_vacc_anytime_2nd_vacc[which(data_vacc_anytime_2nd_vacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc_anytime_2nd_vacc$COHORT_END_DATE <- as.Date(data_vacc_anytime_2nd_vacc$DELIVERY_DATE, "%d/%m/%Y")
data_vacc_anytime_2nd_vacc <- data_vacc_anytime_2nd_vacc[which(data_vacc_anytime_2nd_vacc$VACC_1ST_FLAG == 1),]

data_missing_1st_vacc <- data_temp6[which(data_temp6$PREG_START_DATE_ESTIMATED==0),]
data_missing_2nd_vacc <- data_temp7[which(data_temp7$PREG_START_DATE_ESTIMATED==0),]

data_vacc_anytime_noearly_1st_vacc <- data_temp6
data_vacc_anytime_noearly_1st_vacc$PREG_START_DATE <- as.Date(data_vacc_anytime_noearly_1st_vacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_1st_vacc <- data_vacc_anytime_noearly_1st_vacc[which(data_vacc_anytime_noearly_1st_vacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc_anytime_noearly_1st_vacc$COHORT_END_DATE <- as.Date(data_vacc_anytime_noearly_1st_vacc$DELIVERY_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_1st_vacc <- data_vacc_anytime_noearly_1st_vacc[which(data_vacc_anytime_noearly_1st_vacc$VACC_1ST_FLAG == 1),]
data_vacc_anytime_noearly_1st_vacc$VACC_1ST_DATE <- as.Date(data_vacc_anytime_noearly_1st_vacc$VACC_1ST_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_1st_vacc <- data_vacc_anytime_noearly_1st_vacc[which(data_vacc_anytime_noearly_1st_vacc$VACC_1ST_DATE > '2021-06-18'),]

data_vacc_no_pre_covid <- data_temp6
data_vacc_no_pre_covid <- data_vacc_no_pre_covid[which(data_vacc_no_pre_covid$EXP_COVID_PRE_BIN == 0),]

data_vacc_2nd_vacc_no_pre_covid <- data_temp7
data_vacc_2nd_vacc_no_pre_covid <- data_vacc_2nd_vacc_no_pre_covid[which(data_vacc_2nd_vacc_no_pre_covid$EXP_COVID_PRE_BIN == 0),]

data_vacc_anytime_noearly_2nd_vacc <- data_temp7
data_vacc_anytime_noearly_2nd_vacc$PREG_START_DATE <- as.Date(data_vacc_anytime_noearly_2nd_vacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_2nd_vacc <- data_vacc_anytime_noearly_2nd_vacc[which(data_vacc_anytime_noearly_2nd_vacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc_anytime_noearly_2nd_vacc$COHORT_END_DATE <- as.Date(data_vacc_anytime_noearly_2nd_vacc$DELIVERY_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_2nd_vacc <- data_vacc_anytime_noearly_2nd_vacc[which(data_vacc_anytime_noearly_2nd_vacc$VACC_1ST_FLAG == 1),]
data_vacc_anytime_noearly_2nd_vacc$VACC_1ST_DATE <- as.Date(data_vacc_anytime_noearly_2nd_vacc$VACC_1ST_DATE, "%d/%m/%Y")
data_vacc_anytime_noearly_2nd_vacc <- data_vacc_anytime_noearly_2nd_vacc[which(data_vacc_anytime_noearly_2nd_vacc$VACC_1ST_DATE > '2021-06-18'),]

# count number of pregnant women included --------------------------------------
writeLines(c("description nrow"), paste0('D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/database_row_counts.csv'))
df <-  c("data_total", "data_vacc", "data_pre_vacc", "data_1st_vacc", "data_2nd_vacc","data_missing", "data_missing_pre_vacc", "data_missing_post_vacc", "data_hosp", "data_pand_init", "data_before_pand_init", "data_vacc_anytime","data_vacc_anytime_noearly_vacc", "data_vacc_anytime_nonvacc",
         "data_vacc_no_pre_covid","data_vacc_2nd_vacc_no_pre_covid","data_vacc_anytime_1st_vacc", "data_vacc_anytime_2nd_vacc", "data_missing_1st_vacc", "data_missing_2nd_vacc", "data_vacc_anytime_noearly_1st_vacc", "data_vacc_anytime_noearly_2nd_vacc")
purrr::map(df, function(i) {
  nrow_count <- nrow(get(i))
  cat(paste0(i,' ',nrow_count),
      '\n', sep = '', file = paste0("D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/database_row_counts.csv"), append = TRUE)
})

# prepare data to export -------------------------------------------------------
library(readr)
library(plyr)
out <- read_table2("D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/database_row_counts.csv")
nrow(out)
out$nrow <- round_any(out$nrow, 5)
nrow(out)
nrow(out)
data.table::fwrite(out,"D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/database_row_counts.csv")
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)

# prepare table for overall period ---------------------------------------------  
df <- data_temp4

df$COVID_STATUS <-ifelse(df$EXP_COVID_BIN==1, "COVID infection", "No COVID infection")
df$COVID_STATUS <- factor(df$COVID_STATUS, levels = c("COVID infection", "No COVID infection"))
df$COVID_STATUS = relevel(df$COVID_STATUS, ref = "No COVID infection")

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, COV_ETHNICITY, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, COV_HX_PCOS_FLAG, 
          EXP_COVID_PRE_BIN, EXP_DUR_HOSP, EXP_COVID_BIN, EXP_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, OUT_BIN_DUR_VENOUS, 
          OUT_BIN_DUR_ARTERIAL,OUT_BIN_POST_VENOUS, OUT_BIN_POST_ARTERIAL, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           COV_ETHNICITY ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
                           COV_HX_PCOS_FLAG ~ "History of PCOS",
                           COV_HX_PREGNANCY_MAX ~ "Previous pregnancy",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           EXP_COVID_BIN ~ "Infection during pregnancy",
                           EXP_TRIMESTER ~ "Trimester of exposure",
                           EXP_COVID_PRE_BIN ~ "Previous COVID infection",
                           EXP_DUR_HOSP ~ "COVID Hospitalization",
                           VACC_PRE_INF  ~ "Vaccination before COVID infection",
                           VACC_1ST_DUR_FLAG ~ "First vaccination during pregnancy",
                           VACC_2ND_DUR_FLAG ~ "Second vaccination during pregnancy",
                           OUT_BIN_GEST_DIABETES ~ "Gestational diabetes",
                           OUT_BIN_GEST_HYPERTENSION ~ "Gestational hypertension",
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia",
                           OUT_BIN_DUR_VENOUS ~ "Venous event",
                           OUT_BIN_DUR_ARTERIAL ~ "Arterial event",
                           OUT_BIN_POST_VENOUS ~ "Venous event after pregnancy",
                           OUT_BIN_POST_ARTERIAL ~ "Arterial event after pregnangcy"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 
tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")

# round to the nearest 5 -------------------------------------------------------  
round_to_5 <- function(x) {
  round(as.numeric(x) / 5) * 5
}

tbl <- as_tibble(tbl_df$table_body)

tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_pregnancy <- tbl_df

# prepare table for overall period for outcomes at birth -----------------------  
df_birth <- df[which(df$PREG_START_DATE_ESTIMATED==0),]

tbl_df <- df_birth %>%
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE, 
         OUT_BIN_BIRTH_STILLBIRTH_MAX, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age",
                           OUT_BIN_BIRTH_STILLBIRTH_MAX ~ "Stillbirth"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
tbl <- as_tibble(tbl_df$table_body)

# round to the nearest 5 -------------------------------------------------------
tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_birth <- tbl_df

# combine tables ---------------------------------------------------------------
tbl_summary <- tbl_stack(list(tbl_df_pregnancy, tbl_df_birth))

# prepare data to export -------------------------------------------------------
gt_table <- tbl_summary %>% as_gt()
gtsave(gt_table , filename = "D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/overall.pdf")


# prepare table for prevaccination era -----------------------------------------  
df <- data_temp4
ls(df)

df <- df[which(df$DELIVERY_DATE < '2020-12-08'),]

df$COVID_STATUS <-ifelse(df$EXP_COVID_BIN==1, "COVID infection", "No COVID infection")
df$COVID_STATUS <- factor(df$COVID_STATUS, levels = c("COVID infection", "No COVID infection"))
df$COVID_STATUS = relevel(df$COVID_STATUS, ref = "No COVID infection")


tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, COV_ETHNICITY, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, COV_HX_PCOS_FLAG, 
          EXP_COVID_PRE_BIN, EXP_DUR_HOSP, EXP_COVID_BIN, EXP_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, OUT_BIN_DUR_VENOUS, 
          OUT_BIN_DUR_ARTERIAL,OUT_BIN_POST_VENOUS, OUT_BIN_POST_ARTERIAL, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           COV_ETHNICITY ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
                           COV_HX_PCOS_FLAG ~ "History of PCOS",
                           COV_HX_PREGNANCY_MAX ~ "Previous pregnancy",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           EXP_COVID_BIN ~ "Infection during pregnancy",
                           EXP_TRIMESTER ~ "Trimester of exposure",
                           EXP_COVID_PRE_BIN ~ "Previous COVID infection",
                           EXP_DUR_HOSP ~ "COVID Hospitalization",
                           VACC_PRE_INF  ~ "Vaccination before COVID infection",
                           VACC_1ST_DUR_FLAG ~ "First vaccination during pregnancy",
                           VACC_2ND_DUR_FLAG ~ "Second vaccination during pregnancy",
                           OUT_BIN_GEST_DIABETES ~ "Gestational diabetes",
                           OUT_BIN_GEST_HYPERTENSION ~ "Gestational hypertension",
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia",
                           OUT_BIN_DUR_VENOUS ~ "Venous event",
                           OUT_BIN_DUR_ARTERIAL ~ "Arterial event",
                           OUT_BIN_POST_VENOUS ~ "Venous event after pregnancy",
                           OUT_BIN_POST_ARTERIAL ~ "Arterial event after pregnangcy"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")

# round to the nearest 5 -------------------------------------------------------
round_to_5 <- function(x) {
  round(as.numeric(x) / 5) * 5
}

tbl <- as_tibble(tbl_df$table_body)

tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_pregnancy <- tbl_df

# prepare table for prevaccination era for outcomes at birth -------------------
df_birth <- df[which(df$PREG_START_DATE_ESTIMATED==0),]

tbl_df <- df_birth %>%
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE, 
         OUT_BIN_BIRTH_STILLBIRTH_MAX, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age",
                           OUT_BIN_BIRTH_STILLBIRTH_MAX ~ "Stillbirth"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
tbl <- as_tibble(tbl_df$table_body)

# round to the nearest 5 -------------------------------------------------------
tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_birth <- tbl_df

# combine tables ---------------------------------------------------------------
tbl_summary <- tbl_stack(list(tbl_df_pregnancy, tbl_df_birth))

# prepare data to export -------------------------------------------------------
gt_table <- tbl_summary %>% as_gt()
gtsave(gt_table , filename = "D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/prevaccination.pdf")


# prepare table for vaccination era --------------------------------------------  
df <- data_temp4
ls(df)

df <- df[which(df$PREG_START_DATE >= '2020-12-08'),]

df$COVID_STATUS <-ifelse(df$EXP_COVID_BIN==1, "COVID infection", "No COVID infection")
df$COVID_STATUS <- factor(df$COVID_STATUS, levels = c("COVID infection", "No COVID infection"))
df$COVID_STATUS = relevel(df$COVID_STATUS, ref = "No COVID infection")

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, COV_ETHNICITY, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, COV_HX_PCOS_FLAG, 
          EXP_COVID_PRE_BIN, EXP_DUR_HOSP, EXP_COVID_BIN, EXP_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, OUT_BIN_DUR_VENOUS, 
          OUT_BIN_DUR_ARTERIAL,OUT_BIN_POST_VENOUS, OUT_BIN_POST_ARTERIAL, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           COV_ETHNICITY ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
                           COV_HX_PCOS_FLAG ~ "History of PCOS",
                           COV_HX_PREGNANCY_MAX ~ "Previous pregnancy",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           EXP_COVID_BIN ~ "COVID Infection during pregnacy",
                           EXP_TRIMESTER ~ "Trimester of exposure",
                           EXP_COVID_PRE_BIN ~ "Previous COVID infection",
                           EXP_DUR_HOSP ~ "COVID Hospitalization",
                           VACC_PRE_INF  ~ "Vaccination before COVID infection",
                           VACC_1ST_DUR_FLAG ~ "First vaccination during pregnancy",
                           VACC_2ND_DUR_FLAG ~ "Second vaccination during pregnancy",
                           OUT_BIN_GEST_DIABETES ~ "Gestational diabetes",
                           OUT_BIN_GEST_HYPERTENSION ~ "Gestational hypertension",
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia",
                           OUT_BIN_DUR_VENOUS ~ "Venous event",
                           OUT_BIN_DUR_ARTERIAL ~ "Arterial event",
                           OUT_BIN_POST_VENOUS ~ "Venous event after pregnancy",
                           OUT_BIN_POST_ARTERIAL ~ "Arterial event after pregnangcy"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")

# round to the nearest 5 -------------------------------------------------------
round_to_5 <- function(x) {
  round(as.numeric(x) / 5) * 5
}

tbl <- as_tibble(tbl_df$table_body)

tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_pregnancy <- tbl_df

# prepare table for vaccination era for outcomes at birth -----------------------  
df_birth <- df[which(df$PREG_START_DATE_ESTIMATED==0),]

tbl_df <- df_birth %>%
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE, 
         OUT_BIN_BIRTH_STILLBIRTH_MAX, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age",
                           OUT_BIN_BIRTH_STILLBIRTH_MAX ~ "Stillbirth"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
tbl <- as_tibble(tbl_df$table_body)

# round to the nearest 5 -------------------------------------------------------
tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_birth <- tbl_df

# combine tables ---------------------------------------------------------------
tbl_summary <- tbl_stack(list(tbl_df_pregnancy, tbl_df_birth))

# prepare data to export -------------------------------------------------------
gt_table <- tbl_summary %>% as_gt()
gtsave(gt_table , filename = "D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/vaccination_era.pdf")


# prepare table for 1st vaccination analysis -----------------------------------  
df <- data_temp6
ls(df)

df$VACC_STATUS <-ifelse(df$VACC_1ST_DUR_FLAG==1, "COVID vaccination", "No COVID vaccination")
df$VACC_STATUS <- factor(df$VACC_STATUS, levels = c("COVID vaccination", "No COVID vaccination"))
df$VACC_STATUS = relevel(df$VACC_STATUS, ref = "No COVID vaccination")

table(df$VACC_STATUS)

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, COV_ETHNICITY, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, COV_HX_PCOS_FLAG, 
          EXP_COVID_PRE_BIN, EXP_COVID_BIN, EXP_DUR_HOSP, EXP_VACC_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, OUT_BIN_DUR_VENOUS, 
          OUT_BIN_DUR_ARTERIAL,OUT_BIN_POST_VENOUS, OUT_BIN_POST_ARTERIAL, VACC_STATUS) %>%
  tbl_summary(by = VACC_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           COV_ETHNICITY ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
                           COV_HX_PCOS_FLAG ~ "History of PCOS",
                           COV_HX_PREGNANCY_MAX ~ "Previous pregnancy",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           EXP_VACC_TRIMESTER ~ "Trimester of vaccination",
                           EXP_COVID_PRE_BIN ~ "Previous COVID infection",
                           EXP_COVID_BIN ~ "COVID infection",
                           EXP_DUR_HOSP ~ "COVID Hospitalization",
                           VACC_PRE_INF  ~ "Vaccination before COVID infection",
                           VACC_1ST_DUR_FLAG ~ "First vaccination during pregnancy",
                           VACC_2ND_DUR_FLAG ~ "Second vaccination during pregnancy",
                           OUT_BIN_GEST_DIABETES ~ "Gestational diabetes",
                           OUT_BIN_GEST_HYPERTENSION ~ "Gestational hypertension",
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia",
                           OUT_BIN_DUR_VENOUS ~ "Venous event",
                           OUT_BIN_DUR_ARTERIAL ~ "Arterial event",
                           OUT_BIN_POST_VENOUS ~ "Venous event after pregnancy",
                           OUT_BIN_POST_ARTERIAL ~ "Arterial event after pregnangcy"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")

# round to the nearest 5 -------------------------------------------------------
round_to_5 <- function(x) {
  round(as.numeric(x) / 5) * 5
}

tbl <- as_tibble(tbl_df$table_body)

tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_pregnancy <- tbl_df

# prepare table for 1st vaccination analysis for outcomes at birth ------------- 
df_birth <- df[which(df$PREG_START_DATE_ESTIMATED==0),]

tbl_df <- df_birth %>%
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE, 
         OUT_BIN_BIRTH_STILLBIRTH_MAX, VACC_STATUS) %>%
  tbl_summary(by = VACC_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age",
                           OUT_BIN_BIRTH_STILLBIRTH_MAX ~ "Stillbirth"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
tbl <- as_tibble(tbl_df$table_body)

# round to the nearest 5 -------------------------------------------------------
tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_birth <- tbl_df

# combine tables ---------------------------------------------------------------
tbl_summary <- tbl_stack(list(tbl_df_pregnancy, tbl_df_birth))

# prepare data to export -------------------------------------------------------
gt_table <- tbl_summary %>% as_gt()
gtsave(gt_table , filename = "D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/1st_vaccination_analysis.pdf")


# prepare table for 2nd vaccination analysis -----------------------------------
df <- data_temp7
ls(df)

df$VACC_STATUS <-ifelse(df$VACC_2ND_DUR_FLAG==1, "COVID vaccination", "No COVID vaccination")
df$VACC_STATUS <- factor(df$VACC_STATUS, levels = c("COVID vaccination", "No COVID vaccination"))
df$VACC_STATUS = relevel(df$VACC_STATUS, ref = "No COVID vaccination")

table(df$VACC_STATUS)

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, COV_ETHNICITY, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, COV_HX_PCOS_FLAG, 
          EXP_COVID_PRE_BIN, EXP_COVID_BIN, EXP_DUR_HOSP, EXP_2ND_VACC_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, OUT_BIN_DUR_VENOUS, 
          OUT_BIN_DUR_ARTERIAL,OUT_BIN_POST_VENOUS, OUT_BIN_POST_ARTERIAL, VACC_STATUS) %>%
  tbl_summary(by = VACC_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           COV_ETHNICITY ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
                           COV_HX_PCOS_FLAG ~ "History of PCOS",
                           COV_HX_PREGNANCY_MAX ~ "Previous pregnancy",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           EXP_COVID_PRE_BIN ~ "Previous COVID infection",
                           EXP_COVID_BIN ~ "COVID infection during pregnancy",
                           EXP_DUR_HOSP ~ "COVID Hospitalization",
                           EXP_2ND_VACC_TRIMESTER ~ "Trimester of exposure, 2nd vaccination",
                           VACC_PRE_INF  ~ "Vaccination before COVID infection",
                           VACC_1ST_DUR_FLAG ~ "First vaccination during pregnancy",
                           VACC_2ND_DUR_FLAG ~ "Second vaccination during pregnancy",
                           OUT_BIN_GEST_DIABETES ~ "Gestational diabetes",
                           OUT_BIN_GEST_HYPERTENSION ~ "Gestational hypertension",
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia",
                           OUT_BIN_DUR_VENOUS ~ "Venous event",
                           OUT_BIN_DUR_ARTERIAL ~ "Arterial event",
                           OUT_BIN_POST_VENOUS ~ "Venous event after pregnancy",
                           OUT_BIN_POST_ARTERIAL ~ "Arterial event after pregnangcy"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")

# round to the nearest 5 -------------------------------------------------------
round_to_5 <- function(x) {
  round(as.numeric(x) / 5) * 5
}

tbl <- as_tibble(tbl_df$table_body)

tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_pregnancy <- tbl_df

# prepare table for 2nd vaccination analysis for outcomes at birth -------------
df_birth <- df[which(df$PREG_START_DATE_ESTIMATED==0),]

tbl_df <- df_birth %>%
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE, 
         OUT_BIN_BIRTH_STILLBIRTH_MAX, VACC_STATUS) %>%
  tbl_summary(by = VACC_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age",
                           OUT_BIN_BIRTH_STILLBIRTH_MAX ~ "Stillbirth"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
tbl <- as_tibble(tbl_df$table_body)

# round to the nearest 5 -------------------------------------------------------
tbl <- tbl %>%
  mutate(
    stat_0 = str_replace(stat_0, ",", ""),  
    data_rounded_stat_0 = str_extract(stat_0, "\\d+"),
    percent_stat_0 = str_extract(stat_0, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_1 = str_replace(stat_1, ",", ""),   # Extract the number part
    data_rounded_stat_1 = str_extract(stat_1, "\\d+"),
    percent_stat_1 = str_extract(stat_1, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)"),
    stat_2 = str_replace(stat_2, ",", ""),   # Extract the number part
    data_rounded_stat_2 = str_extract(stat_2, "\\d+"),
    percent_stat_2 = str_extract(stat_2, "\\(<0\\.1%\\)|\\(\\d+(\\.\\d+)?%\\)")
  ) %>%
  mutate(
    data_rounded_stat_0 = round_to_5(data_rounded_stat_0),
    data_rounded_stat_1 = round_to_5(data_rounded_stat_1),
    data_rounded_stat_2 = round_to_5(data_rounded_stat_2)
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "NA", ""),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "NA", ""),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),  
    stat_2 = str_replace_all(stat_2, "NA", "")
  ) %>%
  mutate(
    stat_0 = paste0(data_rounded_stat_0, " ", percent_stat_0),  
    stat_0 = str_replace_all(stat_0, "^5 \\(", "<10 \\("),
    stat_1 = paste0(data_rounded_stat_1, " ", percent_stat_1),  
    stat_1 = str_replace_all(stat_1, "^5 \\(", "<10 \\("),
    stat_2 = paste0(data_rounded_stat_2, " ", percent_stat_2),
    stat_2 = str_replace_all(stat_2, "^5 \\(", "<10 \\(")
  ) %>%
  select(-data_rounded_stat_2, -percent_stat_2, -data_rounded_stat_1, -percent_stat_1, -data_rounded_stat_0, -percent_stat_0)

tbl_df$table_body <- tbl
tbl_df_birth <- tbl_df

# combine tables ---------------------------------------------------------------
tbl_summary <- tbl_stack(list(tbl_df_pregnancy, tbl_df_birth))

# prepare data to export -------------------------------------------------------
gt_table <- tbl_summary %>% as_gt()
gtsave(gt_table , filename = "D:/PhotonUser/My Files/Home Folder/collab/CCU018/CCU018_01/output/2nd_vaccination_analysis.pdf")
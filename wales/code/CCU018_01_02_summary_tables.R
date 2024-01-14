################################################################################
## TITLE: Data preparation of Wales dataset for CCU018_01
##
## Description: Prepare summary tables
##
## By : Elena Raffetti
##
################################################################################

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
#install.packages("gt")

# load required packages -------------------------------------------------------
library(stringr)
library(ggplot2)
library(incidence)
library(tidyverse)
library(epiR)
library(scales)
library(survival)
library(survminer)
library(reshape2)
library(date)
library(lubridate)
library(splines)
library(purrr)
library(ggpubr)
library(gtsummary)
library(dplyr)
library(stringr)
library(gt)

# load data --------------------------------------------------------------------
df <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis.csv",
                        data.table = FALSE)

# define dataframes that include sensitivity analyses ---------------------------
df$COHORT_END_DATE <- as.Date(df$DELIVERY_DATE, "%d/%m/%Y")

data_total <- df

data_missing <- data_total[which(data_total$PREG_START_DATE_ESTIMATED==0),]

data_pre_vacc <- df
nrow(data_pre_vacc)
data_pre_vacc$COHORT_END_DATE <- as.Date(data_pre_vacc$COHORT_END_DATE, "%d/%m/%Y")
data_pre_vacc <- data_pre_vacc[which(data_pre_vacc$COHORT_END_DATE < '2020-12-08'),]

data_missing_pre_vacc <- data_pre_vacc[which(data_pre_vacc$PREG_START_DATE_ESTIMATED==0),]

data_vacc <- df
data_vacc$PREG_START_DATE <- as.Date(data_vacc$PREG_START_DATE, "%d/%m/%Y")
data_vacc <- data_vacc[which(data_vacc$PREG_START_DATE >= '2020-12-08'),]
data_vacc$COHORT_END_DATE <- as.Date(data_vacc$COHORT_END_DATE, "%d/%m/%Y")

data_missing_vacc <- data_vacc[which(data_vacc$PREG_START_DATE_ESTIMATED==0),]

data_1st_vacc <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis_vaccination.csv",
                        data.table = FALSE)

data_missing_1st_vacc <- data_1st_vacc[which(data_1st_vacc$PREG_START_DATE_ESTIMATED==0),]

# count number of pregnant women included --------------------------------------
writeLines(c("description nrow"), paste0('S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/database_row_counts.csv'))
df <-  c("data_total", "data_vacc", "data_pre_vacc", "data_1st_vacc", "data_missing", "data_missing_pre_vacc", "data_missing_vacc", "data_missing_1st_vacc")
purrr::map(df, function(i) {
  nrow_count <- nrow(get(i))
  cat(paste0(i,' ',nrow_count),
       '\n', sep = '', file = paste0("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/database_row_counts.csv"), append = TRUE)
})


# prepare table for overall period --------------------------------------------- 
df <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis.csv",
                        data.table = FALSE)
ls(df)

df$COVID_STATUS <-ifelse(df$EXP_COVID_BIN==1, "COVID infection", "No COVID infection")
df$COVID_STATUS <- factor(df$COVID_STATUS, levels = c("COVID infection", "No COVID infection"))
df$COVID_STATUS = relevel(df$COVID_STATUS, ref = "No COVID infection")

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, ETHNIC_CAT, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, 
          EXP_COVID_PRE_BIN, EXP_COVID_BIN, EXP_DUR_HOSP, EXP_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           ETHNIC_CAT ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
                           COV_HX_PREGNANCY_MAX ~ "Previous pregnancy",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           EXP_TRIMESTER ~ "Trimester of exposure",
                           EXP_COVID_BIN ~ "Infection during pregnancy",
                           EXP_COVID_PRE_BIN ~ "Previous COVID infection",
                           EXP_DUR_HOSP ~ "COVID Hospitalization",
                           VACC_PRE_INF  ~ "Vaccination before COVID infection",
                           VACC_1ST_DUR_FLAG ~ "First vaccination during pregnancy",
                           VACC_2ND_DUR_FLAG ~ "Second vaccination during pregnancy",
                           OUT_BIN_GEST_DIABETES ~ "Gestational diabetes",
                           OUT_BIN_GEST_HYPERTENSION ~ "Gestational hypertension",
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia"
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
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
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
gtsave(gt_table , filename = "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/overall.html")


# prepare table for prevaccination era ----------------------------------------- 
df <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis.csv",
                        data.table = FALSE)
ls(df)

df <- df[which(df$DELIVERY_DATE < '2020-12-08'),]

df$COVID_STATUS <-ifelse(df$EXP_COVID_BIN==1, "COVID infection", "No COVID infection")
df$COVID_STATUS <- factor(df$COVID_STATUS, levels = c("COVID infection", "No COVID infection"))
df$COVID_STATUS = relevel(df$COVID_STATUS, ref = "No COVID infection")

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, ETHNIC_CAT, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, 
          EXP_COVID_PRE_BIN, EXP_COVID_BIN, EXP_DUR_HOSP, EXP_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           ETHNIC_CAT ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
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
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia"
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
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE,  COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
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
gtsave(gt_table , filename = "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/prevaccination.html")


# prepare table for vaccination era --------------------------------------------  
df <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis.csv",
                        data.table = FALSE)
ls(df)

df <- df[which(df$PREG_START_DATE >= '2020-12-08'),]

df$COVID_STATUS <-ifelse(df$EXP_COVID_BIN==1, "COVID infection", "No COVID infection")
df$COVID_STATUS <- factor(df$COVID_STATUS, levels = c("COVID infection", "No COVID infection"))
df$COVID_STATUS = relevel(df$COVID_STATUS, ref = "No COVID infection")

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, ETHNIC_CAT, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX, 
          EXP_COVID_PRE_BIN, EXP_COVID_BIN, EXP_DUR_HOSP, EXP_TRIMESTER, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           ETHNIC_CAT ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
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
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 
tbl <- as_tibble(tbl_df$table_body)

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
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE,  COVID_STATUS) %>%
  tbl_summary(by = COVID_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
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
gtsave(gt_table , filename = "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/vaccination_era.html")


# prepare table for 1st vaccination analysis ----------------------------------- 
df <- data.table::fread("S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/CCU018_analysis_vaccination.csv",
                        data.table = FALSE)
df$VACC_STATUS <- ifelse(df$VACC_1ST_DUR_FLAG==1, "COVID vaccination", "No COVID vaccination")
df$VACC_STATUS <- factor(df$VACC_STATUS, levels = c("COVID vaccination", "No COVID vaccination"))
df$VACC_STATUS <- relevel(df$VACC_STATUS, ref = ("No COVID vaccination"))

table(df$VACC_STATUS)

tbl_df <-df %>%
  select( agegroup, COV_DEPRIVATION, COV_ETHNICITY_3lev, ETHNIC_CAT, COV_SMOKING_STATUS, COV_HX_CVD_HEM, COV_HX_DEPRESSION_FLAG,
          COV_HX_DIABETES_DIS, COV_HX_HYPERTENSIVE_DIS, COV_HX_OTHER_DIS, COV_HX_PREGNANCY_MAX,  
          EXP_VACC_TRIMESTER, EXP_COVID_PRE_BIN, EXP_COVID_BIN, EXP_DUR_HOSP, VACC_PRE_INF, VACC_1ST_DUR_FLAG, VACC_2ND_DUR_FLAG,
          OUT_BIN_GEST_DIABETES, OUT_BIN_GEST_HYPERTENSION, OUT_BIN_PREECLAMPSIA, VACC_STATUS) %>%
  tbl_summary(by = VACC_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              label = list(agegroup ~ "Age",
                           COV_DEPRIVATION ~ "Deprivation",
                           COV_SMOKING_STATUS ~ "Smoking status",
                           COV_ETHNICITY_3lev ~ "Ethnicity 3 level",
                           ETHNIC_CAT  ~ "Ethnicity",
                           COV_HX_CVD_HEM ~ "History of cardiovascular and hematological diseases",
                           COV_HX_DEPRESSION_FLAG ~ "History of depression",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           COV_HX_HYPERTENSIVE_DIS ~ "History of hypertension",
                           COV_HX_OTHER_DIS ~ "History of other conditions",
                           COV_HX_PREGNANCY_MAX ~ "Previous pregnancy",
                           COV_HX_DIABETES_DIS ~ "History of diabetes",
                           EXP_VACC_TRIMESTER ~ "Trimester of vaccination",
                           EXP_COVID_PRE_BIN ~ "Previous COVID infection",
                           EXP_COVID_BIN  ~ "COVID infection during pregnancy",
                           EXP_DUR_HOSP ~ "COVID Hospitalization",
                           VACC_PRE_INF  ~ "Vaccination before COVID infection",
                           VACC_1ST_DUR_FLAG ~ "First vaccination during pregnancy",
                           VACC_2ND_DUR_FLAG ~ "Second vaccination during pregnancy",
                           OUT_BIN_GEST_DIABETES ~ "Gestational diabetes",
                           OUT_BIN_GEST_HYPERTENSION ~ "Gestational hypertension",
                           OUT_BIN_PREECLAMPSIA ~ "Preeclampsia"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl <- as_tibble(tbl_df$table_body)

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
  select(OUT_BIN_BIRTH_PRETERM, OUT_BIN_BIRTH_VERY_PRETERM, OUT_BIN_BIRTH_SMALL_GEST_AGE, VACC_STATUS) %>%
  tbl_summary(by = VACC_STATUS,
              statistic = list(all_continuous()  ~ "{mean} ({sd})"),
              #missing = "no",
              label = list(OUT_BIN_BIRTH_PRETERM ~ "Preterm",
                           OUT_BIN_BIRTH_VERY_PRETERM ~ "Very preterm",
                           OUT_BIN_BIRTH_SMALL_GEST_AGE ~ "Small for gestational age"
              )
  ) %>%
  add_overall() %>%
  bold_labels() 

tbl_df <- modify_header(tbl_df, all_stat_cols() ~ "****")
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
gtsave(gt_table , filename = "S:/WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)/CCU018/Scripts/R_scripts/output/1st_vaccination_analysis.html")

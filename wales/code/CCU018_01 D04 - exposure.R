# CCU018_01_exposure

# Description: This script creates the exposure table which captures Covid-19 Infection and Vaccination

# Project: CCU018

# Author(s): Elena Raffetti, John Nolan

# Date last updated: 2022/02/02

# Date last run: 2022/02/02

# Data Input: 
# SAILWWMCCV.CCU018_01_cohort
# SAILWWMCCV.CCU018_01_codelist
# WLGP
# PEDW
# WDDS
# ADDE
# MIDS

# Data Output:
# SAILWWMCCV.CCU018_01_OUT_EXPOSURE

# Output Description:
# 

# run parameters script
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\CCU018_01 - 01 - parameters.R")


# rename current output tables to archive

# set archive date
archive_date <- str_replace_all(as.character(as.Date(out_date, '%Y-%m-%d')-1), "-", "")

# archive the current exposure table
curr_tab <- tbl(con, in_schema(dbc,path_exposure))
curr_dat <- curr_tab %>%
  collect()

# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_exposure, "_", archive_date)), curr_dat)
dbRemoveTable(con, SQL(paste0(dbc, ".", path_exposure)))




#---------------------------------------------.
# DATA - create connection to required tables
#---------------------------------------------.

# project cohort
project_cohort <- tbl(con, in_schema(dbc, path_cohort))

# codelists
codelist_covid_icd10 <- tbl(con, in_schema(dbc, path_codelist_covid_icd10))
codelist_covid_read <- tbl(con, in_schema(dbc, path_codelist_covid_read))


# WLGP data
c19_wlgp_tbl <- tbl(con, in_schema(db, path_wlgp))

# PEDW data
c19_pedw_diag_tbl <- tbl(con, in_schema(db, path_pedw_diag))
c19_pedw_oper_tbl <- tbl(con, in_schema(db, path_pedw_oper))
c19_pedw_episode_tbl <- tbl(con, in_schema(db, path_pedw_episode))
c19_pedw_spell_tbl <- tbl(con, in_schema(db, path_pedw_spell))

# PATD Test Data
c19_patd_test_results_tbl <- tbl(con, in_schema(db, path_patd_test_results))

# CVVD Vaccination data
c19_cvvd_tbl <- tbl(con, in_schema(db, path_cvvd))


#---------------------------------------------.
# PREPARE 
#---------------------------------------------.

# INDIVIDUALS AND BASELINE DATES
project_cohort <- project_cohort %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, DELIVERY_DATE, FU_END_DATE)

# CHECK
nrow(project_cohort %>% distinct(PERSON_ID) %>% collect())
# 43,715 
nrow(project_cohort %>% distinct(PERSON_ID, DELIVERY_DATE) %>% collect())
# 44,448

# CVVD Table
c19_cvvd_tbl <- c19_cvvd_tbl %>%
  filter(ALF_HAS_BAD_VACC_RECORD == 0) %>%
  select(ALF_E, VACC_DATE, APPT_DATE, VACC_NAME, VACC_DOSE_SEQ) %>%
  rename("PERSON_ID" = "ALF_E") %>%
  inner_join(project_cohort,
             by = "PERSON_ID") %>%
  arrange(PERSON_ID, VACC_DOSE_SEQ)


#---------------------------------------------.
# COVID-19 INFECTION
#---------------------------------------------.

# create test result data

patd_test_results <- c19_patd_test_results_tbl %>%
  filter(TESTSETNAME == "COVID19" | TESTNAME == "Coronavirus SARS CoV 2 PCR") %>%
  filter(year(SPCM_COLLECTED_DT) %in% c(2020, 2021, 2022) |
           year(SPCM_RECEIVED_DT) %in% c(2020, 2021, 2022) | 
           year(FIRSTAUTHORISED_DT) %in% c(2020, 2021, 2022) |
           year(AUTHORISED_DT) %in% c(2020, 2021, 2022)) %>%
  select(ALF_E, SPCM_COLLECTED_DT, SPCM_RECEIVED_DT,
         AUTHORISED_DT, FIRSTAUTHORISED_DT,
         SPCM_CD, SPCM_NAME,
         RESULT_CD, RESULTNAME,
         COVID19TESTRESULT,
         CTU_PERS_TYPE, CTU_PERS_TYPECONSOLIDATED,
         PAT_TYPE_CD, PAT_TYPE_DESC) %>% 
  group_by(ALF_E) %>%
  window_order(SPCM_COLLECTED_DT) %>%
  mutate(test_order = row_number(),
         test_date_cleaned = case_when(year(SPCM_COLLECTED_DT) %in% c(2020, 2021, 2022) ~ SPCM_COLLECTED_DT,
                                       year(SPCM_RECEIVED_DT) %in% c(2020, 2021, 2022) ~ SPCM_RECEIVED_DT,
                                       year(FIRSTAUTHORISED_DT) %in% c(2020, 2021, 2022) ~ FIRSTAUTHORISED_DT,
                                       year(AUTHORISED_DT) %in% c(2020, 2021, 2022) ~ AUTHORISED_DT)) %>%
  mutate(positive_test_dt = case_when(COVID19TESTRESULT == "Positive" ~ test_date_cleaned)) %>%
  group_by(ALF_E) %>%
  mutate(first_positive_test_dt = min(positive_test_dt, na.rm = T)) %>%
  ungroup %>%
  rename("PERSON_ID" = "ALF_E") %>%
  collect() %>%
  arrange(PERSON_ID, SPCM_COLLECTED_DT)
  

# create COVID-19 related hospital data
phen_pedw_covid <- c19_pedw_spell_tbl %>%
  select(ALF_E, PROV_UNIT_CD, SPELL_NUM_E,
         ADMIS_DT, ADMIS_MTHD_CD, ADMIS_SOURCE_CD, ADMIS_SPEC_CD,
         DISCH_DT, DISCH_MTHD_CD, DISCH_SPEC_CD, RES_WARD_CD) %>%
  left_join(c19_pedw_episode_tbl %>%
              select(SPELL_NUM_E, PROV_UNIT_CD,
                     EPI_STR_DT, EPI_END_DT, EPI_NUM),
            by = c("SPELL_NUM_E", "PROV_UNIT_CD")) %>%
  left_join(c19_pedw_diag_tbl %>%
              select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM,
                     DIAG_CD_1234, DIAG_CD, DIAG_NUM),
            by = c("SPELL_NUM_E", "PROV_UNIT_CD", "EPI_NUM")) %>%
  inner_join(codelist_covid_icd10 %>%
               filter(IS_LATEST == 1) %>%
               select(CODE, CATEGORY),
             by = c("DIAG_CD_1234" = "CODE")) %>%
  filter(year(ADMIS_DT) >= 2020 & year(ADMIS_DT) <= 2022) %>%
  arrange(ALF_E, ADMIS_DT, EPI_STR_DT) %>%
  rename("PERSON_ID" = "ALF_E") %>%
  collect()
  
  
# create COVID-19 related WLGP data
phen_wlgp_covid <- c19_wlgp_tbl %>%
  select(ALF_E, WOB, GNDR_CD, EVENT_DT, EVENT_CD) %>%
  inner_join(codelist_covid_read %>%
               filter(IS_LATEST == 1) %>%
               select(CODE, CATEGORY, DESC) %>%
               rename("EVENT_CD_CATEGORY" = "CATEGORY",
                      "EVENT_CD_DESCRIPTION" = "DESC"),
             by = c("EVENT_CD" = "CODE")) %>%
  filter(year(EVENT_DT) >= 2020 & year(EVENT_DT) <= 2022) %>%
  rename("PERSON_ID" = "ALF_E") %>%
  collect()



# combine positive covid-19 cases for cohort into single dataframe

# PATD
covid19_case_patd <- patd_test_results %>%
  filter(COVID19TESTRESULT == "Positive") %>%
  select(PERSON_ID, SPCM_COLLECTED_DT) %>%
  rename("RECORD_DATE" = "SPCM_COLLECTED_DT") %>%
  inner_join(project_cohort %>%
               collect() %>%
               mutate(PERSON_ID = as.integer(PERSON_ID)),
             by = "PERSON_ID") %>%
  filter(RECORD_DATE <= DELIVERY_DATE) %>%
  mutate(COVID19_STATUS = "Positive PCR Test",
         PHENOTYPE = "01_Covid_positive_test",
         SOURCE = "PATD") %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, DELIVERY_DATE, RECORD_DATE, COVID19_STATUS, PHENOTYPE, SOURCE)

# PEDW
covid19_test_pedw <- phen_pedw_covid %>%
  select(PERSON_ID, ADMIS_DT, DIAG_CD, DIAG_NUM) %>%
  rename("RECORD_DATE" = "ADMIS_DT") %>%
  inner_join(project_cohort %>%
               collect() %>%
               mutate(PERSON_ID = as.integer(PERSON_ID)),
             by = "PERSON_ID") %>%
  filter(RECORD_DATE <= DELIVERY_DATE) %>%
  mutate(COVID19_STATUS = case_when(DIAG_CD == "U071" ~ "Confirmed_COVID19",
                                    DIAG_CD == "U072" ~ "Suspected_COVID19"),
         PHENOTYPE = case_when(DIAG_NUM == 1 ~ "02_Covid_admission_primary_position",
                               T ~ "02_Covid_admission_any_position"),
         SOURCE = "PEDW") %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, DELIVERY_DATE, RECORD_DATE, COVID19_STATUS, PHENOTYPE, SOURCE, DIAG_NUM)

# WLGP
covid19_test_wlgp <- phen_wlgp_covid %>%
  select(PERSON_ID, EVENT_DT, EVENT_CD_CATEGORY) %>%
  rename("RECORD_DATE" = "EVENT_DT") %>%
  inner_join(project_cohort %>%
               collect() %>%
               mutate(PERSON_ID = as.integer(PERSON_ID)),
             by = "PERSON_ID") %>%
  rename("COVID19_STATUS" = "EVENT_CD_CATEGORY") %>%
  filter(RECORD_DATE <= DELIVERY_DATE) %>%
  mutate(SOURCE = "WLGP",
         PHENOTYPE ="01_GP_Covid_diagnosis") %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, DELIVERY_DATE, RECORD_DATE, COVID19_STATUS, PHENOTYPE, SOURCE)

# COMBINE
covid19_test_all <- covid19_case_patd %>%
  bind_rows(covid19_test_pedw,
            covid19_test_wlgp) %>%
  mutate(COVID19_STATUS = str_trim(COVID19_STATUS))

# CHECKS
with(covid19_test_all, table(SOURCE))
with(covid19_test_all, table(SOURCE, COVID19_STATUS))
with(covid19_test_all, table(SOURCE, PHENOTYPE))


# Derive Exposure Pre and Post Pregnancy

exposure <- covid19_test_all %>%
  filter(COVID19_STATUS %in% c("Confirmed", "Confirmed_COVID19", "Positive PCR Test")) %>%
  filter(RECORD_DATE < PREG_START_DATE) %>%
  arrange(PERSON_ID, PREG_ID, RECORD_DATE, SOURCE) %>%
  group_by(PERSON_ID) %>%
  mutate(rn = row_number()) %>%
  filter(rn == 1) %>%
  select(PERSON_ID, PREG_ID, RECORD_DATE, SOURCE, PHENOTYPE) %>%
  mutate(EXP_TYPE = "EXP_PRE_COVID") %>%
  bind_rows(covid19_test_all %>%
              filter(COVID19_STATUS %in% c("Confirmed", "Confirmed_COVID19", "Positive PCR Test")) %>%
              filter(RECORD_DATE >= PREG_START_DATE) %>%
              arrange(PERSON_ID, PREG_ID, RECORD_DATE, SOURCE) %>%
              group_by(PERSON_ID) %>%
              mutate(rn = row_number()) %>%
              filter(rn == 1) %>%
              select(PERSON_ID, PREG_ID, RECORD_DATE, SOURCE, PHENOTYPE) %>%
              mutate(EXP_TYPE = "EXP_DUR_COVID")) %>%
  left_join(covid19_test_all %>%
              filter(SOURCE == "PEDW",
                     DIAG_NUM == 1) %>%
              select(PERSON_ID, RECORD_DATE) %>%
              rename("HOSP_RECORD_DATE" = "RECORD_DATE"),
            by = "PERSON_ID") %>%
  mutate(days_diff = difftime(HOSP_RECORD_DATE, RECORD_DATE, units = "days")) %>%
  mutate(SEVERITY = case_when(days_diff >= 0 & days_diff <= 28 ~ 1,
                              T ~ 0)) %>%
  group_by(PERSON_ID, PREG_ID, RECORD_DATE, EXP_TYPE, SOURCE, PHENOTYPE) %>%
  summarise(SEVERITY = max(SEVERITY)) %>%
  ungroup() %>%
  mutate(SEVERITY = case_when(SEVERITY == 1 ~ "Hospitalised",
                              SEVERITY == 0 ~ "Non-Hospitalised")) %>%
  select(PERSON_ID, PREG_ID, RECORD_DATE, SOURCE, EXP_TYPE, SEVERITY, PHENOTYPE)
  
# convert to wide format

exposure_wide <- exposure %>%
  pivot_wider(id_cols = c(PERSON_ID, PREG_ID),
              names_from = EXP_TYPE,
              values_from = c(RECORD_DATE, SOURCE, PHENOTYPE, SEVERITY),
              names_glue = "{EXP_TYPE}_{.value}") %>%
  select(PERSON_ID, PREG_ID, 
         EXP_PRE_COVID_RECORD_DATE, EXP_PRE_COVID_SOURCE, EXP_PRE_COVID_PHENOTYPE, EXP_PRE_COVID_SEVERITY,
         EXP_DUR_COVID_RECORD_DATE, EXP_DUR_COVID_SOURCE, EXP_DUR_COVID_PHENOTYPE, EXP_DUR_COVID_SEVERITY)



#---------------------------------------------.
# VACCINATION
#---------------------------------------------.


# read in the required vaccination data
vacc_data <- c19_cvvd_tbl %>%
  collect()

# Check VACC_DATE field

# No records with missing vaccination date
length(vacc_data$VACC_DATE[is.na(vacc_data$VACC_DATE)])

# monthly summary
# data from December 2020 to July 2022
vacc_data_monthly <- vacc_data %>%
  mutate(year_mon = floor_date(VACC_DATE, "month")) %>%
  group_by(year_mon) %>%
  summarise(patients = n_distinct(PERSON_ID),
            records = n())%>%
  arrange(year_mon)

# difference between APPT_DATE and VACC_DATE
vacc_data <- vacc_data %>%
  mutate(days_diff = difftime(VACC_DATE, APPT_DATE, units = "days"))

# dates are the same in > 99% of cases
days_diff_summary <- vacc_data %>%
  group_by(days_diff) %>%
  summarise(vol = n()) %>%
  mutate(prop = vol/sum(vol))


## FIRST VACCINATION

# flag earliest vaccination record for each individual
vacc_first <- vacc_data %>%
  arrange(PERSON_ID, PREG_ID, VACC_DATE) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  mutate(rn = row_number())

# select first record
vacc_first <- vacc_first %>%
  filter(rn == 1) 

# should be first vaccine for most/all
with(vacc_first, table(VACC_DOSE_SEQ))

# check

# vaccinations
length(vacc_first$PERSON_ID)
# pregnancies
nrow(vacc_first %>%
       distinct(PERSON_ID, PREG_ID))
# patients
length(unique(vacc_first$PERSON_ID))


# select and create required fields
vacc_first <- vacc_first %>%
  select(PERSON_ID, PREG_ID, VACC_DATE) %>%
  rename("VACC_1ST_DATE" = "VACC_DATE") %>%
  mutate(VACC_1ST_FLAG = 1)


## VACCINATIONS DURING PREGNANCY

# limit to vaccinations during pregnancy
# add sequence variable (rn) to vaccinations
vacc_data <- vacc_data %>%
  filter(VACC_DATE >= PREG_START_DATE &
           VACC_DATE < DELIVERY_DATE) %>%
  arrange(PERSON_ID, PREG_ID, VACC_DATE) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  mutate(rn = row_number())


## FIRST VACCINATION DURING PREGNANCY

# select first record
vacc_first_dur_preg <- vacc_data %>%
  filter(rn == 1) 

# expect to see cases where it is the 2nd/3rd/Booster this time
with(vacc_first_dur_preg, table(VACC_DOSE_SEQ))

# check

# vaccinations
length(vacc_first_dur_preg$PERSON_ID)
# pregnancies
nrow(vacc_first_dur_preg %>%
       distinct(PERSON_ID, PREG_ID))
# patients
length(unique(vacc_first_dur_preg$PERSON_ID))


# select and create required fields
vacc_first_dur_preg <- vacc_first_dur_preg %>%
  select(PERSON_ID, PREG_ID, VACC_DATE) %>%
  rename("VACC_1ST_DUR_DATE" = "VACC_DATE") %>%
  mutate(VACC_1ST_DUR_FLAG = 1)


## SECOND VACCINATION DURING PREGNANCY

# select first record
vacc_second_dur_preg <- vacc_data %>%
  filter(rn == 2) 

# expect to see cases where it is the 2nd/3rd/Booster this time
with(vacc_second_dur_preg, table(VACC_DOSE_SEQ))

# check

# vaccinations
length(vacc_second_dur_preg$PERSON_ID)
# pregnancies
nrow(vacc_second_dur_preg %>%
       distinct(PERSON_ID, PREG_ID))
# patients
length(unique(vacc_second_dur_preg$PERSON_ID))


# select and create required fields
vacc_second_dur_preg <- vacc_second_dur_preg %>%
  select(PERSON_ID, PREG_ID, VACC_DATE) %>%
  rename("VACC_2ND_DUR_DATE" = "VACC_DATE") %>%
  mutate(VACC_2ND_DUR_FLAG = 1)



# CREATE FINAL VACCINATION OUTPUT
out_vacc <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(vacc_first,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(vacc_first_dur_preg,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(vacc_second_dur_preg,
            by = c("PERSON_ID", "PREG_ID"))



#---------------------------------------------.
# CREATE FINAL EXPOSURE AND VACCINATION TABLE
#---------------------------------------------.

exp_vacc_out <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(exposure_wide,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(out_vacc,
            by = c("PERSON_ID", "PREG_ID"))


# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_exposure)), exp_vacc_out)

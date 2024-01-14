# CCU018_01_cohort

# Description: This script selects the cohort for Project CCU018_01

# Project: CCU018_01

# Author(s): Elena Raffetti, Tom Bolton, John Nolan

# Date last updated: 2022/02/02

# Date last run: 2022/02/02

# Data Input: 
# SAILWMCCV.C19_COHORT_MIDS_BIRTH
# SAILWMCCV.C19_COHORT_MIDS_INITIAL_ASSESSMENT
# SAILWMCCV.C19_COHORT20
# SAILWMCCV.C19_COHORT_PATD_DF_COVID_LIMS_TESTRESULTS
# SAILWMCCV.C19_COHORT20_MORTALITY

# Data Output:
# SAILWWMCCV.CCU018_01_cohort
# SAILWWMCCV.CCU018_01_skinny

# Output Description:

# SAILWWMCCV.CCU018_01_cohort
# A cohort of deliveries (1 row per delivery)
# for pregnancies starting between "2019-08-01" & "2021-08-01"


# run parameters script
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\CCU018_01 - 01 - parameters.R")

# rename current output tables to archive

# set archive date
archive_date <- str_replace_all(as.character(as.Date(out_date, '%Y-%m-%d')-1), "-", "")

# archive the cohort table
curr_tab <- tbl(con, in_schema(dbc,path_cohort))
curr_dat <- curr_tab %>%
  collect()

# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_cohort, "_", archive_date)), curr_dat)
dbRemoveTable(con, SQL(paste0(dbc, ".", path_cohort)))

# archive the skinny table
curr_tab <- tbl(con, in_schema(dbc, path_skinny))
curr_dat <- curr_tab %>%
  collect()

# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_skinny, "_", archive_date)), curr_dat)
dbRemoveTable(con, SQL(paste0(dbc, ".", path_skinny)))



#---------------------------------------------.
# DATA - create connection to required tables
#---------------------------------------------.

# MIDS Initial Assessment
c19_mids_init_assess_tbl <- tbl(con, in_schema(db, path_mids_init_assess))

# MIDS Births
c19_mids_births_tbl <- tbl(con, in_schema(db, path_mids_birth))

# SAIL COHORT20
c19_cohort20_tbl <- tbl(con, in_schema(db, path_cohort20))

# PATD Test Results
c19_patd_test_results_tbl <- tbl(con, in_schema(db, path_patd_test_results))

# DEATHS data
c19_deaths_tbl <- tbl(con, in_schema(db, path_deaths))

#---------------------------------------------.
# Initial Assessment Records Analysis and Preparation:
# There is a lot of data prep and manipulation applied to the initial
# assessment data however as it stands this data is only used to supplement the
# estimated pregnancy start dates for those patients with missing gestation weeks on
# their birth record
#---------------------------------------------.

# read in initial assessment data
c19_mids_init_assess_dat <-c19_mids_init_assess_tbl %>%
  collect()
# 265,724

# we use gest weeks to estimate pregnancy start date
# what is the completeness of this variable in the Initial Assessment Data?
init_ass_gest_weeks <- c19_mids_init_assess_dat %>%
  mutate(missing_gest = case_when(is.na(GEST_WEEKS) ~ 1, T ~ 0)) %>%
  group_by(missing_gest) %>%
  summarise(vol = n()) %>%
  mutate(perc = vol/sum(vol))
# missing on 5.5% of records
# not critical as we will not use initial assessment to identify pregnancies/deliveries for cohort
# only to supplement data on MID BIRTHS records


# limit to pregnancies within our time-frame (using gestation weeks)
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  mutate(PREG_START_DATE = INITIAL_ASS_DT - (7*GEST_WEEKS)) %>%
  filter(PREG_START_DATE >= as.Date(cohort_start_date) & 
           PREG_START_DATE < as.Date(cohort_end_date))
# 61,172 records

# how many records with missing ID?
nrow(c19_mids_init_assess_dat %>%
       filter(is.na(MOTHER_ALF_E)))
# 128

# how many records with missing gestation weeks?
nrow(c19_mids_init_assess_dat %>% 
       filter(is.na(GEST_WEEKS)))
#0, because we used gest weeks to filter the data!

# how many records with gestation weeks LE 3?
nrow(c19_mids_init_assess_dat %>%
       filter(GEST_WEEKS <= 3))
# 144

# how many records with gestation weeks GE 50?
nrow(c19_mids_init_assess_dat %>% 
       filter(GEST_WEEKS >= 50))
# 35

# filter cohort and select required fields
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  filter(GEST_WEEKS > 3 & GEST_WEEKS <= 49) %>%
  filter(!(is.na(GEST_WEEKS))) %>%
  filter(!(is.na(MOTHER_ALF_E))) %>%
  select(MOTHER_ALF_E, PREG_START_DATE, INITIAL_ASS_DT, GEST_WEEKS,
         SERVICE_USER_GRAVIDA_CD, SERVICE_USER_PARITY_CD) %>%
  rename("NUM_PREGN" = "SERVICE_USER_GRAVIDA_CD",
         "PARITY" = "SERVICE_USER_PARITY_CD") %>%
  arrange(MOTHER_ALF_E, INITIAL_ASS_DT)
# 60,867

# add comparison fields from previous record for those patients with recs > 1
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  arrange(MOTHER_ALF_E, INITIAL_ASS_DT, GEST_WEEKS) %>%
  group_by(MOTHER_ALF_E) %>%
  mutate(PREVIOUS_ASS_DT = lag(INITIAL_ASS_DT, 1),
         PREVIOUS_GEST_WEEKS = lag(GEST_WEEKS, 1),
         PREVIOUS_NUM_PREGN = lag(NUM_PREGN, 1),
         PREVIOUS_PARITY = lag(PARITY, 1),
         SEQ = row_number())


# number of assessments where no change in NUM_PREGN?
nrow(c19_mids_init_assess_dat %>% 
       filter(NUM_PREGN == PREVIOUS_NUM_PREGN & !(is.na(NUM_PREGN)) & !(is.na(PREVIOUS_NUM_PREGN))))
# 2,448

# remove these records
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  filter(!(!(is.na(NUM_PREGN)) & !(is.na(PREVIOUS_NUM_PREGN)) & NUM_PREGN == PREVIOUS_NUM_PREGN))

# number of assessments where parity = prev parity?
nrow(c19_mids_init_assess_dat %>%
       filter(PARITY == PREVIOUS_PARITY & !(is.na(PARITY)) & !(is.na(PREVIOUS_PARITY)) & (PARITY == 1 | PARITY == 2)))
# 1,163

# remove these records
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  filter(!(PARITY == PREVIOUS_PARITY & !(is.na(PARITY)) & !(is.na(PREVIOUS_PARITY)) & (PARITY == 1 | PARITY == 2)))

# days between assessments is < 28 days and GEST_WEEKS >= PREV_GEST_WEEKS
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  mutate(days_diff = case_when(SEQ > 1 ~ difftime(INITIAL_ASS_DT, PREVIOUS_ASS_DT, units = "days"),
                               T ~ 365))

nrow(c19_mids_init_assess_dat %>%
       filter(days_diff < 28 & GEST_WEEKS >= PREVIOUS_GEST_WEEKS))
# 286

# remove these records
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  filter(!(days_diff < 28 & GEST_WEEKS >= PREVIOUS_GEST_WEEKS))

# overlapping pregnancy periods, 14 days of buffer
nrow(c19_mids_init_assess_dat %>%
       filter(days_diff < ((GEST_WEEKS * 7) + 14)))
# 240

# remove these records
c19_mids_init_assess_dat <- c19_mids_init_assess_dat %>%
  filter(!(days_diff < ((GEST_WEEKS * 7) + 14)))

# counts of patients and pregnancies / patients
nrow(c19_mids_init_assess_dat)
# 56,730
length(unique(c19_mids_init_assess_dat$MOTHER_ALF_E))
# 55,134


# view data for patients with multiple pregnancies
c19_mids_init_assess_multiple_preg_cases <- c19_mids_init_assess_dat %>%
  arrange(MOTHER_ALF_E, INITIAL_ASS_DT) %>%
  group_by(MOTHER_ALF_E) %>%
  mutate(SEQ = row_number(),
         TOTAL_PREGS = n()) %>%
  ungroup() %>%
  filter(TOTAL_PREGS > 1)

with(c19_mids_init_assess_multiple_preg_cases, table(SEQ))


#---------------------------------------------.
# Birth Records Analysis and Preparation 
#---------------------------------------------.

# read in births data
c19_mids_birth_dat <-c19_mids_births_tbl %>%
  collect()
# 213,258 records

# check the time-period covered by initial assessment data
c19_mids_birth_dat_monthly <- c19_mids_birth_dat %>%
  mutate(year_mon = floor_date(BABY_BIRTH_DT, "month")) %>%
  group_by(year_mon) %>%
  summarise(mothers = n_distinct(MOTHER_ALF_E),
            children = n_distinct(CHILD_ALF_E)) %>%
  arrange(year_mon)

# plot data coverage
# data looks complete up end of March 2022
c19_mids_birth_dat_monthly %>%
  ggplot() +
  geom_col(aes(x = year_mon, y = children), fill = "lightblue") +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "6 months") +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Births",
       title = "Monthly number of births in MIDS Birth Dataset")
# we would expect then to have delivery records for pregnancies starting up to May/June 2021

# we use gestation weeks to estimate pregnancy start date
# gestation weeks summary table
births_gest_weeks_summary <- c19_mids_birth_dat %>%
  group_by(LABOUR_ONSET_GEST_WEEKS) %>%
  summarise(vol = n()) %>%
  mutate(perc = vol/sum(vol))
# 1.8% missing
# also 90 record where LABOUR_ONSET_GEST_WEEKS = 99 

# recode cases where LABOUR_ONSET_GEST_WEEKS to NA, these reords will then have an estimated pregnancy start date (-40 weeks, 280 days)
c19_mids_birth_dat$LABOUR_ONSET_GEST_WEEKS[c19_mids_birth_dat$LABOUR_ONSET_GEST_WEEKS == 99] <- NA

# exclude all other pregnancies where LABOUR_ONSET_GEST_WEEKS is <+ 3 and > 49
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  filter(is.na(LABOUR_ONSET_GEST_WEEKS) | (LABOUR_ONSET_GEST_WEEKS > 3 & LABOUR_ONSET_GEST_WEEKS <= 49))

# select pregnancies/births that fall within study time-frame
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  mutate(PREG_START_DATE = case_when(is.na(LABOUR_ONSET_GEST_WEEKS) ~ BABY_BIRTH_DT - 280,
                                     T ~ BABY_BIRTH_DT - (7*LABOUR_ONSET_GEST_WEEKS)),
         PREG_START_DATE_EST = case_when(is.na(LABOUR_ONSET_GEST_WEEKS) ~ 1, T ~ 0)) %>%
  filter(PREG_START_DATE >= as.Date(cohort_start_date) &
           PREG_START_DATE < as.Date(cohort_end_date))
# 53,552 records (45,636 when we didn't set PREG_START_DATE to BABY_BIRTH_DT - 280 days)

# remove pregnancies after the project follow-up end date
print(project_fu_end_date)
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  filter(BABY_BIRTH_DT <= as.Date(project_fu_end_date))
# 49,360

# calculate pregnancy duration
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  mutate(PREG_DUR = difftime(BABY_BIRTH_DT, PREG_START_DATE, units = "days"))

with(c19_mids_birth_dat, table(PREG_DUR))


# plot of gest weeks v birth weight
c19_mids_birth_dat %>%
  filter(LABOUR_ONSET_GEST_WEEKS <= 50) %>%
  filter(SERVICE_USER_WEIGHT_GRAMS <= 9000) %>%
  ggplot() +
  geom_point(aes(x = LABOUR_ONSET_GEST_WEEKS, y = SERVICE_USER_WEIGHT_GRAMS),
             position = "jitter",
             size = 0.5) +
  theme_few() +
  labs(x = "Gestation Weeks",
       y = "Birthweight (g)")
  
# how many records with missing ID?
nrow(c19_mids_birth_dat %>%
       filter(is.na(MOTHER_ALF_E)))
# 35

# remove records with missing ID
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  filter(!(is.na(MOTHER_ALF_E)))

# how many records with missing gestation weeks?
nrow(c19_mids_birth_dat %>% 
       filter(is.na(LABOUR_ONSET_GEST_WEEKS)))
# 1,187

# add comparison fields for those with recs > 1
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  arrange(MOTHER_ALF_E, BABY_BIRTH_DT, BIRTH_ORDER, LABOUR_ONSET_GEST_WEEKS) %>%
  group_by(MOTHER_ALF_E) %>%
  mutate(PREVIOUS_BIRTH_DT = lag(BABY_BIRTH_DT, 1),
         PREVIOUS_GEST_WEEKS = lag(LABOUR_ONSET_GEST_WEEKS, 1),
         SEQ = row_number()) %>%
  ungroup() %>%
  mutate(days_diff = case_when(SEQ > 1 ~ difftime(BABY_BIRTH_DT, PREVIOUS_BIRTH_DT, units = "days"),
                               T ~ 365))

# filter down to 1 record per MOTHER_ALF_E and BABY_BIRTH_DT
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  arrange(MOTHER_ALF_E, BABY_BIRTH_DT, BIRTH_ORDER) %>%
  group_by(MOTHER_ALF_E, BABY_BIRTH_DT) %>%
  mutate(SEQ = row_number()) %>%
  filter(SEQ == 1)

# there are still records with BIRTH_ORDER > 1
# had assumed that these records would be delivery of 2nd taking place day after first but this is not the case
# records actually behave like singleton births so will leave these in here
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  mutate(FLAG = case_when(BIRTH_ORDER > 1 ~ 1, T ~ 0)) %>%
  group_by(MOTHER_ALF_E) %>%
  mutate(FLAG = max(FLAG))

c19_mids_birth_dat_flag <- c19_mids_birth_dat %>%
  filter(FLAG == 1)

# check for multiple deliveries on same day
# should we flag multiple births here?
nrow(c19_mids_birth_dat %>%
       filter(BABY_BIRTH_DT == PREVIOUS_BIRTH_DT))
# 0 records

# look at the distribution of date differences
c19_mids_birth_days_between_dels <- c19_mids_birth_dat %>%
  group_by(days_diff) %>%
  summarise(vol = n())
# 1 value = 28 than all remaining values are > 250, looks sensible

# remove gaps <= 28 days
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  filter(!(days_diff <= 28))

# days between births < duration of pregnancy
nrow(c19_mids_birth_dat %>%
       filter(SEQ > 1 & days_diff < ((LABOUR_ONSET_GEST_WEEKS * 7) + 14)))
# 0 records

# remove these records
c19_mids_birth_dat <- c19_mids_birth_dat %>%
  filter(!(SEQ > 1 & days_diff < ((LABOUR_ONSET_GEST_WEEKS * 7) + 14)) |
           is.na(LABOUR_ONSET_GEST_WEEKS))



#---------------------------------------------.
# join the births data to the initial assessment data
#---------------------------------------------.

pregnancy_cohort <- c19_mids_birth_dat %>%
  select(MOTHER_ALF_E, 
         PREG_START_DATE, PREG_START_DATE_EST, BABY_BIRTH_DT, 
         LABOUR_ONSET_GEST_WEEKS) %>%
  mutate(BIRTH = 1) %>%
  full_join(c19_mids_init_assess_dat %>%
              rename("PREG_START_DATE_INIT" = "PREG_START_DATE",
                     "GEST_WEEKS_INIT" = "GEST_WEEKS") %>%
              select(MOTHER_ALF_E, PREG_START_DATE_INIT, GEST_WEEKS_INIT, INITIAL_ASS_DT) %>%
              mutate(INIT = 1),
            by = "MOTHER_ALF_E") %>%
  mutate(BIRTH = case_when(is.na(BIRTH) ~ 0, T ~ BIRTH),
         INIT = case_when(is.na(INIT) ~ 0, T ~ INIT)) %>%
  mutate(days_diff = abs(difftime(PREG_START_DATE, PREG_START_DATE_INIT, units = "days")))

# keep only records that are in the MIDS Birth records
pregnancy_cohort <- pregnancy_cohort %>%
  filter(BIRTH == 1)

# now keep the linked MIDS Initial Assessment record that has the smallest date difference
# also keep MIDS Birth records without a matching MIDS Initial Assessment Record
pregnancy_cohort <- pregnancy_cohort %>%
  group_by(MOTHER_ALF_E, PREG_START_DATE) %>%
  mutate(days_diff_min = min(days_diff)) %>%
  filter(days_diff == days_diff_min | is.na(days_diff))

# source summary
pregnancy_cohort %>%
  group_by(BIRTH, INIT) %>%
  summarise(vol = n())

# check that number of pregnancies in cohort is same as in birth dataframe
nrow(pregnancy_cohort) == nrow(c19_mids_birth_dat)
nrow(pregnancy_cohort %>% distinct(MOTHER_ALF_E, BABY_BIRTH_DT)) == nrow(c19_mids_birth_dat %>% distinct(MOTHER_ALF_E, BABY_BIRTH_DT))
# 48,634 distinct pregnancies

# supplement gestation weeks data by taking from init assessment where missing in births data
nrow(pregnancy_cohort %>% filter(is.na(LABOUR_ONSET_GEST_WEEKS)))
nrow(pregnancy_cohort %>% filter(PREG_START_DATE_EST == 1))

# update pregnancy start date to be init_ass_dt - (7*GEST_WEEKS_INIT)
pregnancy_cohort <- pregnancy_cohort %>%
  mutate(PREG_START_DATE = case_when(PREG_START_DATE_EST == 1  &
                                       !(is.na(PREG_START_DATE_INIT)) &
                                       difftime(BABY_BIRTH_DT, PREG_START_DATE_INIT, units = "days") < 300 ~ PREG_START_DATE_INIT,
                                     T ~ PREG_START_DATE))

# recompute SEQ variable
pregnancy_cohort <- pregnancy_cohort %>%
  group_by(MOTHER_ALF_E) %>%
  mutate(PREG_ID = row_number())

# final variables for cohort table
pregnancy_cohort <- pregnancy_cohort %>%
  rename("PERSON_ID" = "MOTHER_ALF_E",
         "DELIVERY_DATE" = "BABY_BIRTH_DT") %>%
  group_by(PERSON_ID) %>%
  mutate(MAX_SEQ = max(PREG_ID)) %>%
  ungroup() %>%
  mutate(FU_END_DATE = case_when(PREG_ID < MAX_SEQ & DELIVERY_DATE + 365 > lead(PREG_START_DATE, 1) ~ lead(PREG_START_DATE, 1),
                                         T ~ DELIVERY_DATE + 365)) %>%
  mutate(FU_END_DATE = case_when(FU_END_DATE > as.Date(project_fu_end_date) ~ as.Date(project_fu_end_date),
                                         T ~ FU_END_DATE)) %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, PREG_START_DATE_EST, DELIVERY_DATE, FU_END_DATE)


# how many multiple pregnancies in study period?
with(pregnancy_cohort, table(PREG_ID))


#---------------------------------------------.
# inner join to COVID20 cohort (included pregnancies)
# the following exclusions are applied in this section:
# aged <18 at start of study period
# those with null gender code
# those who moved into Wales or born after 2020-01-01
# those with Date of Death before start of study period
# those with Positive Covid Test prior to 2020-01-01
#---------------------------------------------.

# RE-WRITE this section to inner join the c19 cohort to preg cohort with all
# required vars
# then detail at each stage how many drop out of cohort.

c19_cohort20 <- c19_cohort20_tbl %>%
  select(ALF_E, GNDR_CD, DOD, DER_AGE_, WOB, MIGRATION, 
         WIMD2019_QUINTILE_INCEPTION,
         COHORT_START_DATE, COHORT_END_DATE, GP_COVERAGE_END_DATE) %>%
  rename("PERSON_ID" = "ALF_E") %>%
  collect()

# inner join to our pregnancy cohort
c19_cohort20 <- c19_cohort20 %>%
  inner_join(pregnancy_cohort %>%
               distinct(PERSON_ID),
             by = "PERSON_ID")
# 41,862 rows
# 1 row per patient as expected
length(unique(c19_cohort20$PERSON_ID))
# 47,298
length(unique(pregnancy_cohort$PERSON_ID))
# 47,814
# So we lose 516 (1%) of pregnancies

# cohort quality assurance steps
# aged < 18
nrow(c19_cohort20 %>% filter(DER_AGE_ < 18) %>% distinct(PERSON_ID))
# 874
c19_cohort20 <- c19_cohort20 %>%
  filter(DER_AGE_ >= 18)

# WOB > current date
nrow(c19_cohort20 %>% filter(WOB > Sys.Date()) %>% distinct(PERSON_ID))
c19_cohort20 <- c19_cohort20 %>%
  filter(!(WOB) > Sys.Date())

# missing gender code
nrow(c19_cohort20 %>% filter(is.na(GNDR_CD)) %>% distinct(PERSON_ID))
c19_cohort20 <- c19_cohort20 %>%
  filter(!(is.na(GNDR_CD)))

# MIGRATION = 0
nrow(c19_cohort20 %>% filter(MIGRATION != 0) %>% distinct(PERSON_ID))
# 2,457
c19_cohort20 <- c19_cohort20 %>%
  filter(MIGRATION == 0)

# select patients from cohort where GP_COVERAGE_END_DATE is not null
nrow(c19_cohort20 %>% filter(is.na(GP_COVERAGE_END_DATE)) %>% distinct(PERSON_ID))
# 2,249
c19_cohort20 <- c19_cohort20 %>%
  filter(!(is.na(GP_COVERAGE_END_DATE)))

# GP_COVERAGE_END_DATE >= COHORT_START_DATE as specified in C19_COHORT20 Guidance on SAIL Confluence
nrow(c19_cohort20 %>% filter(GP_COVERAGE_END_DATE < COHORT_START_DATE) %>% distinct(PERSON_ID))
# 2,747
c19_cohort20 <- c19_cohort20 %>%
  filter(GP_COVERAGE_END_DATE >= COHORT_START_DATE)

# Alive at study start date
nrow(c19_cohort20 %>% filter(!(is.na(DOD))) %>% distinct(PERSON_ID))
nrow(c19_cohort20 %>% filter(DOD < as.Date(cohort_start_date)) %>% distinct(PERSON_ID))
nrow(c19_cohort20 %>% filter(DOD < as.Date(cohort_start_date) | !(is.na(DOD))) %>% distinct(PERSON_ID))
# 0 deceased before cohort start date
c19_cohort20 <- c19_cohort20 %>%
  filter(DOD >= as.Date(cohort_start_date) | is.na(DOD)) 

# update pregnancy cohort to only have eligible records
pregnancy_cohort <- pregnancy_cohort %>%
  inner_join(c19_cohort20 %>%
               distinct(PERSON_ID, GP_COVERAGE_END_DATE, DOD),
             by = "PERSON_ID")
length(unique(pregnancy_cohort$PERSON_ID))
# 38,969

# exclude pregnancies where GP_COVERAGE_END_DATE < DELIVERY_DATE
# update FU_END_DATE and FU_DAYS to factor in GP_COVERAGE_END_DATE
pregnancy_cohort <- pregnancy_cohort %>%
  filter(GP_COVERAGE_END_DATE >= DELIVERY_DATE) %>%
  rowwise() %>%
  mutate(FU_END_DATE = min(FU_END_DATE, GP_COVERAGE_END_DATE, DOD, na.rm = T)) %>%
  ungroup() %>%
  mutate(FU_DAYS = as.numeric(difftime(FU_END_DATE, DELIVERY_DATE, units = "days"))) %>%
  select(-c(GP_COVERAGE_END_DATE, DOD))

# distribution of follow-up days
pregnancy_cohort %>%
  mutate(PREG_ID = as.factor(PREG_ID)) %>%
  ggplot() +
  geom_histogram(aes(x = FU_DAYS, fill = PREG_ID),
                 position = "identity",
                 binwidth = 10,
                 alpha = 0.7) +
  theme_few() +
  labs(title = "Follow-Up Days Distribution",
       x = "Days", y = "n") +
  theme_few() +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values = c("lightblue", "orange"))


with(c19_mids_birth_dat, table(PREG_DUR))

# check for patients with positive test prior to start of study period
c19_pos_test_result_pre_study <- c19_patd_test_results_tbl %>%
  rename("PERSON_ID" = "ALF_E") %>%
  filter(COVID19TESTRESULT == "Positive") %>%
  filter(SPCM_COLLECTED_DT < '2020-01-01') %>%
  distinct(PERSON_ID) %>%
  mutate(POS_TEST = 1) %>%
  collect()

# match these to pregnancy cohort
pregnancy_cohort <- pregnancy_cohort %>%
  left_join(c19_pos_test_result_pre_study,
            by = "PERSON_ID") %>%
  mutate(POS_TEST = case_when(is.na(POS_TEST) ~ 0,
                              T ~ POS_TEST))

# quantify and exclude any matches
with(pregnancy_cohort, table(POS_TEST))
pregnancy_cohort <- pregnancy_cohort %>%
  filter(POS_TEST == 0) %>%
  select(-POS_TEST)

  
# plot of number of pregnancy by month of start date
pregnancy_cohort %>%
  mutate(year_mon = floor_date(PREG_START_DATE, "month")) %>%
  mutate(PREG_ID = factor(PREG_ID)) %>%
  group_by(year_mon, PREG_ID) %>%
  summarise(vol = n()) %>%
  ggplot() +
  geom_col(aes(x = year_mon, y = vol, fill = PREG_ID)) +
  theme_few() +
  theme(legend.position = "right", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "2 months") +
  scale_fill_manual(values = c("lightblue", "orange")) +
  labs(x = "", y = "N",
       title = "CCU018 Cohort Pregnancies by Month")

# pregnancy cohort
dbWriteTable(con, SQL(paste0(dbc, ".", path_cohort)), pregnancy_cohort)

# create final cohort details table and write to database
c19_cohort20 <- c19_cohort20 %>%
  select(PERSON_ID, WOB, GNDR_CD, DOD, WIMD2019_QUINTILE_INCEPTION)

# final filtering against updated pregnancy cohort table
c19_cohort20 <- c19_cohort20 %>%
  inner_join(pregnancy_cohort %>%
               distinct(PERSON_ID),
             by = "PERSON_ID")

# add ethnicity derived from MIDS sources
mids_ethnicity_lkp <- c19_mids_births_tbl %>%
  select(MOTHER_ALF_E, MAT_SERVICE_USER_ETHNIC_GRP_CD, BABY_BIRTH_DT) %>%
  rename("PERSON_ID" = "MOTHER_ALF_E",
         "ETHNIC" = "MAT_SERVICE_USER_ETHNIC_GRP_CD",
         "RECORD_DATE" = "BABY_BIRTH_DT") %>%
  collect() %>%
  bind_rows(c19_mids_init_assess_tbl %>%
              select(MOTHER_ALF_E, SERVICE_USER_ETHNIC_GRP_CD, INITIAL_ASS_DT) %>%
              rename("ALF_E" = "MOTHER_ALF_E",
                     "ETHNIC" = "SERVICE_USER_ETHNIC_GRP_CD",
                     "RECORD_DATE" = "INITIAL_ASS_DT") %>%
              collect()) %>%
  arrange(PERSON_ID, desc(RECORD_DATE)) %>%
  group_by(PERSON_ID) %>%
  mutate(SEQ = row_number()) %>%
  filter(SEQ == 1) %>%
  select(PERSON_ID, ETHNIC)


# add ethnic group code to cohort data
c19_cohort20 <- c19_cohort20 %>%
  left_join(mids_ethnicity_lkp,
            by = "PERSON_ID")
with(c19_cohort20, table(ETHNIC))

# convert to upper case
c19_cohort20 <- c19_cohort20 %>%
  mutate(ETHNIC_GRP_CD = toupper(ETHNIC))
with(c19_cohort20, table(ETHNIC))

# create higher level Ethnicity grouping
c19_cohort20 <- c19_cohort20 %>%
  mutate(ETHNIC_CAT = case_when(ETHNIC %in% c("A") ~ 0,
                               ETHNIC %in% c("D", "E", "F", "G") ~ 1,
                               ETHNIC %in% c("H", "J", "K", "L") ~ 2,
                               ETHNIC %in% c("M", "N", "P") ~ 3,
                               ETHNIC %in% c("R", "S") ~ 4,
                               T ~ 9))
# create a final skinny table
c19_cohort20 <- c19_cohort20 %>%
  left_join(pregnancy_cohort %>%
              select(PERSON_ID, PREG_ID, PREG_START_DATE),
            by = "PERSON_ID") %>%
  mutate(AGE = as.integer(interval(WOB, PREG_START_DATE) %/% years(1))) %>%
  rename("SEX" = "GNDR_CD",
         "WIMD_2019_QUINTILES" = "WIMD2019_QUINTILE_INCEPTION") %>%
  select(PERSON_ID, PREG_ID, SEX, WOB, AGE, ETHNIC, ETHNIC_CAT, WIMD_2019_QUINTILES, DOD)

with(c19_cohort20, table(AGE))


# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_skinny)), c19_cohort20)
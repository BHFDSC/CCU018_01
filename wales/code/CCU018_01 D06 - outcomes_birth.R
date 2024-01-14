# CCU018_01_outcomes_birth

# Description: This script creates the outcomes at birth table

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

# Data Output:
# SAILWWMCCV.CCU018_01_outcomes

# Output Description:
# A table with required outcomes during each pregnancy in wide format (1 row per delivery)


# TO DO

# Tidy up output summaries
# And code tables by source
# do we need to do any cleaning for patients with outcome codes prior to pregnancy?
# format plot consistent with NHS D TRE, and select correct plot type

# run parameters script
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\CCU018_01 - 01 - parameters.R")


# rename current output tables to archive

# set archive date
archive_date <- str_replace_all(as.character(as.Date(out_date, '%Y-%m-%d')-1), "-", "")

# archive the current outcomes at birth table
curr_tab <- tbl(con, in_schema(dbc, path_outcomes_birth))
curr_dat <- curr_tab %>%
  collect()

# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_outcomes_birth, "_", archive_date)), curr_dat)
dbRemoveTable(con, SQL(paste0(dbc, ".", path_outcomes_birth)))


#---------------------------------------------.
# DATA - create connection to required tables
#---------------------------------------------.

# project cohort
project_cohort <- tbl(con, in_schema(dbc, path_cohort))

# project codelist
project_codelist <- tbl(con, in_schema(dbc, path_codelist))

# WLGP data
c19_wlgp_tbl <- tbl(con, in_schema(db, path_wlgp))

# PEDW data
c19_pedw_diag_tbl <- tbl(con, in_schema(db, path_pedw_diag))
c19_pedw_oper_tbl <- tbl(con, in_schema(db, path_pedw_oper))
c19_pedw_episode_tbl <- tbl(con, in_schema(db, path_pedw_episode))
c19_pedw_spell_tbl <- tbl(con, in_schema(db, path_pedw_spell))


# MIDS - Maternity INdicator Dataset
c19_mids_births_tbl <- tbl(con, in_schema(db, path_mids_birth))


#---------------------------------------------.
# PREPARE 
#---------------------------------------------.

# INDIVIDUALS AND BASELINE DATES
project_cohort <- project_cohort %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, DELIVERY_DATE)

# CHECK
nrow(project_cohort %>% distinct(PERSON_ID) %>% collect())
# 43,715 
nrow(project_cohort %>% distinct(PERSON_ID, PREG_ID) %>% collect())
# 44,448


# WLGP 

c19_wlgp_tbl <- c19_wlgp_tbl %>%
  rename("PERSON_ID" = "ALF_E",
         "CODE" = "EVENT_CD",
         "RECORD_DATE" = "EVENT_DT") %>%
  inner_join(project_cohort,
             by = "PERSON_ID")

# PEDW

c19_pedw_spell_tbl <- c19_pedw_spell_tbl %>%
  select(SPELL_NUM_E, PROV_UNIT_CD, ALF_E) %>%
  rename("PERSON_ID" = "ALF_E") %>%
  inner_join(project_cohort,
             by = "PERSON_ID")

c19_pedw_episode_tbl <- c19_pedw_episode_tbl %>%
  select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, EPI_STR_DT)

# MIDS

# BIRTHS
c19_mids_births_tbl <- c19_mids_births_tbl %>%
  rename("PERSON_ID" = "MOTHER_ALF_E") %>%
  inner_join(project_cohort,
             by = c("PERSON_ID"))

# CODELISTS
project_codelist <- project_codelist %>%
  filter(NAME %in% c("STILLBIRTH", "MULTIPLE_GESTATION"))

# check
with(project_codelist %>% collect(), table(TERMINOLOGY))
with(project_codelist %>% collect(), table(NAME, TERMINOLOGY))


#---------------------------------------------.
# BIRTH OUTCOMES
#---------------------------------------------.

## MULTIPLE GESTATION

# Derived from Birth Record Variables
multi_gest_delcode <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, DELIVERY_DATE, BIRTH_ORDER, LABOUR_ONSET_FOETUS_NUM, CHILD_ALF_E) %>%
  collect() %>%
  mutate(OUT_BIRTH_MULTI_GEST_DELCODE = case_when(BIRTH_ORDER > 1 | LABOUR_ONSET_FOETUS_NUM > 1 ~ 1,
                                                  T ~ 0),
         OUT_BIRTH_MULTI_GEST_DELCODE_BIRTH_ORDER = case_when(BIRTH_ORDER > 1 ~ 1,
                                                              T ~ 0),
         OUT_BIRTH_MULTI_GEST_DELCODE_FOETUS_NUM = case_when(LABOUR_ONSET_FOETUS_NUM > 1 ~ 1,
                                                             T ~ 0)) %>%
  arrange(PERSON_ID, PREG_ID, DELIVERY_DATE) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(OUT_BIRTH_MULTI_GEST_DELCODE = max(OUT_BIRTH_MULTI_GEST_DELCODE),
            OUT_BIRTH_MULTI_GEST_DELCODE_BIRTH_ORDER = max(OUT_BIRTH_MULTI_GEST_DELCODE_BIRTH_ORDER),
            OUT_BIRTH_MULTI_GEST_DELCODE_FOETUS_NUM = max(OUT_BIRTH_MULTI_GEST_DELCODE_FOETUS_NUM))

with(multi_gest_delcode, table(OUT_BIRTH_MULTI_GEST_DELCODE))
with(multi_gest_delcode, table(OUT_BIRTH_MULTI_GEST_DELCODE_BIRTH_ORDER, OUT_BIRTH_MULTI_GEST_DELCODE_FOETUS_NUM))

multi_gest_delcode <- multi_gest_delcode %>%
  select(-c(OUT_BIRTH_MULTI_GEST_DELCODE_BIRTH_ORDER, OUT_BIRTH_MULTI_GEST_DELCODE_FOETUS_NUM))


# Derived from presence of multiple CHILD_ALF_Es for a single MOTHER_ALF_E/BABY_BIRTH_DT combination
multi_gest_multi_id <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, CHILD_ALF_E) %>%
  collect() %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(CHILD_IDS = n_distinct(CHILD_ALF_E)) %>%
  mutate(OUT_BIRTH_MULTI_GEST_MULTI_ID = case_when(CHILD_IDS > 1 ~ 1,
                                                   T ~ 0))
with(multi_gest_multi_id, table(OUT_BIRTH_MULTI_GEST_MULTI_ID))

# Derived from PEDW and WLGP codes

# PEDW ICD10
multi_gest_pedw_icd10 <- c19_pedw_diag_tbl %>%
  select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, DIAG_CD_123) %>%
  rename("CODE" = "DIAG_CD_123") %>%
  union_all(c19_pedw_diag_tbl %>%
              select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, DIAG_CD_1234) %>%
              rename("CODE" = "DIAG_CD_1234")) %>%
  inner_join(c19_pedw_spell_tbl,
             by = c("SPELL_NUM_E", "PROV_UNIT_CD")) %>%
  left_join(c19_pedw_episode_tbl,
            by = c("SPELL_NUM_E", "PROV_UNIT_CD", "EPI_NUM")) %>%
  rename("RECORD_DATE" = "EPI_STR_DT") %>%
  inner_join(project_codelist %>%
               filter(TERMINOLOGY == "ICD10") %>%
               filter(NAME == "MULTIPLE_GESTATION"),
             by = "CODE") %>%
  select(-c(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM)) %>%
  collect()

# check
length(unique(multi_gest_pedw_icd10$PERSON_ID))

# date filter
multi_gest_pedw_icd10 <- multi_gest_pedw_icd10 %>%
  filter(RECORD_DATE >= PREG_START_DATE &
           RECORD_DATE <= (DELIVERY_DATE + days(28)))

# check
length(unique(multi_gest_pedw_icd10$PERSON_ID))


# PEDW OPCS4
multi_gest_pedw_opcs <- c19_pedw_oper_tbl %>%
  select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, OPER_CD_123) %>%
  rename("CODE" = "OPER_CD_123") %>%
  union_all(c19_pedw_oper_tbl %>%
              select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, OPER_CD) %>%
              rename("CODE" = "OPER_CD")) %>%
  inner_join(c19_pedw_spell_tbl,
             by = c("SPELL_NUM_E", "PROV_UNIT_CD")) %>%
  left_join(c19_pedw_episode_tbl,
            by = c("SPELL_NUM_E", "PROV_UNIT_CD", "EPI_NUM")) %>%
  rename("RECORD_DATE" = "EPI_STR_DT") %>%
  inner_join(project_codelist %>%
               filter(TERMINOLOGY == "OPCS4") %>%
               filter(NAME == "MULTIPLE_GESTATION"),
             by = "CODE") %>%
  select(-c(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM)) %>%
  collect()

# check
length(unique(multi_gest_pedw_opcs$PERSON_ID))

# date filter
multi_gest_pedw_opcs <- multi_gest_pedw_opcs %>%
  filter(RECORD_DATE >= PREG_START_DATE &
           RECORD_DATE <= (DELIVERY_DATE + days(28)))

# check
length(unique(multi_gest_pedw_opcs$PERSON_ID))


# combine PEDW ICD10 and OPCS4
multi_gest_pedw <- multi_gest_pedw_icd10 %>%
  bind_rows(multi_gest_pedw_opcs)


# WLGP

multi_gest_wlgp <- c19_wlgp_tbl %>%
  inner_join(project_codelist %>%
               filter(TERMINOLOGY == "READ") %>%
               filter(NAME == "MULTIPLE_GESTATION"),
             by = "CODE") %>%
  select(PERSON_ID, PREG_ID, CODE, DESC, NAME, TERMINOLOGY, RECORD_DATE, PREG_START_DATE, DELIVERY_DATE) %>%
  collect() 

# check
length(unique(multi_gest_wlgp$PERSON_ID))

# date filter
multi_gest_wlgp <- multi_gest_wlgp %>%
  filter(RECORD_DATE >= PREG_START_DATE &
           RECORD_DATE <= (DELIVERY_DATE + days(28)))
# check
length(unique(multi_gest_wlgp$PERSON_ID))


# Multiple Gestation Derived from PEDW and WLGP codes - Code Summary

multi_gest_codes_all_sources <- multi_gest_pedw %>%
  select(NAME, PERSON_ID, PREG_ID, CODE, TERMINOLOGY, DESC) %>%
  bind_rows(multi_gest_wlgp %>%
              select(NAME, PERSON_ID, PREG_ID, CODE, TERMINOLOGY, DESC))

multi_gest_code_summary <- project_codelist %>%
  filter(NAME == "MULTIPLE_GESTATION") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(multi_gest_codes_all_sources %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)


# PEDW - summarise for each ID
multi_gest_pedw <- multi_gest_pedw %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(OUT_BIRTH_MULTI_GEST_GENCODE_HES = 1) %>%
  distinct(PERSON_ID, PREG_ID, OUT_BIRTH_MULTI_GEST_GENCODE_HES)

# WLGP - summarise for each ID
multi_gest_wlgp <- multi_gest_wlgp %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(OUT_BIRTH_MULTI_GEST_GENCODE_GP = 1) %>%
  distinct(PERSON_ID, PREG_ID, OUT_BIRTH_MULTI_GEST_GENCODE_GP)


# Max Multiple Gestation Value from across all derivations
multi_gest_all <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(multi_gest_delcode, by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(multi_gest_multi_id, by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(multi_gest_pedw, by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(multi_gest_wlgp, by = c("PERSON_ID", "PREG_ID")) %>%
  mutate(OUT_BIRTH_MULTI_GEST_GENCODE_HES = case_when(is.na(OUT_BIRTH_MULTI_GEST_GENCODE_HES) ~ 0, T ~ OUT_BIRTH_MULTI_GEST_GENCODE_HES),
         OUT_BIRTH_MULTI_GEST_GENCODE_GP = case_when(is.na(OUT_BIRTH_MULTI_GEST_GENCODE_GP) ~ 0, T ~ OUT_BIRTH_MULTI_GEST_GENCODE_GP)) %>%
  rowwise() %>%
  mutate(OUT_BIRTH_MULTI_GEST_MAX = max(OUT_BIRTH_MULTI_GEST_DELCODE, 
                                        OUT_BIRTH_MULTI_GEST_MULTI_ID,
                                        OUT_BIRTH_MULTI_GEST_GENCODE_HES,
                                        OUT_BIRTH_MULTI_GEST_GENCODE_GP)) %>%
  select(PERSON_ID, PREG_ID, 
         OUT_BIRTH_MULTI_GEST_DELCODE, OUT_BIRTH_MULTI_GEST_MULTI_ID,
         OUT_BIRTH_MULTI_GEST_GENCODE_HES, OUT_BIRTH_MULTI_GEST_GENCODE_GP,
         OUT_BIRTH_MULTI_GEST_MAX)


# Multiple Gestation Source Summary
# OUT_BIRTH_MULTI_GEST_DELCODE, OUT_BIRTH_MULTI_GEST_MULTI_ID, OUT_BIRTH_MULTI_GEST_GENCODE_HES, OUT_BIRTH_MULTI_GEST_GENCODE_GP
multi_gest_all %>%
  mutate(SOURCE_SUMMARY = paste0(OUT_BIRTH_MULTI_GEST_DELCODE, OUT_BIRTH_MULTI_GEST_MULTI_ID,
                                 OUT_BIRTH_MULTI_GEST_GENCODE_HES, OUT_BIRTH_MULTI_GEST_GENCODE_GP)) %>%
  group_by(SOURCE_SUMMARY) %>%
  summarise(vol = n()) %>%
  arrange(SOURCE_SUMMARY)



# PRESENTATION

presentation <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, DELIVERY_DATE, LABOUR_ONSET_FOETAL_PRESENTATION_CD) %>%
  collect() %>%
  arrange(PERSON_ID, DELIVERY_DATE) %>%
  group_by(PERSON_ID, DELIVERY_DATE) %>% 
  mutate(SEQ = row_number()) %>%
  filter(SEQ == 1) %>%
  mutate(OUT_BIRTH_PRES_LAB = case_when(LABOUR_ONSET_FOETAL_PRESENTATION_CD == 1 ~ 1,
                                                    LABOUR_ONSET_FOETAL_PRESENTATION_CD == 2 ~ 2,
                                                    LABOUR_ONSET_FOETAL_PRESENTATION_CD == 8 ~ 8)) %>%
  rename("OUT_BIRTH_PRES_LAB_RECORD_DATE" = "DELIVERY_DATE") %>%
  select(PERSON_ID, PREG_ID, OUT_BIRTH_PRES_LAB, OUT_BIRTH_PRES_LAB_RECORD_DATE)

with(presentation, table(OUT_BIRTH_PRES_LAB))
length(unique(presentation$PERSON_ID))
nrow(presentation %>%
       distinct(PERSON_ID, OUT_BIRTH_PRES_LAB_RECORD_DATE))


# INDUCTION OF LABOUR

ind_labour <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, DELIVERY_DATE, LABOUR_ONSET_MODE_CD) %>%
  collect() %>%
  arrange(PERSON_ID, DELIVERY_DATE) %>%
  group_by(PERSON_ID, DELIVERY_DATE) %>% 
  mutate(SEQ = row_number()) %>%
  filter(SEQ == 1) %>%
  mutate(OUT_BIRTH_IND_LAB = case_when(LABOUR_ONSET_MODE_CD %in% c("1", "2") ~ 0,
                                       LABOUR_ONSET_MODE_CD %in% c("3", "4", "5") ~ 1)) %>%
  rename("OUT_BIRTH_IND_LAB_RECORD_DATE" = "DELIVERY_DATE") %>%
  select(PERSON_ID, PREG_ID, OUT_BIRTH_IND_LAB, OUT_BIRTH_IND_LAB_RECORD_DATE)

with(ind_labour, table(OUT_BIRTH_IND_LAB))
length(unique(ind_labour$PERSON_ID))
nrow(ind_labour %>%
       distinct(PERSON_ID, OUT_BIRTH_IND_LAB))


# MODE OF DELIVERY

mode_del <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, DELIVERY_DATE, LABOUR_ONSET_MODE_CD) %>%
  collect() %>%
  arrange(PERSON_ID, DELIVERY_DATE) %>%
  group_by(PERSON_ID, DELIVERY_DATE) %>% 
  mutate(SEQ = row_number()) %>%
  filter(SEQ == 1) %>%
  mutate(OUT_BIRTH_MODE_DEL = case_when(LABOUR_ONSET_MODE_CD == 1 ~ 1,
                                         LABOUR_ONSET_MODE_CD == 2 ~ 2,
                                         LABOUR_ONSET_MODE_CD == 3 ~ 2, 
                                         LABOUR_ONSET_MODE_CD == 4 ~ 3,
                                         LABOUR_ONSET_MODE_CD == 5 ~ 4)) %>%
  rename("OUT_BIRTH_MODE_DEL_RECORD_DATE" = "DELIVERY_DATE") %>%
  select(PERSON_ID, PREG_ID, OUT_BIRTH_MODE_DEL, OUT_BIRTH_MODE_DEL_RECORD_DATE)

with(mode_del, table(OUT_BIRTH_MODE_DEL))
length(unique(mode_del$PERSON_ID))
nrow(mode_del %>%
       distinct(PERSON_ID, OUT_BIRTH_MODE_DEL_RECORD_DATE))


# Low Birth Weight for Gestational Age
# Drop out an multiple gestation deliveries

lbw_gestage <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, LABOUR_ONSET_GEST_WEEKS, SERVICE_USER_WEIGHT_GRAMS) %>%
  collect() %>%
  left_join(multi_gest_all %>%
              select(PERSON_ID, PREG_ID, OUT_BIRTH_MULTI_GEST_MAX),
            by = c("PERSON_ID", "PREG_ID")) %>%
  filter(OUT_BIRTH_MULTI_GEST_MAX == 0) %>%
  select(-OUT_BIRTH_MULTI_GEST_MAX) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(LABOUR_ONSET_GEST_WEEKS = min(LABOUR_ONSET_GEST_WEEKS),
            SERVICE_USER_WEIGHT_GRAMS = min(SERVICE_USER_WEIGHT_GRAMS))



# flag records in bottom 5% of BW values within each GEST WEEK
# only create a LBW flag where the number of births in cohort is >= 100
lbw_gestage <- lbw_gestage %>%
  group_by(LABOUR_ONSET_GEST_WEEKS) %>%
  mutate(bw_vtile = ntile(SERVICE_USER_WEIGHT_GRAMS, 20),
         births = n()) %>%
  ungroup() %>%
  mutate(OUT_BIRTH_SMALL_GEST_AGE = case_when(bw_vtile == 1 & births >= 100 ~ 1, T ~ 0))


# plot of birth weight by gestation weeks - flagged bottom 5% in each gest week
lbw_gestage %>%
  mutate(OUT_BIRTH_SMALL_GEST_AGE = factor(OUT_BIRTH_SMALL_GEST_AGE,
                              levels = c(0, 1),
                              labels = c("N", "Y"),
                              ordered = T)) %>%
  filter(LABOUR_ONSET_GEST_WEEKS > 30) %>%
  filter(LABOUR_ONSET_GEST_WEEKS != 99) %>%
  filter(!(is.na(LABOUR_ONSET_GEST_WEEKS))) %>%
  filter(!(is.na(SERVICE_USER_WEIGHT_GRAMS))) %>%
  ggplot() +
  geom_point(aes(x = LABOUR_ONSET_GEST_WEEKS, y = SERVICE_USER_WEIGHT_GRAMS, colour = OUT_BIRTH_SMALL_GEST_AGE),
             position = "jitter",
             size = 0.5) +
  scale_y_continuous(breaks = seq(0, 6000, 500)) +
  theme_few() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.9)) +
  labs(x = "Gestation Weeks",
       y = "Birthweight (g)")


# Birthweight by Gestation Week summary
lbw_gest_week_summary <- lbw_gestage %>%
  mutate(lbw = case_when(OUT_BIRTH_SMALL_GEST_AGE == 1 ~ SERVICE_USER_WEIGHT_GRAMS, T ~ 0)) %>%
  group_by(LABOUR_ONSET_GEST_WEEKS) %>%
  summarise(births = n(),
            min_bw = min(SERVICE_USER_WEIGHT_GRAMS, na.rm = T),
            pctile5 = max(lbw, na.rm = T),
            max_bw = max(SERVICE_USER_WEIGHT_GRAMS, na.rm = T)) %>%
  filter(births >= 100)

# keep only variables required for final dataset
lbw_gestage <- lbw_gestage %>%
  select(PERSON_ID, PREG_ID, OUT_BIRTH_SMALL_GEST_AGE)


# PRETERM / VERY PRETERM / EXTREMELY PRETERM
# Note that there are ~ 17 pregnancies that have > 1 gest weeks value
# we will take the minimum gest weeka value in these cases

preterm <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, LABOUR_ONSET_GEST_WEEKS) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(LABOUR_ONSET_GEST_WEEKS = min(LABOUR_ONSET_GEST_WEEKS, na.rm = T)) %>%
  collect() 

nrow(preterm %>%
       distinct(PERSON_ID, PREG_ID, LABOUR_ONSET_GEST_WEEKS))

preterm <- preterm %>%
  mutate(OUT_BIRTH_PRETERM = case_when(LABOUR_ONSET_GEST_WEEKS < 37 ~ 1, T ~ 0),
         OUT_BIRTH_VERY_PRETERM = case_when(LABOUR_ONSET_GEST_WEEKS < 32 ~ 1, T ~ 0),
         OUT_BIRTH_EXT_PRETERM = case_when(LABOUR_ONSET_GEST_WEEKS < 28 ~ 1, T ~ 0)) %>%
  select(-LABOUR_ONSET_GEST_WEEKS)


with(preterm, table(OUT_BIRTH_PRETERM))
with(preterm, table(OUT_BIRTH_VERY_PRETERM))
with(preterm, table(OUT_BIRTH_EXT_PRETERM))

# check
length(unique(preterm$PERSON_ID))
nrow(preterm %>%
       distinct(PERSON_ID, PREG_ID, OUT_BIRTH_PRETERM))


# ABNORMAL APGAR

abnormal_apgar <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, BIRTH_APGAR_SCORE) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(BIRTH_APGAR_SCORE = min(BIRTH_APGAR_SCORE, na.rm = T)) %>%
  collect() 

nrow(abnormal_apgar %>%
       distinct(PERSON_ID, PREG_ID, BIRTH_APGAR_SCORE))

abnormal_apgar <- abnormal_apgar %>%
  mutate(OUT_BIRTH_ABNORMAL_APGAR = case_when(BIRTH_APGAR_SCORE < 7 ~ 1, T ~ 0)) %>%
  select(-BIRTH_APGAR_SCORE)


with(abnormal_apgar, table(OUT_BIRTH_ABNORMAL_APGAR))

# check
length(unique(abnormal_apgar$PERSON_ID))
nrow(abnormal_apgar %>%
       distinct(PERSON_ID, PREG_ID, OUT_BIRTH_ABNORMAL_APGAR))


# STILLBIRTH

## Stillbirth derived from Birth Record variables

stillbirth_delvar <- c19_mids_births_tbl %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  select(PERSON_ID, PREG_ID, BIRTH_OUTCOME_CD) %>%
  collect() 

nrow(stillbirth_delvar %>%
       distinct(PERSON_ID, PREG_ID, BIRTH_OUTCOME_CD))

stillbirth_delvar <- stillbirth_delvar %>%
  mutate(OUT_BIRTH_STILLBIRTH_DELVAR = case_when(BIRTH_OUTCOME_CD == 2 ~ 1, T ~ 0)) %>%
  select(-BIRTH_OUTCOME_CD) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(OUT_BIRTH_STILLBIRTH_DELVAR = max(OUT_BIRTH_STILLBIRTH_DELVAR))


nrow(stillbirth_delvar %>%
       distinct(PERSON_ID, PREG_ID, OUT_BIRTH_STILLBIRTH_DELVAR))

with(stillbirth_delvar, table(OUT_BIRTH_STILLBIRTH_DELVAR))

# check
length(unique(stillbirth_delvar$PERSON_ID))
nrow(stillbirth_delvar %>%
       distinct(PERSON_ID, PREG_ID, OUT_BIRTH_STILLBIRTH_DELVAR))


## Stillbirth derived from Codelists

## Stillbirth from PEDW ICD-10

# PEDW

stillbirth_pedw <- c19_pedw_diag_tbl %>%
  select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, DIAG_CD_123) %>%
  rename("CODE" = "DIAG_CD_123") %>%
  union_all(c19_pedw_diag_tbl %>%
              select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, DIAG_CD_1234) %>%
              rename("CODE" = "DIAG_CD_1234")) %>%
  inner_join(c19_pedw_spell_tbl,
             by = c("SPELL_NUM_E", "PROV_UNIT_CD")) %>%
  left_join(c19_pedw_episode_tbl,
            by = c("SPELL_NUM_E", "PROV_UNIT_CD", "EPI_NUM")) %>%
  rename("RECORD_DATE" = "EPI_STR_DT") %>%
  inner_join(project_codelist %>%
               filter(TERMINOLOGY == "ICD10") %>%
               filter(NAME == "STILLBIRTH"),
             by = "CODE") %>%
  select(-c(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM)) %>%
  collect()


# check
length(unique(stillbirth_pedw$PERSON_ID))

# date filter
stillbirth_pedw <- stillbirth_pedw %>%
  filter(RECORD_DATE >= (PREG_START_DATE + weeks(24)) &
           RECORD_DATE <= (DELIVERY_DATE + days(28)))

# check
length(unique(stillbirth_pedw$PERSON_ID))

# WLGP

stillbirth_wlgp <- c19_wlgp_tbl %>%
  inner_join(project_codelist %>%
               filter(TERMINOLOGY == "READ") %>%
               filter(NAME == "STILLBIRTH"),
             by = "CODE") %>%
  select(PERSON_ID, PREG_ID, CODE, DESC, NAME, TERMINOLOGY, RECORD_DATE, PREG_START_DATE, DELIVERY_DATE) %>%
  collect() 

# check
length(unique(stillbirth_wlgp$PERSON_ID))

# date filter
stillbirth_wlgp <- stillbirth_wlgp %>%
  filter(RECORD_DATE >= (PREG_START_DATE + weeks(24)) &
           RECORD_DATE <= (DELIVERY_DATE + days(28)))
# check
length(unique(stillbirth_wlgp$PERSON_ID))


# Stillbirth Derived from PEDW and WLGP codes - Code Summary

stillbirth_codes_all_sources <- stillbirth_pedw %>%
  select(NAME, PERSON_ID, PREG_ID, CODE, TERMINOLOGY, DESC) %>%
  bind_rows(stillbirth_wlgp %>%
              select(NAME, PERSON_ID, PREG_ID, CODE, TERMINOLOGY, DESC))

stillbirth_code_summary <- project_codelist %>%
  filter(NAME == "STILLBIRTH") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(stillbirth_codes_all_sources %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)


# summarise for each ID
stillbirth_pedw <- stillbirth_pedw %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(OUT_BIRTH_STILLBIRTH_GENCODE_HES = 1) %>%
  distinct(PERSON_ID, PREG_ID, OUT_BIRTH_STILLBIRTH_GENCODE_HES)

# summarise for each ID
stillbirth_wlgp <- stillbirth_wlgp %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(OUT_BIRTH_STILLBIRTH_GENCODE_GP = 1) %>%
  distinct(PERSON_ID, PREG_ID, OUT_BIRTH_STILLBIRTH_GENCODE_GP)

# Max Stillbirth Value from across all derivations
stillbirth_all <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(stillbirth_delvar, by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(stillbirth_pedw, by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(stillbirth_wlgp, by = c("PERSON_ID", "PREG_ID")) %>%
  rowwise() %>%
  mutate(OUT_BIRTH_STILLBIRTH_GENCODE_HES = case_when(is.na(OUT_BIRTH_STILLBIRTH_GENCODE_HES) ~ 0, T ~ OUT_BIRTH_STILLBIRTH_GENCODE_HES),
         OUT_BIRTH_STILLBIRTH_GENCODE_GP = case_when(is.na(OUT_BIRTH_STILLBIRTH_GENCODE_GP) ~ 0, T ~ OUT_BIRTH_STILLBIRTH_GENCODE_GP)) %>%
  mutate(OUT_BIRTH_STILLBIRTH_MAX = max(OUT_BIRTH_STILLBIRTH_DELVAR, 
                                        OUT_BIRTH_STILLBIRTH_GENCODE_HES,
                                        OUT_BIRTH_STILLBIRTH_GENCODE_GP)) %>%
  select(PERSON_ID, PREG_ID, 
         OUT_BIRTH_STILLBIRTH_DELVAR, 
         OUT_BIRTH_STILLBIRTH_GENCODE_HES, 
         OUT_BIRTH_STILLBIRTH_GENCODE_GP, 
         OUT_BIRTH_STILLBIRTH_MAX)

with(stillbirth_all, table(OUT_BIRTH_STILLBIRTH_DELVAR, OUT_BIRTH_STILLBIRTH_GENCODE_HES))
with(stillbirth_all, table(OUT_BIRTH_STILLBIRTH_GENCODE_HES, OUT_BIRTH_STILLBIRTH_GENCODE_GP))


# Stillbirth Source Summary
# OUT_BIRTH_STILLBIRTH_DELVAR, OUT_BIRTH_STILLBIRTH_GENCODE_HES, OUT_BIRTH_STILLBIRTH_GENCODE_GP
stillbirth_all %>%
  mutate(SOURCE_SUMMARY = paste0(OUT_BIRTH_STILLBIRTH_DELVAR, 
                                 OUT_BIRTH_STILLBIRTH_GENCODE_HES,
                                 OUT_BIRTH_STILLBIRTH_GENCODE_GP)) %>%
  group_by(SOURCE_SUMMARY) %>%
  summarise(vol = n()) %>%
  arrange(SOURCE_SUMMARY)

#---------------------------------------------.
# CREATE FINAL BIRTH OUTCOMES TABLE
#---------------------------------------------.

out_birth <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(multi_gest_all,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(lbw_gestage,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(preterm,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(abnormal_apgar,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(stillbirth_all,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(presentation,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(ind_labour,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(mode_del,
            by = c("PERSON_ID", "PREG_ID"))


# configure final variable order
out_birth <- out_birth %>%
  select(PERSON_ID, PREG_ID,
         OUT_BIRTH_MULTI_GEST_DELCODE, OUT_BIRTH_MULTI_GEST_MULTI_ID,
         OUT_BIRTH_MULTI_GEST_GENCODE_HES, OUT_BIRTH_MULTI_GEST_GENCODE_GP,
         OUT_BIRTH_MULTI_GEST_MAX,
         OUT_BIRTH_STILLBIRTH_DELVAR, OUT_BIRTH_STILLBIRTH_GENCODE_HES, OUT_BIRTH_STILLBIRTH_GENCODE_GP, OUT_BIRTH_STILLBIRTH_MAX,
         OUT_BIRTH_SMALL_GEST_AGE,
         OUT_BIRTH_PRES_LAB,
         OUT_BIRTH_IND_LAB,
         OUT_BIRTH_MODE_DEL,
         OUT_BIRTH_PRETERM, OUT_BIRTH_VERY_PRETERM, OUT_BIRTH_EXT_PRETERM,
         OUT_BIRTH_ABNORMAL_APGAR)

# write covariates table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_outcomes_birth)), out_birth)

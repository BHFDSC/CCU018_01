# CCU018_01_covariates

# Description: This script creates a 

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
# SAILWWMCCV.CCU018_01_outcomes

# Output Description:
# A table with required covariates identified in records up to pregnancy start date


# TO DO
# How to deal with records with incorrect/defaulted dates 1900,0001?
# Need to fix the Diabetes Name in the READ Phenotype table
# add operations (last 12 months) - what chapters does Elena currently use and what does Tom do?
# missing Corticosteroids / Immunosuppressants from medication history
# resolve differences in how smoking derived in SAIL/TRE
# PHEN_READ_OBESITY needs limited to the Category = "Diagnosis of Obesity"

# run parameters script
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\CCU018_01 - 01 - parameters.R")

# rename current output tables to archive

# set archive date
archive_date <- str_replace_all(as.character(as.Date(out_date, '%Y-%m-%d')-1), "-", "")

# archive the cohort table
curr_tab <- tbl(con, in_schema(dbc,path_covariates))
curr_dat <- curr_tab %>%
  collect()

# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_covariates, "_", archive_date)), curr_dat)
dbRemoveTable(con, SQL(paste0(dbc, ".", path_covariates)))




#---------------------------------------------.
# DATA - create connection to required tables
#---------------------------------------------.

# project cohort
project_cohort <- tbl(con, in_schema(dbc, path_cohort))
project_skinny <- tbl(con, in_schema(dbc, path_skinny))

# codelists
project_codelist <- tbl(con, in_schema(dbc, path_codelist))
codelist_smoking <- tbl(con, in_schema(dbc, path_codelist_smoking))
codelist_bmi <- tbl(con, in_schema(dbc, path_codelist_bmi))
codelist_surgery <- data.frame(CODE = LETTERS[!(LETTERS %in% c("U", "X", "Y", "Z"))])


# WLGP data
c19_wlgp_tbl <- tbl(con, in_schema(db, path_wlgp))

# PEDW data
c19_pedw_diag_tbl <- tbl(con, in_schema(db, path_pedw_diag))
c19_pedw_oper_tbl <- tbl(con, in_schema(db, path_pedw_oper))
c19_pedw_episode_tbl <- tbl(con, in_schema(db, path_pedw_episode))
c19_pedw_spell_tbl <- tbl(con, in_schema(db, path_pedw_spell))

# WDDS data
c19_wdds_tbl <- tbl(con, in_schema(db, path_wdds))

# MIDS
c19_mids_births_tbl <- tbl(con, in_schema(db, path_mids_birth))
c19_mids_init_assess_tbl <- tbl(con, in_schema(db, path_mids_init_assess))


#---------------------------------------------.
# PREPARE 
#---------------------------------------------.

# INDIVIDUALS AND CENSOR START AND CENSOR END DATES
project_cohort <- project_cohort %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, DELIVERY_DATE) %>%
  left_join(project_skinny %>%
              select(PERSON_ID, PREG_ID, WOB),
            by = c("PERSON_ID", "PREG_ID")) %>%
  rename("CENSOR_DATE_START" = "WOB",
         "CENSOR_DATE_END" = "PREG_START_DATE")

# CHECK
nrow(project_cohort %>% distinct(PERSON_ID) %>% collect())
# 43,715 
nrow(project_cohort %>% distinct(PERSON_ID, DELIVERY_DATE) %>% collect())
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

# WDDS

# DMD
c19_wdds_dmd_tbl <- c19_wdds_tbl %>%
  select(ALF_E, DT_PRESCRIBED, DMDCODE_PRESCRIBED) %>%
  rename("PERSON_ID" = "ALF_E",
         "CODE" = "DMDCODE_PRESCRIBED",
         "RECORD_DATE" = "DT_PRESCRIBED") %>%
  inner_join(project_cohort,
             by = "PERSON_ID")
# BNF
c19_wdds_bnf_tbl <- c19_wdds_tbl %>%
  select(ALF_E, DT_PRESCRIBED, BNF_COMBINED) %>%
  rename("PERSON_ID" = "ALF_E",
         "CODE" = "BNF_COMBINED",
         "RECORD_DATE" = "DT_PRESCRIBED") %>%
  inner_join(project_cohort,
             by = "PERSON_ID")

# MIDS

# BIRTHS
c19_mids_births_tbl <- c19_mids_births_tbl %>%
  rename("PERSON_ID" = "MOTHER_ALF_E") %>%
  inner_join(project_cohort,
             by = c("PERSON_ID"))

# INIT

c19_mids_init_assess_tbl <- c19_mids_init_assess_tbl %>%
  rename("PERSON_ID" = "MOTHER_ALF_E") %>%
  inner_join(project_cohort,
             by = c("PERSON_ID"))


#---------------------------------------------.
# MEDICAL HISTORY COVARIATES
#---------------------------------------------.

# prepare required codelist
codelist_hx <- project_codelist %>%
  filter(COVARIATE == 1 & !(TERMINOLOGY == "BNF"))

# check
with(codelist_hx %>% collect(), table(TERMINOLOGY))
with(codelist_hx %>% collect(), table(NAME, TERMINOLOGY))


# WLGP
hx_wlgp <- c19_wlgp_tbl %>%
  inner_join(codelist_hx %>%
               filter(TERMINOLOGY == "READ"),
             by = "CODE") %>%
  collect() 

# check
length(unique(hx_wlgp$PERSON_ID))

# date filter
hx_wlgp <- hx_wlgp %>%
  filter(RECORD_DATE >= (CENSOR_DATE_START %m+% months(-12)) &
           RECORD_DATE <= CENSOR_DATE_END) 

# check
length(unique(hx_wlgp$PERSON_ID))  

# keep required variables and add source label
hx_wlgp <- hx_wlgp %>%
  select(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, DESC, TERMINOLOGY, CENSOR_DATE_END) %>%
  mutate(SOURCE = "WLGP")

# code summary
hx_wlgp_code_summary <- codelist_hx %>%
  filter(TERMINOLOGY == "READ") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(hx_wlgp %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(NAME, desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)

# PEDW

hx_pedw <- c19_pedw_diag_tbl %>%
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
  inner_join(codelist_hx %>%
               filter(TERMINOLOGY == "ICD10"),
             by = "CODE") %>%
  select(-c(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM)) %>%
  collect()

# check
length(unique(hx_pedw$PERSON_ID))

# date filter
hx_pedw <- hx_pedw %>%
  filter(RECORD_DATE >= (CENSOR_DATE_START %m+% months(-12)) &
           RECORD_DATE <= CENSOR_DATE_END)

# check
length(unique(hx_pedw$PERSON_ID))

# keep required variables and add source label
hx_pedw <- hx_pedw %>%
  select(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, DESC, TERMINOLOGY, CENSOR_DATE_END) %>%
  mutate(SOURCE = "PEDW")

# code summary
hx_pedw_code_summary <- codelist_hx %>%
  filter(TERMINOLOGY == "ICD10") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(hx_pedw %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)

# WDDS

hx_wdds <- c19_wdds_dmd_tbl %>%
  inner_join(codelist_hx %>%
               filter(TERMINOLOGY == "DMD"),
             by = "CODE") %>%
  collect()

# check
length(unique(hx_wdds$PERSON_ID))

# date filter
hx_wdds <- hx_wdds %>%
  filter(RECORD_DATE >= (CENSOR_DATE_START %m+% months(-12)) &
           RECORD_DATE <= CENSOR_DATE_END)

# check
length(unique(hx_wdds$PERSON_ID))

# keep required variables and add source label
hx_wdds <- hx_wdds %>%
  distinct(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, DESC, TERMINOLOGY, CENSOR_DATE_END) %>%
  mutate(SOURCE = "WDDS")

# code summary
hx_wdds_code_summary <- codelist_hx %>%
  filter(TERMINOLOGY == "DMD") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(hx_wdds %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)

# COMBINE DATA FROM ALL SOURCES

hx_all <- hx_wlgp %>%
  bind_rows(hx_pedw,
            hx_wdds)

# create medical history covariates summary

hx_summary <- codelist_hx %>%
  distinct(NAME) %>%
  collect() %>%
  mutate(NAME = str_replace_all(NAME, " ", "")) %>%
  left_join(hx_all %>%
              group_by(NAME) %>%
              summarise(n_all = n(),
                        n_id_all = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  left_join(hx_all %>%
              filter(SOURCE == "WLGP") %>%
              group_by(NAME) %>%
              summarise(n_wlgp = n(),
                        n_id_wlgp = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  left_join(hx_all %>%
              filter(SOURCE == "PEDW") %>%
              group_by(NAME) %>%
              summarise(n_pedw = n(),
                        n_id_pedw = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  left_join(hx_all %>%
              filter(SOURCE == "WDDS") %>%
              group_by(NAME) %>%
              summarise(n_wdds = n(),
                        n_id_wdds = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  arrange(NAME)

# write to directory
out_path <- paste0("output/cov_hx_summary_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(hx_summary,
          file = out_path,
          row.names = F)



# GET FIRST RECORD_DATE FOR EACH PREGNANCY/NAME 

hx_first <- hx_all %>%
  arrange(PERSON_ID, PREG_ID, NAME, RECORD_DATE) %>%
  group_by(PERSON_ID, PREG_ID, NAME) %>%
  mutate(REC_SEQ = row_number()) %>%
  filter(REC_SEQ == 1)

# Visualise each outcome by source and date

hx_first <- hx_first %>%
  mutate(DIFF = interval(CENSOR_DATE_END, RECORD_DATE) %/% months(1))

hx_first %>%
  filter(year(RECORD_DATE) >= 1990) %>%
  group_by(NAME, DIFF, SOURCE) %>%
  summarise(vol = n()) %>%
  ggplot() + 
  geom_line(aes(x = DIFF, y = vol, colour = SOURCE),
            size = 1,
            stat = "identity") +
  geom_point(aes(x = DIFF, y = vol, colour = SOURCE),
             size = 1,
             stat = "identity") +
  theme_few() +
  facet_wrap(~ NAME,
             scales = "free_y") +
  theme(strip.text = element_text(hjust = 0, size = 7),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank(), legend.position = "bottom") +
  labs(x = "", y = "")


# wide format
# make sure to include all covariates - even those with no matches
hx_wide <- codelist_hx %>% 
  distinct(NAME) %>%
  collect() %>%
  full_join(hx_first,
            by = "NAME") %>%
  select(PERSON_ID, PREG_ID, NAME, RECORD_DATE) %>%
  mutate(NAME = paste0("COV_HX_", NAME),
         FLAG = 1) %>%
  pivot_wider(id_cols = c(PERSON_ID, PREG_ID),
              names_from = NAME,
              values_from = c(RECORD_DATE, FLAG),
              names_glue = "{NAME}_{.value}") %>%
  mutate_at(vars(matches("FLAG")), ~ case_when(is.na(.) ~ 0, T ~ .))


# re-order columns
cols = order(colnames(hx_wide))
cols = cols[!(cols %in% c("PERSON_ID", "PREG_ID"))]
hx_wide <- hx_wide %>%
  select(PERSON_ID, PREG_ID, all_of(cols)) %>%
  arrange(PERSON_ID)

# check
length(unique(hx_wide$PERSON_ID))
nrow(hx_wide %>%
       distinct(PERSON_ID, PREG_ID))

length(unique(hx_all$PERSON_ID))
nrow(hx_all %>%
       distinct(PERSON_ID, PREG_ID))

# write out a combined code summary
hx_code_summary <- hx_wlgp_code_summary %>% mutate(SOURCE = "WLGP") %>%
  bind_rows(hx_pedw_code_summary %>% mutate(SOURCE = "PEDW"),
            hx_wdds_code_summary %>% mutate(SOURCE = "WDDS")) %>%
  filter(n_recs > 0) %>%
  select(NAME, SOURCE, CODE, DESC, TERMINOLOGY, n_recs, n_id) %>%
  arrange(NAME, desc(n_id))

# write to directory
out_path <- paste0("output/cov_hx_code_summary_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(hx_code_summary,
          file = out_path,
          row.names = F)


#---------------------------------------------.
# MEDICATION HISTORY
#---------------------------------------------.

# prepare required codelist
codelist_meds <- project_codelist %>%
  filter(COVARIATE == 1) %>%
  filter(TERMINOLOGY == "BNF")

# check
with(codelist_meds %>% collect(), table(TERMINOLOGY))
with(codelist_meds %>% collect(), table(NAME, TERMINOLOGY))


# WDDS

meds <- c19_wdds_bnf_tbl %>%
  inner_join(codelist_meds,
             by = "CODE") %>%
  collect() 

# check
length(unique(meds$PERSON_ID))

# date filter
meds <- meds %>%
  filter(RECORD_DATE >= (CENSOR_DATE_END %m+% months(-6)) &
           RECORD_DATE <= CENSOR_DATE_END)
# check
length(unique(meds$PERSON_ID))

# keep required variables and add source label
meds <- meds %>%
  distinct(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, DESC, TERMINOLOGY, CENSOR_DATE_END) %>%
  mutate(SOURCE = "WDDS")

# code summary
meds_wdds_code_summary <- project_codelist %>%
  filter(COVARIATE == 1) %>%
  filter(TERMINOLOGY == "BNF") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(meds %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)

# create medical history covariates summary

meds_summary <- codelist_meds %>%
  distinct(NAME) %>%
  collect() %>%
  mutate(NAME = str_replace_all(NAME, " ", "")) %>%
  left_join(meds %>%
              group_by(NAME) %>%
              summarise(n_all = n(),
                        n_id_all = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  left_join(meds %>%
              filter(SOURCE == "WDDS") %>%
              group_by(NAME) %>%
              summarise(n_wdds = n(),
                        n_id_wdds = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  arrange(NAME)

# write to directory
out_path <- paste0("output/cov_meds_summary_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(meds_summary,
          file = out_path,
          row.names = F)

# GET FIRST RECORD_DATE FOR EACH PREGNANCY/NAME 

meds_first <- meds %>%
  arrange(PERSON_ID, PREG_ID, NAME, RECORD_DATE) %>%
  group_by(PERSON_ID, PREG_ID, NAME) %>%
  mutate(REC_SEQ = row_number()) %>%
  filter(REC_SEQ == 1)

# CHECK

# Visualise each outcome by source and date

meds_first <- meds_first %>%
  mutate(DIFF = interval(CENSOR_DATE_END, RECORD_DATE) %/% months(1))

meds_first %>%
  filter(year(RECORD_DATE) >= 1990) %>%
  group_by(NAME, DIFF, SOURCE) %>%
  summarise(vol = n()) %>%
  ggplot() + 
  geom_line(aes(x = DIFF, y = vol, colour = SOURCE),
            size = 1,
            stat = "identity") +
  geom_point(aes(x = DIFF, y = vol, colour = SOURCE),
             size = 1,
             stat = "identity") +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  facet_wrap(~NAME,
             scales = "free_y") +
  theme(strip.text = element_text(hjust = 0, size = 7),
        axis.text = element_text(size = 6)) +
  labs(x = "", y = "")

# wide format
# make sure to include all covariates - even those with no matches
meds_wide <- codelist_meds %>% 
  distinct(NAME) %>%
  collect() %>%
  full_join(meds_first,
            by = "NAME") %>%
  select(PERSON_ID, PREG_ID, NAME, RECORD_DATE) %>%
  mutate(NAME = paste0("COV_MEDS_", NAME),
         FLAG = 1) %>%
  pivot_wider(id_cols = c(PERSON_ID, PREG_ID),
              names_from = NAME,
              values_from = c(RECORD_DATE, FLAG),
              names_glue = "{NAME}_{.value}") %>%
  mutate_at(vars(matches("FLAG")), ~ case_when(is.na(.) ~ 0, T ~ .))

# re-order columns
cols = order(colnames(meds_wide))
cols = cols[!(cols %in% c("PERSON_ID", "PREG_ID"))]
meds_wide <- meds_wide %>%
  select(PERSON_ID, PREG_ID, all_of(cols)) %>%
  arrange(PERSON_ID)

length(unique(meds_wide$PERSON_ID))
nrow(meds_wide %>%
       distinct(PERSON_ID, PREG_ID))

length(unique(meds$PERSON_ID))
nrow(meds %>%
       distinct(PERSON_ID, PREG_ID))

# write out medication code summary
meds_wdds_code_summary <- meds_wdds_code_summary %>%
  mutate(SOURCE = "WDDS") %>%
  filter(n_recs > 0) %>%
  select(NAME, SOURCE, CODE, DESC, TERMINOLOGY, n_recs, n_id) %>%
  arrange(NAME, desc(n_id))

out_path <- paste0("output/cov_meds_code_summary_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(meds_wdds_code_summary,
          file = out_path,
          row.names = F)


#---------------------------------------------.
# SMOKING
#---------------------------------------------.

# WLGP

smoking <- c19_wlgp_tbl %>%
  inner_join(codelist_smoking %>%
               select(NAME, CODE, DESC, CATEGORY),
             by = "CODE") %>%
  collect()

# check
length(unique(smoking$PERSON_ID))

# date filter
smoking <- smoking %>%
  filter(RECORD_DATE >= (CENSOR_DATE_START %m+% months(-12)) &
           RECORD_DATE <= CENSOR_DATE_END)

# check
length(unique(smoking$PERSON_ID))

# take latest record and associated category
smoking <- smoking %>%
  arrange(PERSON_ID, PREG_ID, RECORD_DATE) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  mutate(rn = row_number(),
         max_rn = max(rn))

smoking_last <- smoking %>%
  filter(rn == max(rn)) %>%
  mutate(CATEGORY = str_replace_all(CATEGORY, " ", "")) %>%
  mutate(COV_SMOKING_STATUS = case_when(CATEGORY == "N" ~ "Never",
                                    CATEGORY == "S" ~ "Current",
                                    CATEGORY == "E" ~ "Ex",
                                    T ~ "Unknown")) %>%
  rename("COV_SMOKING_STATUS_RECORD_DATE" = "RECORD_DATE") %>%
  select(PERSON_ID, PREG_ID, COV_SMOKING_STATUS, COV_SMOKING_STATUS_RECORD_DATE)

# Need to add flags for impossible upgrades, and flags for all smoking statuses and dates

# checks

length(unique(smoking$PERSON_ID))
nrow(smoking %>%
       distinct(PERSON_ID, PREG_ID))

length(unique(smoking_last$PERSON_ID))
nrow(smoking_last %>%
       distinct(PERSON_ID, PREG_ID))

smoking_last %>%
  group_by(COV_SMOKING_STATUS) %>%
  summarise(vol = n())


# SMOKING MIDS INITIAL ASSESSMENT

smoking_mids_init <- c19_mids_init_assess_tbl %>%
  select(PERSON_ID, PREG_ID, CENSOR_DATE_END, INITIAL_ASS_DT, SERVICE_USER_SMOKER_STS_CD, GEST_WEEKS) %>%
  filter(GEST_WEEKS < 20 | !(is.na(GEST_WEEKS))) %>%
  collect() %>%
  mutate(PREGNANCY_START_EST = INITIAL_ASS_DT - (7*GEST_WEEKS)) %>%
  filter(abs(difftime(PREGNANCY_START_EST, CENSOR_DATE_END, units = "days")) < 30) %>%
  filter(SERVICE_USER_SMOKER_STS_CD %in% c(1, 2)) %>%
  rename("COV_SMOKING_MIDS_INIT_RECORD_DATE" = "INITIAL_ASS_DT") %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(COV_SMOKING_MIDS_INIT_RECORD_DATE = min(COV_SMOKING_MIDS_INIT_RECORD_DATE)) %>%
  mutate(COV_SMOKING_MIDS_INIT_FLAG = 1) %>%
  select(PERSON_ID, PREG_ID, COV_SMOKING_MIDS_INIT_FLAG, COV_SMOKING_MIDS_INIT_RECORD_DATE)

# check
with(smoking_mids_init, table(COV_SMOKING_MIDS_INIT_FLAG))  

# check agreement between 2 smoking status sources
smoking_comp <- smoking_last %>%
  inner_join(smoking_mids_init,
             by = "PERSON_ID")
with(smoking_comp, table(COV_SMOKING_STATUS, COV_SMOKING_MIDS_INIT_FLAG))  


# SMOKING MIDS BIRTH RECORDS

smoking_mids_birth <-  c19_mids_births_tbl %>%
  select(PERSON_ID, PREG_ID, DELIVERY_DATE, BABY_BIRTH_DT, BIRTH_ORDER, MAT_SERVICE_USER_SMOKER_STS_CD) %>%
  filter(DELIVERY_DATE == BABY_BIRTH_DT) %>%
  collect() %>%
  filter(MAT_SERVICE_USER_SMOKER_STS_CD %in% c(1, 2)) %>%
  rename("COV_SMOKING_MIDS_BIRTH_RECORD_DATE" = "DELIVERY_DATE") %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(COV_SMOKING_MIDS_BIRTH_RECORD_DATE = min(COV_SMOKING_MIDS_BIRTH_RECORD_DATE)) %>%
  mutate(COV_SMOKING_MIDS_BIRTH_FLAG = 1) %>%
  select(PERSON_ID, PREG_ID, COV_SMOKING_MIDS_BIRTH_FLAG, COV_SMOKING_MIDS_BIRTH_RECORD_DATE)

# check agreement between 2 smoking status sources
smoking_comp <- smoking_last %>%
  inner_join(smoking_mids_birth,
             by = "PERSON_ID")
with(smoking_comp, table(COV_SMOKING_MIDS_BIRTH_FLAG, COV_SMOKING_STATUS))  


# check agreement between 2 MIDS sources
smoking_comp <- smoking_mids_init %>%
  full_join(smoking_mids_birth,
             by = "PERSON_ID") %>%
  mutate(COV_SMOKING_MIDS_INIT_FLAG = case_when(is.na(COV_SMOKING_MIDS_INIT_FLAG) ~ 0, T ~ COV_SMOKING_MIDS_INIT_FLAG),
         COV_SMOKING_MIDS_BIRTH_FLAG = case_when(is.na(COV_SMOKING_MIDS_BIRTH_FLAG) ~ 0, T ~ COV_SMOKING_MIDS_BIRTH_FLAG))

with(smoking_comp, table(COV_SMOKING_MIDS_INIT_FLAG, COV_SMOKING_MIDS_BIRTH_FLAG))  


#---------------------------------------------.
# BMI
#---------------------------------------------.

bmi <- c19_wlgp_tbl %>%
  inner_join(codelist_bmi %>%
               select(NAME, CODE, DESC, CATEGORY),
             by = "CODE") %>%
  collect()

# check
length(unique(bmi$PERSON_ID))

# date filter
bmi <- bmi %>%
  filter(RECORD_DATE >= CENSOR_DATE_START &
           RECORD_DATE <= CENSOR_DATE_END)

length(unique(bmi$PERSON_ID))

# take latest record and associated category
bmi <- bmi %>%
  arrange(PERSON_ID, PREG_ID, RECORD_DATE) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  mutate(rn = row_number(),
         max_rn = max(rn)) %>%
  ungroup() %>%
  mutate(age = as.integer(interval(WOB, RECORD_DATE) %/% years(1)))

bmi_last <- bmi %>%
  filter(rn == max_rn) %>%
  filter(!(is.na(EVENT_VAL)) & EVENT_VAL >= 12 & EVENT_VAL <= 100) %>%
  filter(age >= 18) %>%
  mutate(COV_BMI_VALUE_OBESE = case_when(EVENT_VAL >= 30 ~ 1,
                                 T ~0)) %>%
  rename("COV_BMI_DATE" = "RECORD_DATE",
         "COV_BMI_VALUE" = "EVENT_VAL") %>%
  select(PERSON_ID, PREG_ID, COV_BMI_VALUE, COV_BMI_DATE, COV_BMI_VALUE_OBESE)

# checks

length(unique(bmi$PERSON_ID))
nrow(bmi %>%
       distinct(PERSON_ID, PREG_ID))

length(unique(bmi_last$PERSON_ID))
nrow(bmi_last %>%
       distinct(PERSON_ID, PREG_ID))

# check distribution of BMI
bmi_last %>%
  ggplot() +
  geom_histogram(aes(x = COV_BMI_VALUE),
                 binwidth = 1,
                 fill = "lightblue") +
  theme_few() +
  labs(title = "BMI Distribution",
       x = "BMI", y = "n")


#---------------------------------------------.
# SURGERY IN LAST YEAR / or last 6 months
#---------------------------------------------.

# PEDW

surgery <- c19_pedw_oper_tbl %>%
  select(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM, OPER_DT, OPER_CD_123, OPER_NUM) %>%
  mutate(CODE = str_sub(OPER_CD_123, 1, 1)) %>%
  inner_join(c19_pedw_spell_tbl,
            by = c("SPELL_NUM_E", "PROV_UNIT_CD")) %>%
  rename("RECORD_DATE" = "OPER_DT") %>%
  collect() %>%
  inner_join(codelist_surgery,
             by = "CODE") %>%
  select(-c(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM))

# check
length(unique(surgery$PERSON_ID))

# date filter
surgery <- surgery %>%
  filter(RECORD_DATE >= (CENSOR_DATE_END %m+% months(-12)) &
           RECORD_DATE <= CENSOR_DATE_END)

# check
length(unique(surgery$PERSON_ID))

# keep required variables and add source label
surgery <- surgery %>%
  select(-CODE) %>%
  rename("CODE" = "OPER_CD_123") %>%
  mutate(NAME = "COV_SURG_LAST_YR",
         TERMINOLOGY = "OPCS",
         SOURCE = "PEDW") %>%
  select(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, OPER_NUM, TERMINOLOGY, SOURCE)

# FIRST 

surgery_first <- surgery %>%
  arrange(PERSON_ID, PREG_ID, NAME, RECORD_DATE, OPER_NUM) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  mutate(REC_SEQ = row_number()) %>%
  filter(REC_SEQ == 1) %>%
  mutate(COV_SURG_LAST_YR_FLAG = 1) %>%
  rename("COV_SURG_LAST_YR_RECORD_DATE" = "RECORD_DATE",
         "COV_SURG_LAST_YR_CODE" = "CODE") %>%
  select(PERSON_ID, PREG_ID, COV_SURG_LAST_YR_RECORD_DATE, COV_SURG_LAST_YR_FLAG, COV_SURG_LAST_YR_CODE)


#---------------------------------------------.
# UNIQUE BNF CHAPTERS (Year to Preg Start)
#---------------------------------------------.

unique_bnf_chapters <- c19_wdds_bnf_tbl %>%
  mutate(BNF_CHAPTER = str_sub(CODE, 1, 2)) %>%
  collect()

# check
length(unique(unique_bnf_chapters$PERSON_ID))

# date filter
unique_bnf_chapters <- unique_bnf_chapters %>%
  filter(RECORD_DATE >= (CENSOR_DATE_END %m+% months(-12)) &
           RECORD_DATE <= CENSOR_DATE_END)

# check
length(unique(unique_bnf_chapters$PERSON_ID))

unique_bnf_chapters <- unique_bnf_chapters %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(COV_N_UNIQUE_BNF_CHAPTERS = n_distinct(BNF_CHAPTER))

# check distribution of Unique BNF Chapters
unique_bnf_chapters %>%
  ggplot() +
  geom_histogram(aes(x = COV_N_UNIQUE_BNF_CHAPTERS),
                 binwidth = 1,
                 fill = "lightblue") +
  theme_few() +
  labs(title = "Unique BNF Chapters Distribution",
       x = "BNF Chapters", y = "n")



#---------------------------------------------.
# PREGNANCY COVARIATES
#---------------------------------------------.

# PREVIOUS MULTIPLE PREGNANCIES

# Previous multiple pregnancies derived from birth record variables
prev_multi_gest_delvar <- c19_mids_births_tbl %>%
  filter(BABY_BIRTH_DT < DELIVERY_DATE) %>%
  select(PERSON_ID, PREG_ID, DELIVERY_DATE, BIRTH_ORDER, LABOUR_ONSET_FOETUS_NUM) %>%
  collect() %>%
  mutate(COV_HX_MULTI_GEST_DELCODE = case_when(BIRTH_ORDER > 1 | LABOUR_ONSET_FOETUS_NUM > 1 ~ 1,
                                    T ~ 0),
         COV_HX_MULTI_GEST_DELCODE_BIRTH_ORDER = case_when(BIRTH_ORDER > 1 ~ 1,
                                               T ~ 0),
         COV_HX_MULTI_GEST_DELCODE_FOETUS_NUM = case_when(LABOUR_ONSET_FOETUS_NUM > 1 ~ 1,
                                               T ~ 0)) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(COV_HX_MULTI_GEST_DELCODE = max(COV_HX_MULTI_GEST_DELCODE),
            COV_HX_MULTI_GEST_DELCODE_BIRTH_ORDER = max(COV_HX_MULTI_GEST_DELCODE_BIRTH_ORDER),
            COV_HX_MULTI_GEST_DELCODE_FOETUS_NUM = max(COV_HX_MULTI_GEST_DELCODE_FOETUS_NUM))

with(prev_multi_gest_delvar, table(COV_HX_MULTI_GEST_DELCODE))
with(prev_multi_gest_delvar, table(COV_HX_MULTI_GEST_DELCODE_BIRTH_ORDER, COV_HX_MULTI_GEST_DELCODE_FOETUS_NUM))

prev_multi_gest_delvar <- prev_multi_gest_delvar %>%
  select(-c(COV_HX_MULTI_GEST_DELCODE_BIRTH_ORDER, COV_HX_MULTI_GEST_DELCODE_FOETUS_NUM))

# Previous multiple pregnancies derived from multiple CHILD IDS associated with a single MOTHER_ID/DELIVERY DATE combination
prev_multi_gest_multi_id <- c19_mids_births_tbl %>%
  filter(BABY_BIRTH_DT < DELIVERY_DATE) %>%
  select(PERSON_ID, PREG_ID, BABY_BIRTH_DT, CHILD_ALF_E) %>%
  collect() %>%
  group_by(PERSON_ID, PREG_ID, BABY_BIRTH_DT) %>%
  summarise(CHILD_IDS = n_distinct(CHILD_ALF_E)) %>%
  mutate(COV_HX_MULTI_GEST_MULTI_ID = case_when(CHILD_IDS > 1 ~ 1,
                                                  T ~ 0)) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(COV_HX_MULTI_GEST_MULTI_ID = max(COV_HX_MULTI_GEST_MULTI_ID))
  
with(prev_multi_gest_multi_id, table(COV_HX_MULTI_GEST_MULTI_ID))

# Previous Pregnancies derived from max of all other derivations
prev_multi_gest_all <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(prev_multi_gest_delvar,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(prev_multi_gest_multi_id,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(hx_wide %>%
            select(PERSON_ID, PREG_ID, COV_HX_MULTIPLE_GESTATION_FLAG),
            by = c("PERSON_ID", "PREG_ID")) %>%
  mutate(COV_HX_MULTI_GEST_DELCODE = case_when(is.na(COV_HX_MULTI_GEST_DELCODE) ~ 0, T ~ COV_HX_MULTI_GEST_DELCODE),
         COV_HX_MULTI_GEST_MULTI_ID = case_when(is.na(COV_HX_MULTI_GEST_MULTI_ID) ~ 0, T ~ COV_HX_MULTI_GEST_MULTI_ID),
         COV_HX_MULTIPLE_GESTATION_FLAG = case_when(is.na(COV_HX_MULTIPLE_GESTATION_FLAG) ~ 0, T ~ COV_HX_MULTIPLE_GESTATION_FLAG)) %>%
  rowwise() %>%
  mutate(COV_HX_MULTI_GEST_MAX = max(COV_HX_MULTI_GEST_DELCODE,
                                     COV_HX_MULTI_GEST_MULTI_ID,
                                     COV_HX_MULTIPLE_GESTATION_FLAG,
                                    na.rm = T))


# Previous Multiple Gestation Source Summary
# COV_HX_MULTI_GEST_DELCODE, COV_HX_MULTI_GEST_MULTI_ID, COV_HX_MULTIPLE_GESTATION_FLAG
prev_multi_gest_all %>%
  mutate(SOURCE_SUMMARY = paste0(COV_HX_MULTI_GEST_DELCODE,
                                 COV_HX_MULTI_GEST_MULTI_ID,
                                 COV_HX_MULTIPLE_GESTATION_FLAG)) %>%
  group_by(SOURCE_SUMMARY) %>%
  summarise(vol = n()) %>%
  arrange(SOURCE_SUMMARY)


# PREVIOUS PREGNANCIES

# INIT ASSESS
prev_pregn_init_assess <- c19_mids_init_assess_tbl %>%
  select(PERSON_ID, PREG_ID, DELIVERY_DATE, INITIAL_ASS_DT, SERVICE_USER_GRAVIDA_CD, GEST_WEEKS) %>%
  filter(!(is.na(GEST_WEEKS))) %>%
  collect() %>%
  filter(INITIAL_ASS_DT < DELIVERY_DATE) %>%
  filter(!(is.na(SERVICE_USER_GRAVIDA_CD))) %>%
  mutate(SERVICE_USER_GRAVIDA_CD = as.numeric(SERVICE_USER_GRAVIDA_CD)) %>%
  mutate(COV_HX_PREGNANCY_INIT_ASSESS_VAR = case_when(SERVICE_USER_GRAVIDA_CD > 1 ~ 1,
                                T ~ 0)) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(COV_HX_PREGNANCY_INIT_ASSESS_VAR = max(COV_HX_PREGNANCY_INIT_ASSESS_VAR))

with(prev_pregn_init_assess, table(COV_HX_PREGNANCY_INIT_ASSESS_VAR))

# MIDS BIRTHS
prev_pregn_birth <- c19_mids_births_tbl %>%
  mutate(COV_HX_PREGNANCY_DELRECS = case_when(BABY_BIRTH_DT < DELIVERY_DATE ~ 1,
                                                T ~ 0)) %>%
  group_by(PERSON_ID, PREG_ID) %>%
  summarise(COV_HX_PREGNANCY_DELRECS = max(COV_HX_PREGNANCY_DELRECS, na.rm = T)) %>%
  collect()

with(prev_pregn_birth, table(COV_HX_PREGNANCY_DELRECS))


# Previous Pregnancies derived from max of all other derivations
prev_pregn_all <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(prev_pregn_init_assess,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(prev_pregn_birth,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(hx_wide %>%
              select(PERSON_ID, PREG_ID, COV_HX_PREGNANCY_FLAG),
            by = c("PERSON_ID", "PREG_ID")) %>%
  mutate(COV_HX_PREGNANCY_FLAG = case_when(is.na(COV_HX_PREGNANCY_FLAG) ~ 0, T ~ COV_HX_PREGNANCY_FLAG)) %>%
  rowwise() %>%
  mutate(COV_HX_PREGNANCY_MAX = max(COV_HX_PREGNANCY_INIT_ASSESS_VAR,
                                    COV_HX_PREGNANCY_DELRECS,
                                    COV_HX_PREGNANCY_FLAG,
                                    na.rm = T))

# Previous Multiple Gestation Source Summary
# COV_HX_PREGNANCY_INIT_ASSESS_VAR, COV_HX_PREGNANCY_DELRECS, COV_HX_PREGNANCY_FLAG
prev_pregn_all %>%
  mutate(SOURCE_SUMMARY = paste0(COV_HX_PREGNANCY_INIT_ASSESS_VAR,
                                 COV_HX_PREGNANCY_DELRECS,
                                 COV_HX_PREGNANCY_FLAG)) %>%
  group_by(SOURCE_SUMMARY) %>%
  summarise(vol = n()) %>%
  arrange(SOURCE_SUMMARY)



#---------------------------------------------.
# CREATE FINAL COVARIATES TABLE
#---------------------------------------------.

covariates_out <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(hx_wide %>%
              select(-c(COV_HX_PREGNANCY_FLAG, COV_HX_PREGNANCY_RECORD_DATE,
                        COV_HX_MULTIPLE_GESTATION_FLAG, COV_HX_MULTIPLE_GESTATION_RECORD_DATE)),
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(meds_wide,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(smoking_last,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(smoking_mids_init,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(smoking_mids_birth,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(bmi_last,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(surgery_first,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(unique_bnf_chapters,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(prev_pregn_all,
            by = c("PERSON_ID", "PREG_ID")) %>%
  left_join(prev_multi_gest_all,
            by = c("PERSON_ID", "PREG_ID")) %>%
  mutate_at(vars(matches("FLAG")), ~ case_when(is.na(.) ~ 0, T ~ .)) %>%
  mutate(COV_N_UNIQUE_BNF_CHAPTERS = case_when(is.na(COV_N_UNIQUE_BNF_CHAPTERS) ~ 0L, T ~ COV_N_UNIQUE_BNF_CHAPTERS))
  
# need the bmi obesity flag to retain NA where bmi is NA


# create the composite covariate variables
covariates_out <- covariates_out %>%
  rowwise() %>%
  mutate(COV_HX_ARTERIAL_FLAG = max(COV_HX_AMI_FLAG, COV_HX_STROKE_IS_FLAG, COV_HX_STROKE_NOS_FLAG, COV_HX_OTHER_ARTERIAL_EMBOLISM_FLAG, na.rm = T),
         COV_HX_ARTERIAL_RECORD_DATE = min(COV_HX_AMI_RECORD_DATE, COV_HX_STROKE_IS_RECORD_DATE, COV_HX_STROKE_NOS_RECORD_DATE, COV_HX_OTHER_ARTERIAL_EMBOLISM_RECORD_DATE, na.rm = T),
         COV_HX_VENOUS_FLAG = max(COV_HX_DVT_FLAG, COV_HX_DVT_OTHER_FLAG, COV_HX_ICVT_FLAG, COV_HX_ICVT_PREGNANCY_FLAG, COV_HX_DVT_PREGNANCY_FLAG, COV_HX_PVT_FLAG, COV_HX_PE_FLAG, na.rm = T),
         COV_HX_VENOUS_RECORD_DATE = min(COV_HX_DVT_RECORD_DATE, COV_HX_DVT_OTHER_RECORD_DATE, COV_HX_ICVT_RECORD_DATE, COV_HX_ICVT_PREGNANCY_RECORD_DATE, COV_HX_DVT_PREGNANCY_RECORD_DATE, COV_HX_PVT_RECORD_DATE, COV_HX_PE_RECORD_DATE, na.rm = T),
         COV_HX_HAEMOTOLOGICAL_FLAG = max(COV_HX_DIC_FLAG, COV_HX_TTP_FLAG, COV_HX_THROMBOCYTOPENIA_FLAG, na.rm = T),
         COV_HX_HAEMOTOLOGICAL_RECORD_DATE = min(COV_HX_DIC_RECORD_DATE, COV_HX_TTP_RECORD_DATE, COV_HX_THROMBOCYTOPENIA_RECORD_DATE, na.rm = T),
         COV_HX_OTHER_CVD_FLAG = max(COV_HX_STROKE_SAH_FLAG, COV_HX_STROKE_HS_FLAG, COV_HX_MESENTERIC_THROMBUS_FLAG, COV_HX_ARTERY_DISSECT_FLAG,
                                COV_HX_LIFE_ARRHYTHMIAS_FLAG, COV_HX_CARDIOMYOPATHY_FLAG, COV_HX_HF_FLAG,
                                COV_HX_ANGINA_FLAG, COV_HX_ANGINA_UNSTABLE_FLAG, na.rm = T),
         COV_HX_OTHER_CVD_RECORD_DATE = min(COV_HX_STROKE_SAH_RECORD_DATE, COV_HX_STROKE_HS_RECORD_DATE, COV_HX_MESENTERIC_THROMBUS_RECORD_DATE, COV_HX_ARTERY_DISSECT_RECORD_DATE,
                                     COV_HX_LIFE_ARRHYTHMIAS_RECORD_DATE, COV_HX_CARDIOMYOPATHY_RECORD_DATE, COV_HX_HF_RECORD_DATE,
                                     COV_HX_ANGINA_RECORD_DATE, COV_HX_ANGINA_UNSTABLE_RECORD_DATE, na.rm = T)) %>%
  ungroup()


# configure final variable order
covariates_out <- covariates_out %>%
  select(PERSON_ID, PREG_ID,
         COV_BMI_VALUE, COV_BMI_DATE, COV_BMI_VALUE_OBESE,
         COV_HX_BMI_OBESITY_FLAG, COV_HX_BMI_OBESITY_RECORD_DATE,
         COV_SMOKING_MIDS_INIT_FLAG, COV_SMOKING_MIDS_INIT_RECORD_DATE,
         COV_SMOKING_MIDS_BIRTH_FLAG, COV_SMOKING_MIDS_BIRTH_RECORD_DATE,
         COV_SMOKING_STATUS, COV_SMOKING_STATUS_RECORD_DATE,
         COV_HX_ARTERIAL_FLAG, COV_HX_ARTERIAL_RECORD_DATE,
         COV_HX_VENOUS_FLAG, COV_HX_VENOUS_RECORD_DATE,
         COV_HX_HAEMOTOLOGICAL_FLAG, COV_HX_HAEMOTOLOGICAL_RECORD_DATE,
         COV_HX_OTHER_CVD_FLAG, COV_HX_OTHER_CVD_RECORD_DATE,
         COV_HX_PREECLAMPSIA_FLAG, COV_HX_PREECLAMPSIA_RECORD_DATE,
         COV_HX_GEST_DIABETES_FLAG, COV_HX_GEST_DIABETES_RECORD_DATE,
         COV_SURG_LAST_YR_FLAG, COV_SURG_LAST_YR_CODE, COV_SURG_LAST_YR_RECORD_DATE,
         COV_HX_HYPERTENSION_FLAG, COV_HX_HYPERTENSION_RECORD_DATE, 
         COV_HX_HYPERTENSION_DRUGS_FLAG, COV_HX_HYPERTENSION_DRUGS_RECORD_DATE,
         COV_HX_DIABETES_FLAG, COV_HX_DIABETES_DRUGS_RECORD_DATE,
         COV_HX_DIABETES_DRUGS_FLAG, COV_HX_DIABETES_DRUGS_RECORD_DATE,
         COV_HX_DEPRESSION_FLAG, COV_HX_DEPRESSION_RECORD_DATE,
         COV_HX_CANCER_FLAG, COV_HX_CANCER_RECORD_DATE,
         COV_HX_COPD_FLAG, COV_HX_COPD_RECORD_DATE,
         COV_HX_CKD_FLAG, COV_HX_CKD_RECORD_DATE,
         COV_HX_LIVER_DISEASE_FLAG, COV_HX_LIVER_DISEASE_RECORD_DATE,
         COV_HX_PREGNANCY_INIT_ASSESS_VAR, COV_HX_PREGNANCY_DELRECS, COV_HX_PREGNANCY_FLAG,
         COV_HX_PREGNANCY_MAX,
         COV_HX_MULTI_GEST_DELCODE, COV_HX_MULTI_GEST_MULTI_ID, COV_HX_MULTIPLE_GESTATION_FLAG,
         COV_HX_MULTI_GEST_MAX,
         COV_HX_AMI_FLAG, COV_HX_AMI_RECORD_DATE,
         COV_HX_STROKE_IS_FLAG, COV_HX_STROKE_IS_RECORD_DATE,
         COV_HX_STROKE_NOS_FLAG, COV_HX_STROKE_NOS_RECORD_DATE,
         COV_HX_OTHER_ARTERIAL_EMBOLISM_FLAG, COV_HX_OTHER_ARTERIAL_EMBOLISM_RECORD_DATE,
         COV_HX_DVT_FLAG, COV_HX_DVT_RECORD_DATE, 
         COV_HX_DVT_OTHER_FLAG, COV_HX_DVT_OTHER_RECORD_DATE,
         COV_HX_ICVT_FLAG, COV_HX_ICVT_RECORD_DATE,
         COV_HX_ICVT_PREGNANCY_FLAG, COV_HX_ICVT_PREGNANCY_RECORD_DATE,
         COV_HX_DVT_PREGNANCY_FLAG, COV_HX_DVT_PREGNANCY_RECORD_DATE,
         COV_HX_PVT_FLAG, COV_HX_PVT_RECORD_DATE,
         COV_HX_PE_FLAG, COV_HX_PE_RECORD_DATE,
         COV_HX_DIC_FLAG, COV_HX_DIC_RECORD_DATE,
         COV_HX_TTP_FLAG, COV_HX_TTP_RECORD_DATE,
         COV_HX_THROMBOCYTOPENIA_FLAG, COV_HX_THROMBOCYTOPAENIA_RECORD_DATE,
         COV_HX_STROKE_SAH_FLAG, COV_HX_STROKE_SAH_RECORD_DATE,
         COV_HX_STROKE_HS_FLAG, COV_HX_STROKE_HS_RECORD_DATE,
         COV_HX_MESENTERIC_THROMBUS_FLAG, COV_HX_MESENTERIC_THROMBUS_RECORD_DATE,
         COV_HX_ARTERY_DISSECT_FLAG, COV_HX_ARTERY_DISSECT_RECORD_DATE,
         COV_HX_LIFE_ARRHYTHMIAS_FLAG, COV_HX_LIFE_ARRHYTHMIAS_RECORD_DATE,
         COV_HX_CARDIOMYOPATHY_FLAG, COV_HX_CARDIOMYOPATHY_RECORD_DATE,
         COV_HX_HF_FLAG, COV_HX_HF_RECORD_DATE,
         COV_HX_ANGINA_FLAG, COV_HX_ANGINA_RECORD_DATE,
         COV_HX_ANGINA_UNSTABLE_FLAG, COV_HX_ANGINA_UNSTABLE_RECORD_DATE)
         

# need to recode inf date values to NA for write to DB0
covariates_out$COV_HX_ARTERIAL_RECORD_DATE[is.infinite(covariates_out$COV_HX_ARTERIAL_RECORD_DATE)] <- NA
covariates_out$COV_HX_VENOUS_RECORD_DATE[is.infinite(covariates_out$COV_HX_VENOUS_RECORD_DATE)] <- NA
covariates_out$COV_HX_HAEMOTOLOGICAL_RECORD_DATE[is.infinite(covariates_out$COV_HX_HAEMOTOLOGICAL_RECORD_DATE)] <- NA
covariates_out$COV_HX_OTHER_CVD_RECORD_DATE[is.infinite(covariates_out$COV_HX_OTHER_CVD_RECORD_DATE)] <- NA


# write covariates table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_covariates)), covariates_out)
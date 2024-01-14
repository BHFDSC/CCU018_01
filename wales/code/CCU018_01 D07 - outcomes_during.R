# CCU018_01_outcomes_during

# Description: This script create s the table which captures the outcomes during pregnancy 

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
# DTHS

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

# archive the current outcomes during table
curr_tab <- tbl(con, in_schema(dbc, path_outcomes_during))
curr_dat <- curr_tab %>%
  collect()

# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_outcomes_during, "_", archive_date)), curr_dat)
dbRemoveTable(con, SQL(paste0(dbc, ".", path_outcomes_during)))


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
c19_pedw_episode_tbl <- tbl(con, in_schema(db, path_pedw_episode))
c19_pedw_spell_tbl <- tbl(con, in_schema(db, path_pedw_spell))

# DEATHS data
c19_deaths_tbl <- tbl(con, in_schema(db, path_deaths))

# MIDS
c19_mids_births_tbl <- tbl(con, in_schema(db, path_mids_birth))
c19_mids_init_assess_tbl <- tbl(con, in_schema(db, path_mids_init_assess))


#---------------------------------------------.
# PREPARE 
#---------------------------------------------.

# INDIVIDUALS AND BASELINE DATES
project_cohort <- project_cohort %>%
  select(PERSON_ID, PREG_ID, PREG_START_DATE, DELIVERY_DATE) %>%
  rename("CENSOR_DATE_START" = "PREG_START_DATE",
         "CENSOR_DATE_END" = "DELIVERY_DATE")

# CHECK
nrow(project_cohort %>% distinct(PERSON_ID) %>% collect())
# 43,715 
nrow(project_cohort %>% distinct(PERSON_ID, CENSOR_DATE_END) %>% collect())
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

c19_pedw_diag_tbl <- c19_pedw_diag_tbl %>%
  filter(DIAG_NUM == 1)

# DEATHS

# create a deaths table with long format cause of death coding
deaths_long <- c19_deaths_tbl %>%
  select(ALF_E, DOD,
         COD1, COD2, COD3, COD4,
         COD5, COD6, COD7, COD8) %>%
  pivot_longer(cols = starts_with("COD"),
               names_to = "POS",
               names_prefix = "COD",
               values_to = "DIAG_CD",
               values_drop_na = T) %>%
  rename("PERSON_ID" = "ALF_E") %>%
  inner_join(project_cohort,
             by = "PERSON_ID")

deaths_long <- deaths_long %>%
  filter(POS == 1)


#---------------------------------------------.
# OUTCOMES DURING PREGNANCY
#---------------------------------------------.

codelist_outcomes <- project_codelist %>%
  filter(OUTCOME == 1) %>%
  filter(!(NAME %in% c("STILLBIRTH", "PRETERM")))

# check
with(codelist_outcomes %>% collect(), table(TERMINOLOGY))
with(codelist_outcomes %>% collect(), table(NAME, TERMINOLOGY))

# WLGP

out_wlgp <- c19_wlgp_tbl %>%
  inner_join(codelist_outcomes %>%
               filter(TERMINOLOGY == "READ"),
             by = "CODE") %>%
  collect() 

# check
length(unique(out_wlgp$PERSON_ID))

# date filter
out_wlgp <- out_wlgp %>%
  filter(RECORD_DATE >= CENSOR_DATE_START & 
           RECORD_DATE <= CENSOR_DATE_END) 

# check
length(unique(out_wlgp$PERSON_ID))  

# keep required variables and add source label
out_wlgp <- out_wlgp %>%
  select(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, DESC, TERMINOLOGY, CENSOR_DATE_START, CENSOR_DATE_END) %>%
  mutate(SOURCE = "WLGP")


# code summary
out_wlgp_code_summary <- codelist_outcomes %>%
  filter(TERMINOLOGY == "READ") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(out_wlgp %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)


# PEDW

out_pedw <- c19_pedw_diag_tbl %>%
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
  inner_join(codelist_outcomes %>%
               filter(TERMINOLOGY == "ICD10"),
             by = "CODE") %>%
  select(-c(SPELL_NUM_E, PROV_UNIT_CD, EPI_NUM)) %>%
  collect()

# check
length(unique(out_pedw$PERSON_ID))

# date filter
out_pedw <- out_pedw %>%
  filter(RECORD_DATE >= CENSOR_DATE_START & 
           RECORD_DATE <= CENSOR_DATE_END)

# check
length(unique(out_pedw$PERSON_ID))

# keep required variables and add source label
out_pedw <- out_pedw %>%
  select(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, DESC, TERMINOLOGY, CENSOR_DATE_START, CENSOR_DATE_END) %>%
  mutate(SOURCE = "PEDW")

# code summary
out_pedw_code_summary <- codelist_outcomes %>%
  filter(TERMINOLOGY == "ICD10") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(out_pedw %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)


# DEATHS

# merge long format death table to codelist
out_deaths <- deaths_long %>%
  mutate(DIAG_CD = str_sub(DIAG_CD, 1, 3)) %>%
  rename("CODE" = "DIAG_CD") %>%
  union_all(deaths_long %>%
              mutate(DIAG_CD = str_sub(DIAG_CD, 1, 4)) %>%
              rename("CODE" = "DIAG_CD")) %>%
  rename("RECORD_DATE" = "DOD") %>%
  inner_join(codelist_outcomes %>%
               filter(TERMINOLOGY == "ICD10"),
             by = "CODE") %>%
  collect()

# check
length(unique(out_deaths$PERSON_ID))

# date filter
out_deaths <- out_deaths %>%
  filter(RECORD_DATE >= CENSOR_DATE_START & 
           RECORD_DATE <= CENSOR_DATE_END)

# check
length(unique(out_deaths$PERSON_ID))

# keep required variables and add source label
out_deaths <- out_deaths %>%
  select(PERSON_ID, PREG_ID, NAME, CODE, RECORD_DATE, DESC, TERMINOLOGY, CENSOR_DATE_START, CENSOR_DATE_END) %>%
  mutate(SOURCE = "DTHS")

# code summary
out_dths_code_summary <- codelist_outcomes %>%
  filter(TERMINOLOGY == "ICD10") %>%
  collect() %>%
  mutate(CODE = str_replace_all(CODE, " ", "")) %>%
  left_join(out_deaths %>%
              mutate(CODE = str_replace_all(CODE, " ", "")) %>%
              group_by(CODE) %>%
              summarise(n_recs = n(),
                        n_id = n_distinct(PERSON_ID)),
            by = "CODE") %>%
  arrange(desc(n_id)) %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  select(NAME, CODE, DESC, TERMINOLOGY, n_recs, n_id)
 


# combine data from 3 sources and take earliest record for each patient and outcome

out_all <- out_wlgp %>%
  bind_rows(out_pedw,
            out_deaths)

# create outcomes during pregnancy summary

out_dur_summary <- codelist_outcomes %>%
  distinct(NAME) %>%
  collect() %>%
  mutate(NAME = str_replace_all(NAME, " ", "")) %>%
  left_join(out_all %>%
              group_by(NAME) %>%
              summarise(n_all = n(),
                        n_id_all = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  left_join(out_all %>%
              filter(SOURCE == "WLGP") %>%
              group_by(NAME) %>%
              summarise(n_wlgp = n(),
                        n_id_wlgp = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  left_join(out_all %>%
              filter(SOURCE == "PEDW") %>%
              group_by(NAME) %>%
              summarise(n_pedw = n(),
                        n_id_pedw = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  left_join(out_all %>%
              filter(SOURCE == "DTHS") %>%
              group_by(NAME) %>%
              summarise(n_dths = n(),
                        n_id_dths = n_distinct(PERSON_ID)) %>%
              mutate(NAME = str_replace_all(NAME, " ", "")),
            by = "NAME") %>%
  mutate_at(vars(starts_with("n_")), ~ case_when(is.na(.) ~ 0L, T ~ .)) %>%
  arrange(NAME)

# write to directory
out_path <- paste0("output/out_dur_summary_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(out_dur_summary,
          file = out_path,
          row.names = F)

# GET FIRST RECORD_DATE FOR EACH PREGNANCY/NAME 
out_first <- out_all %>%
  arrange(PERSON_ID, NAME, RECORD_DATE) %>%
  group_by(PERSON_ID, PREG_ID, NAME) %>%
  mutate(REC_SEQ = row_number()) %>%
  filter(REC_SEQ == 1)


# Visualise each outcome bu source and date

out_first <- out_first %>%
  mutate(MONTHS_DIFF = interval(CENSOR_DATE_START, RECORD_DATE) %/% months(1))

out_first %>%
  group_by(NAME, MONTHS_DIFF, SOURCE) %>%
  summarise(vol = n()) %>%
  ggplot() + 
  geom_line(aes(x = MONTHS_DIFF, y = vol, colour = SOURCE),
            size = 1,
            stat = "identity") +
  geom_point(aes(x = MONTHS_DIFF, y = vol, colour = SOURCE),
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
out_dur_wide <- codelist_outcomes %>% 
  distinct(NAME) %>%
  collect() %>%
  full_join(out_first,
            by = "NAME") %>%
  select(PERSON_ID, PREG_ID, NAME, RECORD_DATE) %>%
  mutate(NAME = paste0("OUT_DUR_", NAME),
         FLAG = 1) %>%
  pivot_wider(id_cols = c(PERSON_ID, PREG_ID),
              names_from = NAME,
              values_from = c(RECORD_DATE, FLAG),
              names_glue = "{NAME}_{.value}") %>%
  mutate_at(vars(matches("FLAG")), ~ case_when(is.na(.) ~ 0, T ~ .))


# re-order columns
cols = order(colnames(out_dur_wide))
cols = cols[!(cols %in% c("PERSON_ID", "PREG_ID"))]
out_dur_wide <- out_dur_wide %>%
  select(PERSON_ID, PREG_ID, all_of(cols)) %>%
  arrange(PERSON_ID) %>%
  filter(!(is.na(PERSON_ID)))

# check
length(unique(out_dur_wide$PERSON_ID))
nrow(out_dur_wide %>%
       distinct(PERSON_ID, PREG_ID))

length(unique(out_all$PERSON_ID))
nrow(out_all %>%
       distinct(PERSON_ID, PREG_ID))



# write out a combined code summary for outcomes during
out_code_summary <- out_wlgp_code_summary %>% mutate(SOURCE = "WLGP") %>%
  bind_rows(out_pedw_code_summary %>% mutate(SOURCE = "PEDW"),
            out_dths_code_summary %>% mutate(SOURCE = "DTHS")) %>%
  filter(n_recs > 0) %>%
  select(NAME, SOURCE, CODE, DESC, TERMINOLOGY, n_recs, n_id) %>%
  arrange(NAME, desc(n_id))

# write to directory
out_path <- paste0("output/out_dur_code_summary_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(out_code_summary,
          file = out_path,
          row.names = F)


#---------------------------------------------.
# CREATE FINAL OUTCOMES DURING TABLE
#---------------------------------------------.

out_dur_out <- project_cohort %>%
  collect() %>%
  select(PERSON_ID, PREG_ID) %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  left_join(out_dur_wide,
            by = c("PERSON_ID", "PREG_ID")) %>%
  mutate_at(vars(matches("FLAG")), ~ case_when(is.na(.) ~ 0, T ~ .))


# create the composite outcome during variables
out_dur_out <- out_dur_out %>%
  rowwise() %>%
  mutate(OUT_DUR_ARTERIAL_FLAG = max(OUT_DUR_AMI_FLAG, OUT_DUR_STROKE_IS_FLAG, OUT_DUR_STROKE_NOS_FLAG, OUT_DUR_OTHER_ARTERIAL_EMBOLISM_FLAG, na.rm = T),
         OUT_DUR_ARTERIAL_RECORD_DATE = min(OUT_DUR_AMI_RECORD_DATE, OUT_DUR_STROKE_IS_RECORD_DATE, OUT_DUR_STROKE_NOS_RECORD_DATE, OUT_DUR_OTHER_ARTERIAL_EMBOLISM_RECORD_DATE, na.rm = T),
         OUT_DUR_VENOUS_FLAG = max(OUT_DUR_DVT_FLAG, OUT_DUR_DVT_OTHER_FLAG, OUT_DUR_ICVT_FLAG, OUT_DUR_ICVT_PREGNANCY_FLAG, OUT_DUR_DVT_PREGNANCY_FLAG, OUT_DUR_PVT_FLAG, OUT_DUR_PE_FLAG, na.rm = T),
         OUT_DUR_VENOUS_RECORD_DATE = min(OUT_DUR_DVT_RECORD_DATE, OUT_DUR_DVT_OTHER_RECORD_DATE, OUT_DUR_ICVT_RECORD_DATE, OUT_DUR_ICVT_PREGNANCY_RECORD_DATE, OUT_DUR_DVT_PREGNANCY_RECORD_DATE, OUT_DUR_PVT_RECORD_DATE, OUT_DUR_PE_RECORD_DATE, na.rm = T),
         OUT_DUR_HAEMOTOLOGICAL_FLAG = max(OUT_DUR_DIC_FLAG, OUT_DUR_TTP_FLAG, OUT_DUR_THROMBOCYTOPENIA_FLAG, na.rm = T),
         OUT_DUR_HAEMOTOLOGICAL_RECORD_DATE = min(OUT_DUR_DIC_RECORD_DATE, OUT_DUR_TTP_RECORD_DATE, OUT_DUR_THROMBOCYTOPENIA_RECORD_DATE, na.rm = T),
         OUT_DUR_OTHER_CVD_FLAG = max(OUT_DUR_STROKE_SAH_FLAG, OUT_DUR_STROKE_HS_FLAG, OUT_DUR_MESENTERIC_THROMBUS_FLAG, OUT_DUR_ARTERY_DISSECT_FLAG,
                                     OUT_DUR_LIFE_ARRHYTHMIAS_FLAG, OUT_DUR_CARDIOMYOPATHY_FLAG, OUT_DUR_HF_FLAG,
                                     OUT_DUR_ANGINA_FLAG, OUT_DUR_ANGINA_UNSTABLE_FLAG, na.rm = T),
         OUT_DUR_OTHER_CVD_RECORD_DATE = min(OUT_DUR_STROKE_SAH_RECORD_DATE, OUT_DUR_STROKE_HS_RECORD_DATE, OUT_DUR_MESENTERIC_THROMBUS_RECORD_DATE, OUT_DUR_ARTERY_DISSECT_RECORD_DATE,
                                            OUT_DUR_LIFE_ARRHYTHMIAS_RECORD_DATE, OUT_DUR_CARDIOMYOPATHY_RECORD_DATE, OUT_DUR_HF_RECORD_DATE,
                                            OUT_DUR_ANGINA_RECORD_DATE, OUT_DUR_ANGINA_UNSTABLE_RECORD_DATE, na.rm = T)) %>%
  ungroup()

out_dur_out$OUT_DUR_ARTERIAL_RECORD_DATE[is.infinite(out_dur_out$OUT_DUR_ARTERIAL_RECORD_DATE)] <- NA
out_dur_out$OUT_DUR_VENOUS_RECORD_DATE[is.infinite(out_dur_out$OUT_DUR_VENOUS_RECORD_DATE)] <- NA
out_dur_out$OUT_DUR_HAEMOTOLOGICAL_RECORD_DATE[is.infinite(out_dur_out$OUT_DUR_HAEMOTOLOGICAL_RECORD_DATE)] <- NA
out_dur_out$OUT_DUR_OTHER_CVD_RECORD_DATE[is.infinite(out_dur_out$OUT_DUR_OTHER_CVD_RECORD_DATE)] <- NA


# configure final variable order
out_dur_out <- out_dur_out %>%
  select(PERSON_ID, PREG_ID,
         OUT_DUR_GEST_HYPERTENSION_FLAG, OUT_DUR_GEST_HYPERTENSION_RECORD_DATE,
         OUT_DUR_GEST_DIABETES_FLAG, OUT_DUR_GEST_DIABETES_RECORD_DATE,
         OUT_DUR_PREECLAMPSIA_FLAG, OUT_DUR_PREECLAMPSIA_RECORD_DATE,
         OUT_DUR_ARTERIAL_FLAG, OUT_DUR_ARTERIAL_RECORD_DATE,
         OUT_DUR_VENOUS_FLAG, OUT_DUR_VENOUS_RECORD_DATE,
         OUT_DUR_HAEMOTOLOGICAL_FLAG, OUT_DUR_HAEMOTOLOGICAL_RECORD_DATE,
         OUT_DUR_OTHER_CVD_FLAG, OUT_DUR_OTHER_CVD_RECORD_DATE,
         OUT_DUR_AMI_FLAG, OUT_DUR_AMI_RECORD_DATE,
         OUT_DUR_STROKE_IS_FLAG, OUT_DUR_STROKE_IS_RECORD_DATE,
         OUT_DUR_STROKE_NOS_FLAG, OUT_DUR_STROKE_NOS_FLAG,
         OUT_DUR_OTHER_ARTERIAL_EMBOLISM_FLAG, OUT_DUR_OTHER_ARTERIAL_EMBOLISM_RECORD_DATE,
         OUT_DUR_DVT_FLAG, OUT_DUR_DVT_RECORD_DATE,
         OUT_DUR_DVT_OTHER_FLAG, OUT_DUR_DVT_OTHER_RECORD_DATE,
         OUT_DUR_ICVT_FLAG, OUT_DUR_ICVT_RECORD_DATE,
         OUT_DUR_ICVT_PREGNANCY_FLAG, OUT_DUR_ICVT_PREGNANCY_RECORD_DATE,
         OUT_DUR_DVT_PREGNANCY_FLAG, OUT_DUR_DVT_PREGNANCY_RECORD_DATE,
         OUT_DUR_PVT_FLAG, OUT_DUR_PVT_RECORD_DATE,
         OUT_DUR_PE_FLAG, OUT_DUR_PE_RECORD_DATE,
         OUT_DUR_DIC_FLAG, OUT_DUR_DIC_RECORD_DATE,
         OUT_DUR_TTP_FLAG, OUT_DUR_TTP_RECORD_DATE,
         OUT_DUR_THROMBOCYTOPENIA_FLAG, OUT_DUR_THROMBOCYTOPENIA_RECORD_DATE,
         OUT_DUR_STROKE_SAH_FLAG, OUT_DUR_STROKE_SAH_RECORD_DATE,
         OUT_DUR_STROKE_HS_FLAG, OUT_DUR_STROKE_HS_RECORD_DATE,
         OUT_DUR_MESENTERIC_THROMBUS_FLAG, OUT_DUR_MESENTERIC_THROMBUS_RECORD_DATE,
         OUT_DUR_ARTERY_DISSECT_FLAG, OUT_DUR_ARTERY_DISSECT_RECORD_DATE,
         OUT_DUR_LIFE_ARRHYTHMIAS_FLAG, OUT_DUR_LIFE_ARRHYTHMIAS_RECORD_DATE,
         OUT_DUR_CARDIOMYOPATHY_FLAG, OUT_DUR_CARDIOMYOPATHY_RECORD_DATE,
         OUT_DUR_HF_FLAG, OUT_DUR_HF_RECORD_DATE,
         OUT_DUR_ANGINA_FLAG, OUT_DUR_ANGINA_RECORD_DATE,
         OUT_DUR_ANGINA_UNSTABLE_FLAG, OUT_DUR_ANGINA_UNSTABLE_RECORD_DATE)

# write covariates table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_outcomes_during)),
             out_dur_out)

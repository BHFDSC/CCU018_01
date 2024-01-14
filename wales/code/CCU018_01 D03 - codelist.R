# CCU018 D01-cohort

# Description: Create a complete codelist for all project outcomes, covariates and exposures based on code terminologies (i.e. ICD10, READV2)

# Project: CCU018

# Author(s): Elena Raffetti, John Nolan

# Date last updated: 2022/02/02

# Date last run: 2022/02/02

# Data Input: 

# Data Output:
# ...

# Output Description:
# A single table containing all codes and descriptions used in the project.
# This table will be used to identify covariates, outcomes etc for the study cohort.

# run parameters script
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\CCU018_01 - 01 - parameters.R")

# rename current output tables to archive

# set archive date
archive_date <- str_replace_all(as.character(as.Date(out_date, '%Y-%m-%d')-1), "-", "")

# archive the cohort table
curr_tab <- tbl(con, in_schema(dbc,path_codelist))
curr_dat <- curr_tab %>%
  collect()

# write patient characteristics table to database
dbWriteTable(con, SQL(paste0(dbc, ".", path_codelist, "_", archive_date)), curr_dat)
dbRemoveTable(con, SQL(paste0(dbc, ".", path_codelist)))


#---------------------------------------------.
# Phenotypes
#---------------------------------------------.

# read in codelist generatitor csv file
phenotypes <- read.csv("codelists\\CCU018_codelist_generator.csv")

# read in all required codelists

# create dataframe to hold codelist
ccu018_codelist <- data.frame(NAME = character(),
                              CODE = character(),
                              DESC = character(),
                              CATEGORY = character(),
                              TERMINOLOGY = character(),
                              OUTCOME = integer(),
                              COVARIATE = integer())

# read in the outcome codes and combine
for (i in 1:nrow(phenotypes)) {
  codelist <- tbl(con, in_schema(dbc, paste0("PHEN_", phenotypes$TERMINOLOGY[i],"_", phenotypes$NAME[i]))) %>%
    collect() %>%
    filter(IS_LATEST == 1 | is.na(IS_LATEST)) %>%
    mutate(TERMINOLOGY = phenotypes$TERMINOLOGY[i],
           OUTCOME = phenotypes$OUTCOME[i],
           COVARIATE = phenotypes$COVARIATE[i]) %>%
    select(any_of(c("NAME", "CODE", "DESC", "CATEGORY", "TERMINOLOGY", "OUTCOME", "COVARIATE")))
  ccu018_codelist <- ccu018_codelist %>%
    bind_rows(codelist)
}

# reformat fields
ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = toupper(str_trim(NAME)),
         CATEGORY = toupper(str_trim(CATEGORY)),
         CODE = str_trim(CODE))


# Change outcome name where identified by medications
ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME %in% c("DIABETES", "HYPERTENSION") & TERMINOLOGY == "DMD" ~ paste0(NAME, "_DRUGS"),
                          T ~ NAME))

# there are 3 codes that are "Covariate Only"
# 2 AMI, 1 VT
ccu018_codelist %>%
  filter(str_detect(toupper(CATEGORY), "COVARIATE"))

ccu018_codelist <- ccu018_codelist %>%
  mutate(OUTCOME = case_when(str_detect(toupper(CATEGORY), "COVARIATE") ~ 0L,
                               T ~ OUTCOME))

# There is a NAME which is NA
# this is because the NAME field is not populated in the Phenotype table
ccu018_codelist %>%
  filter(is.na(NAME))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(is.na(NAME) ~ "DIABETES", T ~ NAME))

# Split the NAME field into sub-categories where required

# ANGINA
with(ccu018_codelist %>% filter(NAME == "ANGINA"),
     table(CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "ANGINA" & CATEGORY == "UNSTABLE ANGINA" ~ "ANGINA_UNSTABLE",
                          NAME == "ANGINA" ~ "ANGINA",
                          T ~ NAME))

# ARTERY DISSECT - Remove unrequired code from list
with(ccu018_codelist %>% filter(NAME == "ARTERY_DISSECT_CCU002"),
     table(CODE))

ccu018_codelist <- ccu018_codelist %>%
  filter(CODE != "I670")


# STROKE_ISCH
with(ccu018_codelist %>% filter(NAME == "STROKE_ISCH_CCU002" & TERMINOLOGY == "ICD10"),
     table(CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "STROKE_ISCH_CCU002" & TERMINOLOGY == "ICD10" ~ CATEGORY,
                          T ~ NAME))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "RETINAL_INFARCTION" & TERMINOLOGY == "ICD10" ~ "STROKE_IS",
                          NAME == "STROKE_SPINAL" & TERMINOLOGY == "ICD10" ~ "STROKE_IS",
                          T ~ NAME))

with(ccu018_codelist %>% filter(NAME == "STROKE_ISCH_CCU002" & TERMINOLOGY == "READ"),
     table(CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "STROKE_ISCH_CCU002" & TERMINOLOGY == "READ" & CATEGORY %in% c("DIAGNOSIS OF ISCHAEMIC STROKE", "HISTORY OF ISCHAEMIC STROKE") ~ "STROKE_IS",
                          NAME == "STROKE_ISCH_CCU002" & TERMINOLOGY == "READ" & CATEGORY %in% c("DIAGNOSIS OF STROKE NOS", "HISTORY OF STROKE NOS") ~ "STROKE_NOS",
                          NAME == "STROKE_ISCH_CCU002" & TERMINOLOGY == "READ" & CATEGORY %in% c("DIAGNOSIS OF TIA", "HISTORY OF TIA") ~ "STROKE_TIA",                        
                          T ~ NAME))
# STROKE_SAH_HS
with(ccu018_codelist %>% filter(NAME == "STROKE_SAH_HS_CCU002" & TERMINOLOGY == "ICD10"),
     table(CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "STROKE_SAH_HS_CCU002" & TERMINOLOGY == "ICD10" ~ CATEGORY,
                          T ~ NAME))

with(ccu018_codelist %>% filter(NAME == "STROKE_SAH_HS_CCU002" & TERMINOLOGY == "READ"),
     table(CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "STROKE_SAH_HS_CCU002" & TERMINOLOGY == "READ" ~ "STROKE_HS",
                          T ~ NAME))


# TCP
with(ccu018_codelist %>% filter(NAME == "TCP"),
     table(TERMINOLOGY, CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "TCP" & TERMINOLOGY == "ICD10" ~ CATEGORY,
                          NAME == "TCP" & TERMINOLOGY == "READ" ~ "THROMBOCYTOPAENIA",
                          T ~ NAME))
# VT
with(ccu018_codelist %>% filter(NAME == "VT_CCU002"),
     table(TERMINOLOGY, CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = case_when(NAME == "VT_CCU002" ~ CATEGORY,
                          T ~ NAME))

# DVT_other - Remove unrequired code I82.1 (included as part of the 3 digit code I82)
# As we remove the 3 digit code I82 we need to add the 4 digit code I823 which is included in NHSD but not specified here
with(ccu018_codelist %>% filter(NAME == "OTHER_DVT"),
     table(CODE))

ccu018_codelist <- ccu018_codelist %>%
  mutate(CODE = case_when(CODE == "I82" ~ "I823",
                          T ~ CODE))

# OBESITY
with(ccu018_codelist %>% filter(NAME == "OBESITY"),
     table(TERMINOLOGY, CATEGORY))

ccu018_codelist <- ccu018_codelist %>%
  filter(NAME != "OBESITY" | (NAME == "OBESITY" & CATEGORY %in% c("BMI_OBESITY", "DIAGNOSIS OF OBESITY"))) %>%
  mutate(NAME = case_when(NAME == "OBESITY" ~ "BMI_OBESITY",
                          T ~ NAME))

# STILLBIRTH - remove 2 codes that should not be included as they are not specifically STILLBIRTH
ccu018_codelist <- ccu018_codelist %>%
  filter(!(CODE %in% c("633..", "ZV27.")))

# rename outcome names where required
ccu018_codelist <- ccu018_codelist %>%
  mutate(NAME = str_replace(NAME, "_CCU002", "")) %>%
  mutate(NAME = case_when(NAME == "DVT_DVT" ~ "DVT",
                          NAME == "OTHER_DVT" ~ "DVT_OTHER",
                          NAME == "DVT_ICVT" ~ "ICVT",
                          NAME == "LIFE_ARRHYTHM" ~ "LIFE_ARRHYTHMIAS",
                          NAME == "ARTERIAL_EMBOLISM_OTHR" ~ "OTHER_ARTERIAL_EMBOLISM",
                          NAME == "PORTAL_VEIN_THROMBOSIS" ~ "PVT",
                          NAME == "VT_COVARIATE_ONLY" ~ "VT",
                          T ~ NAME))



#---------------------------------------------.
# ADD IN PREGNANCY AND MULTIPLE GESTATION CODELISTS
#---------------------------------------------.

# Read in and format PREGNANCY codes
ccu018_codelist_pregnancy <- read.csv(file = "codelists\\ccu018_01_codelist_pregnancy.csv",
                                      header = T,
                                      stringsAsFactors = F) %>%
  rename("CODE" = "code",
         "TERMINOLOGY" = "code_group",
         "DESC" = "desc") %>%
  mutate(NAME = "PREGNANCY",
         OUTCOME = 0,
         COVARIATE = 1,
         CATEGORY = "") %>%
  filter(TERMINOLOGY != "SNOMED") %>%
  mutate(CODE = case_when(TERMINOLOGY == "READ" ~ str_sub(CODE, 1, 5),
                          T ~ CODE)) %>%
  mutate(CODE = case_when(TERMINOLOGY == "READ" & str_length(CODE) == 4 ~ str_pad(CODE, 5, "right", "."),
                          T ~ CODE))
  

# Read in and format MULTIPLE GESTATION codes
ccu018_codelist_multiple_gestation <- read.csv(file = "codelists\\ccu018_01_codelist_multiple_gestation.csv",
                                      header = T,
                                      stringsAsFactors = F) %>%
  rename("CODE" = "code",
         "TERMINOLOGY" = "code_group",
         "DESC" = "desc") %>%
  mutate(NAME = "MULTIPLE_GESTATION",
         OUTCOME = 0,
         COVARIATE = 1,
         CATEGORY = "") %>%
  filter(TERMINOLOGY != "SNOMED") %>%
  mutate(CODE = case_when(TERMINOLOGY == "READ" ~ str_sub(CODE, 1, 5),
                                                         T ~ CODE)) %>%
  mutate(CODE = case_when(TERMINOLOGY == "READ" & str_length(CODE) == 4 ~ str_pad(CODE, 5, "right", "."),
                          T ~ CODE))

# ADD PREGNANCY AND MULTIPLE GESTATION to MAIN CODELIST
ccu018_codelist <- ccu018_codelist %>%
  bind_rows(ccu018_codelist_pregnancy,
            ccu018_codelist_multiple_gestation)



# Add in Corticosteroids and Immunosuppressants
bnf_lookup <- tbl(con, in_schema(dbref, path_bnf_lkp))

codelist_corticosteroids <- bnf_lookup %>%
  filter(BNF_PARAGRAPH_CODE == "080202") %>%
  select(BNF_PRESENTATION_CODE, BNF_PRESENTATION) %>%
  distinct() %>%
  collect() %>%
  rename("CODE" = "BNF_PRESENTATION_CODE",
         "DESC" = "BNF_PRESENTATION") %>%
  mutate(NAME = "CORTICOSTEROIDS",
         TERMINOLOGY = "BNF",
         COVARIATE = 1,
         OUTCOME = 0)
  
codelist_immunosuppressants <- bnf_lookup %>%
  filter(BNF_SECTION_CODE == "0802") %>%
  select(BNF_PRESENTATION_CODE, BNF_PRESENTATION) %>%
  distinct() %>%
  collect() %>%
  rename("CODE" = "BNF_PRESENTATION_CODE",
         "DESC" = "BNF_PRESENTATION") %>%
  mutate(NAME = "IMMUNOSUPPRESSANTS",
         TERMINOLOGY = "BNF",
         COVARIATE = 1,
         OUTCOME = 0)

ccu018_codelist <- ccu018_codelist %>%
  bind_rows(codelist_corticosteroids,
            codelist_immunosuppressants)

# Check
unique(ccu018_codelist$NAME)

# strip out duplicates (duplicate values across all fields)
ccu018_codelist <- ccu018_codelist %>%
  distinct()

# there are also codes that appear > 1 time due to multiple code descriptions being given
# remove these
ccu018_codelist <- ccu018_codelist %>%
  group_by(CODE, NAME, TERMINOLOGY) %>%
  mutate(SEQ = row_number()) %>%
  ungroup() %>%
  filter(SEQ == 1) %>%
  select(-SEQ)


# check for code overlaps
code_overlap <- ccu018_codelist %>%
  group_by(CODE) %>%
  mutate(rn = row_number(),
         max_rn = max(rn)) %>%
  filter(max_rn > 1) %>%
  select(CODE, TERMINOLOGY, NAME, rn) %>%
  pivot_wider(id_cols = c(CODE, TERMINOLOGY),
              names_from = rn,
              values_from = NAME) %>%
  arrange(CODE)
# write to directory
out_path <- paste0("output/code_overlap_summary_", str_replace_all(out_date, "-", ""))
write.csv(code_overlap,
          file = out_path,
          row.names = F)

# write project codelist to the database
dbWriteTable(con, SQL(paste0(dbc, ".", path_codelist)), ccu018_codelist)

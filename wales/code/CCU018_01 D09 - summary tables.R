# CCU018_01_summary_tables

# Description: This script produces summary tables for ceach of the output data tables for project CCU018_01

# Project: CCU018

# Author(s): Elena Raffetti, Tom Bolton, John Nolan

# Date last updated: 2022/02/02

# Date last run: 2022/02/02

# Data Input: 

# Data Output:

# Output Description:

# run parameters script
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\CCU018_01 - 01 - parameters.R")
source("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline\\scripts\\common_functions.R")


## OVERALL

# create dataframe with list of tables to summaries

n = length(df_out)

df_out <- data.frame(
  name = df_out,
  path = character(n),
  n_cols = integer(n),
  n_rows = integer(n),
  n_rows_data = integer(n),
  id_var = character(n),
  n_rows_id_var_null = integer(n),
  n_id_var_distinct = integer(n))

# add path to datasets
df_out <- df_out %>%
  mutate(path = paste0(proj, "_OUT_", name))

# add the id variable
df_out <- df_out %>%
  mutate(id_var = case_when(name == "CODELIST" ~ "NAME",
                            T ~ "PERSON_ID"))

# loop through the tables to get required information for each

for(i in 1:nrow(df_out)) {
  # read in required data 
  tmp <- tbl(con, in_schema(dbc, df_out$path[i])) %>%
    collect()
  
  # number of columns
  df_out$n_cols[i] <- ncol(tmp)
  
  # number of records
  df_out$n_rows[i] <- nrow(tmp)


  # number of rows with no data
  tmp <- tmp %>%
    mutate(FLAG_DATA = case_when(df_out$id_var[i] == "PERSON_ID" & ncol(tmp) - 2 - rowSums(is.na(tmp)) > 0 ~ 1,
                                 df_out$id_var[i] == "NAME" & ncol(tmp) - 1 - rowSums(is.na(tmp)) > 0 ~ 1,
                                 T ~ 0))
  df_out$n_rows_data[i] <- sum(tmp$FLAG_DATA)
  
  # number of null or blank ids
  df_out$n_rows_id_var_null[i] <- nrow(tmp %>% filter(is.na(!!sym(df_out$id_var[i])) | !!sym(df_out$id_var[i]) == ""))
    
  # number of distinct IDs
  df_out$n_id_var_distinct[i] <- nrow(tmp %>% distinct(!!sym(df_out$id_var[i])))  
  

}

# write out summary of all datasets
out_path <- paste0("output/table_summary_overall_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(df_out,
          file = out_path,
          row.names = F)

# Individual Table Summaries

# specify variable to treat is ID's and Continuous Variables
id_vars = c("PERSON_ID", "NAME", "CODE", "DESC", "CATEGORY")
cont_vars = c("FU_DAYS", "COV_BMI_VALUE")


## codelist
codelist_summ <- table_summ("SAILWWMCCV", "CCU018_01_OUT_CODELIST", "CODELIST")
# write out codelist summary
out_path <- paste0("output/table_summary_codelist_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(codelist_summ,
          file = out_path,
          row.names = F)

## cohort
cohort_summ <- table_summ("SAILWWMCCV", "CCU018_01_OUT_COHORT", "COHORT")
# write out codelist summary
out_path <- paste0("output/table_summary_cohort_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(cohort_summ,
          file = out_path,
          row.names = F)



## covariates
covariates_summ <- table_summ("SAILWWMCCV", "CCU018_01_OUT_COVARIATES", "COVARIATES")
# write out codelist summary
out_path <- paste0("output/table_summary_covariates_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(covariates_summ,
          file = out_path,
          row.names = F)


## exposure
exposure_summ <- table_summ("SAILWWMCCV", "CCU018_01_OUT_EXPOSURE", "EXPOSURE")
# write out exposure summary
out_path <- paste0("output/table_summary_exposure_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(exposure_summ,
          file = out_path,
          row.names = F)


## outcomes at birth
outcomes_birth_summ <- table_summ("SAILWWMCCV", "CCU018_01_OUT_OUTCOMES_AT_BIRTH", "OUTCOMES_AT_BIRTH")
# write out outcomes at birth summary
out_path <- paste0("output/table_summary_outcomes_at_birth_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(outcomes_birth_summ,
          file = out_path,
          row.names = F)


## outcomes during pregnancy
outcomes_dur_preg <- table_summ("SAILWWMCCV", "CCU018_01_OUT_OUTCOMES_DUR_PREG", "OUTCOMES_DUR_PREG")
# write out outcomes at birth summary
out_path <- paste0("output/table_summary_outcomes_dur_preg_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(outcomes_dur_preg,
          file = out_path,
          row.names = F)



## outcomes post pregnancy
outcomes_post_preg <- table_summ("SAILWWMCCV", "CCU018_01_OUT_OUTCOMES_POST_PREG", "OUTCOMES_POST_PREG")
# write out outcomes at birth summary
out_path <- paste0("output/table_summary_outcomes_post_preg_", str_replace_all(out_date, "-", ""), ".csv")
write.csv(outcomes_post_preg,
          file = out_path,
          row.names = F)

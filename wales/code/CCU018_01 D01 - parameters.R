# CCU018 D01-parameters

# Description: This script defines a set of parameters for use across CCU018_01

# Project: CCU018_01

# Author(s): Elena Raffetti, John Nolan, Tom Bolton

# Approach: This script is run at the start of every project pipeline script so that helper functions and parameters are consistently available

# Reviewers: UNREVIEWED

# Date last updated: 

# Date last run: 



#---------------------------------------------.
# Prepare environment, load required packages, and connect to database.
#---------------------------------------------.


# set working directory
setwd("S:\\WMCC - WMCC - Wales Multi-morbidity Cardiovascular Covid-19 UK (0911)\\CCU018\\R_pipeline")

# clear the environment
rm(list = ls())

# load required packages
library(DBI)
library(odbc)
library(dbplyr)
library(dplyr)
library(tidyr)
library(rstudioapi)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(forcats)
library(stringr)

# environment
options(scipen = 999)

# this connects to PR_SAIL
con <- DBI::dbConnect(odbc::odbc(),
                      "PR_SAIL",
                      uid = askForPassword("Enter Username"),
                      password = askForPassword("Enter Password"))

#---------------------------------------------.
# Project
#---------------------------------------------.

proj <- "CCU018_01"


#---------------------------------------------.
# Databases
#---------------------------------------------.

db <- "SAILWMCCV"
dbc <- "SAILWWMCCV"
dbref <-"SAILUKHDV"

#db_date <- "20220505"


#---------------------------------------------.
# Paths 
#---------------------------------------------.

# Project In Tables

# Covid-19 Cohort20: We will restrict to IDs from this cohort for this project
path_cohort20 <- "C19_COHORT20"

# MIDS - Maternity Indicators Dataset
# Initial assessment records
path_mids_init_assess <- "C19_COHORT_MIDS_INITIAL_ASSESSMENT"
# MIDS births
path_mids_birth <- "C19_COHORT_MIDS_BIRTH"

# WLGP data
path_wlgp <- "C19_COHORT_WLGP_GP_EVENT_CLEANSED"

# PEDW data
path_pedw_diag <- "C19_COHORT_PEDW_DIAG"
path_pedw_oper <- "C19_COHORT_PEDW_OPER"
path_pedw_episode <- "C19_COHORT_PEDW_EPISODE"
path_pedw_spell <- "C19_COHORT_PEDW_SPELL"

# C19 COHORT20 Deaths Data
# Harmonised data from all 4 deaths sources
path_deaths <- "C19_COHORT20_MORTALITY"

# WDDS
path_wdds <- "C19_COHORT20_RRDA_WDDS"

# PATD Covid-19 Testing Data
path_patd_test_results <- "C19_COHORT_PATD_DF_COVID_LIMS_TESTRESULTS"

# CVVD
path_cvvd <- "C19_COHORT20_RRDA_CVVD"

# paths to required codelists
path_codelist <- paste0(proj, "_OUT_CODELIST")
path_codelist_smoking <- "PHEN_READ_SMOKING"
path_codelist_bmi <- "PHEN_READ_BMI"
path_codelist_covid_icd10 <- "PHEN_ICD10_COVID19"
path_codelist_covid_read <- "PHEN_READ_COVID19"
path_bnf_lkp <- "BNF_BNF_CODE_INFORMATION_SCD"


# paths to project out tables
path_cohort <- paste0(proj, "_OUT_COHORT")
path_skinny <- paste0(proj, "_OUT_SKINNY")
path_exposure <- paste0(proj, "_OUT_EXPOSURE")
path_vaccination <- paste0(proj, "_OUT_VACCINATION")
path_covariates <- paste0(proj, "_OUT_COVARIATES")
path_outcomes_during <- paste0(proj, "_OUT_OUTCOMES_DUR_PREG")
path_outcomes_post <- paste0(proj, "_OUT_OUTCOMES_POST_PREG")
path_outcomes_birth <- paste0(proj, "_OUT_OUTCOMES_AT_BIRTH")

# list of output tables
df_out <- c("CODELIST", "COHORT", "SKINNY", 
            "EXPOSURE", "COVARIATES",
            "OUTCOMES_DUR_PREG", "OUTCOMES_POST_PREG", "OUTCOMES_AT_BIRTH")


#---------------------------------------------.
# Dates
#---------------------------------------------.

cohort_start_date <- '2019-08-01'
cohort_end_date <- '2021-08-01'

# need to add this retrospectively following running the CCU018_01 - censor data analysis script 
project_fu_end_date <- '2022-01-31'

# pipeline run date
out_date <- "2022-08-04"


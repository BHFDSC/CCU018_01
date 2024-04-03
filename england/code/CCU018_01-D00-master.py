# Databricks notebook source
# MAGIC %md # CCU018_01-D00-master
# MAGIC
# MAGIC **Description** This notebook runs all notebooks in the correct order.
# MAGIC
# MAGIC **Author(s)** Tom Bolton (John Nolan, Elena Raffetti)

# COMMAND ----------

# list of notebooks

# --------------------------------------------------------------------
# data curation pipeline
# --------------------------------------------------------------------
# (D01-parameters) # called within each individual notebook
# D02-codelist
# D03-cohort
# (D03a-cohort_deliveries_clean) # called within D03-cohort
# D04-table_freeze
# D05-curated_data
# D05a-curated_data_covid
# D06-skinny
# D07-quality_assurance
# D09-covariates
# D09a-covariates_supp
# D10-exposures
# D11-outcomes_during_pregnancy
# D12-outcomes_post_pregnancy
# D13-outcomes_at_birth

# --------------------------------------------------------------------
# data checks
# --------------------------------------------------------------------
# # note: whilst the data curation pipeline includes inbuilt checks, 
# # we have created several notebooks to check key resulting datasets. 
# # To be expanded over time.
# D02a-codelist_check
# D03b-cohort_check
# D06a-skinny_check

# --------------------------------------------------------------------
# daa summaries
# --------------------------------------------------------------------
# # note: summarise resulting datasets and the results of codelist matches for review
# D14-summarise_out_tables
# D14a-summarise_codelist_matches

# COMMAND ----------

# MAGIC %md # 1 Data curation pipeline

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D02-codelist', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D03-cohort', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D04-table_freeze', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D05-curated_data', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D05a-curated_data_covid', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D06-skinny', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D07-quality_assurance', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D08-inclusion_exclusion', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D09-covariates', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D09a-covariates_supp', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D10-exposures', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D11-outcomes_during_pregnancy', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D12-outcomes_post_pregnancy', 3600)

# COMMAND ----------

dbutils.notebook.run('CCU018_01-D13-outcomes_at_birth', 3600)

# COMMAND ----------

# MAGIC %md # 2 Data checks

# COMMAND ----------

path_data_checks = f'/Workspaces/dars_nic_391419_j3w9t_collab/CCU018_01/data_checks'

# COMMAND ----------

dbutils.notebook.run(f'{path_data_checks}/CCU018_01-D02a-codelist_check', 3600)

# COMMAND ----------

dbutils.notebook.run(f'{path_data_checks}/CCU018_01-D03b-cohort_check', 3600)

# COMMAND ----------

dbutils.notebook.run(f'{path_data_checks}/CCU018_01-D06a-skinny_check', 3600)

# COMMAND ----------

# MAGIC %md # 2 Data summaries

# COMMAND ----------

path_data_summaries = f'/Workspaces/dars_nic_391419_j3w9t_collab/CCU018_01/data_summaries'

# COMMAND ----------

dbutils.notebook.run(f'{path_data_summaries}/CCU018_01-D14-summarise_out_tables', 3600)

# COMMAND ----------

dbutils.notebook.run(f'{path_data_summaries}/CCU018_01-D14a-summarise_codelist_matches', 3600)

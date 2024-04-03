# Databricks notebook source
# MAGIC %md # CCU018_01-D05a-curated_data_covid
# MAGIC
# MAGIC **Description** This notebook creates the COVID-19 curated data table for the CCU018_01 project based on work from CCU002 and CCU013, which includes harmonising COVID-19 infections from SGSS, GDPPR, HES APC, SUS, and CHESS. 
# MAGIC  
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002 and CCU013)
# MAGIC
# MAGIC **Data input** out_codelist, in_sgss, in_gdppr, in_hes_apc, in_sus, in_chess (in_hes_cc, cur_deaths_sing)
# MAGIC
# MAGIC **Data output** ccu018_01_cur_covid

# COMMAND ----------

# DBTITLE 1,Libraries
import pyspark.sql.functions as f
import pyspark.sql.types as t
from pyspark.sql import Window

from functools import reduce

import databricks.koalas as ks
import pandas as pd
import numpy as np

import re
import io
import datetime

import matplotlib
import matplotlib.pyplot as plt
from matplotlib import dates as mdates
import seaborn as sns

print("Matplotlib version: ", matplotlib.__version__)
print("Seaborn version: ", sns.__version__)
_datetimenow = datetime.datetime.now() # .strftime("%Y%m%d")
print(f"_datetimenow:  {_datetimenow}")

# COMMAND ----------

# DBTITLE 1,Functions
# MAGIC %run "../shds/common/functions"

# COMMAND ----------

# MAGIC %md # 0 Parameters

# COMMAND ----------

# MAGIC %run "./CCU018_01-D01-parameters"

# COMMAND ----------

start_date = '2020-01-01'
end_date = proj_delivery_date_max 
# note: we are only providing covid pre pregnancy and during pregnancy - if expanding, then check date coverage for each data source
print(start_date, end_date)

# COMMAND ----------

# MAGIC %md # 1 Data

# COMMAND ----------

codelist = spark.table(path_out_codelist)
sgss     = spark.table(path_in_sgss)
gdppr    = spark.table(path_in_gdppr)
hes_apc  = spark.table(path_in_hes_apc)
sus      = spark.table(path_in_sus)
chess    = spark.table(path_in_chess)
hes_cc   = spark.table(path_in_hes_cc)
deaths   = spark.table(path_cur_deaths_sing)

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

# sgss
_sgss = sgss\
  .select(['PERSON_ID_DEID', 'Reporting_Lab_ID', 'Specimen_Date'])\
  .withColumnRenamed('PERSON_ID_DEID', 'PERSON_ID')\
  .withColumnRenamed('Specimen_Date', 'DATE')\
  .where((f.col('DATE') >= start_date) & (f.col('DATE') <= end_date))\
  .dropDuplicates()

# gdppr
# omitted: 'LSOA'
_gdppr = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'CODE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')\
  .where((f.col('DATE') >= start_date) & (f.col('DATE') <= end_date))\
  .dropDuplicates()

# hes_apc
# omitted: 'DISMETH', 'DISDEST', 'DISDATE', 
_hes_apc = hes_apc\
  .select(['PERSON_ID_DEID', 'EPISTART', 'DIAG_4_01', 'DIAG_4_CONCAT', 'OPERTN_4_CONCAT', 'SUSRECID'])\
  .withColumnRenamed('PERSON_ID_DEID', 'PERSON_ID')\
  .withColumnRenamed('EPISTART', 'DATE')\
  .where(f.col('DIAG_4_CONCAT').rlike('U07(1|2)'))\
  .where((f.col('DATE') >= start_date) & (f.col('DATE') <= end_date))\
  .dropDuplicates()

# hes_cc
_hes_cc = hes_cc\
  .withColumnRenamed('PERSON_ID_DEID', 'PERSON_ID')\
  .withColumnRenamed('CCSTARTDATE', 'DATE')\
  .withColumn('DATE', f.to_date(f.substring('DATE', 0, 8), 'yyyyMMdd'))\
  .where((f.col('DATE') >= start_date) & (f.col('DATE') <= end_date))\
  .dropDuplicates()

# sus
_sus = sus\
  .select(['NHS_NUMBER_DEID'
    , 'EPISODE_START_DATE'
    , 'PRIMARY_PROCEDURE_DATE'
    , 'SECONDARY_PROCEDURE_DATE_1'
    , 'DISCHARGE_DESTINATION_HOSPITAL_PROVIDER_SPELL'
    , 'DISCHARGE_METHOD_HOSPITAL_PROVIDER_SPELL'
    , 'END_DATE_HOSPITAL_PROVIDER_SPELL'           
    ]\
    + [col for col in sus.columns if re.match('.*(DIAGNOSIS|PROCEDURE)_CODE.*', col)]
  )\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')\
  .withColumnRenamed('EPISODE_START_DATE', 'DATE')\
  .withColumn('DIAG_CONCAT', f.concat_ws(',', *[col for col in sus.columns if re.match('.*DIAGNOSIS_CODE.*', col)]))\
  .withColumn('PROCEDURE_CONCAT', f.concat_ws(',', *[col for col in sus.columns if re.match('.*PROCEDURE_CODE.*', col)]))\
  .where((f.col('DATE') >= start_date) & (f.col('DATE') <= end_date))\
  .where(\
    ((f.col('END_DATE_HOSPITAL_PROVIDER_SPELL') >= start_date) | (f.col('END_DATE_HOSPITAL_PROVIDER_SPELL').isNull()))\
    & ((f.col('END_DATE_HOSPITAL_PROVIDER_SPELL') <= end_date) | (f.col('END_DATE_HOSPITAL_PROVIDER_SPELL').isNull()))\
  )\
  .where(f.col('DATE').isNotNull())\
  .dropDuplicates()

# chess
_chess = chess\
  .select(['PERSON_ID_DEID', 'Typeofspecimen', 'Covid19', 'AdmittedToICU', 'Highflownasaloxygen', 'NoninvasiveMechanicalventilation', 'Invasivemechanicalventilation', 'RespiratorySupportECMO', 'DateAdmittedICU', 'HospitalAdmissionDate', 'InfectionSwabDate'])\
  .withColumnRenamed('PERSON_ID_DEID', 'PERSON_ID')\
  .withColumnRenamed('InfectionSwabDate', 'DATE')\
  .where(f.col('Covid19') == 'Yes')\
  .where(\
    ((f.col('DATE') >= start_date) | (f.col('DATE').isNull()))\
    & ((f.col('DATE') <= end_date) | (f.col('DATE').isNull()))\
  )\
  .where(\
    ((f.col('HospitalAdmissionDate') >= start_date) | (f.col('HospitalAdmissionDate').isNull()))\
    & ((f.col('HospitalAdmissionDate') <= end_date) | (f.col('HospitalAdmissionDate').isNull()))\
  )\
  .where(\
    ((f.col('DateAdmittedICU') >= start_date) | (f.col('DateAdmittedICU').isNull()))\
    & ((f.col('DateAdmittedICU') <= end_date) | (f.col('DateAdmittedICU').isNull()))\
  )\
  .dropDuplicates()
  
# deaths
_deaths = deaths\
  .where((f.col('REG_DATE_OF_DEATH') >= start_date) & (f.col('REG_DATE_OF_DEATH') <= end_date))

# COMMAND ----------

# MAGIC %md
# MAGIC # 3 Covid positive

# COMMAND ----------

# sgss
_sgss_pos = _sgss\
  .withColumn('covid_phenotype', f.lit('01_Covid_positive_test'))\
  .withColumn('clinical_code', f.lit(''))\
  .withColumn('description', f.lit(''))\
  .withColumn('covid_status', f.lit(''))\
  .withColumn('code', f.lit(''))\
  .withColumn('source', f.lit('sgss'))\
  .select('PERSON_ID', 'DATE', 'covid_phenotype', 'clinical_code', 'description', 'covid_status', 'code', 'source')

# gdppr
_codelist_gdppr = codelist\
  .where(f.col('name') == 'covid19')\
  .select(['code', 'term'])
_gdppr_pos = _gdppr\
  .select(['PERSON_ID', 'DATE', 'CODE'])\
  .join(f.broadcast(_codelist_gdppr), on='code', how='inner')\
  .withColumn('covid_phenotype', f.lit('01_GP_covid_diagnosis'))\
  .withColumnRenamed('CODE', 'clinical_code')\
  .withColumnRenamed('term', 'description')\
  .withColumn('covid_status', f.lit(''))\
  .withColumn('code', f.lit('SNOMED'))\
  .withColumn('source', f.lit('gdppr'))

# COMMAND ----------

# MAGIC %md # 4 Covid admission

# COMMAND ----------

# ------------------------------------------------------------------------------
# hes_apc
# ------------------------------------------------------------------------------
# any
_hes_apc_adm_any = _hes_apc\
  .where(f.col('DIAG_4_CONCAT').rlike('U07(1|2)'))\
  .withColumn('covid_phenotype', f.lit('02_Covid_admission_any_position'))\
  .withColumn('clinical_code',\
    f.when(f.col('DIAG_4_CONCAT').rlike('U071'), 'U071')\
    .when(f.col('DIAG_4_CONCAT').rlike('U072'), 'U072')\
  )\
  .withColumn('description',\
    f.when(f.col('DIAG_4_CONCAT').rlike('U071'), 'Confirmed_COVID19')\
    .when(f.col('DIAG_4_CONCAT').rlike('U072'), 'Suspected_COVID19')\
  )\
  .withColumn('covid_status',\
    f.when(f.col('DIAG_4_CONCAT').rlike('U071'), 'confirmed')\
    .when(f.col('DIAG_4_CONCAT').rlike('U072'), 'suspected')\
  )\
  .withColumn('code', f.lit('ICD10'))\
  .withColumn('source', f.lit('hes_apc'))\
  .select('PERSON_ID', 'DATE', 'covid_phenotype', 'clinical_code', 'description', 'covid_status', 'code', 'source')

# pri
_hes_apc_adm_pri = _hes_apc\
  .where(f.col('DIAG_4_01').rlike('U07(1|2)'))\
  .withColumn('covid_phenotype', f.lit('02_Covid_admission_primary_position'))\
  .withColumn('clinical_code',\
    f.when(f.col('DIAG_4_01').rlike('U071'), 'U071')\
    .when(f.col('DIAG_4_01').rlike('U072'), 'U072')\
  )\
  .withColumn('description',\
    f.when(f.col('DIAG_4_01').rlike('U071'), 'Confirmed_COVID19')\
    .when(f.col('DIAG_4_01').rlike('U072'), 'Suspected_COVID19')\
  )\
  .withColumn('covid_status',\
    f.when(f.col('DIAG_4_01').rlike('U071'), 'confirmed')\
    .when(f.col('DIAG_4_01').rlike('U072'), 'suspected')\
  )\
  .withColumn('code', f.lit('ICD10'))\
  .withColumn('source', f.lit('hes_apc'))\
  .select('PERSON_ID', 'DATE', 'covid_phenotype', 'clinical_code', 'description', 'covid_status', 'code', 'source')


# ------------------------------------------------------------------------------
# sus
# ------------------------------------------------------------------------------
# any
_sus_adm_any = _sus\
  .where(f.col('DIAG_CONCAT').rlike('U07(1|2)'))\
  .withColumn('covid_phenotype', f.lit('02_Covid_admission_any_position'))\
  .withColumn('clinical_code',\
    f.when(f.col('DIAG_CONCAT').rlike('U071'), 'U071')\
    .when(f.col('DIAG_CONCAT').rlike('U072'), 'U072')\
  )\
  .withColumn('description',\
    f.when(f.col('DIAG_CONCAT').rlike('U071'), 'Confirmed_COVID19')\
    .when(f.col('DIAG_CONCAT').rlike('U072'), 'Suspected_COVID19')\
  )\
  .withColumn('covid_status',\
    f.when(f.col('DIAG_CONCAT').rlike('U071'), 'confirmed')\
    .when(f.col('DIAG_CONCAT').rlike('U072'), 'suspected')\
  )\
  .withColumn('code', f.lit('ICD10'))\
  .withColumn('source', f.lit('sus'))\
  .select('PERSON_ID', 'DATE', 'covid_phenotype', 'clinical_code', 'description', 'covid_status', 'code', 'source')

# pri
_sus_adm_pri = _sus\
  .where(f.col('PRIMARY_DIAGNOSIS_CODE').rlike('U07(1|2)'))\
  .withColumn('covid_phenotype', f.lit('02_Covid_admission_primary_position'))\
  .withColumn('clinical_code',\
    f.when(f.col('PRIMARY_DIAGNOSIS_CODE').rlike('U071'), 'U071')\
    .when(f.col('PRIMARY_DIAGNOSIS_CODE').rlike('U072'), 'U072')\
  )\
  .withColumn('description',\
    f.when(f.col('PRIMARY_DIAGNOSIS_CODE').rlike('U071'), 'Confirmed_COVID19')\
    .when(f.col('PRIMARY_DIAGNOSIS_CODE').rlike('U072'), 'Suspected_COVID19')\
  )\
  .withColumn('covid_status',\
    f.when(f.col('PRIMARY_DIAGNOSIS_CODE').rlike('U071'), 'confirmed')\
    .when(f.col('PRIMARY_DIAGNOSIS_CODE').rlike('U072'), 'suspected')\
  )\
  .withColumn('code', f.lit('ICD10'))\
  .withColumn('source', f.lit('sus'))\
  .select('PERSON_ID', 'DATE', 'covid_phenotype', 'clinical_code', 'description', 'covid_status', 'code', 'source')


# ------------------------------------------------------------------------------
# chess
# ------------------------------------------------------------------------------
_chess_adm = _chess\
  .select(['PERSON_ID', f.col('HospitalAdmissionDate').alias('DATE')])\
  .withColumn('covid_phenotype', f.lit('02_Covid_admission_any_position'))\
  .withColumn('clinical_code', f.lit(''))\
  .withColumn('description', f.lit('HospitalAdmissionDate IS NOT null'))\
  .withColumn('covid_status', f.lit('confirmed'))\
  .withColumn('code', f.lit(''))\
  .withColumn('source', f.lit('chess'))

# COMMAND ----------

# MAGIC %md # 5 Covid critical care

# COMMAND ----------

# MAGIC %md ## 5.1 ICU

# COMMAND ----------

# ------------------------------------------------------------------------------
# chess
# ------------------------------------------------------------------------------
_chess_icu = _chess\
  .where(f.col('DateAdmittedICU').isNotNull())\
  .select(['PERSON_ID', 'DateAdmittedICU'])\
  .withColumnRenamed('DateAdmittedICU', 'DATE')\
  .withColumn('covid_phenotype', f.lit('03_ICU_admission'))\
  .withColumn('clinical_code', f.lit(''))\
  .withColumn('description', f.lit('DateAdmittedICU IS NOT null'))\
  .withColumn('covid_status', f.lit('confirmed'))\
  .withColumn('code', f.lit(''))\
  .withColumn('source', f.lit('chess'))


# ------------------------------------------------------------------------------
# hes_cc
# ------------------------------------------------------------------------------
# #HES_CC
# #ID is in HES_CC AND has U071 or U072 from HES_APC 
# spark.sql(f"""
# CREATE OR REPLACE GLOBAL TEMP VIEW {project_prefix}cc_covid as
# SELECT apc.person_id_deid, cc.date,
# '03_ICU_admission' as covid_phenotype,
# "" as clinical_code,
# "id is in hes_cc table" as description,
# "confirmed" as covid_status,
# "" as code,
# 'HES CC' as source, cc.date_is, BRESSUPDAYS, ARESSUPDAYS
# FROM {collab_database_name}.{project_prefix}{temp_hes_apc} as apc
# INNER JOIN {collab_database_name}.{project_prefix}{temp_hes_cc} AS cc
# ON cc.SUSRECID = apc.SUSRECID
# WHERE cc.BESTMATCH = 1
# AND (DIAG_4_CONCAT LIKE '%U071%' OR DIAG_4_CONCAT LIKE '%U072%') """)

# TBC

# COMMAND ----------

# MAGIC %md ## 5.2 NIV

# COMMAND ----------

# TBC

# COMMAND ----------

# MAGIC %md ## 5.3 IMV

# COMMAND ----------

# TBC

# COMMAND ----------

# MAGIC %md ## 5.4 EMCO

# COMMAND ----------

# TBC

# COMMAND ----------

# MAGIC %md # 6 Covid death

# COMMAND ----------

# TBC

# COMMAND ----------

# MAGIC %md # 7 Covid severity

# COMMAND ----------

# TBC

# COMMAND ----------

# MAGIC %md # 8 Combine

# COMMAND ----------

tmp = _sgss_pos\
  .unionByName(_gdppr_pos)\
  .unionByName(_hes_apc_adm_any)\
  .unionByName(_hes_apc_adm_pri)\    
  .unionByName(_sus_adm_any)\
  .unionByName(_sus_adm_pri)\
  .unionByName(_chess_adm)

# COMMAND ----------

display(tmp)

# COMMAND ----------

# MAGIC %md # 9 Check

# COMMAND ----------

# check individual
count_var(_sgss_pos, 'PERSON_ID')
count_var(_gdppr_pos, 'PERSON_ID')
count_var(_sus_adm_any, 'PERSON_ID')
count_var(_sus_adm_pri, 'PERSON_ID')
count_var(_hes_apc_adm_any, 'PERSON_ID')
count_var(_hes_apc_adm_pri, 'PERSON_ID'); print()
count_var(_chess_adm, 'PERSON_ID'); print()

# check combined
count_var(tmp, 'PERSON_ID')
tmpt = tab(tmp, 'covid_phenotype', 'source', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md # F Save

# COMMAND ----------

# save name
outName = f'{proj}_cur_covid'.lower()

# save previous version for comparison purposes
tmpt = spark.sql(f"""SHOW TABLES FROM {dbc}""")\
  .select('tableName')\
  .where(f.col('tableName') == outName)\
  .collect()
if(len(tmpt)>0):
  _datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
  outName_pre = f'{outName}_pre{_datetimenow}'.lower()
  print(outName_pre)
  spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
  spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')

# save
tmp.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

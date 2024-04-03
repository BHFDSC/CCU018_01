# Databricks notebook source
# MAGIC %md # CCU018_01-D10-exposures
# MAGIC
# MAGIC **Description** This notebook creates the exposures for the CCU018_01 project based on work from CCU002 and CCU013, which includes COVID-19 infection and COVID-19 vaccination. Variables for first COVID-19 infection were derived for pre-pregnancy and during pregnancy. COVID-19 vaccination was cleaned based on work from CCU002 and SAIL RRDA_CVVD (Research Ready Data Asset for COVID-19 Vaccination Data). Variables for first COVID-19 vaccination were derived, in addition to the first and second vaccinations during pregnancy. 
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** out_cohort, cur_covid, in_vacc
# MAGIC
# MAGIC **Data output** ccu018_01_out_exposures

# COMMAND ----------

spark.sql('CLEAR CACHE')

# COMMAND ----------

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

# MAGIC %md # 1 Data

# COMMAND ----------

cohort = spark.table(path_out_cohort)
covid  = spark.table(path_cur_covid)
vacc   = spark.table(path_in_vacc)

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

# cohort
# reduce columns
_cohort = cohort\
  .select('PERSON_ID', 'preg_start_date', 'delivery_date', 'fu_end_date')

# check
count_var(_cohort, 'PERSON_ID'); print()
print(_cohort.limit(10).toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md # 3 Infection

# COMMAND ----------

# MAGIC %md ## 3.1 Check

# COMMAND ----------

# check
display(covid)

# COMMAND ----------

# check infection
count_var(covid, 'PERSON_ID'); print()
tmpt = tabstat(covid, 'DATE', date=1); print()
tmpt = tab(covid, 'covid_phenotype', 'covid_status', var2_unstyled=1); print()
tmpt = tab(covid, 'covid_phenotype', 'source', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md ## 3.2 Prepare

# COMMAND ----------

print('------------------------------------------------------------------------------')
print('confirmed COVID-19 (as defined for CCU002)')
print('------------------------------------------------------------------------------')
# dropping the suspected records
covid_confirmed = covid\
  .where(\
    (f.col('covid_phenotype').isin([
      '01_Covid_positive_test'
      , '01_GP_covid_diagnosis'
      , '02_Covid_admission_any_position'
      , '02_Covid_admission_primary_position'
    ]))\
    & (f.col('source').isin(['sgss', 'gdppr', 'hes_apc', 'sus']))\
    & (f.col('covid_status').isin(['confirmed', '']))\
  )

# check
count_var(covid_confirmed, 'PERSON_ID'); print()
tmpt = tabstat(covid_confirmed, 'DATE', date=1); print()
tmpt = tab(covid_confirmed, 'covid_phenotype', 'covid_status', var2_unstyled=1); print()
tmpt = tab(covid_confirmed, 'covid_phenotype', 'source', var2_unstyled=1); print()

# merge in cohort data (i.e., pregnancy start and delivery dates)
covid_confirmed = merge(covid_confirmed, _cohort, ['PERSON_ID']); print()

# check
# assert covid_confirmed.where(f.col('_merge') == 'left_only').count() == 0
# may be some left_only i.e., those who have been excluded in inclusion/exclusionn notebook

# tidy
covid_confirmed = covid_confirmed\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check
count_var(covid_confirmed, 'PERSON_ID'); print()


print('------------------------------------------------------------------------------')
print('confirmed COVID-19 admission primary position (i.e., specific hospitalisation for COVID-19; as defined for CCU002)')
print('------------------------------------------------------------------------------')
covid_confirmed_adm_pri = covid_confirmed\
  .where(f.col('covid_phenotype') == '02_Covid_admission_primary_position')\
  .orderBy('PERSON_ID', 'DATE', 'source')

# check
count_var(covid_confirmed_adm_pri, 'PERSON_ID'); print()
tmpt = tabstat(covid_confirmed_adm_pri, 'DATE', date=1); print()
tmpt = tab(covid_confirmed_adm_pri, 'covid_phenotype', 'covid_status', var2_unstyled=1); print()
tmpt = tab(covid_confirmed_adm_pri, 'covid_phenotype', 'source', var2_unstyled=1); print()

# COMMAND ----------

# check
display(covid_confirmed)

# COMMAND ----------

# check
display(covid_confirmed_adm_pri)

# COMMAND ----------

# MAGIC %md ## 3.3 First pre pregnancy

# COMMAND ----------

# check
count_var(covid_confirmed, 'PERSON_ID'); print()


print('------------------------------------------------------------------------------')
print('filter - pre pregnancy')
print('------------------------------------------------------------------------------')
# filter to pre pregnancy
covid_confirmed_pre = covid_confirmed\
  .where(f.col('DATE') < f.col('preg_start_date'))

# check
count_var(covid_confirmed_pre, 'PERSON_ID'); print()


print('------------------------------------------------------------------------------')
print('first (earliest) confirmed covid infection')
print('------------------------------------------------------------------------------')
# window for row number
_win_rownum = Window\
  .partitionBy('PERSON_ID')\
  .orderBy('date', 'covid_phenotype', 'source')

# filter to first (earliest) confirmed covid infection
covid_confirmed_pre_1st = covid_confirmed_pre\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .where(f.col('_rownum') == 1)\
  .withColumnRenamed('DATE', 'exp_pre_covid_1st_date')\
  .orderBy('PERSON_ID')

# check
count_var(covid_confirmed_pre_1st, 'PERSON_ID'); print()
tmpt = tabstat(covid_confirmed_pre_1st, 'exp_pre_covid_1st_date', date=1); print()
tmpt = tab(covid_confirmed_pre_1st, 'covid_phenotype', 'covid_status', var2_unstyled=1); print()
tmpt = tab(covid_confirmed_pre_1st, 'covid_phenotype', 'source', var2_unstyled=1); print()


print('------------------------------------------------------------------------------')
print('severity of the first (earliest) confirmed covid infection (hospitalised within 28 days)')
print('------------------------------------------------------------------------------')
# inner join first (earliest) confirmed covid infection table to the hospitalisations table  
# filter to hospitalisations on or after the date of first (earliest) confirmed covid infection
# filter to first (earliest) hospitalisation
# calculate the number of days from the date of first (earliest) confirmed covid infection to first (earliest) hospitalisation
# flag where this was within 28 days
covid_confirmed_pre_severity = covid_confirmed_adm_pri\
  .join(\
    covid_confirmed_pre_1st\
      .select('PERSON_ID', 'exp_pre_covid_1st_date')\
    , on='PERSON_ID', how='inner'\
  )\
  .where(f.col('DATE') >= f.col('exp_pre_covid_1st_date'))\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .where(f.col('_rownum') == 1)\
  .withColumnRenamed('DATE', 'exp_pre_covid_adm_date')\
  .withColumn('exp_pre_covid_adm_days', f.datediff(f.col('exp_pre_covid_adm_date'), f.col('exp_pre_covid_1st_date')))\
  .withColumn('exp_pre_covid_adm_days_le_28',\
    f.when(f.col('exp_pre_covid_adm_days') <= 28, 1)\
    .when(f.col('exp_pre_covid_adm_days') > 28, 0)\
  )\
  .select('PERSON_ID', 'exp_pre_covid_adm_date', 'exp_pre_covid_adm_days', 'exp_pre_covid_adm_days_le_28')\
  .orderBy('PERSON_ID')

# check
count_var(covid_confirmed_pre_severity, 'PERSON_ID'); print()
tmpt = tabstat(covid_confirmed_pre_severity, 'exp_pre_covid_adm_date', date=1); print()
tmpt = tab(covid_confirmed_pre_severity, 'exp_pre_covid_adm_days_le_28'); print()
tmpt = tabstat(covid_confirmed_pre_severity, var='exp_pre_covid_adm_days', byvar='exp_pre_covid_adm_days_le_28'); print()
tmpt = tab(covid_confirmed_pre_severity, 'exp_pre_covid_adm_days', 'exp_pre_covid_adm_days_le_28', var2_unstyled=1); print()


print('------------------------------------------------------------------------------')
print('combine')
print('------------------------------------------------------------------------------')
# check
count_var(covid_confirmed_pre_1st, 'PERSON_ID'); print()
count_var(covid_confirmed_pre_severity, 'PERSON_ID'); print()

# note: ignore ties in covid_phenotype for now
covid_confirmed_pre_1st = covid_confirmed_pre_1st\
  .select('PERSON_ID', 'exp_pre_covid_1st_date', 'covid_phenotype')\
  .withColumnRenamed('covid_phenotype', 'exp_pre_covid_1st_phenotype')\
  .join(covid_confirmed_pre_severity, on='PERSON_ID', how='left')
  
# check
count_var(covid_confirmed_pre_1st, 'PERSON_ID'); print()
print(covid_confirmed_pre_1st.limit(10).toPandas().to_string()); print()

# COMMAND ----------

# check result
display(covid_confirmed_pre_1st)

# COMMAND ----------

# MAGIC %md ## 3.4 First during pregnancy

# COMMAND ----------

# check
count_var(covid_confirmed, 'PERSON_ID'); print()


print('------------------------------------------------------------------------------')
print('filter - during pregnancy')
print('------------------------------------------------------------------------------')
covid_confirmed_dur = covid_confirmed\
  .where(\
    (f.col('DATE') >= f.col('preg_start_date'))\
    & (f.col('DATE') < f.col('delivery_date'))\
  )

# check
count_var(covid_confirmed_dur, 'PERSON_ID'); print()


print('------------------------------------------------------------------------------')
print('first (earliest) confirmed covid infection')
print('------------------------------------------------------------------------------')
# window for row number
_win_rownum = Window\
  .partitionBy('PERSON_ID')\
  .orderBy('date', 'covid_phenotype', 'source')

# filter to first (earliest) confirmed covid infection
covid_confirmed_dur_1st = covid_confirmed_dur\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .where(f.col('_rownum') == 1)\
  .withColumnRenamed('DATE', 'exp_dur_covid_1st_date')\
  .orderBy('PERSON_ID')

# check
count_var(covid_confirmed_dur_1st, 'PERSON_ID'); print()
tmpt = tabstat(covid_confirmed_dur_1st, 'exp_dur_covid_1st_date', date=1); print()
tmpt = tab(covid_confirmed_dur_1st, 'covid_phenotype', 'covid_status', var2_unstyled=1); print()
tmpt = tab(covid_confirmed_dur_1st, 'covid_phenotype', 'source', var2_unstyled=1); print()


print('------------------------------------------------------------------------------')
print('severity of the first (earliest) confirmed covid infection (hospitalised within 28 days)')
print('------------------------------------------------------------------------------')
# see annotations above
covid_confirmed_dur_severity = covid_confirmed_adm_pri\
  .join(\
    covid_confirmed_dur_1st\
      .select('PERSON_ID', 'exp_dur_covid_1st_date')\
    , on='PERSON_ID', how='inner'\
  )\
  .where(f.col('DATE') >= f.col('exp_dur_covid_1st_date'))\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .where(f.col('_rownum') == 1)\
  .withColumnRenamed('DATE', 'exp_dur_covid_adm_date')\
  .withColumn('exp_dur_covid_adm_days', f.datediff(f.col('exp_dur_covid_adm_date'), f.col('exp_dur_covid_1st_date')))\
  .withColumn('exp_dur_covid_adm_days_le_28',\
    f.when(f.col('exp_dur_covid_adm_days') <= 28, 1)\
    .when(f.col('exp_dur_covid_adm_days') > 28, 0)\
  )\
  .select('PERSON_ID', 'exp_dur_covid_adm_date', 'exp_dur_covid_adm_days', 'exp_dur_covid_adm_days_le_28')\
  .orderBy('PERSON_ID')

# check
count_var(covid_confirmed_dur_severity, 'PERSON_ID'); print()
tmpt = tabstat(covid_confirmed_dur_severity, 'exp_dur_covid_adm_date', date=1); print()
tmpt = tab(covid_confirmed_dur_severity, 'exp_dur_covid_adm_days_le_28'); print()
tmpt = tabstat(covid_confirmed_dur_severity, var='exp_dur_covid_adm_days', byvar='exp_dur_covid_adm_days_le_28'); print()
tmpt = tab(covid_confirmed_dur_severity, 'exp_dur_covid_adm_days', 'exp_dur_covid_adm_days_le_28', var2_unstyled=1); print()


print('------------------------------------------------------------------------------')
print('combine')
print('------------------------------------------------------------------------------')
# check
count_var(covid_confirmed_dur_1st, 'PERSON_ID'); print()
count_var(covid_confirmed_dur_severity, 'PERSON_ID'); print()

# NOTE
#   ignore ties in covid_phenotype for now
covid_confirmed_dur_1st = covid_confirmed_dur_1st\
  .select('PERSON_ID', 'exp_dur_covid_1st_date', 'covid_phenotype')\
  .withColumnRenamed('covid_phenotype', 'exp_dur_covid_1st_phenotype')\
  .join(covid_confirmed_dur_severity, on='PERSON_ID', how='left')
  
# check
count_var(covid_confirmed_dur_1st, 'PERSON_ID'); print()
print(covid_confirmed_dur_1st.limit(10).toPandas().to_string()); print()

# COMMAND ----------

# check result
display(covid_confirmed_dur_1st)

# COMMAND ----------

# MAGIC %md # 4 Vaccination

# COMMAND ----------

# MAGIC %md ## 4.1 Check

# COMMAND ----------

# check
display(vacc)

# COMMAND ----------

# check vacc for cohort

# check out of range (before vaccination commenced)
# display(vacc.where(f.col('RECORDED_DATE') < '2020-12-08')) 
# <10 rows in total relating to <10 id, where there is a second dose_sequence==1 a year later, remove these <10 erroneous rows
# ~25 rows for DATE (see below)

# check null vaccination dates
# display(vacc.where(f.col('RECORDED_DATE').isNull())) 
# ~60 rows
# 0 rows for DATE (see below)

# compare DATE_AND_TIME vs RECORDED_DATE
# DATE_AND_TIME	- The date and time on which the vaccination intervention was carried out or was meant to be administered
# RECORDED_DATE	- The date that the vaccination administered (procedure) or not administered (situation) was recorded in the source system
tmpp = vacc\
  .withColumn('RECORDED_DATE', f.to_date(f.col('RECORDED_DATE'), 'yyyyMMdd'))\
  .withColumn('DATE', f.to_date(f.substring(f.col('DATE_AND_TIME'), 1, 8), 'yyyyMMdd'))\
  .withColumn('_RECORDED_DATE_flag', f.when(f.col('RECORDED_DATE').isNotNull(), 1).otherwise(0))\
  .withColumn('_DATE_flag', f.when(f.col('DATE').isNotNull(), 1).otherwise(0))\
  .withColumn('_RECORDED_DATE_lt_max', f.when(f.col('RECORDED_DATE') < '2020-12-08', 1).otherwise(0))\
  .withColumn('_DATE_lt_max', f.when(f.col('DATE') < '2020-12-08', 1).otherwise(0))\
  .withColumn('_diff', f.datediff(f.col('DATE'), f.col('RECORDED_DATE')))
tmpt = tab(tmpp, '_DATE_flag', '_RECORDED_DATE_flag', var2_unstyled=1); print()
tmpt = tab(tmpp, '_DATE_lt_max', '_RECORDED_DATE_lt_max', var2_unstyled=1); print()
tmpt = tabstat(tmpp, '_diff'); print()
tmpt = tab(tmpp, '_diff'); print()
# 95% same day agreement, 99% within 1 week

# check declined vaccinations
# VACCINATION_SITUATION_CODE 
# 1324741000000101 'first dose declined' 
# 1324751000000103 'second dose declined' 
tmpt = tab(vacc, 'VACCINATION_SITUATION_CODE'); print()

# check not given vaccinations
tmpt = tab(vacc, 'NOT_GIVEN'); print()

# VACCINATION_PROCEDURE_CODE
# 1324681000000101 'first dose'
# 1324691000000104 'second dose' 
# 1362591000000103 'booster'
tmpt = tab(vacc, 'DOSE_SEQUENCE', 'VACCINATION_PROCEDURE_CODE', var2_unstyled=1); print()

# VACCINE_PRODUCT_CODE
# see lookup table below
tmpt = tab(vacc, 'VACCINE_PRODUCT_CODE'); print()

# COMMAND ----------

display(check_dis(tmpp.where(f.col('_diff') > -600), '_diff', _bins=200))

# COMMAND ----------

display(check_dis(tmpp.where((f.col('_diff') > -600) & (f.col('_diff') != 0)), '_diff', _bins=200))

# COMMAND ----------

# MAGIC %md ## 4.2 Clean

# COMMAND ----------

# clean in accordance with CCU002 and SAIL RRDA_CVVD (Research Ready Data Asset for COVID-19 Vaccination Data)
# note: going forward these cleaning steps should expanded for the full vaccination table and added to the curated tables notebook

# check
tmpc = count_var(vacc, 'PERSON_ID_DEID', ret=1, df_desc='original', indx=1); print()


print('---------------------------------------------------------------------------------')
print('add dose sequence that includes booster as #3')
print('---------------------------------------------------------------------------------')
# rename id 
# reformat date 
# add dose sequence that includes booster as #3
tmp1 = vacc\
  .withColumnRenamed('PERSON_ID_DEID', 'PERSON_ID')\
  .withColumn('DATE', f.to_date(f.substring(f.col('DATE_AND_TIME'), 1, 8), 'yyyyMMdd'))\
  .withColumn('DOSE_SEQUENCE_TB', f.when(f.col('VACCINATION_PROCEDURE_CODE') == '1362591000000103', 3).otherwise(f.col('DOSE_SEQUENCE')))

# check
tmpt = tab(tmp1, 'DOSE_SEQUENCE_TB', 'DOSE_SEQUENCE', var2_unstyled=1); print()
tmpt = tab(tmp1, 'DOSE_SEQUENCE_TB', 'VACCINATION_PROCEDURE_CODE', var2_unstyled=1); print()
tmpt = tabstat(tmp1, 'DATE', date=1); print()


print('---------------------------------------------------------------------------------')
print('add lookup for vaccine product code')
print('---------------------------------------------------------------------------------')
# created using https://termbrowser.nhs.uk/?
lookup_vaccine_product_code = """
VACCINE_PRODUCT_CODE,VACCINE_PRODUCT_DESC,VACCINE_PRODUCT_DESC2
39114911000001105, AstraZeneca,
39115011000001105, AstraZeneca,8 dose
39115111000001106, AstraZeneca,10 dose
39115611000001103, Pfizer,
39115711000001107, Pfizer,6 dose
39230211000001104, Janssen-Cilag,
39326911000001101, Moderna,
39373511000001104, Valneva UK,
39375411000001104, Moderna,10 dose
39473011000001103, Novavax CZ,
40306411000001101, Sinovac Life Sciences,
40348011000001102, Serum Institute of India,
40384611000001108, Pfizer Children 5-11,
"""
lookup_vaccine_product_code = spark.createDataFrame(
    pd.DataFrame(pd.read_csv(io.StringIO(lookup_vaccine_product_code))
  )\
  .fillna('')\
  .astype(str))

# check
count_var(lookup_vaccine_product_code, 'VACCINE_PRODUCT_CODE'); print()
print(lookup_vaccine_product_code.toPandas().to_string()); print()

# merge
tmp2 = merge(tmp1, lookup_vaccine_product_code, ['VACCINE_PRODUCT_CODE']); print()

# check
assert tmp2.where(f.col('_merge').isin(['both', 'left_only'])).count() == tmp2.count()

# tidy
tmp2 = tmp2\
  .drop('_merge')

# concat desc
tmp2 = tmp2\
  .withColumn('VACCINE_PRODUCT_DESC2',\
    f.when(f.col('VACCINE_PRODUCT_DESC2') != '', f.concat(f.lit(' - '), f.col('VACCINE_PRODUCT_DESC2')))\
    .otherwise(f.col('VACCINE_PRODUCT_DESC2'))\
  )\
  .withColumn('VACCINE_PRODUCT_DESC_CONCAT', f.concat_ws('', f.col('VACCINE_PRODUCT_DESC'), f.col('VACCINE_PRODUCT_DESC2')))

# check
tmpt = tab(tmp2, 'VACCINE_PRODUCT_DESC_CONCAT'); print()


print('---------------------------------------------------------------------------------')
print('remove out of range date records')
print('---------------------------------------------------------------------------------')
# remove out of range records (before vaccinations commenced on 8th December 2020; CCU002, flagged in SAIL)
# SAIL also flags vaccinations > today (we could apply > ProductionDate, but we are applying a filter below for > end of follow-up - consider for a curated vaccination table)
tmp3 = tmp2\
  .where(f.col('DATE') >= '2020-12-08')

# check
tmpt = count_var(tmp3, 'PERSON_ID', ret=1, df_desc='post remove out of range date records', indx=2); print()
tmpc = tmpc.unionByName(tmpt)
tmpt = tabstat(tmp3, 'DATE', byvar='VACCINE_PRODUCT_DESC_CONCAT', date=1); print()
tmpt = tabstat(tmp3, 'DATE', byvar='VACCINE_PRODUCT_DESC', date=1); print()

# check by vaccine product (SAIL)
# assert tmp3.where((f.col('VACCINE_PRODUCT_DESC') == 'Pfizer') & (f.col('RECORDED_DATE') < '2020-12-08')).count() == 0 # check not needed
assert tmp3.where((f.col('VACCINE_PRODUCT_DESC') == 'AstraZeneca') & (f.col('DATE') < '2021-01-04')).count() == 0
assert tmp3.where((f.col('VACCINE_PRODUCT_DESC') == 'Moderna') & (f.col('DATE') < '2021-01-04')).count() == 0
assert tmp3.where((f.col('VACCINE_PRODUCT_DESC') == 'Janssen-Cilag') & (f.col('DATE') < '2021-03-01')).count() == 0


print('---------------------------------------------------------------------------------')
print('remove declined vaccination records')
print('---------------------------------------------------------------------------------')
# remove declined vaccination records (CCU002, flagged in SAIL)
tmp4 = tmp3\
  .withColumn('_declined',\
    f.when(f.col('VACCINATION_SITUATION_CODE') == '1324741000000101', 1)\
     .when(f.col('VACCINATION_SITUATION_CODE') == '1324751000000103', 2)\
  )\
  .where(f.col('_declined').isNull())

# check
tmpt = count_var(tmp4, 'PERSON_ID', ret=1, df_desc='post remove declined records', indx=3); print()
tmpc = tmpc.unionByName(tmpt)
assert tmp4.where(f.col('VACCINATION_SITUATION_CODE').isNull()).count() == tmp4.count()
assert tmp4.where(f.col('NOT_GIVEN') == 'FALSE').count() == tmp4.count()


print('---------------------------------------------------------------------------------')
print('drop duplicates (on all variables)')
print('---------------------------------------------------------------------------------')
# reduce columns
# drop duplicates (on all variables)
_list_cols = [
  'PERSON_ID'
  , 'DATE'
  , 'DOSE_SEQUENCE_TB'
  , 'VACCINE_PRODUCT_DESC'
  , 'NHS_NUMBER_STATUS_INDICATOR_CODE'
]
tmp5 = tmp4\
  .select(*_list_cols)\
  .dropDuplicates()

# check
tmpt = count_var(tmp5, 'PERSON_ID', ret=1, df_desc='post drop duplicates (all variables)', indx=4); print()
tmpc = tmpc.unionByName(tmpt)


print('---------------------------------------------------------------------------------')
print('drop duplicates on PERSON_ID and DATE')
print('---------------------------------------------------------------------------------')
# define windows
_win_rownum = Window\
  .partitionBy('PERSON_ID', 'DATE')\
  .orderBy('DOSE_SEQUENCE_TB')
_win_rownummax = Window\
  .partitionBy('PERSON_ID', 'DATE')

# add rownum and rownummax
tmp6 = tmp5\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .withColumn('_rownummax', f.count('PERSON_ID').over(_win_rownummax))\

# check
tmpt = tab(tmp6.where(f.col('_rownum') == 1), '_rownummax'); print()
tmpt = tab(tmp6, '_rownum', 'DOSE_SEQUENCE_TB', var2_unstyled=1); print()

# check
# display(tmp6.where(f.col('_rownummax') > 1))

# check originial records
# display(vacc.where(f.col('PERSON_ID_DEID') == '<REMOVED_FOR_EXPORT>'))

# check originial records
# display(vacc.where(f.col('PERSON_ID_DEID') == '<REMOVED_FOR_EXPORT>'))

# remove duplicate DATE (keep earliest DOSE_SEQUENCE_TB)
# tidy
tmp6 = tmp6\
  .where(f.col('_rownum') == 1)\
  .drop('_rownum', '_rownummax')

# check
tmpt = count_var(tmp6, 'PERSON_ID', ret=1, df_desc='post drop duplicates (PERSON_ID, DATE)', indx=5); print()
tmpc = tmpc.unionByName(tmpt)


print('---------------------------------------------------------------------------------')
print('remove date difference <20 days')
print('---------------------------------------------------------------------------------')
# define windows for rownum and rownummax, which are used below
_win_rownum = Window\
  .partitionBy('PERSON_ID')\
  .orderBy(['DATE', 'DOSE_SEQUENCE_TB'])
_win_rownummax = Window\
  .partitionBy('PERSON_ID')

# add rownum and rownummax
# create date difference between date and previous date within individuals
# create indicator for when the date difference is <20 days (assumed to be the minimum legitimate interval between vaccine administration [SAIL])
tmp7 = tmp6\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .withColumn('_rownummax', f.count('PERSON_ID').over(_win_rownummax))\
  .withColumn('_diff', f.datediff(f.col('DATE'), f.lag(f.col('DATE'), 1).over(_win_rownum)))\
  .withColumn('_diff_lt20', f.when(f.col('_diff') < 20, 1).otherwise(0))\
  .orderBy('PERSON_ID', '_rownum')

# check
tmpt = tab(tmp7, '_diff_lt20'); print()
tmpt = tab(tmp7, '_rownum', '_diff_lt20', var2_unstyled=1); print()
count_var(tmp7.where(f.col('_diff_lt20') == 1), 'PERSON_ID'); print()
tmpt = tabstat(tmp7, '_diff'); print()
tmpt = tab(tmp7, '_diff'); print()
tmpt = tab(tmp7.where(f.col('_diff').isNotNull()), '_diff', cumsum=1); print()
tmp7a = tmp7

# remove date diff < 20 days
# tidy
tmp7 = tmp7\
  .where(f.col('_diff_lt20') != 1)\
  .drop('_rownum', '_rownummax', '_diff', '_diff_lt20')

# check
tmpt = count_var(tmp7, 'PERSON_ID', ret=1, df_desc='post remove date difference <20 days', indx=6); print()
tmpc = tmpc.unionByName(tmpt)


print('---------------------------------------------------------------------------------')
print('flag sequential dose sequence')
print('---------------------------------------------------------------------------------')
# e.g., no second dose before first dose, no multiple second doses

# define windows for rownum and rownummax, which are used below
_win_rownum = Window\
  .partitionBy('PERSON_ID')\
  .orderBy('DATE', 'DOSE_SEQUENCE_TB')
_win_rownummax = Window\
  .partitionBy('PERSON_ID')
_win_egen = Window\
  .partitionBy('PERSON_ID')\
  .orderBy('DATE', 'DOSE_SEQUENCE_TB')\
  .rowsBetween(Window.unboundedPreceding, Window.unboundedFollowing)

# add row number and maximum row number
# compare dose sequence with row number (record level)
# create an indicator of any invalid dose sequence for an individual 
tmp8 = tmp7\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .withColumn('_rownummax', f.count('PERSON_ID').over(_win_rownummax))\
  .withColumn('_ind_invalid_dose_seq', f.when(f.col('DOSE_SEQUENCE_TB') != f.col('_rownum'), 1).otherwise(0))\
  .withColumn('_ind_invalid_dose_seq_max', f.max(f.col('_ind_invalid_dose_seq')).over(_win_egen))

# check
tmpt = tab(tmp8, '_ind_invalid_dose_seq'); print()
tmpt = tab(tmp8.where(f.col('_rownum') == 1), '_ind_invalid_dose_seq_max'); print()
tmpt = tab(tmp8.where(f.col('_rownum') == 1), '_rownummax'); print()
tmpt = tab(tmp8, '_rownum', 'DOSE_SEQUENCE_TB', var2_unstyled=1); print()

# check records
# tmpp = tmp8\
#   .where((f.col('DOSE_SEQUENCE_TB') == 1) & (f.col('_rownum') == 2))\
#   .select('PERSON_ID')\
#   .distinct()
# tmpp = tmp8\
#   .join(tmpp, on='PERSON_ID', how='inner')
# display(tmpp)

# Data dictionary:
# - DOSE_SEQUENCE - Nominal position in a series of vaccines.
# - VACCINATION_PROCEDURE_CODE - A unique SNOMED Concept Id code relating to vaccine that was administered (procedure) 

# check vs NHS_NUMBER_STATUS_INDICATOR_CODE
tmpt = tab(tmp8.where(f.col('_rownum') == 1), 'NHS_NUMBER_STATUS_INDICATOR_CODE', '_ind_invalid_dose_seq_max', var2_unstyled=1); print()

# add column percentages (to be added as an option to the tab function)
# print(tmpt.keys(), type(tmpt))
tmpt[('_ind_invalid_dose_seq_max', '0_COLPCT')] = ((tmpt[('_ind_invalid_dose_seq_max', 0)].replace(',', '', regex=True).astype(int)/int(re.sub(',', '', tmpt.loc[('Total'), ('_ind_invalid_dose_seq_max', 0)])))*100).round(2) 
tmpt[('_ind_invalid_dose_seq_max', '1_COLPCT')] = ((tmpt[('_ind_invalid_dose_seq_max', 1)].replace(',', '', regex=True).astype(int)/int(re.sub(',', '', tmpt.loc[('Total'), ('_ind_invalid_dose_seq_max', 1)])))*100).round(2) 
pd.set_option('display.max_columns', len(tmpt.columns))
pd.set_option('expand_frame_repr', False) 
print(tmpt); print()
pd.reset_option('display.max_columns')
pd.reset_option('expand_frame_repr')
# evidence that the invalid dose sequence may be due to poorer linkage (i.e., absence of NHS number)

# tidy
tmp8 = tmp8\
  .drop('NHS_NUMBER_STATUS_INDICATOR_CODE', '_rownum', '_rownummax', '_ind_invalid_dose_seq')

# check
tmpt = count_var(tmp8, 'PERSON_ID', ret=1, df_desc='post flag invalid dose sequence', indx=7); print()
tmpc = tmpc.unionByName(tmpt)  

# check    
print(tmp8.limit(10).toPandas().to_string()); print() 

# COMMAND ----------

display(check_dis(tmp7a, '_diff', _bins=100))

# COMMAND ----------

# check flow table
tmpp = tmpc.toPandas()
tmpp['n'] = tmpp['n'].astype(int)
tmpp['n_id'] = tmpp['n_id'].astype(int)
tmpp['n_id_distinct'] = tmpp['n_id_distinct'].astype(int)
tmpp['diff_n'] = (tmpp['n'] - tmpp['n'].shift(1)).fillna(0).astype(int)
tmpp['diff_n_id'] = (tmpp['n_id'] - tmpp['n_id'].shift(1)).fillna(0).astype(int)
tmpp['diff_n_id_distinct'] = (tmpp['n_id_distinct'] - tmpp['n_id_distinct'].shift(1)).fillna(0).astype(int)
print(tmpp.to_string())

# COMMAND ----------

# MAGIC %md ## 4.3 Prepare

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print('merge with cohort')
print('---------------------------------------------------------------------------------')
# check
count_var(_cohort, 'PERSON_ID'); print()

# merge vacc and cohort
tmp9 = merge(tmp8, _cohort, ['PERSON_ID']); print()

# check
# assert tmp9.where(f.col('_merge') == 'left_only').count() == 0
# may be some left_only i.e., those who have been excluded in inclusion/exclusion notebook

# tidy
tmp9 = tmp9\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check
tmpt = count_var(tmp9, 'PERSON_ID', ret=1, df_desc='post merge with cohort', indx=2); print()
tmpc = tmpc.unionByName(tmpt)


print('---------------------------------------------------------------------------------')
print('remove records after follow up end date')
print('---------------------------------------------------------------------------------')
tmp10 = tmp9\
  .where(f.col('DATE') <= f.col('fu_end_date'))

# check
tmpt = count_var(tmp10, 'PERSON_ID', ret=1, df_desc='post remove records > follow up end date', indx=5); print()
tmpc = tmpc.unionByName(tmpt)
tmpt = tabstat(tmp10, 'DATE', date=1); print()

# check    
print(tmp10.limit(10).toPandas().to_string()); print() 

# recheck flow table
tmpp = tmpc.toPandas()
tmpp['n'] = tmpp['n'].astype(int)
tmpp['n_id'] = tmpp['n_id'].astype(int)
tmpp['n_id_distinct'] = tmpp['n_id_distinct'].astype(int)
tmpp['diff_n'] = (tmpp['n'] - tmpp['n'].shift(1)).fillna(0).astype(int)
tmpp['diff_n_id'] = (tmpp['n_id'] - tmpp['n_id'].shift(1)).fillna(0).astype(int)
tmpp['diff_n_id_distinct'] = (tmpp['n_id_distinct'] - tmpp['n_id_distinct'].shift(1)).fillna(0).astype(int)
print(tmpp.to_string())

# ease of reference below
vacc_clean = tmp10

# COMMAND ----------

# MAGIC %md ## 4.4 First anytime

# COMMAND ----------

# define windows for rownum and rownummax, which are used below
_win_rownum = Window\
  .partitionBy('PERSON_ID')\
  .orderBy('DATE', 'DOSE_SEQUENCE_TB')
_win_rownummax = Window\
  .partitionBy('PERSON_ID')

# first vaccination
tmp1 = vacc_clean\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .withColumn('_rownummax', f.count('PERSON_ID').over(_win_rownummax))\
  .orderBy('PERSON_ID', '_rownum')

# check
tmpt = tab(tmp1.where(f.col('_rownum') == 1), '_rownummax'); print()
tmpt = tab(tmp1, '_rownum', 'DOSE_SEQUENCE_TB', var2_unstyled=1); print()

# filter to first
tmp2 = tmp1\
  .where(f.col('_rownum') == 1)

# check
count_var(tmp2, 'PERSON_ID'); print()

# finalise
vacc_any_1st = tmp2\
  .select('PERSON_ID', 'DATE')\
  .withColumnRenamed('DATE', 'vacc_1st_date')\
  .withColumn('vacc_1st_flag', f.lit(1))

# check
print(vacc_any_1st.limit(10).toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md ## 4.5 First and second during pregnancy

# COMMAND ----------

# filter to during pregnancy
# add row number and row number max per individual
tmp1 = vacc_clean\
  .where(\
    (f.col('DATE') >= f.col('preg_start_date'))\
    & (f.col('DATE') < f.col('delivery_date'))\
  )\
  .withColumn('_rownum', f.row_number().over(_win_rownum))\
  .withColumn('_rownummax', f.count('PERSON_ID').over(_win_rownummax))\
  .orderBy('PERSON_ID', '_rownum')

# check
tmpt = tab(tmp1.where(f.col('_rownum') == 1), '_rownummax'); print()
tmpt = tab(tmp1, '_rownum', 'DOSE_SEQUENCE_TB', var2_unstyled=1); print()

# filter to first and second only
# reduce columns
tmp2 = tmp1\
  .where(f.col('_rownum').isin([1,2]))\
  .select('PERSON_ID', '_rownum', 'DATE')

# reshape from long to wide format
# add difference between first and second for checking purposes
tmp3 = reshape_long_to_wide(tmp2, i=['PERSON_ID'], j='_rownum', stubname='DATE')\
  .withColumn('_diff', f.datediff(f.col('DATE2'), f.col('DATE1')))

# check 
count_var(tmp3, 'PERSON_ID'); print()
tmpt = tab(tmp3, '_diff', cumsum=1); print()
tmpt = tab(tmp3.where(f.col('_diff').isNotNull()), '_diff', cumsum=1); print()

# finalise
vacc_dur_1st = tmp3\
  .drop('_diff')\
  .withColumnRenamed('DATE1', 'vacc_1st_dur_date')\
  .withColumnRenamed('DATE2', 'vacc_2nd_dur_date')\
  .withColumn('vacc_1st_dur_flag', f.when(f.col('vacc_1st_dur_date').isNotNull(), 1))\
  .withColumn('vacc_2nd_dur_flag', f.when(f.col('vacc_2nd_dur_date').isNotNull(), 1))

# check
tmpt = tab(vacc_dur_1st, 'vacc_1st_dur_flag', 'vacc_2nd_dur_flag', var2_unstyled=1); print()
print(vacc_dur_1st.limit(10).toPandas().to_string()); print()

# COMMAND ----------

display(check_dis(tmp3, '_diff', _bins=100))

# COMMAND ----------

# MAGIC %md # 5 Save

# COMMAND ----------

# check
count_var(covid_confirmed_pre_1st, 'PERSON_ID'); print()
count_var(covid_confirmed_dur_1st, 'PERSON_ID'); print()  
count_var(vacc_any_1st, 'PERSON_ID'); print()  
count_var(vacc_dur_1st, 'PERSON_ID'); print()  
   
# merge all
tmp1 = covid_confirmed_pre_1st\
  .join(covid_confirmed_dur_1st, on=['PERSON_ID'], how='outer')\
  .join(vacc_any_1st, on=['PERSON_ID'], how='outer')\
  .join(vacc_dur_1st, on=['PERSON_ID'], how='outer')\

# check
count_var(tmp1, 'PERSON_ID'); print()
print(len(tmp1.columns)); print()
print(pd.DataFrame({f'_cols': tmp1.columns}).to_string()); print()

# add cohort_id
_cohort_id = cohort\
  .select('PERSON_ID')

# check
count_var(_cohort_id, 'PERSON_ID'); print()

# merge
tmp2 = merge(tmp1, _cohort_id, ['PERSON_ID']); print()

# check
assert tmp2.select('_merge').where(f.col('_merge').isin(['both', 'right_only'])).count() == tmp2.count()

# tidy
tmp2 = tmp2\
  .drop('_merge')

# check
count_var(tmp2, 'PERSON_ID'); print()

# COMMAND ----------

# check final
display(tmp2)

# COMMAND ----------

# save name
outName = f'{proj}_out_exposures'.lower()

# save previous version for comparison purposes
_datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
outName_pre = f'{outName}_pre{_datetimenow}'.lower()
print(outName_pre)
spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')

# save
tmp2.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

# COMMAND ----------



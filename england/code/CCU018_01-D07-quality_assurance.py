# Databricks notebook source
# MAGIC %md # CCU018_01-D07-quality_assurance
# MAGIC
# MAGIC **Description** This notebook creates the quality assurance table based on work from CCU002 and previous work by Spiros Denaxas, which identifies indviduals with conflicting/incomplete data to potentially remove from the cohort. Note from CCU002 re pregnancy and prostate cancer code lists: pregnancy codes were identified by searching all available codes in the GDPPR dictionary; prostate cancer codes were sourced from CALIBER and were reviewed after the READ to SNOMED conversion. 
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** tmp_skinny, cur_deaths_sing, in_gdppr, out_codelist
# MAGIC
# MAGIC **Data output** ccu018_01_tmp_quality_assurance (ccu018_01_tmp_skinny_unassembled, ccu018_01_tmp_skinny_assembled)

# COMMAND ----------

spark.sql('CLEAR CACHE')

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

# MAGIC %md 
# MAGIC # 0 Parameters

# COMMAND ----------

# MAGIC %run "./CCU018_01-D01-parameters"

# COMMAND ----------

# MAGIC %md # 1 Data

# COMMAND ----------

skinny   = spark.table(path_tmp_skinny)
deaths   = spark.table(path_cur_deaths_sing)
gdppr    = spark.table(path_in_gdppr)
# spark.sql(f""" REFRESH TABLE {path_out_codelist}""")
codelist = spark.table(path_out_codelist)

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print('skinny')
print('---------------------------------------------------------------------------------')
# reduce
_skinny = skinny\
  .select('PERSON_ID', 'DOB', 'SEX', 'DOD')

# check
count_var(_skinny, 'PERSON_ID'); print()


print('---------------------------------------------------------------------------------')
print('deaths')
print('---------------------------------------------------------------------------------')
# reduce
_deaths = deaths\
  .select('PERSON_ID', 'REG_DATE', 'REG_DATE_OF_DEATH')

# check
count_var(_deaths, 'PERSON_ID'); print()

# merge
_qa = merge(_skinny, _deaths, ['PERSON_ID']); print()

# check
assert _qa.where(f.col('_merge') == 'right_only').count() == 0
count_var(_qa, 'PERSON_ID'); print()

# tidy
_qa = _qa\
  .withColumn('in_deaths', f.when(f.col('_merge') == 'both', 1).otherwise(0))\
  .drop('_merge')


print('---------------------------------------------------------------------------------')
print('gdppr')
print('---------------------------------------------------------------------------------')
# check
count_var(gdppr, 'NHS_NUMBER_DEID'); print()

_gdppr = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'CODE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')\
  .where(f.col('DATE').isNotNull())

# check
count_var(_gdppr, 'PERSON_ID'); print()

# COMMAND ----------

# check
display(_qa)

# COMMAND ----------

# check DOD agreement
tmp = _qa\
  .withColumn('equality', f.col('DOD') == f.col('REG_DATE_OF_DEATH'))\
  .withColumn('null_safe_equality', udf_null_safe_equality('DOD', 'REG_DATE_OF_DEATH'))
tmpt = tab(tmp, 'equality', 'null_safe_equality', var2_unstyled=1); print()

tmp1 = tmp\
  .where(f.col('DOD').isNull())
tmpt = tab(tmp1, 'REG_DATE_OF_DEATH'); print()

tmp2 = tmp\
  .where(f.col('REG_DATE_OF_DEATH').isNull())
tmpt = tab(tmp2, 'DOD'); print()

# COMMAND ----------

# check
display(_gdppr)

# COMMAND ----------

# MAGIC %md # 3 Medical conditions

# COMMAND ----------

# MAGIC %md ## 3.1 Codelist

# COMMAND ----------

# codelist hx
codelist_hx = codelist\
  .where(f.col('name').isin(['pregnancy', 'prostate_cancer']))

# check
tmpt = tab(codelist_hx, 'name', 'terminology', var2_unstyled=1); print()
_list_terms = list(\
  codelist_hx\
    .select('terminology')\
    .distinct()\
    .toPandas()['terminology']\
  )
# assert set(_list_terms) <= set(['SNOMED'])
# AssertionError above, there now exists ICD10, OPCS4 codes for pregnancy, restrict to SNOMED
codelist_hx = codelist_hx\
  .where(f.col('terminology') == 'SNOMED')

# codelist hx gdppr
codelist_hx_gdppr = codelist_hx

# COMMAND ----------

# MAGIC %md ## 3.2 Create

# COMMAND ----------

# gdppr
_tmp_codelist_hx_gdppr = codelist_hx_gdppr\
  .select(['code', 'name'])
_hx_gdppr = _gdppr\
  .join(_tmp_codelist_hx_gdppr, on='code', how='inner')

# first event of each name
_win = Window\
  .partitionBy(['PERSON_ID', 'name'])\
  .orderBy('DATE')
_hx_1st = _hx_gdppr\
  .withColumn('_rownum', f.row_number().over(_win))\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'DATE', 'name')\
  .orderBy('PERSON_ID', 'DATE', 'name')
# note: ignore ties for source for now...

# reshape
_hx_1st_wide = _hx_1st\
  .withColumn('name', f.concat(f.lit('_'), f.lower(f.col('name'))))\
  .groupBy('PERSON_ID')\
  .pivot('name')\
  .agg(f.first('DATE'))\
  .withColumn('_pregnancy_ind', f.when(f.col('_pregnancy').isNotNull(), 1))\
  .withColumn('_prostate_cancer_ind', f.when(f.col('_prostate_cancer').isNotNull(), 1))\
  .orderBy('PERSON_ID')

# COMMAND ----------

# MAGIC %md ## 3.3 Check

# COMMAND ----------

display(_hx_1st_wide)

# COMMAND ----------

count_var(_hx_1st_wide, 'PERSON_ID'); print()
count_var(_hx_1st_wide, '_pregnancy'); print()
count_var(_hx_1st_wide, '_prostate_cancer'); print()
tmpt = tab(_hx_1st_wide, '_pregnancy_ind', '_prostate_cancer_ind', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md # 4 Rules

# COMMAND ----------

# MAGIC %md ## 4.1 Prepare

# COMMAND ----------

# check
count_var(_qa, 'PERSON_ID'); print()
count_var(_hx_1st_wide, 'PERSON_ID'); print()
_qa = merge(_qa, _hx_1st_wide, ['PERSON_ID']); print()

# check
assert _qa.where(f.col('_merge') == 'right_only').count() == 0
count_var(_qa, 'PERSON_ID'); print()

# tidy
_qa = _qa\
  .drop('_merge')

# COMMAND ----------

# check
display(_qa)

# COMMAND ----------

# ------------------------------------------------------------------------------
# preparation: rule 8 (Patients have all missing record_dates and dates)
# ------------------------------------------------------------------------------
print('gdppr'); print()
count_var(gdppr, 'NHS_NUMBER_DEID')

# identify records with null date
_gdppr_null = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'RECORD_DATE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')\
  .withColumn('_null', f.when((f.col('DATE').isNull()) & (f.col('RECORD_DATE').isNull()), 1).otherwise(0))

# check
tmpt = tab(_gdppr_null, '_null'); print()

# summarise per individual
_gdppr_null_summ = _gdppr_null\
  .groupBy('PERSON_ID')\
  .agg(\
    f.sum(f.when(f.col('_null') == 0, 1).otherwise(0)).alias('_n_gdppr_notnull')\
    , f.sum(f.col('_null')).alias('_n_gdppr_null')\
  )\
  .where(f.col('_n_gdppr_null') > 0)

# check
tmp = _gdppr_null_summ\
  .select('_n_gdppr_null')\
  .groupBy()\
  .sum()\
  .collect()[0][0]
print(tmp); print()

# check
print(_gdppr_null_summ.toPandas().to_string()); print()

# check
count_var(_qa, 'PERSON_ID'); print()
count_var(_gdppr_null_summ, 'PERSON_ID'); print()

# merge
_qa = merge(_qa, _gdppr_null_summ, ['PERSON_ID']); print()

# check
assert _qa.where(f.col('_merge') == 'right_only').count() == 0
count_var(_qa, 'PERSON_ID'); print()

# tidy
_qa = _qa\
  .drop('_merge')

# COMMAND ----------

# check
display(_qa)

# COMMAND ----------

# MAGIC %md ## 4.2 Create

# COMMAND ----------

# Based on CCU002
# Rule 1: Year of birth is after the year of death
# Rule 2: Patient does not have mandatory fields completed (nhs_number, sex, Date of birth)
# Rule 3: Year of Birth Predates NHS Established Year or Year is over the Current Date
# Rule 4: Remove those with only null/invalid dates of death
# Rule 5: Remove those where registered date of death before the actual date of death
# Rule 6: Pregnancy/birth codes for men
# Rule 7: Prostate Cancer Codes for women
# Rule 8: Patients have all missing record_dates and dates
_qax = _qa\
  .withColumn('YOB', f.year(f.col('DOB')))\
  .withColumn('YOD', f.year(f.col('DOD')))\
  .withColumn('_rule_1', f.when(f.col('YOB') > f.col('YOD'), 1).otherwise(0))\
  .withColumn('_rule_2',\
    f.when(\
      (f.col('SEX').isNull()) | (~f.col('SEX').isin([1,2]))\
      | (f.col('DOB').isNull())\
      | (f.col('PERSON_ID').isNull()) | (f.col('PERSON_ID') == '') | (f.col('PERSON_ID') == ' ')\
    , 1).otherwise(0)\
  )\
  .withColumn('_rule_3',\
    f.when(\
      (f.col('YOB') < 1793) | (f.col('YOB') > datetime.datetime.today().year)\
    , 1).otherwise(0)\
  )\
  .withColumn('_rule_4',\
    f.when(\
      (f.col('in_deaths') == 1)\
      & (\
        (f.col('REG_DATE_OF_DEATH').isNull())\
        | (f.col('REG_DATE_OF_DEATH') <= f.to_date(f.lit('1900-01-01')))\
        | (f.col('REG_DATE_OF_DEATH') > f.current_date())\
      )\
    , 1).otherwise(0)\
  )\
  .withColumn('_rule_5', f.when(f.col('REG_DATE_OF_DEATH') > f.col('REG_DATE'), 1).otherwise(0))\
  .withColumn('_rule_6', f.when((f.col('SEX') == 1) & (f.col('_pregnancy_ind') == 1) , 1).otherwise(0))\
  .withColumn('_rule_7', f.when((f.col('SEX') == 2) & (f.col('_prostate_cancer_ind') == 1) , 1).otherwise(0))\
  .withColumn('_rule_8', f.when((f.col('_n_gdppr_null') > 0) & (f.col('_n_gdppr_notnull') == 0) , 1).otherwise(0)) 

# row total
_qax = _qax\
  .withColumn('_rule_total', sum([f.col(col) for col in _qax.columns if re.match('^_rule_.*$', col)]))

# check
tmpt = tab(_qax, '_rule_total')

# cache and check
_qax.cache()
count_var(_qax, 'PERSON_ID'); print()

# COMMAND ----------

# MAGIC %md ## 4.3 Checks

# COMMAND ----------

# check rule frequency
for i in list(range(1, 9)):
  tmpt = tab(_qax, f'_rule_{i}'); print()
  
 # check rule patterns
_qax = _qax\
  .withColumn('_rule_concat', f.concat(*[f'_rule_{i}' for i in list(range(1, 9))]))
tmpt = tab(_qax, '_rule_concat')

# tmp = _qax\
#   .groupBy('_rule_concat')\
#   .agg(f.count(f.col('_rule_concat')).alias('n'))\
#   .orderBy(f.desc('n'))
# display(tmp)

# COMMAND ----------

# MAGIC %md # 5 Save 

# COMMAND ----------

# reduce columns
tmp1 = _qax\
  .select(['PERSON_ID'] + [col for col in _qax.columns if re.match('^_rule_.*$', col)])\
  .drop('_rule_concat')

# recode 0 to null (for summary table)
for v in [col for col in tmp1.columns if re.match('^_rule_.*$', col)]:
  tmp1 = tmp1\
    .withColumn(v, f.when(f.col(v) == 0, f.lit(None)).otherwise(f.col(v)))

# check
count_var(tmp1, 'PERSON_ID')

# COMMAND ----------

# check 
display(tmp1)

# COMMAND ----------

# check 
display(tmp1.where(f.col('_rule_total').isNotNull()))

# COMMAND ----------

# save name
# 20220807 changed from _out_quality_assurance to _tmp_quality_assurance (not an "out" table - as number of individuals will be higher than the final cohort)
outName = f'{proj}_tmp_quality_assurance'.lower()

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
tmp1.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

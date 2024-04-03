# Databricks notebook source
# MAGIC %md # CCU018_01-D13-outcomes_at_birth
# MAGIC  
# MAGIC **Description** This notebook creates the outcomes at birth for the CCU018_01 project, which includes variables for a multiple gestation and stillbirth (each combining three different sources of information), pre-term (combining two different sources of information), resuscitation method, small for gestational age, and induction of labour.
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** out_cohort, tmp_cohort_del, out_codelist, cur_hes_apc_long, cur_hes_apc_oper_long, in_gdppr, tmp_hes_apc_mat
# MAGIC
# MAGIC **Data output** ccu018_01_out_outcomes_at_birth

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

# MAGIC %md # 0 Parameters

# COMMAND ----------

# MAGIC %run "./CCU018_01-D01-parameters"

# COMMAND ----------

out_prefix = 'birth_'

# COMMAND ----------

# MAGIC %md # 1 Data

# COMMAND ----------

# spark.sql(f"""REFRESH TABLE {path_out_cohort}""")
codelist          = spark.table(path_out_codelist)
# hes_apc_mat_all   = get_archive_table('hes_apc_mat') # all years
hes_apc_mat       = spark.table(path_tmp_hes_apc_mat) # 1920, 2021, 2122
gdppr             = spark.table(path_in_gdppr) 
hes_apc           = spark.table(path_in_hes_apc) # cohort-specific
hes_apc_long      = spark.table(path_cur_hes_apc_long) 
hes_apc_oper_long = spark.table(path_cur_hes_apc_oper_long)
cohort            = spark.table(path_out_cohort)
delivery_record   = spark.table(path_tmp_cohort_del)
# covariates      = spark.table(path_out_covariates)

# COMMAND ----------

# restrict delivery_record to the inclusion / exclusion cohort
# merge with checks
delivery_record = merge(delivery_record, cohort.select('PERSON_ID'), ['PERSON_ID'], validate='1:1', assert_results=['both', 'left_only'], keep_results=['both'], indicator=0); print()

# COMMAND ----------

# MAGIC %md # 2 multi_gest

# COMMAND ----------

# MAGIC %md ## 2.1 delvar

# COMMAND ----------

# delvar = delivery record variables

# create
tmp1 = delivery_record\
  .select('PERSON_ID', 'NUMBABY', 'NUMTAILB')\
  .withColumn('NUMBABY_int', f.col('NUMBABY').cast(t.IntegerType()))\
  .withColumn('NUMTAILB_int', f.col('NUMTAILB').cast(t.IntegerType()))\
  .withColumn('out_birth_multi_gest_delvar',\
    f.when((f.col('NUMBABY_int') > 1) | (f.col('NUMTAILB_int') > 1), 1)\
    .when((f.col('NUMBABY_int') == 1) | (f.col('NUMTAILB_int') == 1), 0)\
  )\
  .withColumn('_concat',\
    f.concat(\
      f.when(f.col('NUMBABY_int').isNull(), f.lit('_')).otherwise(f.col('NUMBABY_int'))\
      , f.when(f.col('NUMTAILB_int').isNull(), f.lit('_')).otherwise(f.col('NUMTAILB_int'))\
    )\
  )

# checks
tmpt = tab(tmp1, 'NUMBABY', 'NUMBABY_int', var2_unstyled=1); print()
tmpt = tab(tmp1, 'NUMTAILB', 'NUMTAILB_int', var2_unstyled=1); print()
tmpt = tab(tmp1, 'NUMBABY_int', 'NUMTAILB_int', var2_unstyled=1); print()
# print(114755+644563+2); print()
tmpt = tab(tmp1, '_concat', 'out_birth_multi_gest_delvar', var2_unstyled=1); print()

# tidy
multi_gest_delvar = tmp1\
  .select('PERSON_ID', 'out_birth_multi_gest_delvar')

# check
count_var(multi_gest_delvar, 'PERSON_ID'); print()
print(multi_gest_delvar.limit(10).toPandas().to_string()); print()

# cache
multi_gest_delvar.cache()
print(f'{multi_gest_delvar.count():,}')

# COMMAND ----------

# MAGIC %md ## 2.2 delcode

# COMMAND ----------

# delcode = delivery record codes

# ==============================================================================
# %md ### 2.2.1 Data
# ==============================================================================
# ------------------------------------------------------------------------------
# delivery_record
# ------------------------------------------------------------------------------
print('delivery_record'); print()
_delivery_record = delivery_record\
  .select('PERSON_ID', 'EPIKEY')\
  .withColumn('CENSOR_DATE_START', f.lit(None).cast(t.StringType()))\
  .withColumn('CENSOR_DATE_END', f.lit(None).cast(t.StringType()))

# check
count_var(_delivery_record, 'PERSON_ID'); print()
count_var(_delivery_record, 'EPIKEY'); print()


# ------------------------------------------------------------------------------
# hes_apc
# ------------------------------------------------------------------------------
print('hes_apc'); print()
# .where(f.col('DIAG_POSITION') == 1)\ # removed from below - considering any position
_hes_apc = hes_apc_long\
  .select(['PERSON_ID', 'EPIKEY', 'EPISTART', 'CODE'])\
  .withColumnRenamed('PERSON_ID', 'PERSON_ID_hes_apc')\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc, 'PERSON_ID_hes_apc'); print()
count_var(_hes_apc, 'EPIKEY'); print()

# merge
_hes_apc = merge(_delivery_record, _hes_apc, ['EPIKEY']); print()
assert _hes_apc.select('_merge').where(f.col('_merge') == 'left_only').count() == 0
_hes_apc = _hes_apc\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check id
_hes_apc = _hes_apc\
  .withColumn('_nse', udf_null_safe_equality('PERSON_ID', 'PERSON_ID_hes_apc').cast(t.IntegerType()))
tmpt = tab(_hes_apc, '_nse'); print()
assert _hes_apc.select('_nse').where(f.col('_nse') == 0).count() == 0

# check
count_var(_hes_apc, 'PERSON_ID'); print()
count_var(_hes_apc, 'EPIKEY'); print()

# tidy
_hes_apc = _hes_apc\
  .drop('EPIKEY', 'PERSON_ID_hes_apc', '_nse')


# ------------------------------------------------------------------------------
# hes_apc_oper
# ------------------------------------------------------------------------------
print('hes_apc_oper'); print()
_hes_apc_oper = hes_apc_oper_long\
  .select(['PERSON_ID', 'EPIKEY', 'EPISTART', 'CODE'])\
  .withColumnRenamed('PERSON_ID', 'PERSON_ID_hes_apc_oper')\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc_oper, 'PERSON_ID_hes_apc_oper'); print()
count_var(_hes_apc_oper, 'EPIKEY'); print()

# merge
_hes_apc_oper = merge(_delivery_record, _hes_apc_oper, ['EPIKEY']); print()
# assert _hes_apc_oper.select('_merge').where(f.col('_merge') == 'left_only').count() == 0
# some individuals do not have operation codes on record, which, from curated_tables, means that they are excluded from hes_apc_oper_long
_hes_apc_oper = _hes_apc_oper\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check id
_hes_apc_oper = _hes_apc_oper\
  .withColumn('_nse', udf_null_safe_equality('PERSON_ID', 'PERSON_ID_hes_apc_oper').cast(t.IntegerType()))
tmpt = tab(_hes_apc_oper, '_nse'); print()
assert _hes_apc_oper.select('_nse').where(f.col('_nse') == 0).count() == 0

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()
count_var(_hes_apc_oper, 'EPIKEY'); print()

# tidy
_hes_apc_oper = _hes_apc_oper\
  .drop('EPIKEY', 'PERSON_ID_hes_apc_oper', '_nse')


# ------------------------------------------------------------------------------
# cache
# ------------------------------------------------------------------------------
_hes_apc.cache()
print(f'{_hes_apc.count():,}')
_hes_apc_oper.cache()
print(f'{_hes_apc_oper.count():,}')

# check 
print(_hes_apc.limit(10).toPandas().to_string()); print()
print(_hes_apc_oper.limit(10).toPandas().to_string()); print()

# COMMAND ----------

# ==============================================================================
# %md ### 2.2.2 Codelist
# ==============================================================================
# outcomes codelist
codelist_out = codelist\
  .where((f.col('name').isin(['multi_gest']))\
    & ((f.col('covariate_only').isNull()) | (f.col('covariate_only') != 1))\
    & ((f.col('code_type').isNull()) | (f.col('code_type') == '') | (f.col('code_type') == 1))\
  )

# check
tmpt = tab(codelist_out, 'name' , 'terminology', var2_unstyled=1); print()

# snomed
codelist_out_snomed = codelist_out\
  .where(f.col('terminology') == 'SNOMED')

# icd10
codelist_out_icd10 = codelist_out\
  .where(f.col('terminology') == 'ICD10')

# opcs4
codelist_out_opcs4 = codelist_out\
  .where(f.col('terminology') == 'OPCS4')

# check
tmpt = tab(codelist_out_snomed, 'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_icd10,  'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_opcs4,  'name' , 'terminology', var2_unstyled=1); print()

# COMMAND ----------

# ==============================================================================
# %md ### 2.2.3 Create
# ==============================================================================
# hes_apc
_tmp_codelist_out_icd10 = codelist_out_icd10\
  .select(['code', 'name'])
_out_hes_apc = _hes_apc\
  .join(_tmp_codelist_out_icd10, on='code', how='inner')\
  .withColumn('source', f.lit('hes_apc'))

# hes_apc_oper
_tmp_codelist_out_opcs4 = codelist_out_opcs4\
  .select(['code', 'name'])  
_out_hes_apc_oper = _hes_apc_oper\
  .join(_tmp_codelist_out_opcs4, on='code', how='inner')\
  .withColumn('source', f.lit('hes_apc_oper'))

# append
_out_all = _out_hes_apc\
  .unionByName(_out_hes_apc_oper)\
  .withColumn('sourcen',\
    f.when(f.col('source') == 'hes_apc', 1)\
    .when(f.col('source') == 'hes_apc_oper', 2)\
  )

# first event of each name
_win = Window\
  .partitionBy(['PERSON_ID', 'name'])\
  .orderBy('DATE', 'sourcen', 'code')
_out_1st = _out_all\
  .withColumn('_rownum', f.row_number().over(_win))\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'DATE', 'name', 'source')\
  .orderBy('PERSON_ID', 'DATE', 'name', 'source')

# join codelist names before reshape to ensure all covariates are created (when no code matches are found)
_tmp_codelist_out = _tmp_codelist_out_icd10\
  .unionByName(_tmp_codelist_out_opcs4)\
  .select('name')\
  .distinct()              
               
# reshape long to wide
_out_1st_wide = _out_1st\
  .join(_tmp_codelist_out, on='name', how='outer')\
  .withColumn('name', f.concat(f.lit(f'out_{out_prefix}'), f.lower(f.col('name'))))\
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')\
  .pivot('name')\
  .agg(f.first('DATE'))\
  .where(f.col('PERSON_ID').isNotNull())\
  .orderBy('PERSON_ID')  

# flag and date
vlist = []
for v in [col for col in list(_out_1st_wide.columns) if re.match(f'^out_{out_prefix}', col)]:
  print(v)
  _out_1st_wide = _out_1st_wide\
    .withColumnRenamed(v, v + '_date')\
    .withColumn(v + '_flag', f.when(f.col(v + '_date').isNotNull(), 1))
  vlist = vlist + [v + '_flag', v + '_date']
_out_1st_wide = _out_1st_wide\
  .select(['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'] + vlist)

# check
count_var(_out_1st_wide, 'PERSON_ID'); print()


# ==============================================================================
# %md ### 2.2.5 Finalise
# ==============================================================================
multi_gest_delcode = _out_1st_wide\
  .withColumn('out_birth_multi_gest_delcode',\
    f.when(f.col('out_birth_multi_gest_flag').isNotNull(), 1)\
  )\
  .select('PERSON_ID', 'out_birth_multi_gest_delcode')

# check
count_var(multi_gest_delcode, 'PERSON_ID'); print()
print(multi_gest_delcode.limit(10).toPandas().to_string()); print()

# cache
multi_gest_delcode.cache()
print(f'{multi_gest_delcode.count():,}')

# COMMAND ----------

# dict_in = {
#     'hes_apc':      ['_hes_apc',      'codelist_out_icd10',  1]
#   , 'hes_apc_oper': ['_hes_apc_oper', 'codelist_out_opcs4',  2]
# }
# name_prefix = f'out_{out_prefix}'

# dict_codematch, _out_1st, _out_1st_wide = code_match(dict_in, name_prefix)

# old = _out_1st_wide
# new = codematch_1st_wide
# file1, file2, file3, file3_differences = compare_files(old, new, ['PERSON_ID'], warningError=0)

# COMMAND ----------

# ==============================================================================
# %md ### 2.2.4 Check
# ==============================================================================
display(_out_1st_wide)

# COMMAND ----------

# dd = {"gdppr": [_out_gdppr, codelist_out_gdppr]}
# df, codelist = dd['gdppr']

# ------------------------------------------------------------------------------------
# summarise by name
# ------------------------------------------------------------------------------------
_n_list = []
_out_summ_name = []
for i, source in enumerate(['all', 'hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  _codelist = ''
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name')\
    .agg(\
      f.count(f.lit(1)).alias(f'n_{source}')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id_{source}')\
    )
  _n_list = _n_list + [f'n_{source}', f'n_id_{source}']
  
  if(source != 'all'):
    _tmpc = globals()[_codelist]\
      .select('name')\
      .distinct()
    _tmp = _tmp\
      .join(_tmpc, on='name', how='outer')    
  
  if(i == 0): _out_summ_name = _tmp
  else:
    _out_summ_name = _out_summ_name\
      .join(_tmp, on='name', how='outer')
       
_out_summ_name = _out_summ_name\
  .na.fill(0, subset=_n_list)\
  .orderBy('name')


# ------------------------------------------------------------------------------------ 
# summarise by name, code
# ------------------------------------------------------------------------------------ 
_out_summ_name_code = []
for i, source in enumerate(['hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'    
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name', 'code')\
    .agg(\
      f.count(f.lit(1)).alias(f'n')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id')\
    )
  _tmpc = globals()[_codelist]\
    .select('name', 'code', 'terminology', 'term')\
    .distinct()
  _tmp = _tmp\
    .join(_tmpc, on=['name', 'code'], how='outer')\
    .withColumn('source', f.lit(f'{source}'))    

  if(i == 0): _out_summ_name_code = _tmp
  else:
    _out_summ_name_code = _out_summ_name_code\
      .unionByName(_tmp)
       
_out_summ_name_code = _out_summ_name_code\
  .na.fill(0, subset=['n', 'n_id'])\
  .orderBy('name', 'source', 'code')

# COMMAND ----------

# check codelist match summary by name
display(_out_summ_name)

# COMMAND ----------

# check codelist match summary by name and code
display(_out_summ_name_code)

# COMMAND ----------

# MAGIC %md ## 2.3 gencode

# COMMAND ----------

# gencode = general record codes

# ==============================================================================
# %md ### 2.3.1 Data
# ==============================================================================

# ------------------------------------------------------------------------------
# individual_censor_dates
# ------------------------------------------------------------------------------
# during pregnacy and up to 28 days after
individual_censor_dates = cohort\
  .select('PERSON_ID', 'preg_start_date', 'delivery_date')\
  .withColumn('CENSOR_DATE_END', f.date_add(f.col('delivery_date'), 28))\
  .withColumnRenamed('preg_start_date', 'CENSOR_DATE_START')\

# check
print(individual_censor_dates.limit(10).toPandas().to_string()); print()

individual_censor_dates = individual_censor_dates\
  .drop('delivery_date')

# check
count_var(individual_censor_dates, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# gdppr
# ------------------------------------------------------------------------------
print('gdppr'); print()
_gdppr = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'CODE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')

# check
count_var(_gdppr, 'PERSON_ID'); print()

_gdppr = _gdppr\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_gdppr, 'PERSON_ID'); print()

_gdppr = _gdppr\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_gdppr, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# hes_apc
# ------------------------------------------------------------------------------
print('hes_apc'); print()
# .where(f.col('DIAG_POSITION') == 1)\ # removed from below - considering any position
_hes_apc = hes_apc_long\
  .select(['PERSON_ID', 'EPISTART', 'CODE'])\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

_hes_apc = _hes_apc\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

_hes_apc = _hes_apc\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_hes_apc, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# hes_apc_oper
# ------------------------------------------------------------------------------
print('hes_apc_oper'); print()
_hes_apc_oper = hes_apc_oper_long\
  .select(['PERSON_ID', 'EPISTART', 'CODE'])\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

_hes_apc_oper = _hes_apc_oper\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

_hes_apc_oper = _hes_apc_oper\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# cache
# ------------------------------------------------------------------------------
print('cache'); print()
_gdppr.cache()
print(f'{_gdppr.count():,}')
_hes_apc.cache()
print(f'{_hes_apc.count():,}')
_hes_apc_oper.cache()
print(f'{_hes_apc_oper.count():,}')

# COMMAND ----------

# ==============================================================================
# %md ### 2.3.2 Codelist
# ==============================================================================
# outcomes codelist
codelist_out = codelist\
  .where((f.col('name').isin(['multi_gest']))\
    & ((f.col('covariate_only').isNull()) | (f.col('covariate_only') != 1))\
    & ((f.col('code_type').isNull()) | (f.col('code_type') == '') | (f.col('code_type') == 1))\
  )

# check
tmpt = tab(codelist_out, 'name' , 'terminology', var2_unstyled=1); print()

# snomed
codelist_out_snomed = codelist_out\
  .where(f.col('terminology') == 'SNOMED')

# icd10
codelist_out_icd10 = codelist_out\
  .where(f.col('terminology') == 'ICD10')

# opcs4
codelist_out_opcs4 = codelist_out\
  .where(f.col('terminology') == 'OPCS4')

# check
tmpt = tab(codelist_out_snomed, 'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_icd10,  'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_opcs4,  'name' , 'terminology', var2_unstyled=1); print()

# COMMAND ----------

# ==============================================================================
# %md ### 2.3.3 Create
# ==============================================================================
# gdppr
_tmp_codelist_out_snomed = codelist_out_snomed\
  .select(['code', 'name'])
_out_gdppr = _gdppr\
  .join(_tmp_codelist_out_snomed, on='code', how='inner')\
  .withColumn('source', f.lit('gdppr'))

# hes_apc
_tmp_codelist_out_icd10 = codelist_out_icd10\
  .select(['code', 'name'])
_out_hes_apc = _hes_apc\
  .join(_tmp_codelist_out_icd10, on='code', how='inner')\
  .withColumn('source', f.lit('hes'))
  # note: intentionally setting source == 'hes' (not 'hes_apc') for later grouping

# hes_apc_oper
_tmp_codelist_out_opcs4 = codelist_out_opcs4\
  .select(['code', 'name'])  
_out_hes_apc_oper = _hes_apc_oper\
  .join(_tmp_codelist_out_opcs4, on='code', how='inner')\
  .withColumn('source', f.lit('hes'))
  # note: intentionally setting source == 'hes' (not 'hes_apc_oper') for later grouping

# append
_out_all = _out_gdppr\
  .unionByName(_out_hes_apc)\
  .unionByName(_out_hes_apc_oper)

# first event of each name AND SOURCE
_win = Window\
  .partitionBy(['PERSON_ID', 'name', 'source'])\
  .orderBy('DATE', 'code')
_out_1st = _out_all\
  .withColumn('_rownum', f.row_number().over(_win))\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'DATE', 'name', 'source')\
  .orderBy('PERSON_ID', 'DATE', 'name', 'source')

# note: ignore ties for source for now...

# join codelist names before reshape to ensure all covariates are created (when no code matches are found)
_tmp_codelist_out = _tmp_codelist_out_snomed.withColumn('source', f.lit('gdppr'))\
  .unionByName(_tmp_codelist_out_icd10.withColumn('source', f.lit('hes')))\
  .select('name', 'source')\
  .distinct()

# reshape long to wide
_out_1st_wide = _out_1st\
  .join(_tmp_codelist_out, on=['name', 'source'], how='outer')\
  .withColumn('name', f.concat(f.lit(f'out_{out_prefix}'), f.lower(f.col('name')), f.lit('_'), f.lower(f.col('source'))))\
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')\
  .pivot('name')\
  .agg(f.first('DATE'))\
  .where(f.col('PERSON_ID').isNotNull())\
  .orderBy('PERSON_ID')  

# flag and date
vlist = []
for v in [col for col in list(_out_1st_wide.columns) if re.match(f'^out_{out_prefix}', col)]:
  print(v)
  _out_1st_wide = _out_1st_wide\
    .withColumnRenamed(v, v + '_date')\
    .withColumn(v + '_flag', f.when(f.col(v + '_date').isNotNull(), 1))
  vlist = vlist + [v + '_flag', v + '_date']
_out_1st_wide = _out_1st_wide\
  .select(['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'] + vlist)

# check
count_var(_out_1st_wide, 'PERSON_ID')


# ==============================================================================
# %md ### 2.3.5 Finalise
# ==============================================================================
multi_gest_gencode = _out_1st_wide\
  .withColumn('out_birth_multi_gest_gencode_gdppr',\
    f.when(f.col('out_birth_multi_gest_gdppr_flag').isNotNull(), 1)\
  )\
  .withColumn('out_birth_multi_gest_gencode_hes',\
    f.when(f.col('out_birth_multi_gest_hes_flag').isNotNull(), 1)\
  )\
  .withColumnRenamed('out_birth_multi_gest_gdppr_date', 'out_birth_multi_gest_gencode_gdppr_date')\
  .withColumnRenamed('out_birth_multi_gest_hes_date', 'out_birth_multi_gest_gencode_hes_date')\
  .select('PERSON_ID', 'out_birth_multi_gest_gencode_gdppr', 'out_birth_multi_gest_gencode_gdppr_date', 'out_birth_multi_gest_gencode_hes', 'out_birth_multi_gest_gencode_hes_date')

# check
count_var(multi_gest_gencode, 'PERSON_ID'); print()
print(multi_gest_gencode.limit(10).toPandas().to_string()); print()

# cache
multi_gest_gencode.cache()
print(f'{multi_gest_gencode.count():,}')

# COMMAND ----------

# ==============================================================================
# %md ### 2.3.4 Check
# ==============================================================================
display(_out_1st)

# COMMAND ----------

display(_out_1st_wide)

# COMMAND ----------

# plot
_tmp = _out_1st\
  .withColumn('diff', f.datediff(f.col('DATE'), f.col('CENSOR_DATE_START'))/365.25)
_tmpp = _tmp\
  .toPandas()

# toggle sharey=True
rows_of_5 = np.ceil(len(_tmpp['name'].drop_duplicates())/5).astype(int)
fig, axes = plt.subplots(rows_of_5, 5, figsize=(13,2*rows_of_5), sharex=True) # , sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 2)
names = ['hes', 'gdppr']  
  
vlist = list(_tmpp[['name']].drop_duplicates().sort_values('name')['name']) 
for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, v)
  tmp2d1 = _tmpp[(_tmpp[f'diff'] > -30) & (_tmpp[f'name'] == v)]
  s1 = list(tmp2d1[tmp2d1[f'source'] == 'hes'][f'diff'])
  s2 = list(tmp2d1[tmp2d1[f'source'] == 'gdppr'][f'diff'])
  ax.hist([s1, s2], bins = list(np.linspace(0,1,100)), stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'{v}')
  ax.xaxis.set_tick_params(labelbottom=True)
  if(i==0): ax.legend(loc='upper right')

plt.tight_layout();
display(fig)

# COMMAND ----------

# dd = {"gdppr": [_out_gdppr, codelist_out_gdppr]}
# df, codelist = dd['gdppr']

# ------------------------------------------------------------------------------------
# summarise by name
# ------------------------------------------------------------------------------------
_n_list = []
_out_summ_name = []
for i, source in enumerate(['all', 'gdppr', 'hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  _codelist = ''
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name')\
    .agg(\
      f.count(f.lit(1)).alias(f'n_{source}')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id_{source}')\
    )
  _n_list = _n_list + [f'n_{source}', f'n_id_{source}']
  
  if(source != 'all'):
    _tmpc = globals()[_codelist]\
      .select('name')\
      .distinct()
    _tmp = _tmp\
      .join(_tmpc, on='name', how='outer')    
  
  if(i == 0): _out_summ_name = _tmp
  else:
    _out_summ_name = _out_summ_name\
      .join(_tmp, on='name', how='outer')
       
_out_summ_name = _out_summ_name\
  .na.fill(0, subset=_n_list)\
  .orderBy('name')


# ------------------------------------------------------------------------------------ 
# summarise by name, code
# ------------------------------------------------------------------------------------ 
_out_summ_name_code = []
for i, source in enumerate(['gdppr', 'hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'    
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name', 'code')\
    .agg(\
      f.count(f.lit(1)).alias(f'n')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id')\
    )
  _tmpc = globals()[_codelist]\
    .select('name', 'code', 'terminology', 'term')\
    .distinct()
  _tmp = _tmp\
    .join(_tmpc, on=['name', 'code'], how='outer')\
    .withColumn('source', f.lit(f'{source}'))    

  if(i == 0): _out_summ_name_code = _tmp
  else:
    _out_summ_name_code = _out_summ_name_code\
      .unionByName(_tmp)
       
_out_summ_name_code = _out_summ_name_code\
  .na.fill(0, subset=['n', 'n_id'])\
  .orderBy('name', 'source', 'code')

# COMMAND ----------

display(_out_summ_name)

# COMMAND ----------

display(_out_summ_name_code)

# COMMAND ----------

# MAGIC %md ## 2.4 max

# COMMAND ----------

# check
count_var(multi_gest_delvar, 'PERSON_ID'); print()
count_var(multi_gest_delcode, 'PERSON_ID'); print()
count_var(multi_gest_gencode, 'PERSON_ID'); print()

# check
tmpt = tab(multi_gest_delvar, 'out_birth_multi_gest_delvar'); print()
tmpt = tab(multi_gest_delcode, 'out_birth_multi_gest_delcode'); print()
tmpt = tab(multi_gest_gencode, 'out_birth_multi_gest_gencode_hes'); print()
tmpt = tab(multi_gest_gencode, 'out_birth_multi_gest_gencode_gdppr'); print()

# join
multi_gest = multi_gest_delvar\
  .join(multi_gest_delcode, on=['PERSON_ID'], how='outer')\
  .join(multi_gest_gencode, on=['PERSON_ID'], how='outer')

# check
count_var(multi_gest, 'PERSON_ID'); print()
print(len(multi_gest.columns)); print()
print(pd.DataFrame({f'_cols': multi_gest.columns}).to_string()); print()

# max
multi_gest = multi_gest\
  .withColumn('out_birth_multi_gest_max',\
    f.greatest(\
        'out_birth_multi_gest_delvar'
      , 'out_birth_multi_gest_delcode'
      , 'out_birth_multi_gest_gencode_hes'
      , 'out_birth_multi_gest_gencode_gdppr'
    )\
  )

# check
tmpt = tab(multi_gest, 'out_birth_multi_gest_max'); print()
tmp1 = multi_gest\
  .withColumn('vchg',\
    f.concat(\
        f.when(f.col('out_birth_multi_gest_delvar').isNull(), f.lit('_')).otherwise(f.col('out_birth_multi_gest_delvar'))
      , f.when(f.col('out_birth_multi_gest_delcode').isNull(), f.lit('_')).otherwise(f.col('out_birth_multi_gest_delcode'))
      , f.when(f.col('out_birth_multi_gest_gencode_hes').isNull(), f.lit('_')).otherwise(f.col('out_birth_multi_gest_gencode_hes'))
      , f.when(f.col('out_birth_multi_gest_gencode_gdppr' ).isNull(), f.lit('_')).otherwise(f.col('out_birth_multi_gest_gencode_gdppr'))            
    )\
  )
tmpt = tab(tmp1, 'vchg', 'out_birth_multi_gest_max', var2_unstyled=1); print()

# check
count_var(multi_gest, 'PERSON_ID'); print()
print(multi_gest.limit(10).toPandas().to_string()); print()

# cache
multi_gest.cache()
print(f'{multi_gest.count():,}')

# uncache components

# COMMAND ----------

# check final
display(multi_gest)

# COMMAND ----------

# MAGIC %md # 3 stillbirth

# COMMAND ----------

# ------------------------------------------------------------------------------
# individual_censor_dates
# ------------------------------------------------------------------------------
# after 24 weeks of gestation during pregnancy
individual_censor_dates = cohort\
  .select('PERSON_ID', 'preg_start_date', 'delivery_date', 'preg_start_date_corrected')\
  .withColumn('CENSOR_DATE_END', f.date_add(f.col('delivery_date'), 28))\
  .withColumn('GESTAT_tmp', f.datediff(f.col('delivery_date'), f.col('preg_start_date'))/7)\
  .withColumn('GESTAT_tmp_ge_24', f.when(f.col('GESTAT_tmp') >= 24, 1).otherwise(0))\
  .withColumn('CENSOR_DATE_START', f.date_add(f.col('preg_start_date'), (24*7) - 1 ))

# check
# tmpt = tab(individual_censor_dates, 'GESTAT_tmp', 'preg_start_date_corrected', var2_unstyled=1); print()
# tmpt = tab(individual_censor_dates, 'GESTAT_tmp', 'GESTAT_tmp_ge_24', var2_unstyled=1); print()
tmpt = tab(individual_censor_dates, 'GESTAT_tmp_ge_24'); print()
count_var(individual_censor_dates, 'PERSON_ID'); print()
# print(individual_censor_dates.limit(10).toPandas().to_string()); print()

# tidy
individual_censor_dates = individual_censor_dates\
  .where(f.col('GESTAT_tmp_ge_24') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START')

# check
count_var(individual_censor_dates, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# delivery_record
# ------------------------------------------------------------------------------
print('delivery_record'); print()
_delivery_record = delivery_record\
  .select('PERSON_ID', 'EPIKEY')

# check
count_var(_delivery_record, 'PERSON_ID'); print()
count_var(_delivery_record, 'EPIKEY'); print()

# merge
_delivery_record_gt_24w = merge(individual_censor_dates, _delivery_record, ['PERSON_ID']); print()
assert _delivery_record_gt_24w.select('_merge').where(f.col('_merge') == 'left_only').count() == 0
_delivery_record_gt_24w = _delivery_record_gt_24w\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check
count_var(_delivery_record_gt_24w, 'PERSON_ID'); print()
count_var(_delivery_record_gt_24w, 'EPIKEY'); print()

# COMMAND ----------

# MAGIC %md ### 3.1 delvar

# COMMAND ----------

# ------------------------------------------------------------------------------
# hes_apc_mat
# ------------------------------------------------------------------------------
vlist = [col for col in hes_apc_mat.columns if re.match('^BIRSTAT_[1-9]$', col)] 
_hes_apc_mat = hes_apc_mat\
  .select(['EPIKEY', 'NUMTAILB'] + vlist)

# check
count_var(_hes_apc_mat, 'EPIKEY'); print()
print(len(_hes_apc_mat.columns)); print()
print(pd.DataFrame({f'_cols': _hes_apc_mat.columns}).to_string()); print()

# merge
_hes_apc_mat = merge(_delivery_record_gt_24w, _hes_apc_mat, ['EPIKEY']); print()
assert _hes_apc_mat.select('_merge').where(f.col('_merge') == 'left_only').count() == 0
_hes_apc_mat = _hes_apc_mat\
  .where(f.col('_merge') == 'both')\
  .drop('_merge', 'EPIKEY')


# ------------------------------------------------------------------------------
# birstat
# ------------------------------------------------------------------------------
# reshape wide to long
# filter to rows with potential information
# indicators for stillbirth, live, unknown, null
# collapse by id, taking max
# concatenate max
# create stillbirth, care taken with unknown and null
tmp1 = reshape_wide_to_long_multi(_hes_apc_mat, i=['PERSON_ID', 'NUMTAILB'], j='POSITION', stubnames=['BIRSTAT_'])\
  .withColumn('POSITION', f.col('POSITION').cast(t.IntegerType()))\
  .withColumnRenamed('BIRSTAT_', 'BIRSTAT_precast')\
  .withColumn('BIRSTAT_', f.col('BIRSTAT_precast').cast(t.IntegerType()))\

# check
tmpt = tab(tmp1, 'BIRSTAT_', 'BIRSTAT_precast', var2_unstyled=1); print()

tmp1 = tmp1\
  .where(\
    (f.col('NUMTAILB').isNull())\
    | (f.col('POSITION') <= f.col('NUMTAILB'))\
  )\
  .withColumn('ind_234',  f.when(f.col('BIRSTAT_').isin([2,3,4]), 1))\
  .withColumn('ind_1',    f.when(f.col('BIRSTAT_').isin([1]), 1))\
  .withColumn('ind_9',    f.when(f.col('BIRSTAT_').isin([9]), 1))\
  .withColumn('ind_null', f.when(f.col('BIRSTAT_').isNull(), 1))\
  .groupBy('PERSON_ID', 'NUMTAILB')\
  .agg(\
    f.max(f.col('ind_234')).alias('ind_234_max')\
    , f.max(f.col('ind_1')).alias('ind_1_max')\
    , f.max(f.col('ind_9')).alias('ind_9_max')\
    , f.max(f.col('ind_null')).alias('ind_null_max')\
  )\
  .withColumn('s19n',\
    f.concat(\
        f.when(f.col('ind_234_max').isNull(), f.lit('_')).otherwise(f.col('ind_234_max'))\
      , f.when(f.col('ind_1_max').isNull(), f.lit('_')).otherwise(f.col('ind_1_max'))\
      , f.when(f.col('ind_9_max').isNull(), f.lit('_')).otherwise(f.col('ind_9_max'))\
      , f.when(f.col('ind_null_max' ).isNull(), f.lit('_')).otherwise(f.col('ind_null_max'))\
    )\
  )\
  .withColumn('_stillbirth',\
    f.when(f.col('ind_234_max') == 1, 1)\
    .when((f.col('ind_9_max') == 1) | (f.col('ind_null_max') == 1), None)\
    .when(f.col('ind_1_max') == 1, 0)\
  )

# check
count_var(tmp1, 'PERSON_ID'); print()
tmpt = tab(tmp1, 's19n', '_stillbirth', var2_unstyled=1); print()

# tidy
stillbirth_delvar = tmp1\
  .select('PERSON_ID', '_stillbirth')\
  .withColumnRenamed('_stillbirth', 'out_birth_stillbirth_delvar')

# check
tmpt = tab(stillbirth_delvar, 'out_birth_stillbirth_delvar'); print()
print(stillbirth_delvar.limit(10).toPandas().to_string()); print()

# cache
stillbirth_delvar.cache()
print(f'{stillbirth_delvar.count():,}')

# COMMAND ----------

# MAGIC %md ### 3.2 delcode

# COMMAND ----------

# delcode = delivery record codes

# ==============================================================================
# %md ### 3.2.1 Data
# ==============================================================================

# ------------------------------------------------------------------------------
# individual_censor_dates
# ------------------------------------------------------------------------------
# see above


# ------------------------------------------------------------------------------
# delivery_record
# ------------------------------------------------------------------------------
# see above


# ------------------------------------------------------------------------------
# hes_apc
# ------------------------------------------------------------------------------
print('hes_apc'); print()
# .where(f.col('DIAG_POSITION') == 1)\ # removed from below - considering any position
_hes_apc = hes_apc_long\
  .select(['PERSON_ID', 'EPIKEY', 'EPISTART', 'CODE'])\
  .withColumnRenamed('PERSON_ID', 'PERSON_ID_hes_apc')\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc, 'PERSON_ID_hes_apc'); print()
count_var(_hes_apc, 'EPIKEY'); print()

# merge
_hes_apc = merge(_delivery_record_gt_24w, _hes_apc, ['EPIKEY']); print()
assert _hes_apc.select('_merge').where(f.col('_merge') == 'left_only').count() == 0
_hes_apc = _hes_apc\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check id
_hes_apc = _hes_apc\
  .withColumn('_nse', udf_null_safe_equality('PERSON_ID', 'PERSON_ID_hes_apc').cast(t.IntegerType()))
tmpt = tab(_hes_apc, '_nse'); print()
assert _hes_apc.select('_nse').where(f.col('_nse') == 0).count() == 0

# check
count_var(_hes_apc, 'PERSON_ID'); print()
count_var(_hes_apc, 'EPIKEY'); print()

# tidy
_hes_apc = _hes_apc\
  .drop('EPIKEY', 'PERSON_ID_hes_apc', '_nse')

# >= 24 weeks (-1 above to allow > below) # for consistency with gencode
_hes_apc = _hes_apc\
  .where(f.col('DATE') > f.col('CENSOR_DATE_START'))

# check
count_var(_hes_apc, 'PERSON_ID'); print()
print(_hes_apc.limit(10).toPandas().to_string()); print()


# ------------------------------------------------------------------------------
# hes_apc_oper
# ------------------------------------------------------------------------------
print('hes_apc_oper'); print()
_hes_apc_oper = hes_apc_oper_long\
  .select(['PERSON_ID', 'EPIKEY', 'EPISTART', 'CODE'])\
  .withColumnRenamed('PERSON_ID', 'PERSON_ID_hes_apc_oper')\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc_oper, 'PERSON_ID_hes_apc_oper'); print()
count_var(_hes_apc_oper, 'EPIKEY'); print()

# merge
_hes_apc_oper = merge(_delivery_record_gt_24w, _hes_apc_oper, ['EPIKEY']); print()
# assert _hes_apc_oper.select('_merge').where(f.col('_merge') == 'left_only').count() == 0
# some individuals do not have operation codes on record, which, from curated_tables, means that they are excluded from hes_apc_oper_long
_hes_apc_oper = _hes_apc_oper\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check id
_hes_apc_oper = _hes_apc_oper\
  .withColumn('_nse', udf_null_safe_equality('PERSON_ID', 'PERSON_ID_hes_apc_oper').cast(t.IntegerType()))
tmpt = tab(_hes_apc_oper, '_nse'); print()
assert _hes_apc_oper.select('_nse').where(f.col('_nse') == 0).count() == 0

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()
count_var(_hes_apc_oper, 'EPIKEY'); print()

# tidy
_hes_apc_oper = _hes_apc_oper\
  .drop('EPIKEY', 'PERSON_ID_hes_apc_oper', '_nse')

# >= 24 weeks (-1 above to allow > below) # for consistency with gencode
_hes_apc_oper = _hes_apc_oper\
  .where(f.col('DATE') > f.col('CENSOR_DATE_START'))

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()
print(_hes_apc_oper.limit(10).toPandas().to_string()); print()


# ------------------------------------------------------------------------------
# cache
# ------------------------------------------------------------------------------
_hes_apc.cache()
print(f'{_hes_apc.count():,}')
_hes_apc_oper.cache()
print(f'{_hes_apc_oper.count():,}')

# COMMAND ----------

# ==============================================================================
# %md ### 3.2.2 Codelist
# ==============================================================================
# outcomes codelist
codelist_out = codelist\
  .where((f.col('name').isin(['stillbirth']))\
    & ((f.col('covariate_only').isNull()) | (f.col('covariate_only') != 1))\
    & ((f.col('code_type').isNull()) | (f.col('code_type') == '') | (f.col('code_type') == 1))\
  )

# check
tmpt = tab(codelist_out, 'name' , 'terminology', var2_unstyled=1); print()

# snomed
codelist_out_snomed = codelist_out\
  .where(f.col('terminology') == 'SNOMED')

# icd10
codelist_out_icd10 = codelist_out\
  .where(f.col('terminology') == 'ICD10')

# opcs4
codelist_out_opcs4 = codelist_out\
  .where(f.col('terminology') == 'OPCS4')

# check
tmpt = tab(codelist_out_snomed, 'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_icd10,  'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_opcs4,  'name' , 'terminology', var2_unstyled=1); print()

# COMMAND ----------

# ==============================================================================
# %md ### 3.2.3 Create
# ==============================================================================
# hes_apc
_tmp_codelist_out_icd10 = codelist_out_icd10\
  .select(['code', 'name'])
_out_hes_apc = _hes_apc\
  .join(_tmp_codelist_out_icd10, on='code', how='inner')\
  .withColumn('source', f.lit('hes_apc'))

# hes_apc_oper
_tmp_codelist_out_opcs4 = codelist_out_opcs4\
  .select(['code', 'name'])  
_out_hes_apc_oper = _hes_apc_oper\
  .join(_tmp_codelist_out_opcs4, on='code', how='inner')\
  .withColumn('source', f.lit('hes_apc_oper'))

# append
_out_all = _out_hes_apc\
  .unionByName(_out_hes_apc_oper)\
  .withColumn('sourcen',\
    f.when(f.col('source') == 'hes_apc', 1)\
    .when(f.col('source') == 'hes_apc_oper', 2)\
  )

# first event of each name
_win = Window\
  .partitionBy(['PERSON_ID', 'name'])\
  .orderBy('DATE', 'sourcen', 'code')
_out_1st = _out_all\
  .withColumn('_rownum', f.row_number().over(_win))\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'DATE', 'name', 'source')\
  .orderBy('PERSON_ID', 'DATE', 'name', 'source')

# note: ignore ties for source for now...

# join codelist names before reshape to ensure all covariates are created (when no code matches are found)
_tmp_codelist_out = _tmp_codelist_out_icd10\
  .unionByName(_tmp_codelist_out_opcs4)\
  .select('name')\
  .distinct()              
               
# reshape long to wide
_out_1st_wide = _out_1st\
  .join(_tmp_codelist_out, on='name', how='outer')\
  .withColumn('name', f.concat(f.lit(f'out_{out_prefix}'), f.lower(f.col('name'))))\
  .groupBy('PERSON_ID')\
  .pivot('name')\
  .agg(f.first('DATE'))\
  .where(f.col('PERSON_ID').isNotNull())\
  .orderBy('PERSON_ID')  

# flag and date
vlist = []
for v in [col for col in list(_out_1st_wide.columns) if re.match(f'^out_{out_prefix}', col)]:
  print(v)
  _out_1st_wide = _out_1st_wide\
    .withColumnRenamed(v, v + '_date')\
    .withColumn(v + '_flag', f.when(f.col(v + '_date').isNotNull(), 1))
  vlist = vlist + [v + '_flag', v + '_date']
_out_1st_wide = _out_1st_wide\
  .select(['PERSON_ID'] + vlist)
# 'CENSOR_DATE_START', 'CENSOR_DATE_END'

# check
count_var(_out_1st_wide, 'PERSON_ID'); print()
print(_out_1st.limit(10).toPandas().to_string()); print()
print(_out_1st_wide.limit(10).toPandas().to_string()); print()


# ==============================================================================
# %md ### 3.2.5 Finalise
# ==============================================================================
stillbirth_delcode = _out_1st_wide\
  .withColumn('out_birth_stillbirth_delcode',\
    f.when(f.col('out_birth_stillbirth_flag').isNotNull(), 1)\
  )\
  .select('PERSON_ID', 'out_birth_stillbirth_delcode')

# check
tmpt = tab(stillbirth_delcode, 'out_birth_stillbirth_delcode'); print()
print(stillbirth_delcode.limit(10).toPandas().to_string()); print()

# cache
stillbirth_delcode.cache()
print(f'{stillbirth_delcode.count():,}')

# COMMAND ----------

# ==============================================================================
# %md ### 3.2.4 Check
# ==============================================================================
# check
display(_out_1st)

# COMMAND ----------

# check
display(_out_1st_wide)

# COMMAND ----------


# dd = {"gdppr": [_out_gdppr, codelist_out_gdppr]}
# df, codelist = dd['gdppr']

# ------------------------------------------------------------------------------------
# summarise by name
# ------------------------------------------------------------------------------------
_n_list = []
_out_summ_name = []
for i, source in enumerate(['all', 'hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  _codelist = ''
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name')\
    .agg(\
      f.count(f.lit(1)).alias(f'n_{source}')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id_{source}')\
    )
  _n_list = _n_list + [f'n_{source}', f'n_id_{source}']
  
  if(source != 'all'):
    _tmpc = globals()[_codelist]\
      .select('name')\
      .distinct()
    _tmp = _tmp\
      .join(_tmpc, on='name', how='outer')    
  
  if(i == 0): _out_summ_name = _tmp
  else:
    _out_summ_name = _out_summ_name\
      .join(_tmp, on='name', how='outer')
       
_out_summ_name = _out_summ_name\
  .na.fill(0, subset=_n_list)\
  .orderBy('name')


# ------------------------------------------------------------------------------------ 
# summarise by name, code
# ------------------------------------------------------------------------------------ 
_out_summ_name_code = []
for i, source in enumerate(['hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'    
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name', 'code')\
    .agg(\
      f.count(f.lit(1)).alias(f'n')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id')\
    )
  _tmpc = globals()[_codelist]\
    .select('name', 'code', 'terminology', 'term')\
    .distinct()
  _tmp = _tmp\
    .join(_tmpc, on=['name', 'code'], how='outer')\
    .withColumn('source', f.lit(f'{source}'))    

  if(i == 0): _out_summ_name_code = _tmp
  else:
    _out_summ_name_code = _out_summ_name_code\
      .unionByName(_tmp)
       
_out_summ_name_code = _out_summ_name_code\
  .na.fill(0, subset=['n', 'n_id'])\
  .orderBy('name', 'source', 'code')

# COMMAND ----------

# check codelist match summary by name
display(_out_summ_name)

# COMMAND ----------

# check codelist match summary by name and code
display(_out_summ_name_code)

# COMMAND ----------

# MAGIC %md ### 3.3 gencode

# COMMAND ----------

# gencode = general record codes

# ==============================================================================
# %md ### 3.3.1 Data
# ==============================================================================

# ------------------------------------------------------------------------------
# individual_censor_dates
# ------------------------------------------------------------------------------
# after 24 weeks of gestation during pregnacy and up to 28 days after
individual_censor_dates = cohort\
  .select('PERSON_ID', 'preg_start_date', 'delivery_date', 'preg_start_date_corrected')\
  .withColumn('CENSOR_DATE_END', f.date_add(f.col('delivery_date'), 28))\
  .withColumn('GESTAT_tmp', f.datediff(f.col('delivery_date'), f.col('preg_start_date'))/7)\
  .withColumn('GESTAT_tmp_ge_24', f.when(f.col('GESTAT_tmp') >= 24, 1).otherwise(0))\
  .withColumn('CENSOR_DATE_START', f.date_add(f.col('preg_start_date'), (24*7) - 1 ))

# check
# tmpt = tab(individual_censor_dates, 'GESTAT_tmp', 'preg_start_date_corrected', var2_unstyled=1); print()
# tmpt = tab(individual_censor_dates, 'GESTAT_tmp', 'GESTAT_tmp_ge_24', var2_unstyled=1); print()
tmpt = tab(individual_censor_dates, 'GESTAT_tmp_ge_24'); print()
count_var(individual_censor_dates, 'PERSON_ID'); print()
# print(individual_censor_dates.limit(10).toPandas().to_string()); print()

# tidy
individual_censor_dates = individual_censor_dates\
  .where(f.col('GESTAT_tmp_ge_24') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')

# check
count_var(individual_censor_dates, 'PERSON_ID'); print()
assert individual_censor_dates.where(f.col('CENSOR_DATE_START') > f.col('CENSOR_DATE_END')).count() == 0


# ------------------------------------------------------------------------------
# gdppr
# ------------------------------------------------------------------------------
print('gdppr'); print()
_gdppr = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'CODE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')

# check
count_var(_gdppr, 'PERSON_ID'); print()

_gdppr = _gdppr\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_gdppr, 'PERSON_ID'); print()

_gdppr = _gdppr\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_gdppr, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# hes_apc
# ------------------------------------------------------------------------------
print('hes_apc'); print()
# .where(f.col('DIAG_POSITION') == 1)\ # removed from below - considering any position
_hes_apc = hes_apc_long\
  .select(['PERSON_ID', 'EPISTART', 'CODE'])\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

_hes_apc = _hes_apc\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

_hes_apc = _hes_apc\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_hes_apc, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# hes_apc_oper
# ------------------------------------------------------------------------------
print('hes_apc_oper'); print()
_hes_apc_oper = hes_apc_oper_long\
  .select(['PERSON_ID', 'EPISTART', 'CODE'])\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

_hes_apc_oper = _hes_apc_oper\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

_hes_apc_oper = _hes_apc_oper\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# cache
# ------------------------------------------------------------------------------
_gdppr.cache()
print(f'{_gdppr.count():,}')
_hes_apc.cache()
print(f'{_hes_apc.count():,}')
_hes_apc_oper.cache()
print(f'{_hes_apc_oper.count():,}')

# COMMAND ----------

# ==============================================================================
# %md ### 3.3.2 Codelist
# ==============================================================================
# outcomes codelist
codelist_out = codelist\
  .where((f.col('name').isin(['stillbirth']))\
    & ((f.col('covariate_only').isNull()) | (f.col('covariate_only') != 1))\
    & ((f.col('code_type').isNull()) | (f.col('code_type') == '') | (f.col('code_type') == 1))\
  )

# check
tmpt = tab(codelist_out, 'name' , 'terminology', var2_unstyled=1); print()

# snomed
codelist_out_snomed = codelist_out\
  .where(f.col('terminology') == 'SNOMED')

# icd10
codelist_out_icd10 = codelist_out\
  .where(f.col('terminology') == 'ICD10')

# opcs4
codelist_out_opcs4 = codelist_out\
  .where(f.col('terminology') == 'OPCS4')

# check
tmpt = tab(codelist_out_snomed, 'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_icd10,  'name' , 'terminology', var2_unstyled=1); print()
tmpt = tab(codelist_out_opcs4,  'name' , 'terminology', var2_unstyled=1); print()

# COMMAND ----------

# ==============================================================================
# %md ### 3.3.3 Create
# ==============================================================================
# gdppr
_tmp_codelist_out_snomed = codelist_out_snomed\
  .select(['code', 'name'])
_out_gdppr = _gdppr\
  .join(_tmp_codelist_out_snomed, on='code', how='inner')\
  .withColumn('source', f.lit('gdppr'))

# hes_apc
_tmp_codelist_out_icd10 = codelist_out_icd10\
  .select(['code', 'name'])
_out_hes_apc = _hes_apc\
  .join(_tmp_codelist_out_icd10, on='code', how='inner')\
  .withColumn('source', f.lit('hes'))
  # note: intentionally setting source == 'hes' (not 'hes_apc') for later grouping

# hes_apc_oper
_tmp_codelist_out_opcs4 = codelist_out_opcs4\
  .select(['code', 'name'])  
_out_hes_apc_oper = _hes_apc_oper\
  .join(_tmp_codelist_out_opcs4, on='code', how='inner')\
  .withColumn('source', f.lit('hes'))
  # note: intentionally setting source == 'hes' (not 'hes_apc_oper') for later grouping

# append
_out_all = _out_gdppr\
  .unionByName(_out_hes_apc)\
  .unionByName(_out_hes_apc_oper)

# first event of each name AND SOURCE
_win = Window\
  .partitionBy(['PERSON_ID', 'name', 'source'])\
  .orderBy('DATE', 'code')
_out_1st = _out_all\
  .withColumn('_rownum', f.row_number().over(_win))\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'DATE', 'name', 'source')\
  .orderBy('PERSON_ID', 'DATE', 'name', 'source')

# note: ignore ties for source for now...

# join codelist names before reshape to ensure all covariates are created (when no code matches are found)
_tmp_codelist_out = _tmp_codelist_out_snomed.withColumn('source', f.lit('gdppr'))\
  .unionByName(_tmp_codelist_out_icd10.withColumn('source', f.lit('hes')))\
  .select('name', 'source')\
  .distinct()

# reshape long to wide
_out_1st_wide = _out_1st\
  .join(_tmp_codelist_out, on=['name', 'source'], how='outer')\
  .withColumn('name', f.concat(f.lit(f'out_{out_prefix}'), f.lower(f.col('name')), f.lit('_'), f.lower(f.col('source'))))\
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')\
  .pivot('name')\
  .agg(f.first('DATE'))\
  .where(f.col('PERSON_ID').isNotNull())\
  .orderBy('PERSON_ID')  

# flag and date
vlist = []
for v in [col for col in list(_out_1st_wide.columns) if re.match(f'^out_{out_prefix}', col)]:
  print(v)
  _out_1st_wide = _out_1st_wide\
    .withColumnRenamed(v, v + '_date')\
    .withColumn(v + '_flag', f.when(f.col(v + '_date').isNotNull(), 1))
  vlist = vlist + [v + '_flag', v + '_date']
_out_1st_wide = _out_1st_wide\
  .select(['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'] + vlist)

# check
count_var(_out_1st_wide, 'PERSON_ID'); print()

# ==============================================================================
# %md ### 3.3.5 Finalise
# ==============================================================================
stillbirth_gencode = _out_1st_wide\
  .withColumn('out_birth_stillbirth_gencode_gdppr',\
    f.when(f.col('out_birth_stillbirth_gdppr_flag').isNotNull(), 1)\
  )\
  .withColumn('out_birth_stillbirth_gencode_hes',\
    f.when(f.col('out_birth_stillbirth_hes_flag').isNotNull(), 1)\
  )\
  .withColumnRenamed('out_birth_stillbirth_gdppr_date', 'out_birth_stillbirth_gencode_gdppr_date')\
  .withColumnRenamed('out_birth_stillbirth_hes_date', 'out_birth_stillbirth_gencode_hes_date')\
  .select('PERSON_ID', 'out_birth_stillbirth_gencode_gdppr', 'out_birth_stillbirth_gencode_gdppr_date', 'out_birth_stillbirth_gencode_hes', 'out_birth_stillbirth_gencode_hes_date')

# check
count_var(stillbirth_gencode, 'PERSON_ID'); print()
print(stillbirth_gencode.limit(10).toPandas().to_string()); print()

# cache
stillbirth_gencode.cache()
print(f'{stillbirth_gencode.count():,}')

# COMMAND ----------

# ==============================================================================
# %md ### 3.3.4 Check
# ==============================================================================
display(_out_1st)

# COMMAND ----------

display(_out_1st_wide)

# COMMAND ----------

_tmp = _out_1st\
  .withColumn('diff', f.datediff(f.col('DATE'), f.col('CENSOR_DATE_START'))/365.25)
_tmpp = _tmp\
  .toPandas()

rows_of_5 = np.ceil(len(_tmpp['name'].drop_duplicates())/5).astype(int)
fig, axes = plt.subplots(rows_of_5, 5, figsize=(13,2*rows_of_5), sharex=True) # , sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 3)
# names = ['hes_apc', 'gdppr', 'deaths']  
names = ['hes', 'gdppr', 'deaths']  
  
vlist = list(_tmpp[['name']].drop_duplicates().sort_values('name')['name']) # ['AMI', 'BMI_obesity', 'CKD', 'COPD']  
for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, v)
  tmp2d1 = _tmpp[(_tmpp[f'diff'] > -30) & (_tmpp[f'name'] == v)]
  s1 = list(tmp2d1[tmp2d1[f'source'] == 'hes'][f'diff']) # was hes_apc
  s2 = list(tmp2d1[tmp2d1[f'source'] == 'gdppr'][f'diff'])
  s3 = list(tmp2d1[tmp2d1[f'source'] == 'deaths'][f'diff'])
  ax.hist([s1, s2, s3], bins = list(np.linspace(0,1,100)), stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'{v}')
  ax.xaxis.set_tick_params(labelbottom=True)
  if(i==0): ax.legend(loc='upper right')
plt.tight_layout();
display(fig)

# COMMAND ----------

# dd = {"gdppr": [_out_gdppr, codelist_out_gdppr]}
# df, codelist = dd['gdppr']

# ------------------------------------------------------------------------------------
# summarise by name
# ------------------------------------------------------------------------------------
_n_list = []
_out_summ_name = []
for i, source in enumerate(['all', 'gdppr', 'hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  _codelist = ''
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name')\
    .agg(\
      f.count(f.lit(1)).alias(f'n_{source}')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id_{source}')\
    )
  _n_list = _n_list + [f'n_{source}', f'n_id_{source}']
  
  if(source != 'all'):
    _tmpc = globals()[_codelist]\
      .select('name')\
      .distinct()
    _tmp = _tmp\
      .join(_tmpc, on='name', how='outer')    
  
  if(i == 0): _out_summ_name = _tmp
  else:
    _out_summ_name = _out_summ_name\
      .join(_tmp, on='name', how='outer')
       
_out_summ_name = _out_summ_name\
  .na.fill(0, subset=_n_list)\
  .orderBy('name')


# ------------------------------------------------------------------------------------ 
# summarise by name, code
# ------------------------------------------------------------------------------------ 
_out_summ_name_code = []
for i, source in enumerate(['gdppr', 'hes_apc', 'hes_apc_oper']):
  _out = f'_out_{source}'
  if(source in ['hes_apc', 'deaths']): _codelist = f'codelist_out_icd10'
  elif(source in ['gdppr']): _codelist = f'codelist_out_snomed'
  elif(source in ['hes_apc_oper']): _codelist = f'codelist_out_opcs4'    
  print(i, source, _out, _codelist)
  
  _tmp = globals()[_out]\
    .groupBy('name', 'code')\
    .agg(\
      f.count(f.lit(1)).alias(f'n')\
      , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id')\
    )
  _tmpc = globals()[_codelist]\
    .select('name', 'code', 'terminology', 'term')\
    .distinct()
  _tmp = _tmp\
    .join(_tmpc, on=['name', 'code'], how='outer')\
    .withColumn('source', f.lit(f'{source}'))    

  if(i == 0): _out_summ_name_code = _tmp
  else:
    _out_summ_name_code = _out_summ_name_code\
      .unionByName(_tmp)
       
_out_summ_name_code = _out_summ_name_code\
  .na.fill(0, subset=['n', 'n_id'])\
  .orderBy('name', 'source', 'code')

# COMMAND ----------

# check codelist match summary by name
display(_out_summ_name)

# COMMAND ----------

# check codelist match summary by name and code
display(_out_summ_name_code)

# COMMAND ----------

# MAGIC %md ### 3.4 max

# COMMAND ----------

# check
count_var(stillbirth_delvar, 'PERSON_ID')
count_var(stillbirth_delcode, 'PERSON_ID')
count_var(stillbirth_gencode, 'PERSON_ID')

# check
tmpt = tab(stillbirth_delvar, 'out_birth_stillbirth_delvar'); print()
tmpt = tab(stillbirth_delcode, 'out_birth_stillbirth_delcode'); print()
tmpt = tab(stillbirth_gencode, 'out_birth_stillbirth_gencode_hes'); print()
# tmpt = tab(stillbirth_gencode, 'out_birth_stillbirth_gencode_gdppr'); print()

# join
stillbirth = stillbirth_delvar\
  .join(stillbirth_delcode, on=['PERSON_ID'], how='outer')\
  .join(stillbirth_gencode, on=['PERSON_ID'], how='outer')

# check
count_var(stillbirth, 'PERSON_ID')
print(len(stillbirth.columns))
print(pd.DataFrame({f'_cols': stillbirth.columns}).to_string())

# max
stillbirth = stillbirth\
  .withColumn('out_birth_stillbirth_max',\
    f.greatest(\
        'out_birth_stillbirth_delvar'
      , 'out_birth_stillbirth_delcode'
      , 'out_birth_stillbirth_gencode_hes'
      , 'out_birth_stillbirth_gencode_gdppr'
    )\
  )

# check
tmp1 = stillbirth\
  .withColumn('vchg',\
    f.concat(\
        f.when(f.col('out_birth_stillbirth_delvar').isNull(), f.lit('_')).otherwise(f.col('out_birth_stillbirth_delvar'))
      , f.when(f.col('out_birth_stillbirth_delcode').isNull(), f.lit('_')).otherwise(f.col('out_birth_stillbirth_delcode'))
      , f.when(f.col('out_birth_stillbirth_gencode_hes').isNull(), f.lit('_')).otherwise(f.col('out_birth_stillbirth_gencode_hes'))
      , f.when(f.col('out_birth_stillbirth_gencode_gdppr' ).isNull(), f.lit('_')).otherwise(f.col('out_birth_stillbirth_gencode_gdppr'))            
    )\
  )
tmpt = tab(tmp1, 'vchg', 'out_birth_stillbirth_max', var2_unstyled=1); print()

# check
count_var(stillbirth, 'PERSON_ID'); print()
print(stillbirth.limit(10).toPandas().to_string()); print()

# cache
stillbirth.cache()
print(f'{stillbirth.count():,}')

# uncache components

# COMMAND ----------

# MAGIC %md # 4 preterm

# COMMAND ----------

# MAGIC %md ## 4.1 delvar

# COMMAND ----------

tmp1 = delivery_record\
  .select('PERSON_ID', 'GESTAT_1')

# check
count_var(tmp1, 'PERSON_ID'); print()

# filter 
tmp2 = tmp1\
  .where((f.col('GESTAT_1').isNotNull()) & (f.col('GESTAT_1') != 99))

# check
count_var(tmp2, 'PERSON_ID'); print()
tmpt = tabstat(tmp2, 'GESTAT_1'); print()

# create
tmp2 = tmp2\
  .withColumn('out_birth_preterm', f.when(f.col('GESTAT_1') < 37, 1).otherwise(0))\
  .withColumn('out_birth_very_preterm', f.when(f.col('GESTAT_1') < 32, 1).otherwise(0))\
  .withColumn('out_birth_ext_preterm', f.when(f.col('GESTAT_1') < 28, 1).otherwise(0))

# check 
tmpt = tab(tmp2, 'out_birth_preterm'); print()
tmpt = tab(tmp2, 'out_birth_very_preterm'); print()
tmpt = tab(tmp2, 'out_birth_ext_preterm'); print()
tmpt = tabstat(tmp2, 'GESTAT_1', byvar='out_birth_preterm'); print()
tmpt = tabstat(tmp2, 'GESTAT_1', byvar='out_birth_very_preterm'); print()
tmpt = tabstat(tmp2, 'GESTAT_1', byvar='out_birth_ext_preterm'); print()
tmp2 = tmp2\
  .withColumn('_concat',\
    f.concat('out_birth_preterm', 'out_birth_very_preterm', 'out_birth_ext_preterm')\
  )\
 .withColumn('_sum', sum([f.col(v) for v in ['out_birth_preterm', 'out_birth_very_preterm', 'out_birth_ext_preterm']]))
tmpt = tab(tmp2, '_concat', '_sum', var2_unstyled=1); print()

# finalise
preterm_delvar = tmp2\
  .select('PERSON_ID', 'out_birth_preterm', 'out_birth_very_preterm', 'out_birth_ext_preterm')
  
# check
count_var(preterm_delvar, 'PERSON_ID'); print()
print(preterm_delvar.limit(10).toPandas().to_string()); print()

# cache
preterm_delvar.cache()
print(f'{preterm_delvar.count():,}')

# COMMAND ----------

# MAGIC %md ## 4.2 gencode

# COMMAND ----------

# Only 5-character ICD-10 codes are useful, SNOMED only for now

# gencode = general record codes

# ==============================================================================
# %md ### 4.2.1 Data
# ==============================================================================

print('--------------------------------------------------------------------------------------')
print('individual_censor_dates')
print('--------------------------------------------------------------------------------------')
# after 24 weeks of gestation during pregnacy and up to 28 days after
individual_censor_dates = cohort\
  .select('PERSON_ID', 'preg_start_date', 'delivery_date', 'preg_start_date_corrected')\
  .withColumn('CENSOR_DATE_END', f.date_add(f.col('delivery_date'), 28))\
  .withColumn('GESTAT_tmp', f.datediff(f.col('delivery_date'), f.col('preg_start_date'))/7)\
  .withColumn('GESTAT_tmp_ge_24', f.when(f.col('GESTAT_tmp') >= 24, 1).otherwise(0))\
  .withColumn('CENSOR_DATE_START', f.date_add(f.col('preg_start_date'), (24*7) - 1 ))

# check
# tmpt = tab(individual_censor_dates, 'GESTAT_tmp', 'preg_start_date_corrected', var2_unstyled=1); print()
# tmpt = tab(individual_censor_dates, 'GESTAT_tmp', 'GESTAT_tmp_ge_24', var2_unstyled=1); print()
tmpt = tab(individual_censor_dates, 'GESTAT_tmp_ge_24'); print()
count_var(individual_censor_dates, 'PERSON_ID'); print()
# print(individual_censor_dates.limit(10).toPandas().to_string()); print()

# tidy
individual_censor_dates = individual_censor_dates\
  .where(f.col('GESTAT_tmp_ge_24') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')

# check
count_var(individual_censor_dates, 'PERSON_ID'); print()
assert individual_censor_dates.where(f.col('CENSOR_DATE_START') > f.col('CENSOR_DATE_END')).count() == 0


print('--------------------------------------------------------------------------------------')
print('gdppr')
print('--------------------------------------------------------------------------------------')
_gdppr = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'CODE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')

# check
count_var(_gdppr, 'PERSON_ID'); print()

# add censor dates
_gdppr = _gdppr\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_gdppr, 'PERSON_ID'); print()

# filter to after CENSOR_DATE_START and on or before CENSOR_DATE_END
_gdppr = _gdppr\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_gdppr, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('cache')
print('--------------------------------------------------------------------------------------')
_gdppr.cache()
print(f'_gdppr {_gdppr.count():,}')


# ==============================================================================
# %md ### 4.2.2 Codelist
# ==============================================================================
# outcomes codelist
codelist_out = codelist\
  .where((f.col('name').isin(['preterm']))\
    & ((f.col('covariate_only').isNull()) | (f.col('covariate_only') != 1))\
    & ((f.col('code_type').isNull()) | (f.col('code_type') == '') | (f.col('code_type') == 1))\
  )

# check
tmpt = tab(codelist_out, 'name' , 'terminology', var2_unstyled=1); print()
assert codelist_out.where(f.col('terminology') != 'SNOMED').count() == 0

# snomed
codelist_out_snomed = codelist_out\
  .where(f.col('terminology') == 'SNOMED')

# check
tmpt = tab(codelist_out_snomed, 'name' , 'terminology', var2_unstyled=1); print()


# ==============================================================================
# %md ### 4.2.3 Create
# ==============================================================================
_out_birth_preterm_in = {
    'gdppr':   ['_gdppr',   'codelist_out_snomed', 1]
}
_out_birth_preterm, _out_birth_preterm_1st, _out_birth_preterm_1st_wide = codelist_match(_out_birth_preterm_in, _name_prefix=f'out_{out_prefix}')
_out_birth_preterm_summ_name, _out_birth_preterm_summ_name_code = codelist_match_summ(_out_birth_preterm_in, _out_birth_preterm)

# check 
tmp1 = merge(_gdppr, codelist_out_snomed, ['code'])
assert tmp1.where(f.col('_merge') == 'both').count() == 0

# COMMAND ----------

display(_out_birth_preterm_1st_wide)

# COMMAND ----------

_tmp = _out_birth_preterm_1st\
  .withColumn('diff', f.datediff(f.col('DATE'), f.col('CENSOR_DATE_START'))/365.25)
_tmpp = _tmp\
  .toPandas()

rows_of_5 = np.ceil(len(_tmpp['name'].drop_duplicates())/5).astype(int)
fig, axes = plt.subplots(rows_of_5, 5, figsize=(13,2*rows_of_5), sharex=True) # , sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 3)
# names = ['hes_apc', 'gdppr', 'deaths']  
names = ['hes', 'gdppr', 'deaths']  
  
vlist = list(_tmpp[['name']].drop_duplicates().sort_values('name')['name']) # ['AMI', 'BMI_obesity', 'CKD', 'COPD']  
for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, v)
  tmp2d1 = _tmpp[(_tmpp[f'diff'] > -30) & (_tmpp[f'name'] == v)]
  s1 = list(tmp2d1[tmp2d1[f'source'] == 'hes'][f'diff']) # was hes_apc
  s2 = list(tmp2d1[tmp2d1[f'source'] == 'gdppr'][f'diff'])
  s3 = list(tmp2d1[tmp2d1[f'source'] == 'deaths'][f'diff'])
  ax.hist([s1, s2, s3], bins = list(np.linspace(0,1,100)), stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'{v}')
  ax.xaxis.set_tick_params(labelbottom=True)
  if(i==0): ax.legend(loc='upper right')
plt.tight_layout();
display(fig)

# COMMAND ----------

# MAGIC %md ## 4.3 max

# COMMAND ----------

preterm = preterm_delvar 

# check
count_var(preterm, 'PERSON_ID'); print()
print(preterm.limit(10).toPandas().to_string()); print()

# cache
preterm.cache()
print(f'{preterm.count():,}')

# COMMAND ----------

# MAGIC %md # 5 resus

# COMMAND ----------

# ------------------------------------------------------------------------------
# hes_apc_mat
# ------------------------------------------------------------------------------
vlist = [col for col in hes_apc_mat.columns if re.match('^BIRESUS_[1-9]$', col)] 
_hes_apc_mat = hes_apc_mat\
  .select(['EPIKEY', 'NUMTAILB'] + vlist)

# check
count_var(_hes_apc_mat, 'EPIKEY'); print()
print(len(_hes_apc_mat.columns)); print()
print(pd.DataFrame({f'_cols': _hes_apc_mat.columns}).to_string()); print()

# merge
# note: use _delivery_record_gt_24w from stillbirth
_hes_apc_mat = merge(_delivery_record_gt_24w, _hes_apc_mat, ['EPIKEY']); print()
assert _hes_apc_mat.select('_merge').where(f.col('_merge') == 'left_only').count() == 0
_hes_apc_mat = _hes_apc_mat\
  .where(f.col('_merge') == 'both')\
  .drop('_merge', 'EPIKEY')


# ------------------------------------------------------------------------------
# biresus
# ------------------------------------------------------------------------------
# reshape wide to long
# filter to rows with potential information
# indicators for ...
# collapse by id, taking max
# concatenate max
# create resus, care taken with unknown and null
tmp1 = reshape_wide_to_long_multi(_hes_apc_mat, i=['PERSON_ID', 'NUMTAILB'], j='POSITION', stubnames=['BIRESUS_'])\
  .withColumn('POSITION', f.col('POSITION').cast(t.IntegerType()))\
  .withColumnRenamed('BIRESUS_', 'BIRESUS_precast')\
  .withColumn('BIRESUS_', f.col('BIRESUS_precast').cast(t.IntegerType()))

# check
tmpt = tab(tmp1, 'BIRESUS_', 'BIRESUS_precast', var2_unstyled=1); print()
tmpt = tab(tmp1, 'BIRESUS_', 'NUMTAILB', var2_unstyled=1); print()

tmp1 = tmp1\
  .where(\
    (f.col('NUMTAILB').isNull())\
    | (f.col('POSITION') <= f.col('NUMTAILB'))\
  )\
  .withColumn('ind_123456', f.when(f.col('BIRESUS_').isin([1,2,3,4,5,6]), 1))\
  .withColumn('ind_8',      f.when(f.col('BIRESUS_').isin([8]), 1))\
  .withColumn('ind_9',      f.when(f.col('BIRESUS_').isin([9]), 1))\
  .withColumn('ind_null',   f.when(f.col('BIRESUS_').isNull(), 1))\
  .groupBy('PERSON_ID', 'NUMTAILB')\
  .agg(\
    f.max(f.col('ind_123456')).alias('ind_123456_max')\
    , f.max(f.col('ind_8')).alias('ind_8_max')\
    , f.max(f.col('ind_9')).alias('ind_9_max')\
    , f.max(f.col('ind_null')).alias('ind_null_max')\
  )\
  .withColumn('rx9n',\
    f.concat(\
        f.when(f.col('ind_123456_max').isNull(), f.lit('_')).otherwise(f.col('ind_123456_max'))\
      , f.when(f.col('ind_8_max').isNull(), f.lit('_')).otherwise(f.col('ind_8_max'))\
      , f.when(f.col('ind_9_max').isNull(), f.lit('_')).otherwise(f.col('ind_9_max'))\
      , f.when(f.col('ind_null_max' ).isNull(), f.lit('_')).otherwise(f.col('ind_null_max'))\
    )\
  )\
  .withColumn('_resus',\
    f.when(f.col('ind_123456_max') == 1, 1)\
    .when((f.col('ind_9_max') == 1) | (f.col('ind_null_max') == 1), None)\
    .when(f.col('ind_8_max') == 1, 0)\
  )

# check
count_var(tmp1, 'PERSON_ID'); print()
tmpt = tab(tmp1, 'rx9n', '_resus', var2_unstyled=1); print()

# tidy
resus = tmp1\
  .select('PERSON_ID', '_resus')\
  .withColumnRenamed('_resus', 'out_birth_resus')

# check
tmpt = tab(resus, 'out_birth_resus'); print()
print(resus.limit(10).toPandas().to_string()); print()

# cache
resus.cache()
print(f'{resus.count():,}')

# COMMAND ----------

# MAGIC %md # 6 small_gest_age

# COMMAND ----------

# delivery record
_delivery_record = delivery_record\
  .select('PERSON_ID', 'GESTAT_1', 'BIRWEIT_1')

# multi gest
_multi_gest = multi_gest\
  .select('PERSON_ID', 'out_birth_multi_gest_max')
  
# check
count_var(_delivery_record, 'PERSON_ID'); print()
count_var(_multi_gest, 'PERSON_ID'); print()

# merge
tmp1 = merge(_delivery_record, _multi_gest, ['PERSON_ID']); print()
assert tmp1.select('_merge').where(f.col('_merge').isin(['left_only', 'right_only'])).count() == 0
tmp1 = tmp1\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check
count_var(tmp1, 'PERSON_ID'); print()
tmpt = tab(_multi_gest, 'out_birth_multi_gest_max'); print()

# filter to those with no evidence of multi gest
tmp2 = tmp1\
  .where(f.col('out_birth_multi_gest_max') == 0)

# check
count_var(tmp2, 'PERSON_ID'); print()
print(tmp2.limit(10).toPandas().to_string()); print()

# check GESTAT_1 and BIRWEIT_1
assert dict(tmp2.dtypes)['GESTAT_1'] == 'int'
assert dict(tmp2.dtypes)['BIRWEIT_1'] == 'int'
tmpt = tab(tmp2, 'GESTAT_1'); print()
tmpt = tabstat(tmp2, 'BIRWEIT_1', byvar='GESTAT_1'); print()

# filter to those with valid GESTAT_1 and BIRWEIT_1
tmp3 = tmp2\
    .where((f.col('GESTAT_1').isNotNull()) & (f.col('GESTAT_1') != 99))\
    .where((f.col('BIRWEIT_1').isNotNull()) & (f.col('BIRWEIT_1') != 9999))

# check
count_var(tmp3, 'PERSON_ID'); print()


# ------------------------------------------------------------------------------
# look-up
# ------------------------------------------------------------------------------
# calculate percentiles
tmp4 = tmp3\
  .groupBy('GESTAT_1')\
  .agg(\
    f.count(f.lit(1)).alias('n_rows')\
    , f.count(f.col('BIRWEIT_1')).alias('n')\
    , f.expr(f'percentile(BIRWEIT_1, array(0.05))')[0].alias('p05')\
    , f.expr(f'percentile(BIRWEIT_1, array(0.50))')[0].alias('p50')\
    , f.expr(f'percentile(BIRWEIT_1, array(0.95))')[0].alias('p95')\
  )\
  .orderBy('GESTAT_1')

# check
print(tmp4.toPandas().to_string()); print()

# filter to weeks 24-42 (ER)
tmp5 = tmp4\
  .where((f.col('GESTAT_1') >= 24) & (f.col('GESTAT_1') <= 42))\
  .select('GESTAT_1', 'p05')\
  .withColumnRenamed('p05', 'BIRWEIT_1_p05')

# check
print(tmp5.toPandas().to_string()); print()

# finalise
lookup = tmp5


# ------------------------------------------------------------------------------
# low bw for ga
# ------------------------------------------------------------------------------
# check
count_var(tmp3, 'PERSON_ID'); print()

# filter to weeks 24-42 (ER)
tmp6 = tmp3\
  .where((f.col('GESTAT_1') >= 24) & (f.col('GESTAT_1') <= 42))\

# check
count_var(tmp6, 'PERSON_ID'); print()

# merge
tmp7 = merge(tmp6, lookup, ['GESTAT_1']); print()
assert tmp7.select('_merge').where(f.col('_merge').isin(['left_only', 'right_only'])).count() == 0
tmp7 = tmp7\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check
count_var(tmp7, 'PERSON_ID'); print()

# low bw for ga
tmp7 = tmp7\
  .withColumn('out_birth_small_gest_age',\
    f.when(f.col('BIRWEIT_1') < f.col('BIRWEIT_1_p05'), 1)\
    .when(f.col('BIRWEIT_1') >= f.col('BIRWEIT_1_p05'), 0)\
  )

# check
tmpt = tab(tmp7, 'out_birth_multi_gest_max'); print()
tmpt = tab(tmp7, 'out_birth_small_gest_age'); print()
tmpt = tabstat(tmp7, 'BIRWEIT_1', byvar='out_birth_small_gest_age'); print()

# finalise
small_gest_age = tmp7
  
# check
count_var(small_gest_age, 'PERSON_ID'); print()
print(small_gest_age.limit(10).toPandas().to_string()); print()

# cache
small_gest_age.cache()
print(f'{small_gest_age.count():,}')

# COMMAND ----------

# check lookup
display(tmp4)

# COMMAND ----------

# MAGIC %md # 7 ind_lab

# COMMAND ----------

# Note: ind_lab = Induction of labour
tmp1 = delivery_record\
  .select('PERSON_ID', 'DELONSET')\

# check
count_var(tmp1, 'PERSON_ID'); print()
assert dict(tmp1.dtypes)['DELONSET'] == 'string'

# convert
tmp1 = tmp1\
  .withColumnRenamed('DELONSET', 'DELONSET_precast')\
  .withColumn('DELONSET', f.col('DELONSET_precast').cast(t.IntegerType()))
tmpt = tab(tmp1, 'DELONSET_precast', 'DELONSET', var2_unstyled=1); print()

# recode
tmp1 = tmp1\
  .withColumn('out_birth_ind_lab',\
    f.when(f.col('DELONSET').isin([3,4,5]), 1)\
    .when(f.col('DELONSET').isin([1,2]), 0)\
  )

# check
tmpt = tab(tmp1, 'DELONSET', 'out_birth_ind_lab', var2_unstyled=1); print()

# finalise
ind_lab = tmp1\
  .select('PERSON_ID', 'out_birth_ind_lab')
  
# check
count_var(ind_lab, 'PERSON_ID'); print()
print(ind_lab.limit(10).toPandas().to_string()); print()

# cache
ind_lab.cache()
print(f'{ind_lab.count():,}')

# COMMAND ----------

# MAGIC %md # 8 Save

# COMMAND ----------

# prepare
small_gest_age = small_gest_age\
  .drop('out_birth_multi_gest_max', 'GESTAT_1', 'BIRWEIT_1', 'BIRWEIT_1_p05')

# check
count_var(multi_gest, 'PERSON_ID'); print()
count_var(stillbirth, 'PERSON_ID'); print()
count_var(preterm, 'PERSON_ID'); print()
count_var(small_gest_age, 'PERSON_ID'); print()
count_var(resus, 'PERSON_ID'); print()
count_var(ind_lab, 'PERSON_ID'); print()

# join
out_birth = multi_gest\
  .join(stillbirth, on=['PERSON_ID'], how='outer')\
  .join(small_gest_age, on=['PERSON_ID'], how='outer')\
  .join(resus, on=['PERSON_ID'], how='outer')\
  .join(preterm, on=['PERSON_ID'], how='outer')\
  .join(ind_lab, on=['PERSON_ID'], how='outer')

# check
count_var(out_birth, 'PERSON_ID'); print()
print(len(out_birth.columns)); print()
print(pd.DataFrame({f'_cols': out_birth.columns}).to_string()); print()

# COMMAND ----------

# check final
display(out_birth)

# COMMAND ----------

# save name
outName = f'{proj}_out_outcomes_at_birth'.lower()

# save previous version for comparison purposes
_datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
outName_pre = f'{outName}_pre{_datetimenow}'.lower()
print(outName_pre)
spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')
# 20220722_193414 - created a previous version ready for rerun, but realised rerun not need - not impacted by fu_end_date change, so deleted the previous version to avoid confusion. Remove this note when next previous version is created.

# save
out_birth.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

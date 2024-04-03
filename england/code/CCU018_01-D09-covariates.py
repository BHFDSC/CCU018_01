# Databricks notebook source
# MAGIC %md # CCU018_01-D09-covariates
# MAGIC  
# MAGIC **Description** This notebook creates the covariates for the CCU018_01 project, which are defined with respect to the start of pregnancy for each individual and includes variables for medical history, medication history, smoking status, BMI, surgery in the last year, and number of unique BNF chapters. 
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** out_codelist, out_cohort, out_skinny,\
# MAGIC in_gdppr, cur_hes_apc_long, cur_hes_apc_oper_long, in_pmeds, in_hes_apc,\
# MAGIC gp_refset
# MAGIC
# MAGIC **Data output** ccu018_01_out_covariates

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

codelist          = spark.table(path_out_codelist)
cohort            = spark.table(path_out_cohort)
skinny            = spark.table(path_out_skinny)
gdppr             = spark.table(path_in_gdppr)
hes_apc_long      = spark.table(path_cur_hes_apc_long)
hes_apc_oper_long = spark.table(path_cur_hes_apc_oper_long)
pmeds             = spark.table(path_in_pmeds)
hes_apc           = spark.table(path_in_hes_apc)
gp_refset         = spark.table(path_ref_gp_refset)

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

print('--------------------------------------------------------------------------------------')
print('individual_censor_dates')
print('--------------------------------------------------------------------------------------')
tmp1 = cohort\
  .select('PERSON_ID', 'preg_start_date')\
  .withColumnRenamed('preg_start_date', 'CENSOR_DATE_END')

# subtract 1 year from DOB for tolerance
tmp2 = skinny\
  .select('PERSON_ID', 'DOB')\
  .withColumn('CENSOR_DATE_START', f.date_add(f.col('DOB'), -365))
  
# check  
count_var(tmp1, 'PERSON_ID'); print()
count_var(tmp2, 'PERSON_ID'); print()

# merge
individual_censor_dates = merge(tmp1, tmp2, ['PERSON_ID']); print()
assert individual_censor_dates.where(f.col('_merge') == 'both').count() == individual_censor_dates.count()
assert individual_censor_dates.where(f.col('CENSOR_DATE_START') > f.col('CENSOR_DATE_END')).count() == 0

# check
individual_censor_dates = individual_censor_dates\
  .withColumn('_diff', f.datediff(f.col('CENSOR_DATE_END'), f.col('CENSOR_DATE_START'))/365.25)\
  .withColumn('_age', f.datediff(f.col('CENSOR_DATE_END'), f.col('DOB'))/365.25)
tmpt = tabstat(individual_censor_dates, '_diff'); print()
tmpt = tabstat(individual_censor_dates, '_age'); print()

# tidy 
individual_censor_dates = individual_censor_dates\
 .drop('DOB', '_merge', '_diff', '_age')

# check
count_var(individual_censor_dates, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('gdppr')
print('--------------------------------------------------------------------------------------')
# reduce and rename columns
_gdppr = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'RECORD_DATE', 'CODE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')

# check
count_var(_gdppr, 'PERSON_ID'); print()

# add individual censor dates
_gdppr = _gdppr\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_gdppr, 'PERSON_ID'); print()

# check before CENSOR_DATE_END, accounting for nulls and using RECORD_DATE where needed
# 1 - both DATE and RECORD_DATE are null
# 2 - DATE is null, but RECORD_DATE is not null and RECORD_DATE <= CENSOR_DATE_END
# 3 - DATE is null, but RECORD_DATE is not null and RECORD_DATE > CENSOR_DATE_END
# 4 - DATE is not null and DATE <= CENSOR_DATE_END
# 5 - DATE is not null and DATE > CENSOR_DATE_END
_gdppr = _gdppr\
  .withColumn('_tmp1',\
    f.when((f.col('DATE').isNull()) & (f.col('RECORD_DATE').isNull()), 1)\
     .when((f.col('DATE').isNull()) & (f.col('RECORD_DATE').isNotNull()) & (f.col('RECORD_DATE') <= f.col('CENSOR_DATE_END')), 2)\
     .when((f.col('DATE').isNull()) & (f.col('RECORD_DATE').isNotNull()) & (f.col('RECORD_DATE') > f.col('CENSOR_DATE_END')), 3)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') <= f.col('CENSOR_DATE_END')), 4)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') >  f.col('CENSOR_DATE_END')), 5)\
  )
tmpt = tab(_gdppr, '_tmp1'); print()

# filter to before CENSOR_DATE_END 
# keep _tmp1 == 2 and 4
# replace DATE with RECORD_DATE where DATE is null
# tidy
_gdppr = _gdppr\
  .where(f.col('_tmp1').isin([2, 4]))\
  .withColumn('DATE', f.when(f.col('DATE').isNull(), f.col('RECORD_DATE')).otherwise(f.col('DATE')))\
  .drop('_tmp1')

# check
count_var(_gdppr, 'PERSON_ID'); print()

# check on or after CENSOR_DATE_START, using RECORD_DATE where needed (often in the case of dummy/erroneous DATE)
# note: nulls were replaced in previous data step
# 1 - DATE >= CENSOR_DATE_START
# 2 - DATE <  CENSOR_DATE_START and RECORD_DATE <  CENSOR_DATE_START
# 3 - DATE <  CENSOR_DATE_START and RECORD_DATE >= CENSOR_DATE_START and RECORD_DATE <= CENSOR_DATE_END
# 4 - DATE <  CENSOR_DATE_START and RECORD_DATE >= CENSOR_DATE_START and RECORD_DATE >  CENSOR_DATE_END
_gdppr = _gdppr\
  .withColumn('_tmp2',\
    f.when((f.col('DATE') >= f.col('CENSOR_DATE_START')), 1)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') <  f.col('CENSOR_DATE_START')), 2)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') >= f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') <= f.col('CENSOR_DATE_END')), 3)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') >= f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') >  f.col('CENSOR_DATE_END')), 4)\
  )
tmpt = tab(_gdppr, '_tmp2'); print()

# filter to on or after CENSOR_DATE_START
# keep _tmp2 == 1 and 3
# replace DATE with RECORD_DATE where RECORD_DATE is more appropriate
# tidy
_gdppr = _gdppr\
  .where(f.col('_tmp2').isin([1, 3]))\
  .withColumn('DATE', f.when(f.col('_tmp2') == 3, f.col('RECORD_DATE')).otherwise(f.col('DATE')))\
  .drop('_tmp2', 'RECORD_DATE')

# check
count_var(_gdppr, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('hes_apc')
print('--------------------------------------------------------------------------------------')
# reduce and rename columns
_hes_apc = hes_apc_long\
  .select(['PERSON_ID', 'EPISTART', 'CODE'])\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

# add individual censor dates
_hes_apc = _hes_apc\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

# check before CENSOR_DATE_END, accounting for nulls
# note: checked in curated_data for potential columns to use in the case of null DATE (EPISTART) - no substantial gain from other columns
# 1 - DATE is null
# 2 - DATE is not null and DATE <= CENSOR_DATE_END
# 3 - DATE is not null and DATE > CENSOR_DATE_END
_hes_apc = _hes_apc\
  .withColumn('_tmp1',\
    f.when((f.col('DATE').isNull()), 1)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') <= f.col('CENSOR_DATE_END')), 2)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') >  f.col('CENSOR_DATE_END')), 3)\
  )
tmpt = tab(_hes_apc, '_tmp1'); print()

# filter to before CENSOR_DATE_END
# keep _tmp1 == 2
# tidy
_hes_apc = _hes_apc\
  .where(f.col('_tmp1').isin([2]))\
  .drop('_tmp1')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

# check on or after CENSOR_DATE_START
# note: nulls were replaced in previous data step
# 1 - DATE >= CENSOR_DATE_START
# 2 - DATE <  CENSOR_DATE_START
_hes_apc = _hes_apc\
  .withColumn('_tmp2',\
    f.when((f.col('DATE') >= f.col('CENSOR_DATE_START')), 1)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_START')), 2)\
  )
tmpt = tab(_hes_apc, '_tmp2'); print()

# filter to on or after CENSOR_DATE_START
# keep _tmp2 == 1
# tidy
_hes_apc = _hes_apc\
  .where(f.col('_tmp2').isin([1]))\
  .drop('_tmp2')

# check
count_var(_hes_apc, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('hes_apc_oper')
print('--------------------------------------------------------------------------------------')
# reduce and rename columns
_hes_apc_oper = hes_apc_oper_long\
  .select(['PERSON_ID', 'EPIKEY', 'DIAG_DIGITS', 'DIAG_POSITION', 'EPISTART', 'CODE'])\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

# add individual censor dates
_hes_apc_oper = _hes_apc_oper\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

# check before CENSOR_DATE_END, accounting for nulls
# note: checked in curated_data for potential columns to use in the case of null DATE (EPISTART) - no substantial gain from other columns
# ...
_hes_apc_oper = _hes_apc_oper\
  .withColumn('_tmp1',\
    f.when((f.col('DATE').isNull()), 1)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') <= f.col('CENSOR_DATE_END')), 2)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') >  f.col('CENSOR_DATE_END')), 3)\
  )
tmpt = tab(_hes_apc_oper, '_tmp1'); print()

# filter to before CENSOR_DATE_END
# ...
_hes_apc_oper = _hes_apc_oper\
  .where(f.col('_tmp1').isin([2]))\
  .drop('_tmp1')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

# check on or after CENSOR_DATE_END_less_12m
# ...
_hes_apc_oper = _hes_apc_oper\
  .withColumn('CENSOR_DATE_END_less_12m', f.add_months(f.col('CENSOR_DATE_END'), -12))\
  .withColumn('_tmp2',\
    f.when((f.col('DATE') >= f.col('CENSOR_DATE_END_less_12m')), 1)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_END_less_12m')), 2)\
  )
tmpt = tab(_hes_apc_oper, '_tmp2'); print()

# filter to on or after CENSOR_DATE_END_less_12m
# ...
_hes_apc_oper = _hes_apc_oper\
  .where(f.col('_tmp2').isin([1]))\
  .drop('_tmp2')

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('pmeds')
print('--------------------------------------------------------------------------------------')
# reduce and rename columns
_pmeds = pmeds\
  .select(['Person_ID_DEID', 'ProcessingPeriodDate', 'PrescribedBNFCode'])\
  .withColumnRenamed('Person_ID_DEID', 'PERSON_ID')\
  .withColumnRenamed('ProcessingPeriodDate', 'DATE')\
  .withColumnRenamed('PrescribedBNFCode', 'CODE')

# check
count_var(_pmeds, 'PERSON_ID'); print()

# add censor dates
_pmeds = _pmeds\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_pmeds, 'PERSON_ID'); print()

# check before CENSOR_DATE_END, accounting for nulls
# note: checked in curated_data for potential columns to use in the case of null DATE - none
# ...
_pmeds = _pmeds\
  .withColumn('_tmp1',\
    f.when((f.col('DATE').isNull()), 1)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') <= f.col('CENSOR_DATE_END')), 2)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') >  f.col('CENSOR_DATE_END')), 3)\
  )
tmpt = tab(_pmeds, '_tmp1'); print()

# filter to before CENSOR_DATE_END
# ...
_pmeds = _pmeds\
  .where(f.col('_tmp1').isin([2]))\
  .drop('_tmp1')

# check
count_var(_pmeds, 'PERSON_ID'); print()

# check on or after CENSOR_DATE_END_less_180d
# ...
_pmeds = _pmeds\
  .withColumn('CENSOR_DATE_END_less_180d', f.date_add(f.col('CENSOR_DATE_END'), -180))\
  .withColumn('_tmp2',\
    f.when((f.col('DATE') >= f.col('CENSOR_DATE_END_less_180d')), 1)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_END_less_180d')), 2)\
  )
tmpt = tab(_pmeds, '_tmp2'); print()

# filter to on or after CENSOR_DATE_END_less_12m
# ...
_pmeds = _pmeds\
  .where(f.col('_tmp2').isin([1]))\
  .drop('_tmp2')

# check
count_var(_pmeds, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('cache')
print('--------------------------------------------------------------------------------------')
_gdppr.cache()
print(f'_gdppr        {_gdppr.count():,}')
_hes_apc.cache()
print(f'_hes_apc      {_hes_apc.count():,}')
_hes_apc_oper.cache()
print(f'_hes_apc_oper {_hes_apc_oper.count():,}')
_pmeds.cache()
print(f'_pmeds        {_pmeds.count():,}')

# COMMAND ----------

# MAGIC %md # 3 Medical history

# COMMAND ----------

# MAGIC %md ## 3.1 Codelist

# COMMAND ----------

print('------------------------------------------------------------------------------')
print('codelist for medical history (hx)')
print('------------------------------------------------------------------------------')
# not BNF (medications, addressed separately below) 
# not smoking (addressed separately below) 
codelist_hx = codelist\
  .where(\
    (f.col('terminology') != 'BNF')\
    & ~(f.col('name').rlike('^smoking_.*$'))
  )

# check recognised terminologies only
tmpt = tab(codelist_hx, 'name', 'terminology', var2_unstyled=1)
_list_terms = list(\
  codelist_hx\
    .select('terminology')\
    .distinct()\
    .toPandas()['terminology']\
  )
assert set(_list_terms) <= set(['DMD', 'SNOMED', 'ICD10', 'OPCS4'])

# check isid name code
assert codelist_hx.select('name', 'code').dropDuplicates().count() == codelist_hx.count()

# partition codelist_hx into codelists for gdppr (DMD, SNOMED) and hes_apc (ICD10)
codelist_hx_gdppr = codelist_hx\
  .where(f.col('terminology').isin(['DMD', 'SNOMED']))
codelist_hx_hes_apc = codelist_hx\
  .where(f.col('terminology').isin(['ICD10']))


print('------------------------------------------------------------------------------')
print('add composite events to the codelists')
print('------------------------------------------------------------------------------')
# note: temporary code - composite events to be defined as a column within the codelist going forward
# ...
codelist_hx_composite = {}
for term in ['gdppr', 'hes_apc']:
  print(term)
  _tmpm = []
  for i, c in enumerate(composite_events):
    print(' ', i, c, '=', composite_events[c])
    _tmp = globals()[f'codelist_hx_{term}']\
      .where(f.col('name').isin(composite_events[c]))\
      .withColumnRenamed('name', 'name_old')\
      .withColumn('name', f.lit(c))     
    if(i == 0): _tmpm = _tmp
    else: _tmpm = _tmpm.unionByName(_tmp)      
  tmpt = tab(_tmpm, 'name_old', 'name', var2_unstyled=1); print()  
  codelist_hx_composite[term] = _tmpm

# gdppr
codelist_hx_gdppr = codelist_hx_gdppr\
  .withColumn('name_old', f.lit(''))\
  .unionByName(codelist_hx_composite['gdppr'])
 
# check
tmpt = tab(codelist_hx_gdppr, 'name' , 'terminology', var2_unstyled=1); print()  
  
# hes_apc
codelist_hx_hes_apc = codelist_hx_hes_apc\
  .withColumn('name_old', f.lit(''))\
  .unionByName(codelist_hx_composite['hes_apc'])

# check
tmpt = tab(codelist_hx_hes_apc, 'name' , 'terminology', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md ## 3.2 Create

# COMMAND ----------

# dictionary - dataset, codelist, and ordering in the event of tied records
_hx_in = {
    'gdppr':   ['_gdppr',   'codelist_hx_gdppr', 2]
  , 'hes_apc': ['_hes_apc', 'codelist_hx_hes_apc',  1]
}

# run codelist match and codelist match summary functions
_hx, _hx_1st, _hx_1st_wide = codelist_match(_hx_in, _name_prefix=f'cov_hx_')
_hx_summ_name, _hx_summ_name_code = codelist_match_summ(_hx_in, _hx)

# COMMAND ----------

# MAGIC %md ## 3.2 Check

# COMMAND ----------

# check
display(_hx_1st_wide)

# COMMAND ----------

# plot
_tmp = _hx_1st\
  .withColumn('diff', f.datediff(f.col('DATE'), f.col('CENSOR_DATE_END'))/365.25)
_tmpp = _tmp\
  .toPandas()

fig, axes = plt.subplots(10, 5, figsize=(13,2*10), sharex=True) # , sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 2)
names = ['hes_apc', 'gdppr']  
  
vlist = list(_tmpp[['name']].drop_duplicates().sort_values('name')['name']) 
for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, v)
  tmp2d1 = _tmpp[(_tmpp[f'diff'] > -30) & (_tmpp[f'name'] == v)]
  s1 = list(tmp2d1[tmp2d1[f'source'] == 'hes_apc'][f'diff'])
  s2 = list(tmp2d1[tmp2d1[f'source'] == 'gdppr'][f'diff'])
  
  ax.hist([s1, s2], bins = list(np.linspace(-30,0,100)), stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'{v}')
  ax.xaxis.set_tick_params(labelbottom=True)
  if(i==0): ax.legend(loc='upper left')
  #ax.legend(loc='upper left')
  
plt.tight_layout();
display(fig)

# COMMAND ----------

# check codelist match summary by name
display(_hx_summ_name)

# COMMAND ----------

# check codelist match summary by name and code
display(_hx_summ_name_code)

# COMMAND ----------

# MAGIC %md 
# MAGIC # 4 Medication history

# COMMAND ----------

# MAGIC %md ## 4.1 Codelist

# COMMAND ----------

codelist_meds = codelist\
  .where(f.col('terminology') == 'BNF')

# check
tmpt = tab(codelist_meds, 'name', 'terminology', var2_unstyled=1)

# COMMAND ----------

# MAGIC %md ## 4.2 Create

# COMMAND ----------

# dictionary - dataset, codelist, and ordering in the event of tied records
_meds_in = {
  'pmeds': ['_pmeds', 'codelist_meds',  1]
}

# run codelist match and codelist match summary functions
_meds, _meds_1st, _meds_1st_wide = codelist_match(_meds_in, _name_prefix=f'cov_meds_')
_meds_summ_name, _meds_summ_name_code = codelist_match_summ(_meds_in, _meds)

# COMMAND ----------

# MAGIC %md ## 4.3 Check

# COMMAND ----------

# check result
display(_meds_1st_wide)

# COMMAND ----------

# plot
_tmp = _meds_1st\
  .withColumn('diff', f.datediff(f.col('DATE'), f.col('CENSOR_DATE_END')))
_tmpp = _tmp\
  .toPandas()

fig, axes = plt.subplots(2, 5, figsize=(13,2*2), sharex=True) # , sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 1)
names = ['pmeds']  
  
vlist = list(_tmpp[['name']].drop_duplicates().sort_values('name')['name']) 
for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, v)
  tmp2d1 = _tmpp[(_tmpp[f'diff'] > -190) & (_tmpp[f'name'] == v)]
  s1 = list(tmp2d1[f'diff'])
    
  ax.hist([s1], bins = 90, color=colors, label=names) # normed=True stacked=True
  ax.set_title(f'{v}')
  if(i==0): ax.legend(loc='upper left')
  #ax.legend(loc='upper left')

plt.tight_layout();
display(fig)

# COMMAND ----------

# check codelist match summary by name
display(_meds_summ_name)

# COMMAND ----------

# check codelist match summary by name and code
display(_meds_summ_name_code)

# COMMAND ----------

# MAGIC %md # 5 Smoking status

# COMMAND ----------

# MAGIC %md ## 5.1 Codelist

# COMMAND ----------

codelist_smoking = codelist\
  .where(f.col('name').rlike('^smoking_.*$'))

# check
tmpt = tab(codelist_smoking, 'name', 'terminology', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md ## 5.2 Create

# COMMAND ----------

# dictionary - dataset, codelist, and ordering in the event of tied records
_smoking_in = {
  'gdppr': ['_gdppr', 'codelist_smoking',  1]
}

# run codelist match and codelist match summary functions
_smoking, _smoking_last, _smoking_last_wide = codelist_match(_smoking_in, _name_prefix=f'cov_', _last_event=1)
_smoking_summ_name, _smoking_summ_name_code = codelist_match_summ(_smoking_in, _smoking)

# get max date and corresponding var
# identify ties
# define smoking status, priority ordering = current, ex, never
# identify possible upgrades to never status based on evidence of current or ex
vlist = ['cov_smoking_current_date', 'cov_smoking_ex_date', 'cov_smoking_never_date']
_smoking_last_wide = _smoking_last_wide\
  .withColumn('cov_smoking_max_date', f.greatest(*[f.col(v) for v in vlist]))\
  .withColumn('cov_smoking_max_var', f.concat_ws(';', f.array([f.when(f.col(v) == f.col('cov_smoking_max_date'), v).otherwise(None) for v in vlist])))\
  .withColumn('cov_smoking_status',\
    f.when(f.col('cov_smoking_max_var').rlike('current'), 'Current')\
     .when(f.col('cov_smoking_max_var').rlike('ex'), 'Ex')\
     .when(f.col('cov_smoking_max_var').rlike('never'), 'Never')\
  )\
  .withColumn('cov_smoking_status_tie', f.when(f.col('cov_smoking_max_var').rlike(';'), 1))\
  .withColumn('cov_smoking_status_upgrade_never',\
    f.when(\
      (f.col('cov_smoking_status') == 'Never')\
      & ((f.col('cov_smoking_ex_date').isNotNull()) | (f.col('cov_smoking_current_date').isNotNull()))\
    , 1)\
  )
  # drop('cov_smoking_max_date', 'cov_smoking_max_var', 'cov_smoking_status_tie', 'cov_smoking_status_upgrade_never')

# COMMAND ----------

# MAGIC %md ## 5.3 Check

# COMMAND ----------

# check result
display(_smoking_last_wide)

# COMMAND ----------

# check ties and possible upgrades to never status
tmpt = tab(_smoking_last_wide, 'cov_smoking_status_tie'); print()
tmpt = tab(_smoking_last_wide, 'cov_smoking_status', 'cov_smoking_status_upgrade_never', var2_unstyled=1); print()

# COMMAND ----------

# plot
_tmp = _smoking_last_wide\
  .withColumn('diff', f.datediff(f.col('cov_smoking_max_date'), f.col('CENSOR_DATE_END'))/365.25)
_tmpp = _tmp\
  .toPandas()

fig, axes = plt.subplots(1, 3, figsize=(13,4), sharex=True) # sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 1)
names = ['gdppr'] # , 'gdppr_snomed', 'hes_ae', 'hes_apc', 'hes_op']  
  
vlist = ['Never', 'Ex', 'Current']  
for i, (ax, cat) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, cat)
  tmp2d1 = _tmpp[_tmpp[f'cov_smoking_status'] == cat]
  tmp2d1 = tmp2d1[tmp2d1[f'diff'] > -20]
  s1 = list(tmp2d1[f'diff'])
  
  ax.hist([s1], bins = 50, stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'{cat}')
  if(i==0): ax.legend(loc='upper left')
    
plt.tight_layout();
display(fig)

# COMMAND ----------

# check codelist match summary by name
display(_smoking_summ_name)

# COMMAND ----------

# check codelist match summary by name and code
display(_smoking_summ_name_code)

# COMMAND ----------

# MAGIC %md # 6 BMI

# COMMAND ----------

# MAGIC %md ## 6.1 Codelist

# COMMAND ----------

# Based on CCU002
# note from CCU002: The snomed codes were dervied by doing a string search and selecting the most relevant codes (this was in an R script) 
# note: BMI codes to be added to the codelist going forward
codelist_bmi_values = [
  '722595002'
  , '914741000000103'
  , '914731000000107'
  , '914721000000105'
  , '35425004'
  , '48499001'
  , '301331008'
  , '6497000'
  , '310252000'
  , '427090001'
  , '408512008'
  , '162864005'
  , '162863004'
  , '412768003'
  , '60621009'
  , '846931000000101'
]
codelist_bmi_values = spark.createDataFrame(pd.DataFrame(codelist_bmi_values, columns = ['code']))
count_var(codelist_bmi_values, 'code'); print()

path_snomed_refset = 'dss_corporate.gpdata_snomed_refset_full'
snomed_refset = gp_refset\
  .withColumnRenamed('SNOMED_conceptId', 'code')\
  .withColumnRenamed('SNOMED_conceptId_description', 'term')
count_var(snomed_refset, 'code'); print()

codelist_bmi_values = merge(snomed_refset, codelist_bmi_values, ['code']); print()
assert all(codelist_bmi_values.toPandas()['_merge'].isin(['both', 'left_only']))

codelist_bmi_values = codelist_bmi_values\
  .where(f.col('_merge') == 'both')\
  .select('code', 'term')\
  .distinct()\
  .withColumn('name', f.lit('BMI_values'))
count_var(codelist_bmi_values, 'code'); print()

print(codelist_bmi_values.toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md ## 6.2 Prepare

# COMMAND ----------

# gdppr
# note: VALUE2_CONDITION is missing for all
_gdppr_bmi = gdppr\
  .withColumn('_id', f.monotonically_increasing_id())\
  .select(['NHS_NUMBER_DEID', 'DATE', 'RECORD_DATE', 'CODE', 'VALUE1_CONDITION', '_id'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')

# check
count_var(_gdppr_bmi, 'PERSON_ID'); print()

# add individual censor dates
_gdppr_bmi = _gdppr_bmi\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_gdppr_bmi, 'PERSON_ID'); print()

# check before CENSOR_DATE_END, accounting for nulls and using RECORD_DATE where needed
# ...
_gdppr_bmi = _gdppr_bmi\
  .withColumn('_tmp1',\
    f.when((f.col('DATE').isNull()) & (f.col('RECORD_DATE').isNull()), 1)\
     .when((f.col('DATE').isNull()) & (f.col('RECORD_DATE').isNotNull()) & (f.col('RECORD_DATE') <= f.col('CENSOR_DATE_END')), 2)\
     .when((f.col('DATE').isNull()) & (f.col('RECORD_DATE').isNotNull()) & (f.col('RECORD_DATE') > f.col('CENSOR_DATE_END')), 3)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') <= f.col('CENSOR_DATE_END')), 4)\
     .when((f.col('DATE').isNotNull()) & (f.col('DATE') >  f.col('CENSOR_DATE_END')), 5)\
  )
tmpt = tab(_gdppr_bmi, '_tmp1'); print()

# filter to before CENSOR_DATE_END
# ...
_gdppr_bmi = _gdppr_bmi\
  .where(f.col('_tmp1').isin([2, 4]))\
  .withColumn('DATE', f.when(f.col('DATE').isNull(), f.col('RECORD_DATE')).otherwise(f.col('DATE')))\
  .drop('_tmp1')

# check
count_var(_gdppr_bmi, 'PERSON_ID'); print()

# check on or after CENSOR_DATE_START, using RECORD_DATE where needed (often in the case of dummy/erroneous DATE)
# note: nulls were replaced in previous data step
# ...
_gdppr_bmi = _gdppr_bmi\
  .withColumn('_tmp2',\
    f.when((f.col('DATE') >= f.col('CENSOR_DATE_START')), 1)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') <  f.col('CENSOR_DATE_START')), 2)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') >= f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') <= f.col('CENSOR_DATE_END')), 3)\
     .when((f.col('DATE') <  f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') >= f.col('CENSOR_DATE_START')) & (f.col('RECORD_DATE') >  f.col('CENSOR_DATE_END')), 4)\
  )
tmpt = tab(_gdppr_bmi, '_tmp2'); print()

# filter to on or after CENSOR_DATE_START
# ...
_gdppr_bmi = _gdppr_bmi\
  .where(f.col('_tmp2').isin([1, 3]))\
  .drop('_tmp2', 'RECORD_DATE')

# check
count_var(_gdppr_bmi, 'PERSON_ID'); print()

# cache
_gdppr_bmi.cache()
print(f'_gdppr_bmi {_gdppr_bmi.count():,}')

# COMMAND ----------

# MAGIC %md ## 6.3 Create

# COMMAND ----------

# codelist matching
_tmp_codelist_bmi_values = codelist_bmi_values\
  .select(['code', 'name'])
_bmi_values = _gdppr_bmi\
  .join(_tmp_codelist_bmi_values, on='code', how='inner')

# check
count_var(_bmi_values, 'PERSON_ID'); print()
tmpb1 = _bmi_values\
  .groupBy('code', 'name')\
  .agg(\
    f.count(f.lit(1)).alias('n')\
    , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id')\
  )

# filter null and out of range values
_bmi_values = _bmi_values\
  .where(\
    (f.col('VALUE1_CONDITION').isNotNull())\
    & (f.col('VALUE1_CONDITION') >= 12)\
    & (f.col('VALUE1_CONDITION') <= 100)\
  )

# check
count_var(_bmi_values, 'PERSON_ID'); print()        
tmpb2 = _bmi_values\
  .groupBy('code', 'name')\
  .agg(\
    f.count(f.lit(1)).alias('n_2')\
    , f.countDistinct(f.col('PERSON_ID')).alias(f'n_id_2')\
  )
tmpb3 = tmpb1\
  .join(tmpb2, on=['code', 'name'], how='outer')
print(tmpb3.toPandas().to_string()); print()

# filter to the last (latest / most recent) recorded status
_win = Window\
  .partitionBy('PERSON_ID', 'name')\
  .orderBy(f.desc('DATE'), '_id')
_bmi_values_last = _bmi_values\
  .withColumn('_rownum', f.row_number().over(_win))\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'VALUE1_CONDITION', 'DATE')\
  .withColumnRenamed('VALUE1_CONDITION', 'cov_BMI_value')\
  .withColumnRenamed('DATE', 'cov_BMI_date')\
  .withColumn('cov_BMI_value_obese', f.when(f.col('cov_BMI_value') >= 30, 1).otherwise(0))\
  .orderBy('PERSON_ID')

# identify ties 
_win = Window\
  .partitionBy(['PERSON_ID', 'name'])\
  .orderBy(f.col('DATE').desc())
_bmi_values_last_tie = _bmi_values\
  .withColumn('_tie_bmi_value', f.dense_rank().over(_win))\
  .where(f.col('_tie_bmi_value') == 1)\
  .groupBy('PERSON_ID')\
  .agg(\
    f.countDistinct(f.col('VALUE1_CONDITION')).alias(f'_tie_bmi_value_n_distinct')\
    , f.countDistinct(f.when(f.col('VALUE1_CONDITION').isNull(), 1)).alias(f'_tie_bmi_value_null')\
    , f.sort_array(f.collect_set(f.col('VALUE1_CONDITION'))).alias('_tie_bmi_value_list')\
  )\
  .withColumn('_tie_bmi_value', f.when((f.col('_tie_bmi_value_n_distinct') + f.col(f'_tie_bmi_value_null')) > 1, 1).otherwise(0))\
  .select('PERSON_ID', '_tie_bmi_value', '_tie_bmi_value_list')\
  .drop('_tie_bmi_value_list')
_bmi_values_last = _bmi_values_last\
  .join(_bmi_values_last_tie, on=['PERSON_ID'], how='left')

count_var(_bmi_values_last, 'PERSON_ID'); print() 
tmpt = tab(_bmi_values_last, '_tie_bmi_value'); print() 

# COMMAND ----------

# MAGIC %md ## 6.3 Check

# COMMAND ----------

# check result
display(_bmi_values_last)

# COMMAND ----------

# plot cov_BMI_value
tmp = _bmi_values_last\
  .select('cov_BMI_value')\
  .withColumn('cov_BMI_value', f.round(f.col('cov_BMI_value'), 2).cast('double'))\
  .where(f.col('cov_BMI_value').isNotNull())\
  .toPandas()

fig, axes = plt.subplots(1, 1, figsize=(13,4*1), sharex=True) # sharey=True , dpi=100) # 
colors = sns.color_palette("tab10", 1)
names = ['gdppr']   
# vlist = ['DOB']  
# for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  # print(i, ax, v)
tmp2d1 = tmp # dd2[dd2[f'_diff_{v}'] > -20]
s1 = list(tmp2d1[f'cov_BMI_value'])
axes.hist([s1], bins = 100, stacked=True, color=colors, label=names) # normed=True
axes.set_title(f'BMI')
axes.legend(loc='upper left')
#ax.legend(loc='upper left')
plt.tight_layout();
display(fig)

# COMMAND ----------

# plot
_tmp = _bmi_values_last\
  .withColumn('diff', f.datediff(f.col('cov_BMI_date'), f.col('CENSOR_DATE_END'))/365.25)
_tmpp = _tmp\
  .toPandas()

fig, axes = plt.subplots(1, 2, figsize=(13/2,4*1), sharex=True) # sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 1)
names = ['gdppr'] 
  
vlist = [0, 1]  
for i, (ax, cat) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, cat)
  tmp2d1 = _tmpp[_tmpp[f'cov_BMI_value_obese'] == cat]
  tmp2d1 = tmp2d1[tmp2d1[f'diff'] > -20]
  s1 = list(tmp2d1[f'diff'])
  
  ax.hist([s1], bins = 50, stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'cov_BMI_value_obese == {cat}')
  if(i==0): ax.legend(loc='upper left')
    
plt.tight_layout();
display(fig)

# COMMAND ----------

# MAGIC %md # 7 Surgery in last year

# COMMAND ----------

# MAGIC %md ## 7.1 Codelist

# COMMAND ----------

# Based on CCU002
codelist_oper = [chr(i) for i in range(ord('A'),ord('U'))] + [chr(i) for i in range(ord('V'),ord('X'))]
print(codelist_oper)

# COMMAND ----------

# MAGIC %md ## 7.2 Create

# COMMAND ----------

# check
count_var(_hes_apc_oper, 'PERSON_ID'); print()

# prepare
_oper = _hes_apc_oper\
  .withColumn('CODE_1', f.substring(f.col('CODE'), 1, 1))\
  .withColumn('DIAG_DIGITS', f.col('DIAG_DIGITS').cast(t.IntegerType()))
  
# check
tmpt = tab(_oper, 'CODE_1'); print()
tmpt = tab(_oper, 'DIAG_DIGITS'); print()

# filter to chapters
_oper = _oper\
  .where(f.col('CODE_1').isin(codelist_oper))

tmpt = tab(_oper, 'CODE_1'); print()
count_var(_oper, 'PERSON_ID'); print()

# first event
_win = Window\
  .partitionBy(['PERSON_ID'])\
  .orderBy('DATE', 'EPIKEY', f.desc('DIAG_DIGITS'), 'DIAG_POSITION')
_oper_1st = _oper\
  .withColumn('_rownum', f.row_number().over(_win))\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'DATE', 'CODE', 'CODE_1')\
  .withColumn('cov_surg_last_yr_flag', f.when(f.col('DATE').isNotNull(), 1))\
  .withColumnRenamed('DATE', 'cov_surg_last_yr_date')\
  .withColumnRenamed('CODE', 'cov_surg_last_yr_code')\
  .withColumnRenamed('CODE_1', 'cov_surg_last_yr_code1')\
  .orderBy('PERSON_ID')\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'cov_surg_last_yr_flag', 'cov_surg_last_yr_date', 'cov_surg_last_yr_code', 'cov_surg_last_yr_code1')

# check
count_var(_oper_1st, 'PERSON_ID'); print()
tmpt = tab(_oper_1st, 'cov_surg_last_yr_code1'); print()
tmpt = tab(_oper_1st, 'cov_surg_last_yr_code'); print()

# COMMAND ----------

# MAGIC %md ## 7.3 Check

# COMMAND ----------

# check 
display(_oper_1st)

# COMMAND ----------

_tmp = _oper_1st\
  .withColumn('diff', f.datediff(f.col('cov_surg_last_yr_date'), f.col('CENSOR_DATE_END'))/365.25)
_tmpp = _tmp\
  .toPandas()

fig, axes = plt.subplots(4, 5, figsize=(13,2*5), sharex=True, sharey=True) # , sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 1)
names = ['hes_apc']  
  
vlist = list(_tmpp[['cov_surg_last_yr_code1']].drop_duplicates().sort_values('cov_surg_last_yr_code1')['cov_surg_last_yr_code1']) 
for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, v)
  tmp2d1 = _tmpp[(_tmpp[f'diff'] > -30) & (_tmpp[f'cov_surg_last_yr_code1'] == v)]
  s1 = list(tmp2d1[f'diff'])
  
  ax.hist([s1], bins = 100, stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'{v}')
  ax.xaxis.set_tick_params(labelbottom=True)
  if(i==0): ax.legend(loc='upper left')
  #ax.legend(loc='upper left')

plt.tight_layout();
display(fig)

# COMMAND ----------

# summarise by code
_oper_summ_code = _oper\
  .where(f.col('DIAG_DIGITS') == 4)\
  .groupBy('CODE')\
  .agg(\
    f.count(f.lit(1)).alias('n')\
    , f.countDistinct(f.col('PERSON_ID')).alias('n_id')\
  )\
  .orderBy('CODE')

# check
display(_oper_summ_code.where(f.col('CODE').rlike('^Q')))
# display(_oper_summ_code.where(f.col('CODE').rlike('^R')))

# COMMAND ----------

# MAGIC %md # 8 Number of disorders

# COMMAND ----------

# TODO - Not priority

# # ## 3.5 Number of disorders
# df1 = gdppr.select(["LSOA", "NHS_NUMBER_DEID", "RECORD_DATE", "CODE"]).filter(gdppr["RECORD_DATE"].isNotNull())
# #display(df1)

# #Lookup obtained from running notebook in functions folder called 'get_children'

# df2= spark.sql("select * from dars_nic_391419_j3w9t_collab.ccu002_vac_disorder_lookup_full") 

# df2=df2.select("CONCEPT_ID")

# df = df1.join(df2, df1['code']==df2['CONCEPT_ID'], how='inner')
# #df.show()

# df = df.filter(df.RECORD_DATE >= to_date(lit(previous_year_date))).filter(df.RECORD_DATE < to_date(lit(index_date))).select("NHS_NUMBER_DEID", "RECORD_DATE", "CONCEPT_ID").dropDuplicates()
# #display(df)

# df_disorder = df.groupBy("NHS_NUMBER_DEID").agg(f.countDistinct("CONCEPT_ID").alias("N_DISORDER"))
# #display(df_disorder)

# #This could either be count distinct record_date or count distinct concept_id, Ive gone with conceptID because googledoc says number of unique codes over the last year (rather than consultations)

# df_disorder= df_disorder.dropDuplicates()
# df_disorder = df_disorder.withColumnRenamed('NHS_NUMBER_DEID', 'ID')
# df_disorder.createOrReplaceGlobalTempView(f"{project_prefix}cov_disorders")

# COMMAND ----------

# MAGIC %md # 9 Number of unique BNF chapters

# COMMAND ----------

# MAGIC %md ## 9.1 Create

# COMMAND ----------

# check
count_var(_pmeds, 'PERSON_ID'); print()

# create
_unique_bnf_chapters = _pmeds\
  .withColumn('_bnf_chapter', f.substring(f.col('CODE'), 1, 2))\
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', '_bnf_chapter')\
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')\
  .agg(f.countDistinct(f.col('_bnf_chapter')).alias('cov_n_unique_bnf_chapters'))

# check
count_var(_unique_bnf_chapters, 'PERSON_ID'); print()

# COMMAND ----------

# MAGIC %md ## 9.2 Check

# COMMAND ----------

# check result
display(_unique_bnf_chapters)

# COMMAND ----------

# check
tmpt = tab(_unique_bnf_chapters, 'cov_n_unique_bnf_chapters')
tmpt = tabstat(_unique_bnf_chapters, 'cov_n_unique_bnf_chapters')

# COMMAND ----------

# MAGIC %md # F Save

# COMMAND ----------

# tidy 
_hx_1st_wide = _hx_1st_wide

# tidy 
_smoking_last_wide = _smoking_last_wide\
  .drop('cov_smoking_max_date', 'cov_smoking_max_var', 'cov_smoking_tie')\
  .withColumnRenamed('cov_smoking_status_upgrade_never', 'cov_smoking_status_conflict')

_oper_1st = _oper_1st\
  .drop('cov_surg_last_yr_code1')

# check
count_var(_hx_1st_wide, 'PERSON_ID'); print()
count_var(_meds_1st_wide, 'PERSON_ID'); print()
count_var(_smoking_last_wide, 'PERSON_ID'); print()
count_var(_bmi_values_last, 'PERSON_ID'); print()
count_var(_oper_1st, 'PERSON_ID'); print()
count_var(_unique_bnf_chapters, 'PERSON_ID'); print()

# join
tmp1 = _hx_1st_wide\
  .join(_meds_1st_wide,       on=['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], how='outer')\
  .join(_smoking_last_wide,   on=['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], how='outer')\
  .join(_bmi_values_last,     on=['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], how='outer')\
  .join(_oper_1st,            on=['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], how='outer')\
  .join(_unique_bnf_chapters, on=['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], how='outer')\

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
outName = f'{proj}_out_covariates'.lower()

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

# save codelist match summary tables
list_tables = []
list_tables = list_tables + ['_hx_summ_name', '_hx_summ_name_code']
list_tables = list_tables + ['_meds_summ_name', '_meds_summ_name_code']
list_tables = list_tables + ['_smoking_summ_name', '_smoking_summ_name_code']
for i, table in enumerate(list_tables):
  print(i, table)
  outName = f'{proj}_out_codelist_match_cov{table}'.lower()
  tmp1 = globals()[table]
  tmp1.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
  spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')
  print(f'  saved {dbc}.{outName}')

# COMMAND ----------

# checks
# tmpt = new
# for v in [col for col in list(new.columns) if re.match(f'^cov_hx_', col)]:
#   tmpt = tmpt\
#     .withColumn(v + '_flag', f.when(f.col(v).isNotNull(), 1).otherwise(0)) 
# for i, c in enumerate(composite_events):
#   print(' ', i, c, '=', composite_events[c]); print()
#   tmpt = tmpt\
#     .withColumn('tmpchk_' + c, sum([f.col(f'cov_hx_' + v.lower() + '_flag') for v in composite_events[c]]))\
#     .withColumn('tmpchk_' + c, f.when(f.col('tmpchk_' + c) > 0, 1).otherwise(0))\
#     .withColumn('tmpchk_' + c + '_null_equality', udf_null_safe_equality(f'cov_hx_' + c + '_flag', 'tmpchk_' + c))
#   tmpr = tab(tmpt, f'cov_hx_' + c + '_flag', 'tmpchk_' + c, var2_unstyled=1); print()
#   assert tmpt.select('tmpchk_' + c + '_null_equality').where(f.col('tmpchk_' + c + '_null_equality') == False).count() == 0

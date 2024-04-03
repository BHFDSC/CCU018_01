# Databricks notebook source
# MAGIC %md # CCU018_01-D11-outcomes_during_pregnancy
# MAGIC
# MAGIC **Description** This notebook creates the outcomes during pregnancy for the CCU018_01 project, which uses the codelist together with GDPPR, HES APC, and Deaths. 
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** out_codelist, out_cohort, in_gdppr, cur_hes_apc_long, cur_deaths_long
# MAGIC
# MAGIC **Data output** ccu018_01_out_outcomes_dur_preg

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

out_prefix = 'dur_'

# COMMAND ----------

# MAGIC %md # 1 Data

# COMMAND ----------

# spark.sql(f"""REFRESH TABLE {path_cur_deaths_long}""")
codelist     = spark.table(path_out_codelist)
cohort       = spark.table(path_out_cohort)
gdppr        = spark.table(path_in_gdppr)
hes_apc_long = spark.table(path_cur_hes_apc_long)
deaths_long  = spark.table(path_cur_deaths_long)

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

print('--------------------------------------------------------------------------------------')
print('individual_censor_dates')
print('--------------------------------------------------------------------------------------')
# reduce and rename columns
individual_censor_dates = cohort\
  .select('PERSON_ID', 'preg_start_date', 'delivery_date')\
  .withColumnRenamed('preg_start_date', 'CENSOR_DATE_START')\
  .withColumnRenamed('delivery_date', 'CENSOR_DATE_END')

# check
count_var(individual_censor_dates, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('gdppr')
print('--------------------------------------------------------------------------------------')
# reduce and rename columns
_gdppr = gdppr\
  .select(['NHS_NUMBER_DEID', 'DATE', 'CODE'])\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')

# check
count_var(_gdppr, 'PERSON_ID'); print()

# add individual censor dates
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
print('hes_apc')
print('--------------------------------------------------------------------------------------')
# filter to primary diagnosis position
# reduce and rename columns
_hes_apc = hes_apc_long\
  .where(f.col('DIAG_POSITION') == 1)\
  .select(['PERSON_ID', 'EPISTART', 'DIAG_POSITION', 'CODE'])\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc, 'PERSON_ID'); print()
tmpt = tab(_hes_apc, 'DIAG_POSITION'); print()

# add individual censor dates
_hes_apc = _hes_apc\
  .drop('DIAG_POSITION')\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_hes_apc, 'PERSON_ID'); print()

# filter to after CENSOR_DATE_START and on or before CENSOR_DATE_END
_hes_apc = _hes_apc\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_hes_apc, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('deaths')
print('--------------------------------------------------------------------------------------')
# reduce and rename columns
# filter to underlying diagnosis position
_deaths = deaths_long\
  .where(f.col('DIAG_POSITION') == 'UNDERLYING')\
  .select(['PERSON_ID', 'DATE', 'DIAG_POSITION', 'CODE'])

# check
count_var(_deaths, 'PERSON_ID'); print()
tmpt = tab(_deaths, 'DIAG_POSITION'); print()

# add individual censor dates
_deaths = _deaths\
  .drop('DIAG_POSITION')\
  .join(individual_censor_dates, on='PERSON_ID', how='inner')

# check
count_var(_deaths, 'PERSON_ID'); print()

# filter to after CENSOR_DATE_START and on or before CENSOR_DATE_END
_deaths = _deaths\
  .where(\
    (f.col('DATE') > f.col('CENSOR_DATE_START'))\
    & (f.col('DATE') <= f.col('CENSOR_DATE_END'))\
  )

# check
count_var(_deaths, 'PERSON_ID'); print()


print('--------------------------------------------------------------------------------------')
print('cache')
print('--------------------------------------------------------------------------------------')
_gdppr.cache()
print(f'_gdppr   {_gdppr.count():,}')
_hes_apc.cache()
print(f'_hes_apc {_hes_apc.count():,}')
_deaths.cache()
print(f'_deaths  {_deaths.count():,}')

# COMMAND ----------

# MAGIC %md # 3 Outcomes

# COMMAND ----------

# MAGIC %md ## 3.1 Codelist

# COMMAND ----------

# check
display(codelist)

# COMMAND ----------

print('------------------------------------------------------------------------------')
print('codelist for outcomes')
print('------------------------------------------------------------------------------')
# check
tmpt = tab(codelist, 'name' , 'terminology', var2_unstyled=1); print()

# outcomes codelist
# remove codes that have been flagged as covariate only
codelist_out = codelist\
  .where(\
    (f.col('name').isin([\
      'AMI'
      , 'DIC'                                                       
      , 'DVT'
      , 'DVT_other'  
      , 'DVT_pregnancy'                                                       
      , 'HF'
      , 'ICVT'
      , 'ICVT_pregnancy'      
      , 'PE'                                                       
      , 'PVT'   
      , 'TTP'                                                          
      , 'angina'
      , 'angina_unstable'    
      , 'artery_dissect'                                                       
      , 'cardiomyopathy'
      , 'life_arrhythmias'
      , 'mesenteric_thrombus'                                                       
      , 'myocarditis'    
      , 'other_arterial_embolism'                                                       
      , 'pericarditis'
      , 'stroke_HS'
      , 'stroke_IS'
      , 'stroke_NOS'      
      , 'stroke_SAH' 
      , 'stroke_TIA' 
      , 'thrombocytopenia'
      , 'thrombophilia'  
      , 'gest_diabetes'
      , 'gest_hypertension'
      , 'preeclampsia'
    ]))\
    & ((f.col('covariate_only').isNull()) | (f.col('covariate_only') != 1))\
    & ((f.col('code_type').isNull()) | (f.col('code_type') == '') | (f.col('code_type') == 1))\
  )

# check
tmpt = tab(codelist_out, 'name' , 'terminology', var2_unstyled=1); print()

# partition codelist_hx into codelists for gdppr (SNOMED) and hes_apc, deaths (ICD10)
# snomed 
# restrict to a subset of names as per CCU002
codelist_out_snomed = codelist_out\
  .where(\
    (f.col('terminology') == 'SNOMED')\
    & (f.col('name').isin([\
      'AMI'
      , 'HF'
      , 'angina'
      , 'angina_unstable'
      , 'stroke_IS'
      , 'stroke_NOS'
      , 'stroke_TIA'
      , 'gest_diabetes'
      , 'gest_hypertension'
      , 'preeclampsia'
    ]))\
  )

# check
tmpt = tab(codelist_out_snomed, 'name' , 'terminology', var2_unstyled=1); print()

# icd10
codelist_out_icd10 = codelist_out\
  .where(f.col('terminology') == 'ICD10')

# check
tmpt = tab(codelist_out_icd10, 'name' , 'terminology', var2_unstyled=1); print()


print('------------------------------------------------------------------------------')
print('add composite events to the codelists')
print('------------------------------------------------------------------------------')
# note: temporary code - composite events to be defined as a column within the codelist going forward
# ...
codelist_out_composite = {}
for term in ['snomed', 'icd10']:
  print(term)
  _tmpm = []
  for i, c in enumerate(composite_events):
    print(' ', i, c, '=', composite_events[c])
    _tmp = globals()[f'codelist_out_{term}']\
      .where(f.col('name').isin(composite_events[c]))\
      .withColumnRenamed('name', 'name_old')\
      .withColumn('name', f.lit(c))     
    if(i == 0): _tmpm = _tmp
    else: _tmpm = _tmpm.unionByName(_tmp)
  tmpt = tab(_tmpm, 'name_old', 'name', var2_unstyled=1); print()  
  codelist_out_composite[term] = _tmpm

# snomed
codelist_out_snomed = codelist_out_snomed\
  .withColumn('name_old', f.lit(''))\
  .unionByName(codelist_out_composite['snomed'])
 
# check
tmpt = tab(codelist_out_snomed, 'name' , 'terminology', var2_unstyled=1); print()  
  
# icd10
codelist_out_icd10 = codelist_out_icd10\
  .withColumn('name_old', f.lit(''))\
  .unionByName(codelist_out_composite['icd10'])

# check
tmpt = tab(codelist_out_icd10, 'name' , 'terminology', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md ## 3.2 Create

# COMMAND ----------

# dictionary - dataset, codelist, and ordering in the event of tied records
_out_dur_in = {
    'gdppr':   ['_gdppr',   'codelist_out_snomed', 2]
  , 'hes_apc': ['_hes_apc', 'codelist_out_icd10',  1]
  , 'deaths':  ['_deaths',  'codelist_out_icd10',  3]
}

# run codelist match and codelist match summary functions
_out_dur, _out_dur_1st, _out_dur_1st_wide = codelist_match(_out_dur_in, _name_prefix=f'out_{out_prefix}')
_out_dur_summ_name, _out_dur_summ_name_code = codelist_match_summ(_out_dur_in, _out_dur)

# COMMAND ----------

# MAGIC %md ## 3.3 Check

# COMMAND ----------

# check result
display(_out_dur_1st_wide)

# COMMAND ----------

# plot
_tmp = _out_dur_1st\
  .withColumn('diff', f.datediff(f.col('DATE'), f.col('CENSOR_DATE_START'))/365.25)
_tmpp = _tmp\
  .toPandas()

# toggle sharey=True
rows_of_5 = np.ceil(len(_tmpp['name'].drop_duplicates())/5).astype(int)
fig, axes = plt.subplots(rows_of_5, 5, figsize=(13,2*rows_of_5), sharex=True) # , sharey=True , dpi=100) # 
 
colors = sns.color_palette("tab10", 3)
names = ['hes_apc', 'gdppr', 'deaths']  
  
vlist = list(_tmpp[['name']].drop_duplicates().sort_values('name')['name']) 
for i, (ax, v) in enumerate(zip(axes.flatten(), vlist)):
  print(i, ax, v)
  tmp2d1 = _tmpp[(_tmpp[f'diff'] > -30) & (_tmpp[f'name'] == v)]
  s1 = list(tmp2d1[tmp2d1[f'source'] == 'hes_apc'][f'diff'])
  s2 = list(tmp2d1[tmp2d1[f'source'] == 'gdppr'][f'diff'])
  s3 = list(tmp2d1[tmp2d1[f'source'] == 'deaths'][f'diff'])
  ax.hist([s1, s2, s3], bins = list(np.linspace(0,1,100)), stacked=True, color=colors, label=names) # normed=True
  ax.set_title(f'{v}')
  ax.xaxis.set_tick_params(labelbottom=True)
  if(i==0): ax.legend(loc='upper right')

plt.tight_layout();
display(fig)

# COMMAND ----------

# check 
_tmp1 = _tmp\
  .withColumn('byvar', f.concat_ws('_', 'name', 'source'))
tmpt = tabstat(_tmp1, 'diff', byvar='byvar')

# COMMAND ----------

# check codelist match summary by name
display(_out_dur_summ_name)

# COMMAND ----------

# check codelist match summary by name and code
display(_out_dur_summ_name_code)

# COMMAND ----------

# MAGIC %md # 4 Save

# COMMAND ----------

tmp1 = _out_dur_1st_wide

# check
count_var(tmp1, 'PERSON_ID'); print()  

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

# save name
outName = f'{proj}_out_outcomes_{out_prefix}preg'.lower()

# save previous version for comparison purposes
_datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
outName_pre = f'{outName}_pre{_datetimenow}'.lower()
print(outName_pre)
spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')
# 20220722_192507 - created a previous version ready for rerun, but realised rerun not need - not impacted by fu_end_date change, so deleted the previous version to avoid confusion. Remove this note when next previous version is created.

# save
tmp2.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

# COMMAND ----------

# save codelist match summary tables
list_tables = ['_out_dur_summ_name', '_out_dur_summ_name_code']
for i, table in enumerate(list_tables):
  print(i, table)
  outName = f'{proj}_out_codelist_match{table}'.lower()
  tmp1 = globals()[table]
  tmp1.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
  spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')
  print(f'  saved {dbc}.{outName}')

# COMMAND ----------

# # checks
# tmpt = tab(new, 'out_dur_dic'); print()
# tmpt = tab(new, 'out_dur_pvt'); print()
# tmpt = tab(new, 'out_dur_ttp'); print()

# tmpt = new
# for v in [col for col in list(new.columns) if re.match(f'^out_{out_prefix}', col)]:
#   tmpt = tmpt\
#     .withColumn(v + '_flag', f.when(f.col(v).isNotNull(), 1).otherwise(0)) 
# for i, c in enumerate(composite_events):
#   print(' ', i, c, '=', composite_events[c]); print()
#   tmpt = tmpt\
#     .withColumn('tmpchk_' + c, sum([f.col(f'out_{out_prefix}' + v.lower() + '_flag') for v in composite_events[c]]))\
#     .withColumn('tmpchk_' + c, f.when(f.col('tmpchk_' + c) > 0, 1).otherwise(0))\
#     .withColumn('tmpchk_' + c + '_null_equality', udf_null_safe_equality(f'out_{out_prefix}' + c + '_flag', 'tmpchk_' + c))
#   tmpr = tab(tmpt, f'out_{out_prefix}' + c + '_flag', 'tmpchk_' + c, var2_unstyled=1); print()
#   assert tmpt.select('tmpchk_' + c + '_null_equality').where(f.col('tmpchk_' + c + '_null_equality') == False).count() == 0

# Databricks notebook source
# MAGIC %md # CCU018_01-D08-inclusion_exclusion
# MAGIC  
# MAGIC **Description** This notebook creates the cohort by applying the inclusion/exclusion criteria for the CCU018_01 project. Namely, pregnancy start age (including 18 <= age < 55) and quality assurance. Note: further inclusion/exclusion is to be applied at the analysis stage. Namely, pregnancy start age (excluding age > 49) and pregnancy duration (excluding < 12 weeks). A flow diagram (table) was used to record each stage. 
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** tmp_cohort, tmp_skinny, tmp_quality_assurance
# MAGIC
# MAGIC **Data output** ccu018_01_out_cohort, ccu018_01_out_skinny\
# MAGIC (ccu018_01_tmp_flow_inc_exc)

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

# MAGIC %md # 1 Data

# COMMAND ----------

cohort  = spark.table(path_tmp_cohort)
skinny  = spark.table(path_tmp_skinny)
qa      = spark.table(path_tmp_quality_assurance)

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print('skinny')
print('---------------------------------------------------------------------------------')
# reduce
_skinny = skinny\
  .select('PERSON_ID', 'DOB')

# merge with checks
tmpp1 = merge(cohort, _skinny, ['PERSON_ID'], validate='1:1', assert_results=['both'], indicator=0); print()

# tidy
# calculate age
tmpp1 = tmpp1\
  .withColumn('preg_start_age', f.datediff(f.col('preg_start_date'), f.col('DOB'))/365.25)\
  .drop('DOB')

# check
tmpt = tabstat(tmpp1, 'preg_start_age'); print()
 

print('---------------------------------------------------------------------------------')
print('quality assurance')
print('---------------------------------------------------------------------------------')
# reduce
_qa = qa\
  .select('PERSON_ID', '_rule_total')

# check 
tmpt = tab(_qa, '_rule_total')

# merge with checks
tmpp2 = merge(tmpp1, _qa, ['PERSON_ID'], validate='1:1', assert_results=['both'], indicator=0); print()

# COMMAND ----------

# MAGIC %md # 3 Inclusion / exclusion

# COMMAND ----------

tmp0 = tmpp2

# check
tmpc = count_var(tmp0, 'PERSON_ID', ret=1, df_desc='original', indx=12); print()

# COMMAND ----------

# MAGIC %md ## 3.1 Age

# COMMAND ----------

# check
tmpt = tabstat(tmp0, 'preg_start_age')

# filter age
tmp1 = tmp0\
  .where((f.col('preg_start_age') >= 18) & (f.col('preg_start_age') < 55))

# check
tmpt = tabstat(tmp1, 'preg_start_age'); print()
tmpt = tmp0\
  .where((f.col('preg_start_age') < 18) | (f.col('preg_start_age') >= 55))\
  .withColumn('preg_start_age', f.col('preg_start_age').cast(t.IntegerType()))
tmpt = tab(tmpt, 'preg_start_age'); print()

# check
tmpt = count_var(tmp1, 'PERSON_ID', ret=1, df_desc='post exclusion age', indx=13); print()
tmpc = tmpc.unionByName(tmpt)

# tidy
tmp1 = tmp1\
  .drop('preg_start_age')

# COMMAND ----------

# MAGIC %md ## 3.2 Quality assurance

# COMMAND ----------

# check
tmpt = tab(tmp1, '_rule_total'); print()

# filter quality assurance
tmp2 = tmp1\
  .where((f.col('_rule_total').isNull()) | (f.col('_rule_total') == 0))\
  .na.fill(0, subset=['_rule_total'])

# check
tmpt = tab(tmp2, '_rule_total'); print()

# check
tmpt = count_var(tmp2, 'PERSON_ID', ret=1, df_desc='post exclusion quality assurance', indx=14); print()
tmpc = tmpc.unionByName(tmpt)

# tidy
tmp2 = tmp2\
  .drop('_rule_total')

# COMMAND ----------

# MAGIC %md # 4 Flow diagram

# COMMAND ----------

# combine with tmp cohort flow
path_tmp_flow_cohort = f'{dbc}.{proj}_tmp_flow_cohort'
spark.sql(f"""REFRESH TABLE {path_tmp_flow_cohort}""")
tmpc1 = spark.table(path_tmp_flow_cohort)\
  .unionByName(tmpc)\
  .withColumn('indx', f.col('indx').cast(t.IntegerType()))\
  .orderBy('indx')\
  .where(~((f.col('indx') == 12) & (f.col('df_desc') == 'original')))

# check
print(tmpc1.toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md # 5 Save

# COMMAND ----------

# MAGIC %md ## 5.1 Cohort

# COMMAND ----------

# check final
display(tmp2)

# COMMAND ----------

# save name
outName = f'{proj}_out_cohort'.lower()

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

tmp2 = spark.table(f'{dbc}.{proj}_out_cohort')
display(tmp2)

# COMMAND ----------

# MAGIC %md ## 5.2 Skinny

# COMMAND ----------

# restrict skinny to the inclusion / exclusion cohort
# merge with checks
tmp3 = merge(skinny, tmp2, ['PERSON_ID'], validate='1:1', assert_results=['both', 'left_only'], keep_results=['both'], indicator=0); print()

# COMMAND ----------

# save name
outName = f'{proj}_out_skinny'.lower()

# save previous version for comparison purposes
_datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
outName_pre = f'{outName}_pre{_datetimenow}'.lower()
print(outName_pre)
spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')

# save
tmp3.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

# COMMAND ----------

tmp3 = spark.table(f'{dbc}.{proj}_out_skinny')
display(tmp3)

# COMMAND ----------

tmp3a = (
  tmp3
  .withColumn('preg_dur', f.datediff(f.col('delivery_date'), f.col('preg_start_date'))/7)
  .withColumn('flag_preg_dur_lt_12', f.when(f.col('preg_dur') < 12, 1).otherwise(0))
  .withColumn('flag_delivery_age_gt_49', f.when(f.col('delivery_age') > 49, 1).otherwise(0))
)
tmpt = tabstat(tmp3a, 'preg_dur'); print()
tmpt = tab(tmp3a, 'flag_preg_dur_lt_12'); print()
tmpt = tabstat(tmp3a, 'preg_dur', byvar='flag_preg_dur_lt_12'); print()

tmpt = tabstat(tmp3a, 'delivery_age'); print()
tmpt = tab(tmp3a, 'flag_delivery_age_gt_49'); print()
tmpt = tabstat(tmp3a, 'delivery_age', byvar='flag_delivery_age_gt_49'); print()

tmpt = tab(tmp3a, 'flag_preg_dur_lt_12', 'flag_delivery_age_gt_49'); print()

# COMMAND ----------

tmpt = tab(tmp3a, 'flag_preg_dur_lt_12', 'flag_delivery_age_gt_49'); print()

# COMMAND ----------

# MAGIC %md ## 5.3 Flow

# COMMAND ----------

# save name
outName = f'{proj}_tmp_flow_inc_exc'.lower()

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
tmpc1.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

# COMMAND ----------

# check flow table
outName = f'{proj}_tmp_flow_inc_exc'.lower()
tmpp = (
  spark.table(f'{dbc}.{outName}')
  .orderBy('indx')
  .select('indx', 'df_desc', 'n', 'n_id', 'n_id_distinct')
  .withColumnRenamed('df_desc', 'stage')
  .toPandas())
tmpp['n'] = tmpp['n'].astype(int)
tmpp['n_id'] = tmpp['n_id'].astype(int)
tmpp['n_id_distinct'] = tmpp['n_id_distinct'].astype(int)
tmpp['n_diff'] = (tmpp['n'] - tmpp['n'].shift(1)).fillna(0).astype(int)
tmpp['n_id_diff'] = (tmpp['n_id'] - tmpp['n_id'].shift(1)).fillna(0).astype(int)
tmpp['n_id_distinct_diff'] = (tmpp['n_id_distinct'] - tmpp['n_id_distinct'].shift(1)).fillna(0).astype(int)
for v in [col for col in tmpp.columns if re.search("^n.*", col)]:
  tmpp.loc[:, v] = tmpp[v].map('{:,.0f}'.format)
for v in [col for col in tmpp.columns if re.search(".*_diff$", col)]:  
  tmpp.loc[tmpp['stage'] == 'original', v] = ''
# tmpp = tmpp.drop('indx', axis=1)
print(tmpp.to_string()); print()

# Databricks notebook source
# MAGIC %md # CCU018_01-D06-skinny
# MAGIC  
# MAGIC **Description** This notebook creates the key patient characteristics table based on work from CCU002 (previously known as the skinny patient table). Information on age (MYOB), sex, ethnicity, LSOA, and date of death are harmonised and selected from multiple data sources (i.e., GDPPR, HES APC, HES AE, HES OP, Deaths) for the cohort of interest for the CCU018_01 project. A single record is created for each individual prioritising the most recent non-null non-unkown information. Ethnicity codes are mapped to descriptions and categories; and LSOA codes are mapped to region and the index of multiple deprivation (IMD).   
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** in_gdppr, in_hes_apc, in_hes_ae, in_hes_op, in_deaths, tmp_cohort, cur_ethnic_desc_cat, cur_lsoa_region, cur_lsoa_imd
# MAGIC
# MAGIC **Data output** ccu018_01_tmp_skinny (ccu018_01_tmp_skinny_unassembled, ccu018_01_tmp_skinny_assembled)

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

# DBTITLE 1,Functions_skinny
# MAGIC %run "../shds/common/skinny"

# COMMAND ----------

# MAGIC %md # 0 Parameters

# COMMAND ----------

# MAGIC %run "./CCU018_01-D01-parameters"

# COMMAND ----------

# MAGIC %md # 1 Data

# COMMAND ----------

# spark.sql(f"""REFRESH TABLE {path_in_hes_apc}""")
# spark.sql(f"""REFRESH TABLE {path_in_hes_op}""")
# spark.sql(f"""REFRESH TABLE {path_in_hes_ae}""")
# spark.sql(f"""REFRESH TABLE {path_in_gdppr}""")
# spark.sql(f"""REFRESH TABLE {path_in_deaths}""")
# spark.sql(f"""REFRESH TABLE {path_tmp_cohort}""")
# spark.sql(f"""REFRESH TABLE {path_cur_ethnic_desc_cat}""")
# spark.sql(f"""REFRESH TABLE {path_cur_lsoa_region}""")
# spark.sql(f"""REFRESH TABLE {path_cur_lsoa_imd}""")

hes_apc         = spark.table(path_in_hes_apc)
hes_op          = spark.table(path_in_hes_op)
hes_ae          = spark.table(path_in_hes_ae)
gdppr           = spark.table(path_in_gdppr)
deaths          = spark.table(path_in_deaths)
cohort          = spark.table(path_tmp_cohort)
ethnic_desc_cat = spark.table(path_cur_ethnic_desc_cat)
lsoa_region     = spark.table(path_cur_lsoa_region)
lsoa_imd        = spark.table(path_cur_lsoa_imd)

# COMMAND ----------

# MAGIC %md # 2 Skinny

# COMMAND ----------

# individual_censor_dates
# note: decision made to change the individual cut off date from preg_start_date to delivery_date 
#       this was to ensure that ~8% of women without gdppr or hes records on or before the preg_start_date are included
# reduce and rename columns
individual_censor_dates = cohort\
  .select('PERSON_ID', 'delivery_date')\
  .withColumnRenamed('delivery_date', 'CENSOR_DATE')

# check
count_var(individual_censor_dates, 'PERSON_ID'); print()
count_var(hes_apc, 'PERSON_ID_DEID'); print()

# skinny
# Note: individual censor dates option used
#       prioritise primary care records turned off - happy to take the most recent HES (delivery) record
unassembled = skinny_unassembled(_hes_apc=hes_apc, _hes_ae=hes_ae, _hes_op=hes_op, _gdppr=gdppr, _deaths=deaths)

# temp save
outName = f'{proj}_tmp_skinny_unassembled'.lower()
unassembled.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')
unassembled = spark.table(f'{dbc}.{outName}')

assembled = skinny_assembled(_unassembled=unassembled, _individual_censor_dates=individual_censor_dates, _prioritise_primary_care=0)

# temp save
outName = f'{proj}_tmp_skinny_assembled'.lower()
assembled.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')
assembled = spark.table(f'{dbc}.{outName}')

# COMMAND ----------

# check
# count_var(unassembled, 'PERSON_ID'); print()
count_var(assembled, 'PERSON_ID'); print()

# COMMAND ----------

# check
display(assembled)

# COMMAND ----------

# MAGIC %md # 3 Ethnic desc cat

# COMMAND ----------

# add ethnicity description and category

# check
count_var(ethnic_desc_cat, 'ETHNIC'); print()
count_var(assembled, 'ETHNIC'); print()

# merge (right_only) - equivalent to left join
assembled = merge(assembled, ethnic_desc_cat, ['ETHNIC'])\
  .where(f.col('_merge') != 'right_only')
print()

# check
tmpt = tab(assembled, 'ETHNIC', '_merge', var2_unstyled=1); print()
count_var(assembled, 'PERSON_ID'); print()

# check
tmpt = tab(assembled, 'ETHNIC_DESC', 'ETHNIC_CAT', var2_unstyled=1); print()
tmpt = tab(assembled, 'ETHNIC_CAT'); print()

# edit
assembled = assembled\
  .withColumn('ETHNIC_CAT',\
    f.when(f.col('ETHNIC_CAT').isNull(), 'Unknown')\
     .otherwise(f.col('ETHNIC_CAT'))\
  )

# check
tmpt = tab(assembled, 'ETHNIC_CAT'); print()

# tidy
assembled = assembled\
  .drop('_merge')

# COMMAND ----------

# MAGIC %md # 4 Region

# COMMAND ----------

# prepare
lsoa_region = lsoa_region\
  .select('LSOA', 'region')

# check
count_var(lsoa_region, 'LSOA'); print()
count_var(assembled, 'LSOA'); print()
tmpt = tab(lsoa_region, 'region'); print()

# merge
assembled = merge(assembled, lsoa_region, ['LSOA'])\
  .where(f.col('_merge') != 'right_only')\
  .withColumn('LSOA_1', f.substring(f.col('LSOA'), 1, 1))
print()

# check
tmpt = tab(assembled, 'LSOA_1', '_merge', var2_unstyled=1); print()
count_var(assembled, 'PERSON_ID'); print()

# edit
assembled = assembled\
  .withColumn('region',\
    f.when(f.col('LSOA_1') == 'W', 'Wales')\
     .when(f.col('LSOA_1') == 'S', 'Scotland')\
     .otherwise(f.col('region'))\
  )

# check
tmpt = tab(assembled, 'region'); print()

# tidy
assembled = assembled\
  .drop('_merge', 'LSOA_1')

# COMMAND ----------

# MAGIC %md # 5 IMD

# COMMAND ----------

# check
count_var(lsoa_imd, 'LSOA'); print()
count_var(assembled, 'LSOA'); print()
tmpt = tab(lsoa_imd, 'IMD_2019_DECILES', 'IMD_2019_QUINTILES', var2_unstyled=1); print()

# merge
assembled = merge(assembled, lsoa_imd, ['LSOA'])\
  .where(f.col('_merge') != 'right_only')\
  .withColumn('LSOA_1', f.substring(f.col('LSOA'), 1, 1))
print()

# check
tmpt = tab(assembled, 'LSOA_1', '_merge', var2_unstyled=1); print()
count_var(assembled, 'PERSON_ID'); print()

# tidy
assembled = assembled\
  .drop('_merge', 'LSOA_1')

# COMMAND ----------

# MAGIC %md # 6 Checks

# COMMAND ----------

# check
count_var(assembled, 'PERSON_ID')

# COMMAND ----------

# check final
display(assembled)

# COMMAND ----------

# see data_checks/...skinny_check for further checks

# COMMAND ----------

# MAGIC %md # 7 Save

# COMMAND ----------

# # test faster save
# tmp1 = spark.table(f'{dbc}.{proj}_tmp_skinny_assembled')\
#   .withColumn('delivery_age', f.datediff(f.col('CENSOR_DATE'), f.col('DOB'))/365.25)\
#   .join(ethnic_desc_cat, on='ETHNIC', how='left')\
#   .withColumn('ETHNIC_CAT',\
#     f.when(f.col('ETHNIC_CAT').isNull(), 'Unknown')\
#     .otherwise(f.col('ETHNIC_CAT'))\
#   )\
#   .join(lsoa_region.select('LSOA', 'region'), on='LSOA', how='left')\
#   .withColumn('region',\
#     f.when(f.substring(f.col('LSOA'), 1, 1) == 'W', 'Wales')\
#     .when(f.substring(f.col('LSOA'), 1, 1) == 'S', 'Scotland')\
#     .otherwise(f.col('region'))\
#   )\
#   .join(lsoa_imd, on='LSOA', how='left')

# # reorder columns
# vlist_ordered = [
#   'PERSON_ID'
#   , 'DOB'
#   , 'delivery_age'
#   , 'SEX'
#   , 'ETHNIC'
#   , 'ETHNIC_DESC'
#   , 'ETHNIC_CAT'
#   , 'LSOA'
#   , 'region'
#   , 'IMD_2019_DECILES'
#   , 'IMD_2019_QUINTILES'
#   , 'DOD'
#   , 'CENSOR_DATE'
#   , 'in_gdppr'
#   , '_date_DOB'
#   , '_date_SEX'
#   , '_date_ETHNIC'
#   , '_date_LSOA'
#   , '_date_DOD'  
#   , '_source_DOB'
#   , '_source_SEX'
#   , '_source_ETHNIC'
#   , '_source_LSOA'
#   , '_source_DOD'   
#   , '_tie_DOB'
#   , '_tie_SEX'
#   , '_tie_ETHNIC'
#   , '_tie_LSOA'
#   , '_tie_DOD'
# ]
# vlist_unordered = [v for v in tmp1.columns if v not in vlist_ordered]
# vlist_all = vlist_ordered + vlist_unordered
# tmp1 = tmp1\
#   .select(*vlist_all)

# COMMAND ----------

# save name
# 20220807 changed from _out_skinny to _tmp_skinny (not an "out" table - as number of individuals will be higher than the final cohort)
outName = f'{proj}_tmp_skinny'.lower()

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
assembled.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

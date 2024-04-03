# Databricks notebook source
# MAGIC %md # CCU018_01-D04-table_freeze
# MAGIC  
# MAGIC **Description** This notebook creates the table freezes (data snapshots) that have been filtered to the relatively small cohort of interest for the CCU018_01 project, which will be used to reduce the runtime of subsequent notebooks of the data curation pipeline.
# MAGIC  
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** ccu018_01_tmp_cohort
# MAGIC
# MAGIC **Data output** ccu018_01_in_... (for all tables in the df_archive table specified in the parameters notebook)

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

# spark.sql(f"""REFRESH TABLE {path_tmp_cohort}""")
cohort = spark.table(path_tmp_cohort)

# temporary code for partial runs
# df_archive = df_archive[4:5].reset_index(drop=True)
# df_archive = df_archive[df_archive['dataset'] != 'hes_apc_otr'].reset_index(drop=True)
# df_archive = df_archive[df_archive['dataset'] == 'sus'].reset_index(drop=True)
# df_archive

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

# check
count_var(cohort, 'PERSON_ID')

# cohort id
_cohort_id = cohort\
  .select('PERSON_ID')\
  .distinct()

# check
count_var(_cohort_id, 'PERSON_ID')

# COMMAND ----------

# MAGIC %md # 3 Freeze tables

# COMMAND ----------

spark.conf.set('spark.sql.legacy.allowCreatingManagedTableUsingNonemptyLocation', 'true')

_datetimenow_all = _datetimenow
for ind in df_archive.index:
  row = df_archive.iloc[ind]
  
  tabpath = row['database'] + '.' + row['table'] 
  prodDate = row['productionDate']
  id = row['idVar']
  print(f'{0 if ind<10 else ""}' + str(ind) + ' ' + tabpath + ' (' + prodDate + ')' + ' [' + id + ']', flush=True)
  
  # get archive table
  tmp = spark.table(tabpath)\
    .where(f.col('ProductionDate') == prodDate)  
  count1 = tmp.count()
  print(f'   {count1:,}')
  if(count1 == 0):
    raise ValueError("count == 0, is ProductionDate valid for this dataset?")   
  
  if(row['dataset'] == 'hes_apc_otr'):
    # add PERSON_ID_DEID
    tmpo = spark.table(f'{dbc}.hes_apc_all_years_archive')\
      .select('ProductionDate', 'EPIKEY', 'PERSON_ID_DEID')\
      .where(f.col('ProductionDate') == prodDate)\
      .drop('ProductionDate')
    
    # check
    count_var(tmp, 'EPIKEY')
    count_var(tmpo, 'EPIKEY')
    
    # merge
    tmp = merge(tmp, tmpo, ['EPIKEY'])
    assert tmp.where(f.col('_merge') != 'both') == 0
    tmp = tmp\
      .drop('_merge')
  
  # get cohort id with harmonised ID for join 
  tmpc = _cohort_id\
    .withColumnRenamed('PERSON_ID', id)  
  
  # join
  _datetimenow = datetime.datetime.now()
  tmp = tmp\
    .join(tmpc, on=[id], how='inner')\
    .withColumn('freezeDate', f.lit(_datetimenow))
  print(f'   {tmp.count():,} ({tmp.select(id).distinct().count():,})')
  
  tableName = row['table']
  
  # save name
  outName = f'{proj}_in_{tableName}'  
  
  # save previous version for comparison purposes
  tmpt = spark.sql(f"""SHOW TABLES FROM {dbc}""")\
    .select('tableName')\
    .where(f.col('tableName') == outName)\
    .collect()
  if(len(tmpt)>0):
    _datetimenow = _datetimenow.strftime("%Y%m%d_%H%M%S")
    outName_pre = f'{outName}_pre{_datetimenow}'.lower()
    # spark.sql(f'ALTER TABLE {dbc}.{outName} RENAME TO {dbc}.{outName}_pre20220522')
    spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
    spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')  
    print('   saved previous version of ' + outName + ' as ' + outName_pre, flush=True)
  else:
    print('   ' + outName + ' does not exist', flush=True)
 
  # save  
  tmp.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
  spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')
  print('   saved ' + outName, flush=True)

# COMMAND ----------

# 20220808
# 00 dars_nic_391419_j3w9t_collab.deaths_dars_nic_391419_j3w9t_archive (2022-03-31 00:00:00.000000) [DEC_CONF_NHS_NUMBER_CLEAN_DEID]
#    saved previous version of ccu018_01_in_deaths_dars_nic_391419_j3w9t_archive as ccu018_01_in_deaths_dars_nic_391419_j3w9t_archive_pre20220808_075624
#    saved ccu018_01_in_deaths_dars_nic_391419_j3w9t_archive
# 01 dars_nic_391419_j3w9t_collab.gdppr_dars_nic_391419_j3w9t_archive (2022-03-31 00:00:00.000000) [NHS_NUMBER_DEID]
#    saved previous version of ccu018_01_in_gdppr_dars_nic_391419_j3w9t_archive as ccu018_01_in_gdppr_dars_nic_391419_j3w9t_archive_pre20220808_075741
#    saved ccu018_01_in_gdppr_dars_nic_391419_j3w9t_archive
# 02 dars_nic_391419_j3w9t_collab.hes_apc_all_years_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_hes_apc_all_years_archive as ccu018_01_in_hes_apc_all_years_archive_pre20220808_082201
#    saved ccu018_01_in_hes_apc_all_years_archive
# 03 dars_nic_391419_j3w9t_collab.hes_apc_mat_all_years_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_hes_apc_mat_all_years_archive as ccu018_01_in_hes_apc_mat_all_years_archive_pre20220808_083348
#    saved ccu018_01_in_hes_apc_mat_all_years_archive
# 04 dars_nic_391419_j3w9t_collab.hes_op_all_years_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_hes_op_all_years_archive as ccu018_01_in_hes_op_all_years_archive_pre20220808_083646
#    saved ccu018_01_in_hes_op_all_years_archive
# 05 dars_nic_391419_j3w9t_collab.hes_ae_all_years_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_hes_ae_all_years_archive as ccu018_01_in_hes_ae_all_years_archive_pre20220808_090541
#    saved ccu018_01_in_hes_ae_all_years_archive
# 06 dars_nic_391419_j3w9t_collab.primary_care_meds_dars_nic_391419_j3w9t_archive (2022-03-31 00:00:00.000000) [Person_ID_DEID]
#    saved previous version of ccu018_01_in_primary_care_meds_dars_nic_391419_j3w9t_archive as ccu018_01_in_primary_care_meds_dars_nic_391419_j3w9t_archive_pre20220808_091132
#    saved ccu018_01_in_primary_care_meds_dars_nic_391419_j3w9t_archive
# 07 dars_nic_391419_j3w9t_collab.hes_cc_all_years_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_hes_cc_all_years_archive as ccu018_01_in_hes_cc_all_years_archive_pre20220808_093550
#    saved ccu018_01_in_hes_cc_all_years_archive
# 08 dars_nic_391419_j3w9t_collab.chess_dars_nic_391419_j3w9t_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_chess_dars_nic_391419_j3w9t_archive as ccu018_01_in_chess_dars_nic_391419_j3w9t_archive_pre20220808_093635
#    saved ccu018_01_in_chess_dars_nic_391419_j3w9t_archive
# 09 dars_nic_391419_j3w9t_collab.sgss_dars_nic_391419_j3w9t_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_sgss_dars_nic_391419_j3w9t_archive as ccu018_01_in_sgss_dars_nic_391419_j3w9t_archive_pre20220808_093712
#    saved ccu018_01_in_sgss_dars_nic_391419_j3w9t_archive
# 10 dars_nic_391419_j3w9t_collab.sus_dars_nic_391419_j3w9t_archive (2022-03-31 00:00:00.000000) [NHS_NUMBER_DEID]
#    saved previous version of ccu018_01_in_sus_dars_nic_391419_j3w9t_archive as ccu018_01_in_sus_dars_nic_391419_j3w9t_archive_pre20220808_093948
#    saved ccu018_01_in_sus_dars_nic_391419_j3w9t_archive
# 11 dars_nic_391419_j3w9t_collab.vaccine_status_dars_nic_391419_j3w9t_archive (2022-03-31 00:00:00.000000) [PERSON_ID_DEID]
#    saved previous version of ccu018_01_in_vaccine_status_dars_nic_391419_j3w9t_archive as ccu018_01_in_vaccine_status_dars_nic_391419_j3w9t_archive_pre20220808_094315
#    saved ccu018_01_in_vaccine_status_dars_nic_391419_j3w9t_archive

# Databricks notebook source
# MAGIC %md # CCU018_01-D09-covariates
# MAGIC  
# MAGIC **Description** This notebook creates the supplementary covariates for the CCU018_01 project, which are defined with respect to the start of pregnancy for each individual and includes variables for a history of pregnancy (combining three different sources of information) and a history of multiple gestation (combining two different sources of information). 
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** out_cohort, out_covariates, tmp_cohort_del, in_hes_apc, hes_apc_mat
# MAGIC
# MAGIC **Data output** ccu018_01_out_covariates_supp

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


hes_apc_mat_all   = get_archive_table('hes_apc_mat') # all years
# hes_apc_mat       = spark.table(path_tmp_hes_apc_mat) # 1920, 2021, 2122
hes_apc           = spark.table(path_in_hes_apc) # cohort-specific
delivery_record   = spark.table(path_tmp_cohort_del)
cohort            = spark.table(path_out_cohort)
covariates        = spark.table(path_out_covariates)

# COMMAND ----------

# restrict delivery_record to the inclusion / exclusion cohort
# merge with checks
delivery_record = merge(delivery_record, cohort.select('PERSON_ID'), ['PERSON_ID'], validate='1:1', assert_results=['both', 'left_only'], keep_results=['both'], indicator=0); print()

# COMMAND ----------

# MAGIC %md # 2 cov_hx_pregnancy

# COMMAND ----------

# MAGIC %md ## 2.1 delvar

# COMMAND ----------

# delvar = delivery record variables (from the delivery record that we have used to define the cohort)

# NUMPREG (Number of Previous Pregnancies): The number of previous pregnancies that resulted in a registrable birth (live or still born). 

# check
assert dict(delivery_record.dtypes)['NUMPREG'] == 'int'
tmpt = tab(delivery_record, 'NUMPREG'); print()

# create
tmp1 = delivery_record\
  .select('PERSON_ID', 'NUMPREG')\
  .withColumn('cov_hx_pregnancy_delvar',\
    f.when(f.col('NUMPREG') == 99, None)\
    .when(f.col('NUMPREG') > 0, 1)\
    .otherwise(f.col('NUMPREG'))\
  )

# check
tmpt = tab(tmp1, 'NUMPREG', 'cov_hx_pregnancy_delvar', var2_unstyled=1); print()
tmpt = tab(tmp1, 'cov_hx_pregnancy_delvar'); print()

# tidy
hx_pregnancy_delvar = tmp1\
  .select('PERSON_ID', 'cov_hx_pregnancy_delvar')

# check
count_var(hx_pregnancy_delvar, 'PERSON_ID'); print()
print(hx_pregnancy_delvar.limit(10).toPandas().to_string()); print()

# cache
hx_pregnancy_delvar.cache()
print(f'{hx_pregnancy_delvar.count():,}')

# COMMAND ----------

# MAGIC %md ## 2.2 delrecs

# COMMAND ----------

# delrecs = delivery records (all delivery record before the cohort baseline)

# ------------------------------------------------------------------------------
# individual_baseline_dates
# ------------------------------------------------------------------------------
# baseline
individual_baseline_dates = cohort\
  .select('PERSON_ID', 'PERSON_ID_hes_apc_mat', '_id_agree', 'EPIKEY', 'preg_start_date')\
  .withColumnRenamed('PERSON_ID', 'PERSON_ID_hes_apc')\
  .withColumnRenamed('PERSON_ID_hes_apc_mat', 'PERSON_ID')\
  .withColumnRenamed('preg_start_date', 'BASELINE_DATE')\
  .select('PERSON_ID', 'PERSON_ID_hes_apc', 'BASELINE_DATE')

# check
count_var(individual_baseline_dates, 'PERSON_ID'); print()
print(individual_baseline_dates.limit(10).toPandas().to_string()); print()


# ------------------------------------------------------------------------------
# hes_apc_mat_all (all years) 
# ------------------------------------------------------------------------------
# Note: hes_apc_mat is 1920, 2021, 2122 only
_hes_apc_mat_all = hes_apc_mat_all\
  .where(f.col('MATERNITY_EPISODE_TYPE').isin([1]))\
  .select('EPIKEY', 'PERSON_ID_DEID', 'NUMBABY', 'NUMTAILB')\
  .withColumnRenamed('PERSON_ID_DEID', 'PERSON_ID')

# check
count_var(_hes_apc_mat_all, 'PERSON_ID'); print()

# merge with checks
_hes_apc_mat_all = merge(individual_baseline_dates, _hes_apc_mat_all, ['PERSON_ID'], validate='1:m', assert_results=['both', 'right_only'], keep_results=['both'], indicator=0); print()

# check
count_var(_hes_apc_mat_all, 'EPIKEY'); print()
count_var(_hes_apc_mat_all, 'PERSON_ID'); print()
print(_hes_apc_mat_all.limit(10).toPandas().to_string()); print()


# ------------------------------------------------------------------------------
# hes_apc
# ------------------------------------------------------------------------------
# reduce and rename columns
_hes_apc = hes_apc\
  .select('EPIKEY', 'EPISTART')\
  .withColumnRenamed('EPISTART', 'DATE')

# check
count_var(_hes_apc, 'EPIKEY'); print()

# merge hes_apc to obtain EPISTART
_hes_apc_mat_all = merge(_hes_apc_mat_all, _hes_apc, ['EPIKEY']); print()

# check
assert _hes_apc_mat_all.select('_merge').where(f.col('_merge') == 'left_only').count() == 3
print(_hes_apc_mat_all.where(f.col('_merge') == 'left_only').toPandas().to_string()); print()

# note: queried with NHSD why a small number of EPIKEYs from _hes_apc_mat_all are not matched in _hes_apc

# tidy
_hes_apc_mat_all = _hes_apc_mat_all\
  .where(f.col('_merge') == 'both')\
  .drop('_merge')

# check
count_var(_hes_apc_mat_all, 'EPIKEY'); print()
count_var(_hes_apc_mat_all, 'PERSON_ID'); print()
print(_hes_apc_mat_all.limit(10).toPandas().to_string()); print()

# filter to on or before baseline
_hes_apc_mat_all = _hes_apc_mat_all\
  .where(f.col('DATE') <= f.col('BASELINE_DATE'))

# check
count_var(_hes_apc_mat_all, 'PERSON_ID'); print()
count_var(_hes_apc_mat_all, 'PERSON_ID_hes_apc'); print()

# finalise
hx_pregnancy_delrecs = _hes_apc_mat_all\
  .select('PERSON_ID_hes_apc')\
  .withColumnRenamed('PERSON_ID_hes_apc', 'PERSON_ID')\
  .distinct()\
  .withColumn('cov_hx_pregnancy_delrecs', f.lit(1))

# check
count_var(hx_pregnancy_delrecs, 'PERSON_ID'); print()
print(hx_pregnancy_delrecs.limit(10).toPandas().to_string()); print()

# cache
hx_pregnancy_delrecs.cache()
print(f'{hx_pregnancy_delrecs.count():,}')

# COMMAND ----------

# MAGIC %md ## 2.3 gencode

# COMMAND ----------

# gencode = general codelist match
hx_pregnancy_gencode = covariates\
  .select('PERSON_ID', 'cov_hx_pregnancy_flag')\
  .withColumnRenamed('cov_hx_pregnancy_flag', 'cov_hx_pregnancy_gencode')

# check
count_var(hx_pregnancy_gencode, 'PERSON_ID'); print()
print(hx_pregnancy_gencode.limit(10).toPandas().to_string()); print()

# cache
hx_pregnancy_gencode.cache()
print(f'{hx_pregnancy_gencode.count():,}')

# COMMAND ----------

# MAGIC %md ## 2.4 max

# COMMAND ----------

# combine the three sources above

# check
count_var(hx_pregnancy_delvar, 'PERSON_ID')
count_var(hx_pregnancy_delrecs, 'PERSON_ID')
count_var(hx_pregnancy_gencode, 'PERSON_ID')

# join
hx_pregnancy = hx_pregnancy_delvar\
  .join(hx_pregnancy_delrecs, on=['PERSON_ID'], how='outer')\
  .join(hx_pregnancy_gencode, on=['PERSON_ID'], how='outer')\

# check
count_var(hx_pregnancy, 'PERSON_ID'); print()
print(len(hx_pregnancy.columns)); print()
print(pd.DataFrame({f'_cols': hx_pregnancy.columns}).to_string()); print()

# max
hx_pregnancy = hx_pregnancy\
  .withColumn('cov_hx_pregnancy_max',\
    f.greatest(\
        'cov_hx_pregnancy_delvar'
      , 'cov_hx_pregnancy_delrecs'
      , 'cov_hx_pregnancy_gencode'
    )\
  )

# check
tmpt = tab(hx_pregnancy, 'cov_hx_pregnancy_max'); print()
tmp1 = hx_pregnancy\
  .withColumn('vrg',\
    f.concat(\
        f.when(f.col('cov_hx_pregnancy_delvar').isNull(), f.lit('_')).otherwise(f.col('cov_hx_pregnancy_delvar'))
      , f.when(f.col('cov_hx_pregnancy_delrecs').isNull(), f.lit('_')).otherwise(f.col('cov_hx_pregnancy_delrecs'))
      , f.when(f.col('cov_hx_pregnancy_gencode').isNull(), f.lit('_')).otherwise(f.col('cov_hx_pregnancy_gencode'))
    )\
  )
tmpt = tab(tmp1, 'vrg', 'cov_hx_pregnancy_max', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md # 3 cov_hx_multi_gest

# COMMAND ----------

# MAGIC %md ## 3.1 delrecsvar

# COMMAND ----------

# delrecsvar = delivery records (all delivery record (variables) before the cohort baseline)

# NUMBABY (Number of Babies): The number of babies delivered at the end of a single pregnancy. Both live and stillborn babies are counted. Until 2002-03, a maximum of 6 babies could be recorded in HES. This field is submitted by the provider. For delivery episodes only, an alternative derivation can be found in the field NUMTAILB, which counts the number of babies actually populated against the episode. This field is populated on delivery and birth episodes and events only (where the EPITYPE is 2,3,5 or 6)."
# NUMTAILB (Number of Baby Tails): Delivery episodes are submitted with one group (tail) of data per baby (e.g. with twins, two tails of baby data will be submitted on the record - these tails include fields such as BIRWEIT and DELMETH and each tail of data is specific to one baby). NUMTAILB is derived in HES and represents the number of valid tails of baby data present on the delivery episode. A valid baby tail is defined as one that has a valid birthweight and a valid delivery method. If no valid baby tail is present on the delivery episode, this field defaults to 1. This field is an alternative to the submitted field NUMBABY, and is populated on delivery episodes and events only (where the EPITYPE is 2 or 5).

# use _hes_apc_mat_all from above (filtered to before baseline)

# check
count_var(_hes_apc_mat_all, 'PERSON_ID'); print()
count_var(_hes_apc_mat_all, 'PERSON_ID_hes_apc'); print()
print(_hes_apc_mat_all.limit(10).toPandas().to_string()); print()

# finalise
tmp1 = _hes_apc_mat_all\
  .select('PERSON_ID_hes_apc', 'NUMBABY', 'NUMTAILB')\
  .withColumnRenamed('PERSON_ID_hes_apc', 'PERSON_ID')\
  .withColumn('NUMBABY_int', f.col('NUMBABY').cast(t.IntegerType()))\
  .withColumn('NUMTAILB_int', f.col('NUMTAILB').cast(t.IntegerType()))\
  .withColumn('cov_hx_multi_gest_delrecsvar',\
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
tmpt = tab(tmp1, '_concat', 'cov_hx_multi_gest_delrecsvar', var2_unstyled=1); print()

hx_multi_gest_delrecsvar = tmp1\
  .select('PERSON_ID', 'cov_hx_multi_gest_delrecsvar')\
  .where(f.col('cov_hx_multi_gest_delrecsvar') == 1)

# check
count_var(hx_multi_gest_delrecsvar, 'PERSON_ID'); print()

# distinct
hx_multi_gest_delrecsvar = hx_multi_gest_delrecsvar\
  .distinct()

# check
count_var(hx_multi_gest_delrecsvar, 'PERSON_ID'); print()
print(hx_multi_gest_delrecsvar.limit(10).toPandas().to_string()); print()

# cache
hx_multi_gest_delrecsvar.cache()
print(f'{hx_multi_gest_delrecsvar.count():,}')

# COMMAND ----------

# MAGIC %md ## 3.2 gencode

# COMMAND ----------

# gencode = general codelist match
hx_multi_gest_gencode = covariates\
  .select('PERSON_ID', 'cov_hx_multi_gest_flag')\
  .withColumnRenamed('cov_hx_multi_gest_flag', 'cov_hx_multi_gest_gencode')

# check
count_var(hx_multi_gest_gencode, 'PERSON_ID'); print()
print(hx_multi_gest_gencode.limit(10).toPandas().to_string()); print()

# cache
hx_multi_gest_gencode.cache()
print(f'{hx_multi_gest_gencode.count():,}')

# COMMAND ----------

# MAGIC %md ## 3.3 max

# COMMAND ----------

# combine the two sources above

# check
count_var(hx_multi_gest_delrecsvar, 'PERSON_ID'); print()
count_var(hx_multi_gest_gencode, 'PERSON_ID'); print()

# join
hx_multi_gest = hx_multi_gest_delrecsvar\
  .join(hx_multi_gest_gencode, on=['PERSON_ID'], how='outer')  

# check
count_var(hx_multi_gest, 'PERSON_ID'); print()
print(len(hx_multi_gest.columns)); print()
print(pd.DataFrame({f'_cols': hx_multi_gest.columns}).to_string()); print()

# max
hx_multi_gest = hx_multi_gest\
  .withColumn('cov_hx_multi_gest_max',\
    f.greatest(\
        'cov_hx_multi_gest_delrecsvar'
      , 'cov_hx_multi_gest_gencode'
    )\
  )

# check
tmpt = tab(hx_multi_gest, 'cov_hx_multi_gest_max'); print()
tmp1 = hx_multi_gest\
  .withColumn('rg',\
    f.concat(\
        f.when(f.col('cov_hx_multi_gest_delrecsvar').isNull(), f.lit('_')).otherwise(f.col('cov_hx_multi_gest_delrecsvar'))
      , f.when(f.col('cov_hx_multi_gest_gencode').isNull(), f.lit('_')).otherwise(f.col('cov_hx_multi_gest_gencode'))
    )\
  )
tmpt = tab(tmp1, 'rg', 'cov_hx_multi_gest_max', var2_unstyled=1); print()

# COMMAND ----------

# MAGIC %md # 4 Save

# COMMAND ----------

# check
count_var(hx_pregnancy, 'PERSON_ID')
count_var(hx_multi_gest, 'PERSON_ID')

# join
tmp1 = hx_pregnancy\
  .join(hx_multi_gest, on=['PERSON_ID'], how='outer')

# check
count_var(tmp1, 'PERSON_ID')
print(len(tmp1.columns))
print(pd.DataFrame({f'_cols': tmp1.columns}).to_string())

# COMMAND ----------

# check final
display(tmp1)

# COMMAND ----------

# save name
outName = f'{proj}_out_covariates_supp'.lower()

# save previous version for comparison purposes
_datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
outName_pre = f'{outName}_pre{_datetimenow}'.lower()
print(outName_pre)
spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')

# save
tmp1.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

# COMMAND ----------



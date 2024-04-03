# Databricks notebook source
# MAGIC %md # CCU018_01-D02-codelist
# MAGIC
# MAGIC **Description** This notebook creates the codelist for use across the data curation pipeline for the CCU018_01 project, including demographics, comorbidities, medications, and maternal. 
# MAGIC
# MAGIC **Author(s)** Tom Bolton, John Nolan, Elena Raffetti (based on work from CCU002)
# MAGIC
# MAGIC **Data input** bhf_phenotypes, map_ctv3_snomed, pmeds
# MAGIC
# MAGIC **Data output** ccu018_01_out_codelist

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

bhf_phenotypes  = spark.table(path_ref_bhf_phenotypes)
map_ctv3_snomed = spark.table(path_ref_map_ctv3_snomed)

# get archive tables at the ProductionDate specified in the parameters notebook (pre table freeze)
# note: pre table freeze - preferable not to restrict to cohort for the purpose of defining a codelist
pmeds           = get_archive_table('pmeds') 

# COMMAND ----------

# MAGIC %md # 2 Prepare

# COMMAND ----------

# ------------------------------------------------------------------------------
# bhf_phenotypes
# ------------------------------------------------------------------------------
bhf_phenotypes = bhf_phenotypes\
  .select(['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate'])\
  .dropDuplicates()

# cache
bhf_phenotypes.cache()
print(f'{bhf_phenotypes.count():,}'); print()

# check
tmpt = tab(bhf_phenotypes, 'name', 'terminology', var2_unstyled=1); print()


# ------------------------------------------------------------------------------
# pmeds
# ------------------------------------------------------------------------------
_pmeds = pmeds\
  .select(['PrescribedBNFCode', 'PrescribedBNFName'])\
  .dropDuplicates(['PrescribedBNFCode'])

# cache
_pmeds.cache()
print(f'{_pmeds.count():,}'); print()

# COMMAND ----------

# check
display(bhf_phenotypes)

# COMMAND ----------

# MAGIC %md # 3 Codelists

# COMMAND ----------

# MAGIC %md ## 3.1 Demographics

# COMMAND ----------

# BMI obesity
codelist_BMI_obesity = bhf_phenotypes.where(f.col('name') == 'BMI_obesity')
tmp_BMI_obesity = spark.createDataFrame(
  [  
    ('BMI_obesity', 'ICD10', 'E66', 'Diagnosis of obesity', '', '')
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_BMI_obesity = codelist_BMI_obesity.unionByName(tmp_BMI_obesity)


# smoking_(current|ex|never)
# 20220707 updated 6 smoking_ex SNOMED codes that had been rounded
tmp_smoking_status = spark.createDataFrame(
  [
    ("160603005","Light cigarette smoker (1-9 cigs/day) (finding)","Current-smoker","Light"),
    ("160606002","Very heavy cigarette smoker (40+ cigs/day) (finding)","Current-smoker","Heavy"),
    ("160613002","Admitted tobacco consumption possibly untrue (finding)","Current-smoker","Unknown"),
    ("160619003","Rolls own cigarettes (finding)","Current-smoker","Unknown"),
    ("230056004","Cigarette consumption (observable entity)","Current-smoker","Unknown"),
    ("230057008","Cigar consumption (observable entity)","Current-smoker","Unknown"),
    ("230058003","Pipe tobacco consumption (observable entity)","Current-smoker","Unknown"),
    ("230060001","Light cigarette smoker (finding)","Current-smoker","Light"),
    ("230062009","Moderate cigarette smoker (finding)","Current-smoker","Moderate"),
    ("230065006","Chain smoker (finding)","Current-smoker","Heavy"),
    ("266918002","Tobacco smoking consumption (observable entity)","Current-smoker","Unknown"),
    ("446172000","Failed attempt to stop smoking (finding)","Current-smoker","Unknown"),
    ("449868002","Smokes tobacco daily (finding)","Current-smoker","Unknown"),
    ("56578002","Moderate smoker (20 or less per day) (finding)","Current-smoker","Moderate"),
    ("56771006","Heavy smoker (over 20 per day) (finding)","Current-smoker","Heavy"),
    ("59978006","Cigar smoker (finding)","Current-smoker","Unknown"),
    ("65568007","Cigarette smoker (finding)","Current-smoker","Unknown"),
    ("134406006","Smoking reduced (finding)","Current-smoker","Unknown"),
    ("160604004","Moderate cigarette smoker (10-19 cigs/day) (finding)","Current-smoker","Moderate"),
    ("160605003","Heavy cigarette smoker (20-39 cigs/day) (finding)","Current-smoker","Heavy"),
    ("160612007","Keeps trying to stop smoking (finding)","Current-smoker","Unknown"),
    ("160616005","Trying to give up smoking (finding)","Current-smoker","Unknown"),
    ("203191000000107","Wants to stop smoking (finding)","Current-smoker","Unknown"),
    ("225934006","Smokes in bed (finding)","Current-smoker","Unknown"),
    ("230059006","Occasional cigarette smoker (finding)","Current-smoker","Light"),
    ("230063004","Heavy cigarette smoker (finding)","Current-smoker","Heavy"),
    ("230064005","Very heavy cigarette smoker (finding)","Current-smoker","Heavy"),
    ("266920004","Trivial cigarette smoker (less than one cigarette/day) (finding)","Current-smoker","Light"),
    ("266929003","Smoking started (finding)","Current-smoker","Unknown"),
    ("308438006","Smoking restarted (finding)","Current-smoker","Unknown"),
    ("394871007","Thinking about stopping smoking (finding)","Current-smoker","Unknown"),
    ("394872000","Ready to stop smoking (finding)","Current-smoker","Unknown"),
    ("394873005","Not interested in stopping smoking (finding)","Current-smoker","Unknown"),
    ("401159003","Reason for restarting smoking (observable entity)","Current-smoker","Unknown"),
    ("413173009","Minutes from waking to first tobacco consumption (observable entity)","Current-smoker","Unknown"),
    ("428041000124106","Occasional tobacco smoker (finding)","Current-smoker","Light"),
    ("77176002","Smoker (finding)","Current-smoker","Unknown"),
    ("82302008","Pipe smoker (finding)","Current-smoker","Unknown"),
    ("836001000000109","Waterpipe tobacco consumption (observable entity)","Current-smoker","Unknown"),
    ("160603005","Light cigarette smoker (1-9 cigs/day) (finding)","Current-smoker","Light"),
    ("160612007","Keeps trying to stop smoking (finding)","Current-smoker","Unknown"),
    ("160613002","Admitted tobacco consumption possibly untrue (finding)","Current-smoker","Unknown"),
    ("160616005","Trying to give up smoking (finding)","Current-smoker","Unknown"),
    ("160619003","Rolls own cigarettes (finding)","Current-smoker","Unknown"),
    ("160625004","Date ceased smoking (observable entity)","Current-smoker","Unknown"),
    ("225934006","Smokes in bed (finding)","Current-smoker","Unknown"),
    ("230056004","Cigarette consumption (observable entity)","Current-smoker","Unknown"),
    ("230057008","Cigar consumption (observable entity)","Current-smoker","Unknown"),
    ("230059006","Occasional cigarette smoker (finding)","Current-smoker","Light"),
    ("230060001","Light cigarette smoker (finding)","Current-smoker","Light"),
    ("230062009","Moderate cigarette smoker (finding)","Current-smoker","Moderate"),
    ("230063004","Heavy cigarette smoker (finding)","Current-smoker","Heavy"),
    ("230064005","Very heavy cigarette smoker (finding)","Current-smoker","Heavy"),
    ("266920004","Trivial cigarette smoker (less than one cigarette/day) (finding)","Current-smoker","Light"),
    ("266929003","Smoking started (finding)","Current-smoker","Unknown"),
    ("394872000","Ready to stop smoking (finding)","Current-smoker","Unknown"),
    ("401159003","Reason for restarting smoking (observable entity)","Current-smoker","Unknown"),
    ("449868002","Smokes tobacco daily (finding)","Current-smoker","Unknown"),
    ("65568007","Cigarette smoker (finding)","Current-smoker","Unknown"),
    ("134406006","Smoking reduced (finding)","Current-smoker","Unknown"),
    ("160604004","Moderate cigarette smoker (10-19 cigs/day) (finding)","Current-smoker","Moderate"),
    ("160605003","Heavy cigarette smoker (20-39 cigs/day) (finding)","Current-smoker","Heavy"),
    ("160606002","Very heavy cigarette smoker (40+ cigs/day) (finding)","Current-smoker","Heavy"),
    ("203191000000107","Wants to stop smoking (finding)","Current-smoker","Unknown"),
    ("308438006","Smoking restarted (finding)","Current-smoker","Unknown"),
    ("394871007","Thinking about stopping smoking (finding)","Current-smoker","Unknown"),
    ("394873005","Not interested in stopping smoking (finding)","Current-smoker","Unknown"),
    ("401201003","Cigarette pack-years (observable entity)","Current-smoker","Unknown"),
    ("413173009","Minutes from waking to first tobacco consumption (observable entity)","Current-smoker","Unknown"),
    ("428041000124106","Occasional tobacco smoker (finding)","Current-smoker","Light"),
    ("446172000","Failed attempt to stop smoking (finding)","Current-smoker","Unknown"),
    ("56578002","Moderate smoker (20 or less per day) (finding)","Current-smoker","Moderate"),
    ("56771006","Heavy smoker (over 20 per day) (finding)","Current-smoker","Heavy"),
    ("59978006","Cigar smoker (finding)","Current-smoker","Unknown"),
    ("77176002","Smoker (finding)","Current-smoker","Unknown"),
    ("82302008","Pipe smoker (finding)","Current-smoker","Unknown"),
    ("836001000000109","Waterpipe tobacco consumption (observable entity)","Current-smoker","Unknown"),
    ("53896009","Tolerant ex-smoker (finding)","Ex-smoker","Unknown"),
    ("1092041000000104","Ex-very heavy smoker (40+/day) (finding)","Ex-smoker","Unknown"),
    ("1092091000000109","Ex-moderate smoker (10-19/day) (finding)","Ex-smoker","Unknown"),
    ("160620009","Ex-pipe smoker (finding)","Ex-smoker","Unknown"),
    ("160621008","Ex-cigar smoker (finding)","Ex-smoker","Unknown"),
    ("228486009","Time since stopped smoking (observable entity)","Ex-smoker","Unknown"),
    ("266921000","Ex-trivial cigarette smoker (<1/day) (finding)","Ex-smoker","Unknown"),
    ("266922007","Ex-light cigarette smoker (1-9/day) (finding)","Ex-smoker","Unknown"),
    ("266923002","Ex-moderate cigarette smoker (10-19/day) (finding)","Ex-smoker","Unknown"),
    ("266928006","Ex-cigarette smoker amount unknown (finding)","Ex-smoker","Unknown"),
    ("281018007","Ex-cigarette smoker (finding)","Ex-smoker","Unknown"),
    ("735128000","Ex-smoker for less than 1 year (finding)","Ex-smoker","Unknown"),
    ("8517006","Ex-smoker (finding)","Ex-smoker","Unknown"),
    ("1092031000000108","Ex-smoker amount unknown (finding)","Ex-smoker","Unknown"),
    ("1092071000000105","Ex-heavy smoker (20-39/day) (finding)","Ex-smoker","Unknown"),
    ("1092111000000104","Ex-light smoker (1-9/day) (finding)","Ex-smoker","Unknown"),
    ("1092131000000107","Ex-trivial smoker (<1/day) (finding)","Ex-smoker","Unknown"),
    ("160617001","Stopped smoking (finding)","Ex-smoker","Unknown"),
    ("160625004","Date ceased smoking (observable entity)","Ex-smoker","Unknown"),
    ("266924008","Ex-heavy cigarette smoker (20-39/day) (finding)","Ex-smoker","Unknown"),
    ("266925009","Ex-very heavy cigarette smoker (40+/day) (finding)","Ex-smoker","Unknown"),
    ("360890004","Intolerant ex-smoker (finding)","Ex-smoker","Unknown"),
    ("360900008","Aggressive ex-smoker (finding)","Ex-smoker","Unknown"),
    ("48031000119106","Ex-smoker for more than 1 year (finding)","Ex-smoker","Unknown"),
    ("492191000000103","Ex roll-up cigarette smoker (finding)","Ex-smoker","Unknown"),
    ("53896009","Tolerant ex-smoker (finding)","Ex-smoker","Unknown"),
    ("735112005","Date ceased using moist tobacco (observable entity)","Ex-smoker","Unknown"),
    ("228486009","Time since stopped smoking (observable entity)","Ex-smoker","Unknown"),
    ("266921000","Ex-trivial cigarette smoker (<1/day) (finding)","Ex-smoker","Unknown"),
    ("266923002","Ex-moderate cigarette smoker (10-19/day) (finding)","Ex-smoker","Unknown"),
    ("266928006","Ex-cigarette smoker amount unknown (finding)","Ex-smoker","Unknown"),
    ("360900008","Aggressive ex-smoker (finding)","Ex-smoker","Unknown"),
    ("492191000000103","Ex roll-up cigarette smoker (finding)","Ex-smoker","Unknown"),
    ("735112005","Date ceased using moist tobacco (observable entity)","Ex-smoker","Unknown"),
    ("735128000","Ex-smoker for less than 1 year (finding)","Ex-smoker","Unknown"),
    ("160617001","Stopped smoking (finding)","Ex-smoker","Unknown"),
    ("160620009","Ex-pipe smoker (finding)","Ex-smoker","Unknown"),
    ("160621008","Ex-cigar smoker (finding)","Ex-smoker","Unknown"),
    ("230058003","Pipe tobacco consumption (observable entity)","Ex-smoker","Unknown"),
    ("230065006","Chain smoker (finding)","Ex-smoker","Unknown"),
    ("266918002","Tobacco smoking consumption (observable entity)","Ex-smoker","Unknown"),
    ("266922007","Ex-light cigarette smoker (1-9/day) (finding)","Ex-smoker","Unknown"),
    ("266924008","Ex-heavy cigarette smoker (20-39/day) (finding)","Ex-smoker","Unknown"),
    ("266925009","Ex-very heavy cigarette smoker (40+/day) (finding)","Ex-smoker","Unknown"),
    ("281018007","Ex-cigarette smoker (finding)","Ex-smoker","Unknown"),
    ("360890004","Intolerant ex-smoker (finding)","Ex-smoker","Unknown"),
    ("48031000119106","Ex-smoker for more than 1 year (finding)","Ex-smoker","Unknown"),
    ("8517006","Ex-smoker (finding)","Ex-smoker","Unknown"),
    ("221000119102","Never smoked any substance (finding)","Never-smoker","NA"),
    ("266919005","Never smoked tobacco (finding)","Never-smoker","NA"),
    ("221000119102","Never smoked any substance (finding)","Never-smoker","NA"),
    ("266919005","Never smoked tobacco (finding)","Never-smoker","NA")
  ],
  ['code', 'term', 'smoking_status', 'severity']
)
codelist_smoking_status = tmp_smoking_status\
  .distinct()\
  .withColumn('name',\
    f.when(f.col('smoking_status') == 'Current-smoker', f.lit('smoking_current'))\
     .when(f.col('smoking_status') == 'Ex-smoker', f.lit('smoking_ex'))\
     .when(f.col('smoking_status') == 'Never-smoker', f.lit('smoking_never'))\
     .otherwise(f.col('smoking_status'))\
  )\
  .withColumn('terminology', f.lit('SNOMED'))\
  .withColumn('code_type', f.lit(''))\
  .withColumn('RecordDate', f.lit(''))\
  .select(['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate'])

# COMMAND ----------

# MAGIC %md ## 3.2 Comorbidities

# COMMAND ----------

# acute myocardial infarction (AMI)
codelist_AMI = bhf_phenotypes\
  .where(f.col('name') == 'AMI')\
  .withColumn('covariate_only', f.when(f.col('code').isin(['I25.2', 'I24.1']), 1))

# chronic kidney disease (CKD)
codelist_CKD = bhf_phenotypes\
  .where(f.col('name') == 'CKD')

# chronic obstructive pulmonary disease (COPD)
codelist_COPD = bhf_phenotypes\
  .where(f.col('name') == 'COPD')

# dementia
codelist_dementia = bhf_phenotypes\
  .where(f.col('name') == 'dementia')

# depression
codelist_depression = bhf_phenotypes\
  .where(f.col('name') == 'depression')

# cancer
# 20220616 ER advised to remove code 417036008 "Liquid based cervical cytology screening (procedure)"
codelist_cancer = bhf_phenotypes\
  .where(f.col('name') == 'cancer')\
  .where(f.col('code') != '417036008')


# angina
codelist_angina = spark.createDataFrame(
  [
    ("angina","ICD10","I20","Angina","",""),
    ("angina","SNOMED","10971000087107","Myocardial ischemia during surgery (disorder)","1","20210127"),
    ("angina","SNOMED","15960061000119102","Unstable angina co-occurrent and due to coronary arteriosclerosis (disorder)","1","20210127"),
    ("angina","SNOMED","15960581000119102","Angina co-occurrent and due to arteriosclerosis of autologous vein coronary artery bypass graft (disorder)","1","20210127"),
    ("angina","SNOMED","15960661000119107","Unstable angina co-occurrent and due to arteriosclerosis of coronary artery bypass graft (disorder)","1","20210127"),
    ("angina","SNOMED","19057007","Status anginosus (disorder)","1","20210127"),
    ("angina","SNOMED","194828000","Angina (disorder)","1","20210127"),
    ("angina","SNOMED","21470009","Syncope anginosa (disorder)","1","20210127"),
    ("angina","SNOMED","233821000","New onset angina (disorder)","1","20210127"),
    ("angina","SNOMED","25106000","Impending infarction (disorder)","1","20210127"),
    ("angina","SNOMED","314116003","Post infarct angina (disorder)","1","20210127"),
    ("angina","SNOMED","371806006","Progressive angina (disorder)","1","20210127"),
    ("angina","SNOMED","371809004","Recurrent angina status post coronary stent placement (disorder)","1","20210127"),
    ("angina","SNOMED","371812001","Recurrent angina status post directional coronary atherectomy (disorder)","1","20210127"),
    ("angina","SNOMED","41334000","Angina, class II (disorder)","1","20210127"),
    ("angina","SNOMED","413439005","Acute ischemic heart disease (disorder)","1","20210127"),
    ("angina","SNOMED","413444003","Acute myocardial ischemia (disorder)","1","20210127"),
    ("angina","SNOMED","413838009","Chronic ischemic heart disease (disorder)","1","20210127"),
    ("angina","SNOMED","429559004","Typical angina (disorder)","1","20210127"),
    ("angina","SNOMED","4557003","Preinfarction syndrome (disorder)","1","20210127"),
    ("angina","SNOMED","59021001","Angina decubitus (disorder)","1","20210127"),
    ("angina","SNOMED","61490001","Angina, class I (disorder)","1","20210127"),
    ("angina","SNOMED","712866001","Resting ischemia co-occurrent and due to ischemic heart disease (disorder)","1","20210127"),
    ("angina","SNOMED","791000119109","Angina associated with type II diabetes mellitus (disorder)","1","20210127"),
    ("angina","SNOMED","85284003","Angina, class III (disorder)","1","20210127"),
    ("angina","SNOMED","89323001","Angina, class IV (disorder)","1","20210127"),
    ("angina","SNOMED","15960141000119102","Angina co-occurrent and due to coronary arteriosclerosis (disorder)","1","20210127"),
    ("angina","SNOMED","15960381000119109","Angina co-occurrent and due to arteriosclerosis of coronary artery bypass graft (disorder)","1","20210127"),
    ("angina","SNOMED","194823009","Acute coronary insufficiency (disorder)","1","20210127"),
    ("angina","SNOMED","225566008","Ischemic chest pain (finding)","1","20210127"),
    ("angina","SNOMED","233819005","Stable angina (disorder)","1","20210127"),
    ("angina","SNOMED","233823002","Silent myocardial ischemia (disorder)","1","20210127"),
    ("angina","SNOMED","300995000","Exercise-induced angina (disorder)","1","20210127"),
    ("angina","SNOMED","315025001","Refractory angina (disorder)","1","20210127"),
    ("angina","SNOMED","35928006","Nocturnal angina (disorder)","1","20210127"),
    ("angina","SNOMED","371807002","Atypical angina (disorder)","1","20210127"),
    ("angina","SNOMED","371808007","Recurrent angina status post percutaneous transluminal coronary angioplasty (disorder)","1","20210127"),
    ("angina","SNOMED","371810009","Recurrent angina status post coronary artery bypass graft (disorder)","1","20210127"),
    ("angina","SNOMED","371811008","Recurrent angina status post rotational atherectomy (disorder)","1","20210127"),
    ("angina","SNOMED","394659003","Acute coronary syndrome (disorder)","1","20210127"),
    ("angina","SNOMED","413844008","Chronic myocardial ischemia (disorder)","1","20210127"),
    ("angina","SNOMED","414545008","Ischemic heart disease (disorder)","1","20210127"),
    ("angina","SNOMED","414795007","Myocardial ischemia (disorder)","1","20210127"),
    ("angina","SNOMED","46109009","Subendocardial ischemia (disorder)","1","20210127"),
    ("angina","SNOMED","697976003","Microvascular ischemia of myocardium (disorder)","1","20210127"),
    ("angina","SNOMED","703214003","Silent coronary vasospastic disease (disorder)","1","20210127"),
    ("angina","SNOMED","713405002","Subacute ischemic heart disease (disorder)","1","20210127"),
    ("angina","SNOMED","87343002","Prinzmetal angina (disorder)","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
) 


# angina_unstable
codelist_angina_unstable = spark.createDataFrame(
  [
    ("angina_unstable","ICD10","I20.1","Unstable angina","",""),
    ("angina_unstable","SNOMED","15960061000119102","Unstable angina co-occurrent and due to coronary arteriosclerosis (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","15960661000119107","Unstable angina co-occurrent and due to arteriosclerosis of coronary artery bypass graft (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","19057007","Status anginosus (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","25106000","Impending infarction (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","4557003","Preinfarction syndrome (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","712866001","Resting ischemia co-occurrent and due to ischemic heart disease (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","315025001","Refractory angina (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","35928006","Nocturnal angina (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","371807002","Atypical angina (disorder)","1","20210127"),
    ("angina_unstable","SNOMED","394659003","Acute coronary syndrome (disorder)","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)   


# cardiomyopathy
codelist_cardiomyopathy = bhf_phenotypes\
  .where((f.col('name') == 'cardiomyopathy') & (f.col('terminology') == 'SNOMED'))
tmp_cardiomyopathy = spark.createDataFrame(
  [
    ("cardiomyopathy","ICD10","I42.0","Dilated cardiomyopathy","1","20210127"),
    ("cardiomyopathy","ICD10","I42.3","Endomyocardial (eosinophilic) disease","1","20210127"),
    ("cardiomyopathy","ICD10","I42.5","Other restrictive cardiomyopathy","1","20210127"),
    ("cardiomyopathy","ICD10","I42.7","Cardiomyopathy due to drugs and other external agents","1","20210127"),
    ("cardiomyopathy","ICD10","I42.8","Other cardiomyopathies","1","20210127"),
    ("cardiomyopathy","ICD10","I42.9","Cardiomyopathy, unspecified","1","20210127"),
    ("cardiomyopathy","ICD10","I43","Cardiomyopathy in diseases classified elsewhere","1","20210127"),
    ("cardiomyopathy","ICD10","I25.5 ","Ischaemic cardiomyopathy","1","20210127"),
    ("cardiomyopathy","ICD10","O90.3 ","Cardiomyopathy in the puerperium","1","20210127")
  ],    
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_cardiomyopathy = codelist_cardiomyopathy\
  .unionByName(tmp_cardiomyopathy)


# COMMAND ----------

# diabetes
codelist_diabetes = bhf_phenotypes\
  .where(f.col('name').rlike('^diabetes.*$'))
tmp_diabetes = spark.createDataFrame(
  [
    ('diabetes','ICD10','E10', 'Insulin-dependent diabetes mellitus','',''),
    ('diabetes','ICD10','E11', 'Non-insulin-dependent diabetes mellitus','',''),
    ('diabetes','ICD10','E12', 'Malnutrition-related diabetes mellitus','',''),
    ('diabetes','ICD10','E13', 'Other specified diabetes mellitus','',''),
    ('diabetes','ICD10','E14', 'Unspecified diabetes mellitus','',''),
    ('diabetes','ICD10','G590','Diabetic mononeuropathy','','',),
    ('diabetes','ICD10','G632','Diabetic polyneuropathy','',''),
    ('diabetes','ICD10','H280','Diabetic cataract','',''),
    ('diabetes','ICD10','H360','Diabetic retinopathy','',''),
    ('diabetes','ICD10','M142','Diabetic arthropathy','',''),
    ('diabetes','ICD10','N083','Glomerular disorders in diabetes mellitus','',''),
    ('diabetes','ICD10','O240','Diabetes mellitus in pregnancy: Pre-existing diabetes mellitus, insulin-dependent','',''),
    ('diabetes','ICD10','O242','Diabetes mellitus in pregnancy: Pre-existing malnutrition-related diabetes mellitus','',''),
    ('diabetes','ICD10','O241','Diabetes mellitus in pregnancy: Pre-existing diabetes mellitus, non-insulin-dependent','',''),
    ('diabetes','ICD10','O243','Diabetes mellitus in pregnancy: Pre-existing diabetes mellitus, unspecified','','')
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_diabetes = codelist_diabetes\
  .unionByName(tmp_diabetes)


# hypertension
codelist_hypertension = bhf_phenotypes\
  .where(f.col('name').rlike('^hypertension.*$'))
tmp_hypertension = spark.createDataFrame(
  [
    ('hypertension', 'ICD10', 'I10', 'Essential (primary) hypertension',     '', ''),
    ('hypertension', 'ICD10', 'I11', 'Hypertensive heart disease',           '', ''),
    ('hypertension', 'ICD10', 'I12', 'Hypertensive renal disease',           '', ''),
    ('hypertension', 'ICD10', 'I13', 'Hypertensive heart and renal disease', '', ''),
    ('hypertension', 'ICD10', 'I15', 'Secondary hypertension',               '', '')    
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_hypertension = codelist_hypertension\
  .unionByName(tmp_hypertension)


# life_arrhythmias
codelist_life_arrhythmias = bhf_phenotypes\
  .where(f.col('name') == 'life_arrhythmias')
tmp_life_arrhythmias = spark.createDataFrame(
  [
    ("life_arrhythmias", "ICD10", "I46.1", "Sudden cardiac death, so described", "", "")    
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_life_arrhythmias = codelist_life_arrhythmias\
  .unionByName(tmp_life_arrhythmias)


# pulmonary embolim (PE)
codelist_PE = spark.createDataFrame(
  [
    ("PE", "ICD10", "I26.0", "Pulmonary embolism", "1", "20210127", None),
    ("PE", "ICD10", "I26.9", "Pulmonary embolism", "1", "20210127", None),
    ("PE", "SNOMED", "438773007", "Pulmonary embolism with mention of acute cor pulmonale", "1", "20210127", 1),
    ("PE", "SNOMED", "133971000119108", "Pulmonary embolism with mention of acute cor pulmonale", "1", "20210127", 1)
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate', 'covariate_only']  
)


# stroke_IS
codelist_stroke_IS = bhf_phenotypes\
  .where((f.col('name') == 'stroke_IS') & (f.col('terminology') == 'SNOMED'))
tmp_stroke_IS = spark.createDataFrame(
  [
    ("stroke_IS", "ICD10", "I63.0", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "I63.1", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "I63.2", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "I63.3", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "I63.4", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "I63.5", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "I63.8", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "I63.9", "Cerebral infarction", "1", "20210127"),
    ("stroke_IS", "ICD10", "H34", "Retinal vascular occlusions", "1", "20210127"),
    ("stroke_IS", "ICD10", "G95.1", "Avascular myelopathies (arterial or venous)", "1", "20210127")    
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_stroke_IS = codelist_stroke_IS\
  .unionByName(tmp_stroke_IS)

# stroke_HS
codelist_stroke_HS = bhf_phenotypes\
  .where(f.col('name') == 'stroke_HS')\
  .withColumn('covariate_only', f.when(f.col('terminology') == 'SNOMED', 1))

# stroke_NOS
codelist_stroke_NOS = bhf_phenotypes\
  .where(\
    (f.col('name') == 'stroke_NOS')\
    & ~(f.col('term').isin([\
      'Stroke in the puerperium (disorder)'
    , 'Cerebrovascular accident (disorder)'
    , 'Right sided cerebral hemisphere cerebrovascular accident (disorder)'
    , 'Left sided cerebral hemisphere cerebrovascular accident (disorder)'
    , 'Brainstem stroke syndrome (disorder)'
    ]))\
  )

# stroke_SAH
codelist_stroke_SAH = bhf_phenotypes\
  .where(f.col('name') == 'stroke_SAH')

# stroke_TIA
codelist_stroke_TIA = spark.createDataFrame(
  [
    ("stroke_TIA","ICD10","G45","Transient cerebral ischaemic attacks and related syndromes","","", 0),
    ("stroke_TIA","SNOMED","195206000","Intermittent cerebral ischaemia","1","20210127", 0),
    ("stroke_TIA","SNOMED","230716006","Anterior circulation transient ischaemic attack","1","20210127", 0),
    ("stroke_TIA","SNOMED","266257000","TIA","1","20210127", 0),
    ("stroke_TIA","SNOMED","230717002","Vertebrobasilar territory transient ischemic attack (disorder)","1","20210127", 0)    
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate', 'covariate_only']  
)

# 20220502 duplicates commented out
# ("stroke_TIA","SNOMED","230716006","Anterior circulation transient ischaemic attack","1","20210127", 0),
# ("stroke_TIA","SNOMED","230717002","Vertebrobasilar territory transient ischaemic attack","1","20210127", 0)


# pericarditis
codelist_pericarditis = spark.createDataFrame(
  [
    ("pericarditis","ICD10","I30","Acute pericarditis","","")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# myocarditis
codelist_myocarditis = spark.createDataFrame(
  [
    ("myocarditis","ICD10","I51.4","Myocarditis, unspecified","",""),
    ("myocarditis","ICD10","I40","Acute myocarditis","",""),
    ("myocarditis","ICD10","I41","Myocarditis in diseases classified elsewhere","","")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)

# COMMAND ----------

# liver_disease

# get mapping file for CTV3 to SNOMED
map_ctv3_snomed_assured = map_ctv3_snomed\
  .where(f.col('IS_ASSURED') == 1)\
  .select(['CTV3_CONCEPTID', 'SCT_CONCEPTID'])\
  .withColumnRenamed('CTV3_CONCEPTID', 'code')\
  .dropDuplicates()
# display(map_ctv3_snomed_assured)

# get liver disease CTV3 codelist
liver_ctv3 = bhf_phenotypes\
  .where((f.col('name') == 'liver_disease') & (f.col('terminology') == 'CTV3'))\
  .select(['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate'])
# display(liver_ctv3)

# map CTV3 to SNOMED
liver_ctv3_snomed = merge(liver_ctv3, map_ctv3_snomed_assured, ['code'])

# check
# display(liver_ctv3_snomed.where(f.col('_merge') == 'left_only').drop('_merge'))
# display(liver_ctv3_snomed.where(f.col('_merge') == 'both').drop(['_merge']))

# reformat
liver_ctv3_snomed = liver_ctv3_snomed\
  .where(f.col('_merge') == 'both')\
  .drop('_merge', 'code')\
  .withColumnRenamed('SCT_CONCEPTID', 'code')\
  .withColumn('terminology', f.lit('SNOMED'))\
  .select(['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate'])\
  .dropDuplicates(['code'])
# display(liver_ctv3_snomed)

# get liver disease CTV3 codelist
liver_icd10 = bhf_phenotypes\
  .where((f.col('name') == 'liver_disease') & (f.col('terminology') == 'ICD10'))\
  .select(['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate'])
# display(liver_icd10)

# combine
codelist_liver_disease = liver_ctv3_snomed.unionByName(liver_icd10)

# COMMAND ----------

# heart failure (HF)
codelist_HF = spark.createDataFrame(
  [
    ("HF","ICD10","I50","Heart failure","",""),
    ("HF","ICD10","I11.0","Hypertensive heart disease with (congestive) heart failure","",""),
    ("HF","ICD10","I13.0","Hypertensive heart and renal disease with (congestive) heart failure","",""),
    ("HF","ICD10","I13.2","Hypertensive heart and renal disease with both (congestive) heart failure and renal failure","",""),
    ("HF","SNOMED","10335000","Chronic right-sided heart failure (disorder)","1","20210127"),
    ("HF","SNOMED","10633002","Acute congestive heart failure","1","20210127"),
    ("HF","SNOMED","42343007","Congestive heart failure","1","20210127"),
    ("HF","SNOMED","43736008","Rheumatic left ventricular failure","1","20210127"),
    ("HF","SNOMED","48447003","Chronic heart failure (disorder)","1","20210127"),
    ("HF","SNOMED","56675007","Acute heart failure","1","20210127"),
    ("HF","SNOMED","71892000","Cardiac asthma","1","20210127"),
    ("HF","SNOMED","79955004","Chronic cor pulmonale","1","20210127"),
    ("HF","SNOMED","83105008","Malignant hypertensive heart disease with congestive heart failure","1","20210127"),
    ("HF","SNOMED","84114007","Heart failure","1","20210127"),
    ("HF","SNOMED","85232009","Left heart failure","1","20210127"),
    ("HF","SNOMED","87837008","Chronic pulmonary heart disease","1","20210127"),
    ("HF","SNOMED","88805009","Chronic congestive heart failure","1","20210127"),
    ("HF","SNOMED","92506005","Biventricular congestive heart failure","1","20210127"),
    ("HF","SNOMED","128404006","Right heart failure","1","20210127"),
    ("HF","SNOMED","134401001","Left ventricular systolic dysfunction","1","20210127"),
    ("HF","SNOMED","134440006","Referral to heart failure clinic","1","20210127"),
    ("HF","SNOMED","194767001","Benign hypertensive heart disease with congestive cardiac failure","1","20210127"),
    ("HF","SNOMED","194779001","Hypertensive heart and renal disease with (congestive) heart failure","1","20210127"),
    ("HF","SNOMED","194781004","Hypertensive heart and renal disease with both (congestive) heart failure and renal failure","1","20210127"),
    ("HF","SNOMED","195111005","Decompensated cardiac failure","1","20210127"),
    ("HF","SNOMED","195112003","Compensated cardiac failure","1","20210127"),
    ("HF","SNOMED","195114002","Acute left ventricular failure","1","20210127"),
    ("HF","SNOMED","206586007","Congenital cardiac failure","1","20210127"),
    ("HF","SNOMED","233924009","Heart failure as a complication of care (disorder)","1","20210127"),
    ("HF","SNOMED","275514001","Impaired left ventricular function","1","20210127"),
    ("HF","SNOMED","314206003","Refractory heart failure (disorder)","1","20210127"),
    ("HF","SNOMED","367363000","Right ventricular failure","1","20210127"),
    ("HF","SNOMED","407596008","Echocardiogram shows left ventricular systolic dysfunction (finding)","1","20210127"),
    ("HF","SNOMED","420300004","New York Heart Association Classification - Class I (finding)","1","20210127"),
    ("HF","SNOMED","420913000","New York Heart Association Classification - Class III (finding)","1","20210127"),
    ("HF","SNOMED","421704003","New York Heart Association Classification - Class II (finding)","1","20210127"),
    ("HF","SNOMED","422293003","New York Heart Association Classification - Class IV (finding)","1","20210127"),
    ("HF","SNOMED","426263006","Congestive heart failure due to left ventricular systolic dysfunction (disorder)","1","20210127"),
    ("HF","SNOMED","426611007","Congestive heart failure due to valvular disease (disorder)","1","20210127"),
    ("HF","SNOMED","430396006","Chronic systolic dysfunction of left ventricle (disorder)","1","20210127"),
    ("HF","SNOMED","446221000","Heart failure with normal ejection fraction (disorder)","1","20210127"),
    ("HF","SNOMED","698592004","Asymptomatic left ventricular systolic dysfunction (disorder)","1","20210127"),
    ("HF","SNOMED","703272007","Heart failure with reduced ejection fraction (disorder)","1","20210127"),
    ("HF","SNOMED","717491000000102","Excepted from heart failure quality indicators - informed dissent (finding)","1","20210127"),
    ("HF","SNOMED","760361000000100","Fast track heart failure referral for transthoracic two dimensional echocardiogram","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# artery_dissect
codelist_artery_dissect = spark.createDataFrame(
  [
    ("artery_dissect","ICD10","I71.0","Dissection of aorta [any part]","",""),
    ("artery_dissect","ICD10","I72.0","Aneurysm and dissection of carotid artery","",""),
    ("artery_dissect","ICD10","I72.1","Aneurysm and dissection of artery of upper extremity","",""),
    ("artery_dissect","ICD10","I72.6","Dissection of vertebral artery","","")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# DVT
codelist_DVT = spark.createDataFrame(
  [
    ("DVT","ICD10","I80","Phlebitis and thrombophlebitis","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)

# DVT_other
codelist_DVT_other = spark.createDataFrame(
  [
    ("DVT_other","ICD10","I82.0","Other vein thrombosis","1","20210127"),
    ("DVT_other","ICD10","I82.2","Other vein thrombosis","1","20210127"),    
    ("DVT_other","ICD10","I82.3","Other vein thrombosis","1","20210127"),
    ("DVT_other","ICD10","I82.8","Other vein thrombosis","1","20210127"),
    ("DVT_other","ICD10","I82.9","Other vein thrombosis","1","20210127")    
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)

# DVT_pregnancy
codelist_DVT_pregnancy = spark.createDataFrame(
  [
    ("DVT_pregnancy","ICD10","O22.3","Thrombosis during pregnancy and puerperium","1","20210127"),
    ("DVT_pregnancy","ICD10","O87.1","Thrombosis during pregnancy and puerperium","1","20210127"),
    ("DVT_pregnancy","ICD10","O87.9","Thrombosis during pregnancy and puerperium","1","20210127"),
    ("DVT_pregnancy","ICD10","O88.2","Thrombosis during pregnancy and puerperium","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# intracranial venous thrombosis
codelist_ICVT = spark.createDataFrame(
  [
    ("ICVT","ICD10","G08","Intracranial venous thrombosis","1","20210127"),
    ("ICVT","ICD10","I67.6","Intracranial venous thrombosis","1","20210127"),
    ("ICVT","ICD10","I63.6","Intracranial venous thrombosis","1","20210127"),
    ("ICVT","SNOMED","195230003","Cerebral infarction due to cerebral venous thrombosis,  nonpyogenic","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_ICVT = codelist_ICVT\
  .withColumn('covariate_only', f.when(f.col('code').isin(['195230003']), 1))

# intracranial venous thrombosis during pregnancy
codelist_ICVT_pregnancy = spark.createDataFrame(
  [
    ("ICVT_pregnancy","ICD10","O22.5","ntracranial venous thrombosis in pregnancy and puerperium","1","20210127"),
    ("ICVT_pregnancy","ICD10","O87.3","ntracranial venous thrombosis in pregnancy and puerperium","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# portal vein thrombosis (PVT)
codelist_PVT = spark.createDataFrame(
  [
    ("PVT","ICD10","I81","Portal vein thrombosis","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# venous thromboembolism (VT)
codelist_VT = spark.createDataFrame(
  [
    ("VT","ICD10","O08.2","Embolism following abortion and ectopic and molar pregnancy","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_VT = codelist_VT\
  .withColumn('covariate_only', f.when(f.col('code').isin(['O08.2']), 1))

# COMMAND ----------

# thrombophilia
codelist_thrombophilia = spark.createDataFrame(
  [
    ("thrombophilia","ICD10","D68.5","Primary thrombophilia","1","20210127"),
    ("thrombophilia","ICD10","D68.6","Other thrombophilia","1","20210127"),
    ("thrombophilia","SNOMED","439001009","Acquired thrombophilia","1","20210127"),
    ("thrombophilia","SNOMED","441882000","History of thrombophilia","1","20210127"),
    ("thrombophilia","SNOMED","439698008","Primary thrombophilia","1","20210127"),
    ("thrombophilia","SNOMED","234467004","Thrombophilia","1","20210127"),
    ("thrombophilia","SNOMED","441697004","Thrombophilia associated with pregnancy","1","20210127"),
    ("thrombophilia","SNOMED","442760001","Thrombophilia caused by antineoplastic agent therapy","1","20210127"),
    ("thrombophilia","SNOMED","442197003","Thrombophilia caused by drug therapy","1","20210127"),
    ("thrombophilia","SNOMED","442654007","Thrombophilia caused by hormone therapy","1","20210127"),
    ("thrombophilia","SNOMED","442363001","Thrombophilia caused by vascular device","1","20210127"),
    ("thrombophilia","SNOMED","439126002","Thrombophilia due to acquired antithrombin III deficiency","1","20210127"),
    ("thrombophilia","SNOMED","439002002","Thrombophilia due to acquired protein C deficiency","1","20210127"),
    ("thrombophilia","SNOMED","439125003","Thrombophilia due to acquired protein S deficiency","1","20210127"),
    ("thrombophilia","SNOMED","441079006","Thrombophilia due to antiphospholipid antibody","1","20210127"),
    ("thrombophilia","SNOMED","441762006","Thrombophilia due to immobilisation","1","20210127"),
    ("thrombophilia","SNOMED","442078001","Thrombophilia due to malignant neoplasm","1","20210127"),
    ("thrombophilia","SNOMED","441946009","Thrombophilia due to myeloproliferative disorder","1","20210127"),
    ("thrombophilia","SNOMED","441990004","Thrombophilia due to paroxysmal nocturnal haemoglobinuria","1","20210127"),
    ("thrombophilia","SNOMED","441945008","Thrombophilia due to trauma","1","20210127"),
    ("thrombophilia","SNOMED","442121006","Thrombophilia due to vascular anomaly","1","20210127"),    
    ("thrombophilia","SNOMED","783250007","Hereditary thrombophilia due to congenital histidine-rich (poly-L) glycoprotein deficiency","1",	"20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)

# 20220520 commented out duplicate
#("thrombophilia","SNOMED","439698008","Hereditary thrombophilia","1","20210127"),


# thrombocytopenia
codelist_thrombocytopenia = spark.createDataFrame(
  [
    ("thrombocytopenia","ICD10","D69.3","Thrombocytopenia","1","20210127"),
    ("thrombocytopenia","ICD10","D69.4","Thrombocytopenia","1","20210127"),
    ("thrombocytopenia","ICD10","D69.5","Thrombocytopenia","1","20210127"),
    ("thrombocytopenia","ICD10","D69.6","Thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","74576004","Acquired thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","439007008","Acquired thrombotic thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","28505005","Acute idiopathic thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","128091003","Autoimmune thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","13172003","Autoimmune thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","438476003","Autoimmune thrombotic thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","111588002","Heparin associated thrombotic thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","73397007","Heparin induced thrombocytopaenia","1","20210127"),
    ("thrombocytopenia","SNOMED","438492008","Hereditary thrombocytopenic disorder","1","20210127"),
    ("thrombocytopenia","SNOMED","441511006","History of immune thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","49341000119108","History of thrombocytopaenia",	"1","20210127"),
    ("thrombocytopenia","SNOMED","726769004","HIT (Heparin induced thrombocytopenia) antibody","1","20210127"),
    ("thrombocytopenia","SNOMED","371106008","Idiopathic maternal thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","32273002","Idiopathic thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","2897005","Immune thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","36070007","Immunodeficiency with thrombocytopenia AND eczema","1","20210127"),
    ("thrombocytopenia","SNOMED","33183004","Post infectious thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","267534000","Primary thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","154826009","Secondary thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","866152006","Thrombocytopenia due to 2019 novel coronavirus","1","20210127"),
    ("thrombocytopenia","SNOMED","82190001","Thrombocytopenia due to defective platelet production","1","20210127"),
    ("thrombocytopenia","SNOMED","78345002","Thrombocytopenia due to diminished platelet production","1","20210127"),
    ("thrombocytopenia","SNOMED","191323001","Thrombocytopenia due to extracorporeal circulation of blood","1","20210127"),
    ("thrombocytopenia","SNOMED","87902006","Thrombocytopenia due to non-immune destruction","1","20210127"),
    ("thrombocytopenia","SNOMED","302873008","Thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","417626001","Thrombocytopenic purpura associated with metabolic disorder","1","20210127"),
    ("thrombocytopenia","SNOMED","402653004","Thrombocytopenic purpura due to defective platelet production","1","20210127"),
    ("thrombocytopenia","SNOMED","402654005","Thrombocytopenic purpura due to platelet consumption","1","20210127"),
    ("thrombocytopenia","SNOMED","78129009","Thrombotic thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","441322009","Drug induced thrombotic thrombocytopenic purpura","1","20210127"),
    ("thrombocytopenia","SNOMED","19307009","Drug-induced immune thrombocytopenia","1","20210127"),
    ("thrombocytopenia","SNOMED","783251006","Hereditary thrombocytopenia with normal platelets","1","20210127"),
    ("thrombocytopenia","SNOMED","191322006","Thrombocytopenia caused by drugs","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)
codelist_thrombocytopenia = codelist_thrombocytopenia\
  .withColumn('covariate_only', f.when(f.col('terminology') == 'SNOMED', 1))

# 20220520 commented out duplicate
# ("thrombocytopenia","SNOMED","32273002","Immune thrombocytopenic purpura","1","20210127"),


# thrombotic thrombocytopenic purpura (TTP)
codelist_TTP = spark.createDataFrame(
  [
    ("TTP","ICD10","M31.1","Thrombotic microangiopathy","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# other_arterial_embolism
codelist_other_arterial_embolism = spark.createDataFrame(
  [
    ("other_arterial_embolism","ICD10","I74","arterial embolism and thrombosis","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# disseminated intravascular coagulation (DIC)
codelist_DIC = spark.createDataFrame(
  [
    ("DIC","ICD10","D65","Disseminated intravascular coagulation","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# mesenteric_thrombus
codelist_mesenteric_thrombus = spark.createDataFrame(
  [
    ("mesenteric_thrombus","ICD10","K55.9","Acute vascular disorders of intestine","1","20210127"),
    ("mesenteric_thrombus","ICD10","K55.0","Acute vascular disorders of intestine","1","20210127")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)


# prostate_cancer (SNOMED codes only)
codelist_prostate_cancer = spark.createDataFrame(
  [
    ("prostate_cancer","SNOMED","126906006","Neoplasm of prostate","",""),
    ("prostate_cancer","SNOMED","81232004","Radical cystoprostatectomy","",""),
    ("prostate_cancer","SNOMED","176106009","Radical cystoprostatourethrectomy","",""),
    ("prostate_cancer","SNOMED","176261008","Radical prostatectomy without pelvic node excision","",""),
    ("prostate_cancer","SNOMED","176262001","Radical prostatectomy with pelvic node sampling","",""),
    ("prostate_cancer","SNOMED","176263006","Radical prostatectomy with pelvic lymphadenectomy","",""),
    ("prostate_cancer","SNOMED","369775001","Gleason Score 2-4: Well differentiated","",""),
    ("prostate_cancer","SNOMED","369777009","Gleason Score 8-10: Poorly differentiated","",""),
    ("prostate_cancer","SNOMED","385377005","Gleason grade finding for prostatic cancer (finding)","",""),
    ("prostate_cancer","SNOMED","394932008","Gleason prostate grade 5-7 (medium) (finding)","",""),
    ("prostate_cancer","SNOMED","399068003","Malignant tumor of prostate (disorder)","",""),
    ("prostate_cancer","SNOMED","428262008","History of malignant neoplasm of prostate (situation)","","")
  ],
  ['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate']  
)

# COMMAND ----------

# covid19 (SNOMED codes only)
# covid_status = ['Lab confirmed incidence', 'Lab confirmed historic', 'Clinically confirmed']
                   
tmp_covid19 = spark.createDataFrame(
  [
    ("1008541000000105","Coronavirus ribonucleic acid detection assay (observable entity)","0","1","Lab confirmed incidence"),
    ("1029481000000103","Coronavirus nucleic acid detection assay (observable entity)","0","1","Lab confirmed incidence"),
    ("120814005","Coronavirus antibody (substance)","0","1","Lab confirmed historic"),
    ("121973000","Measurement of coronavirus antibody (procedure)","0","1","Lab confirmed historic"),
    ("1240381000000105","Severe acute respiratory syndrome coronavirus 2 (organism)","0","1","Clinically confirmed"),
    ("1240391000000107","Antigen of severe acute respiratory syndrome coronavirus 2 (substance)","0","1","Lab confirmed incidence"),
    ("1240401000000105","Antibody to severe acute respiratory syndrome coronavirus 2 (substance)","0","1","Lab confirmed historic"),
    ("1240411000000107","Ribonucleic acid of severe acute respiratory syndrome coronavirus 2 (substance)","0","1","Lab confirmed incidence"),
    ("1240421000000101","Serotype severe acute respiratory syndrome coronavirus 2 (qualifier value)","0","1","Lab confirmed historic"),
    ("1240511000000106","Detection of severe acute respiratory syndrome coronavirus 2 using polymerase chain reaction technique (procedure)","0","1","Lab confirmed incidence"),
    ("1240521000000100","Otitis media caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1240531000000103","Myocarditis caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1240541000000107","Infection of upper respiratory tract caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1240551000000105","Pneumonia caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1240561000000108","Encephalopathy caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1240571000000101","Gastroenteritis caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1240581000000104","Severe acute respiratory syndrome coronavirus 2 ribonucleic acid detected (finding)","0","1","Lab confirmed incidence"),
    ("1240741000000103","Severe acute respiratory syndrome coronavirus 2 serology (observable entity)","0","1","Lab confirmed historic"),
    ("1240751000000100","Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1300631000000101","Coronavirus disease 19 severity score (observable entity)","0","1","Clinically confirmed"),
    ("1300671000000104","Coronavirus disease 19 severity scale (assessment scale)","0","1","Clinically confirmed"),
    ("1300681000000102","Assessment using coronavirus disease 19 severity scale (procedure)","0","1","Clinically confirmed"),
    ("1300721000000109","Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 confirmed by laboratory test (situation)","0","1","Lab confirmed historic"),
    ("1300731000000106","Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 confirmed using clinical diagnostic criteria (situation)","0","1","Clinically confirmed"),
    ("1321181000000108","Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 record extraction simple reference set (foundation metadata concept)","0","1","Clinically confirmed"),
    ("1321191000000105","Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 procedures simple reference set (foundation metadata concept)","0","1","Clinically confirmed"),
    ("1321201000000107","Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 health issues simple reference set (foundation metadata concept)","0","1","Clinically confirmed"),
    ("1321211000000109","Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 presenting complaints simple reference set (foundation metadata concept)","0","1","Clinically confirmed"),
    ("1321241000000105","Cardiomyopathy caused by severe acute respiratory syndrome coronavirus 2 (disorder)","0","1","Clinically confirmed"),
    ("1321301000000101","Severe acute respiratory syndrome coronavirus 2 ribonucleic acid qualitative existence in specimen (observable entity)","0","1","Lab confirmed incidence"),
    ("1321311000000104","Severe acute respiratory syndrome coronavirus 2 immunoglobulin M qualitative existence in specimen (observable entity)","0","1","Lab confirmed historic"),
    ("1321321000000105","Severe acute respiratory syndrome coronavirus 2 immunoglobulin G qualitative existence in specimen (observable entity)","0","1","Lab confirmed historic"),
    ("1321331000000107","Arbitrary concentration of severe acute respiratory syndrome coronavirus 2 total immunoglobulin in serum (observable entity)","0","1","Lab confirmed historic"),
    ("1321341000000103","Arbitrary concentration of severe acute respiratory syndrome coronavirus 2 immunoglobulin G in serum (observable entity)","0","1","Lab confirmed historic"),
    ("1321351000000100","Arbitrary concentration of severe acute respiratory syndrome coronavirus 2 immunoglobulin M in serum (observable entity)","0","1","Lab confirmed historic"),
    ("1321541000000108","Severe acute respiratory syndrome coronavirus 2 immunoglobulin G detected (finding)","0","1","Lab confirmed historic"),
    ("1321551000000106","Severe acute respiratory syndrome coronavirus 2 immunoglobulin M detected (finding)","0","1","Lab confirmed historic"),
    ("1321761000000103","Severe acute respiratory syndrome coronavirus 2 immunoglobulin A detected (finding)","0","1","Lab confirmed historic"),
    ("1321801000000108","Arbitrary concentration of severe acute respiratory syndrome coronavirus 2 immunoglobulin A in serum (observable entity)","0","1","Lab confirmed historic"),
    ("1321811000000105","Severe acute respiratory syndrome coronavirus 2 immunoglobulin A qualitative existence in specimen (observable entity)","0","1","Lab confirmed historic"),
    ("1322781000000102","Severe acute respiratory syndrome coronavirus 2 antigen detection result positive (finding)","0","1","Lab confirmed incidence"),
    ("1322871000000109","Severe acute respiratory syndrome coronavirus 2 antibody detection result positive (finding)","0","1","Lab confirmed historic"),
    ("186747009","Coronavirus infection (disorder)","0","1","Clinically confirmed")
  ],
  ['code', 'term', 'sensitive_status', 'include_binary', 'covid_status']
)

codelist_covid19 = tmp_covid19\
  .distinct()\
  .withColumn('name', f.lit('covid19'))\
  .withColumn('terminology', f.lit('SNOMED'))\
  .withColumn('code_type', f.lit(''))\
  .withColumn('RecordDate', f.lit(''))\
  .select(['name', 'terminology', 'code', 'term', 'code_type', 'RecordDate'])

# COMMAND ----------

# MAGIC %md ## 3.3 Medications

# COMMAND ----------

# anticoagulant
tmp_anticoagulant = _pmeds\
  .where(\
    (f.substring(f.col('PrescribedBNFCode'), 1, 6) == '020802')\
      & ~(\
        (f.substring(f.col('PrescribedBNFCode'), 1, 8) == '0208020I')\
          | (f.substring(f.col('PrescribedBNFCode'), 1, 8) == '0208020W')\
      )\
  )\
  .withColumn('name', f.lit('anticoagulant'))


# antiplatelet
tmp_antiplatelet = _pmeds\
  .where(f.substring(f.col('PrescribedBNFCode'), 1, 4) == '0209')\
  .withColumn('name', f.lit('antiplatelet'))
  

# bp_lowering
# 0204 -- beta blockers
#   exclude 0204000R0 -- propranolol
#   exclude 0204000Q0 -- propranolol
# 020502 -- centrally acting antihypertensives
#   exclude 0205020G -- guanfacine because it is only used for ADHD
# 020504 -- alpha blockers
# 020602 -- calcium channel blockers
# 020203 -- potassium sparing diuretics
# 020201 -- thiazide diuretics
# 020501 -- vasodilator antihypertensives
# 0205051 -- angiotensin-converting enzyme inhibitors
# 0205052 -- angiotensin-II receptor antagonists
#   exclude 0205052AE -- drugs for heart failure, not for hypertension
# 0205053A0 -- aliskiren
tmp_bp_lowering = _pmeds\
  .where(\
    (\
      (f.substring(f.col('PrescribedBNFCode'), 1, 4) == '0204')\
        & ~(\
          (f.substring(f.col('PrescribedBNFCode'), 1, 9) == '0204000R0')\
            | (f.substring(f.col('PrescribedBNFCode'), 1, 9) == '0204000Q0')\
        )\
    )\
    | (\
      (f.substring(f.col('PrescribedBNFCode'), 1, 6) == '020502')\
        & ~(\
          (f.substring(f.col('PrescribedBNFCode'), 1, 8) == '0205020G')\           
        )\
    )\
    | (f.substring(f.col('PrescribedBNFCode'), 1, 6) == '020504')\
    | (f.substring(f.col('PrescribedBNFCode'), 1, 6) == '020602')\
    | (f.substring(f.col('PrescribedBNFCode'), 1, 6) == '020203')\
    | (f.substring(f.col('PrescribedBNFCode'), 1, 6) == '020201')\
    | (f.substring(f.col('PrescribedBNFCode'), 1, 6) == '020501')\
    | (f.substring(f.col('PrescribedBNFCode'), 1, 7) == '0205051')\
    | (\
       (f.substring(f.col('PrescribedBNFCode'), 1, 7) == '0205052')\
        & ~(\ 
           (f.substring(f.col('PrescribedBNFCode'), 1, 9) == '0205052AE')\         
        )\
    )\         
    | (f.substring(f.col('PrescribedBNFCode'), 1, 9) == '0205053A0')\
  )\
  .withColumn('name', f.lit('bp_lowering'))


# cocp
tmp_cocp = _pmeds\
  .where(f.substring(f.col('PrescribedBNFCode'), 1, 6) == '070301')\
  .withColumn('name', f.lit('cocp'))


# hrt
tmp_hrt = _pmeds\
  .where(f.substring(f.col('PrescribedBNFCode'), 1, 7) == '0604011')\
  .withColumn('name', f.lit('hrt'))


# lipid_lowering
tmp_lipid_lowering = _pmeds\
  .where(f.substring(f.col('PrescribedBNFCode'), 1, 4) == '0212')\
  .withColumn('name', f.lit('lipid_lowering'))


# immunosuppressants
tmp_immunosuppressants = _pmeds\
  .where(f.substring(f.col('PrescribedBNFCode'), 1, 4) == '0802')\
  .withColumn('name', f.lit('immunosuppressants'))


# corticosteroids
tmp_corticosteroids = _pmeds\
  .where(f.substring(f.col('PrescribedBNFCode'), 1, 6) == '080202')\
  .withColumn('name', f.lit('corticosteroids'))


# append and tidy
codelist_meds = tmp_anticoagulant\
  .unionByName(tmp_antiplatelet)\
  .unionByName(tmp_bp_lowering)\
  .unionByName(tmp_cocp)\
  .unionByName(tmp_hrt)\
  .unionByName(tmp_lipid_lowering)\
  .unionByName(tmp_immunosuppressants)\
  .unionByName(tmp_corticosteroids)\
  .withColumnRenamed('PrescribedBNFCode', 'code')\
  .withColumnRenamed('PrescribedBNFName', 'term')\
  .withColumn('terminology', f.lit('BNF'))\
  .select(['name', 'terminology', 'code', 'term'])\
  .orderBy(['name', 'terminology', 'code', 'term'])

# COMMAND ----------

# MAGIC %md ## 3.4 Maternal

# COMMAND ----------

# gestational hypertension
codelist_gest_hypertension = spark.createDataFrame(
  [
    ('gest_hypertension','ICD10','O13','Gestational [pregnancy-induced] hypertension'),
    ('gest_hypertension','ICD10','O11','Pre-existing hypertension with pre-eclampsia'),
    ('gest_hypertension','ICD10','O14','Pre-eclampsia'),
    ('gest_hypertension','ICD10','O15','Eclampsia'),
    ('gest_hypertension','SNOMED','198954000','Renal hypertension complicating pregnancy childbirth and the puerperium with postnatal complication (disorder)'),
    ('gest_hypertension','SNOMED','198949009','Renal hypertension complicating pregnancy childbirth and the puerperium (disorder)'),
    ('gest_hypertension','SNOMED','198953006','Renal hypertension complicating pregnancy childbirth and the puerperium - not delivered (disorder)'),
    ('gest_hypertension','SNOMED','198952001','Renal hypertension complicating pregnancy childbirth and the puerperium - delivered with postnatal complication (disorder)'),
    ('gest_hypertension','SNOMED','198951008','Renal hypertension complicating pregnancy childbirth and the puerperium - delivered (disorder)'),
    ('gest_hypertension','SNOMED','81626002','Malignant hypertension in obstetric context (disorder)'),
    ('gest_hypertension','SNOMED','29259002','Malignant hypertension complicating AND/OR reason for care during pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','10562009','Malignant hypertension complicating AND/OR reason for care during childbirth (disorder)'),
    ('gest_hypertension','SNOMED','111438007','Hypertension secondary to renal disease in obstetric context (disorder)'),
    ('gest_hypertension','SNOMED','48552006','Hypertension secondary to renal disease complicating AND/OR reason for care during pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','26078007','Hypertension secondary to renal disease complicating AND/OR reason for care during childbirth (disorder)'),
    ('gest_hypertension','SNOMED','72022006','Essential hypertension in obstetric context (disorder)'),
    ('gest_hypertension','SNOMED','78808002','Essential hypertension complicating AND/OR reason for care during pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','18416000','Essential hypertension complicating AND/OR reason for care during childbirth (disorder)'),
    ('gest_hypertension','SNOMED','63287004','Benign essential hypertension in obstetric context (disorder)'),
    ('gest_hypertension','SNOMED','198947006','Benign essential hypertension complicating pregnancy childbirth and the puerperium with postnatal complication (disorder)'),
    ('gest_hypertension','SNOMED','198942000','Benign essential hypertension complicating pregnancy childbirth and the puerperium (disorder)'),
    ('gest_hypertension','SNOMED','198946002','Benign essential hypertension complicating pregnancy childbirth and the puerperium - not delivered (disorder)'),
    ('gest_hypertension','SNOMED','198945003','Benign essential hypertension complicating pregnancy childbirth and the puerperium - delivered with postnatal complication (disorder)'),
    ('gest_hypertension','SNOMED','198944004','Benign essential hypertension complicating pregnancy childbirth and the puerperium - delivered (disorder)'),
    ('gest_hypertension','SNOMED','23717007','Benign essential hypertension complicating AND/OR reason for care during pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','71874008','Benign essential hypertension complicating AND/OR reason for care during childbirth (disorder)'),
    ('gest_hypertension','SNOMED','48194001','Pregnancy-induced hypertension (disorder)'),
    ('gest_hypertension','SNOMED','288250001','Maternal hypertension (disorder)'),
    ('gest_hypertension','SNOMED','307632004','Non-proteinuric hypertension of pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','237281009','Moderate proteinuric hypertension of pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','698638005','Pregnancy induced hypertension with pulmonary edema (disorder)'), 
    ('gest_hypertension','SNOMED','237279007','Transient hypertension of pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','198968007','Transient hypertension of pregnancy with postnatal complication (disorder)'),
    ('gest_hypertension','SNOMED','198967002','Transient hypertension of pregnancy - not delivered (disorder)'),
    ('gest_hypertension','SNOMED','8762007','Chronic hypertension in obstetric context (disorder)'),
    ('gest_hypertension','SNOMED','37618003','Chronic hypertension complicating AND/OR reason for care during pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','8218002','Chronic hypertension complicating AND/OR reason for care during childbirth (disorder)'),
    ('gest_hypertension','SNOMED','237282002','Impending eclampsia (disorder)'),
    ('gest_hypertension','SNOMED','15938005','Eclampsia (disorder)'),
    ('gest_hypertension','SNOMED','198990007','Eclampsia - delivered (disorder)'),
    ('gest_hypertension','SNOMED','303063000','Eclampsia in puerperium (disorder)'),
    ('gest_hypertension','SNOMED','198991006','Eclampsia - delivered with postnatal complication (disorder)'),
    ('gest_hypertension','SNOMED','198993009','Eclampsia with postnatal complication (disorder)'),
    ('gest_hypertension','SNOMED','198992004','Eclampsia in pregnancy (disorder)'),
    ('gest_hypertension','SNOMED','237283007','Eclampsia in labor (disorder)'),
    ('gest_hypertension','SNOMED','398254007','Pre-eclampsia'),
    ('gest_hypertension','SNOMED','41114007','Mild pre-eclampsia (disorder)'),
    ('gest_hypertension','SNOMED','765182005','Pre-eclampsia in puerperium (disorder)'),
    ('gest_hypertension','SNOMED','46764007','Severe pre-eclampsia (disorder)'),
    ('gest_hypertension','SNOMED','198983002','Severe pre-eclampsia - delivered (disorder)'),
    ('gest_hypertension','SNOMED','198984008','Severe pre-eclampsia - delivered with postnatal complication'),
    ('gest_hypertension','SNOMED','198985009','Severe pre-eclampsia - not delivered (disorder)'),
    ('gest_hypertension','SNOMED','198986005','Severe pre-eclampsia with postnatal complication')
  ],
  ['name', 'terminology', 'code', 'term']  
)

# 20220520 commented out duplicate
# ('gest_hypertension','SNOMED','198968007','Transient hypertension of pregnancy with postnatal complication (disorder)'),
# ('gest_hypertension','SNOMED','307632004','Non-proteinuric hypertension of pregnancy (disorder)'),
# ('gest_hypertension','SNOMED','237281009','Moderate proteinuric hypertension of pregnancy (disorder)')


# gestatonal diabetes
codelist_gest_diabetes = spark.createDataFrame(
  [
    ('gest_diabetes','ICD10','O244','Diabetes mellitus arising in pregnancy'),
    ('gest_diabetes','ICD10','O249','Diabetes mellitus in pregnancy, unspecified'),
    ('gest_diabetes','SNOMED','11687002','Gestational diabetes mellitus (disorder)'),
    ('gest_diabetes','SNOMED','75022004','Gestational diabetes mellitus class A>1< (disorder)'),
    ('gest_diabetes','SNOMED','46894009','Gestational diabetes mellitus class A>2< (disorder)'),
    ('gest_diabetes','SNOMED','40801000119106','Gestational diabetes mellitus complicating pregnancy (disorder)'),
    ('gest_diabetes','SNOMED','10753491000119101','Gestational diabetes mellitus in childbirth (disorder)'),
    ('gest_diabetes','SNOMED','472699005','Gestational diabetes mellitus uncontrolled (finding)'),
    ('gest_diabetes','SNOMED','40791000119105','Postpartum gestational diabetes mellitus (disorder)')
  ],
  ['name', 'terminology', 'code', 'term']  
)



# preeclampsia
codelist_preeclampsia = spark.createDataFrame(
  [
    ('preeclampsia','ICD10','O11','Pre-existing hypertension with pre-eclampsia'),
    ('preeclampsia','ICD10','O14','Pre-eclampsia'),
    ('preeclampsia','ICD10','O15','Eclampsia'),
    ('preeclampsia','SNOMED','69909000','Eclampsia added to pre-existing hypertension (disorder)'),
    ('preeclampsia','SNOMED','10752641000119102','Eclampsia with pre-existing hypertension in childbirth (disorder)'),
    ('preeclampsia','SNOMED','237282002','Impending eclampsia (disorder)'),
    ('preeclampsia','SNOMED','15938005','Eclampsia (disorder)'),
    ('preeclampsia','SNOMED','198990007','Eclampsia - delivered (disorder)'),
    ('preeclampsia','SNOMED','303063000','Eclampsia in puerperium (disorder)'),
    ('preeclampsia','SNOMED','198991006','Eclampsia - delivered with postnatal complication (disorder)'),
    ('preeclampsia','SNOMED','198993009','Eclampsia with postnatal complication (disorder)'),
    ('preeclampsia','SNOMED','198992004','Eclampsia in pregnancy (disorder)'),
    ('preeclampsia','SNOMED','237283007','Eclampsia in labor (disorder)'),
    ('preeclampsia','SNOMED','67359005','Pre-eclampsia added to pre-existing hypertension (disorder)'),
    ('preeclampsia','SNOMED','198999008','Pre-eclampsia or eclampsia with pre-existing hypertension - delivered (disorder)'),
    ('preeclampsia','SNOMED','199000005','Pre-eclampsia or eclampsia with pre-existing hypertension - delivered with postnatal complication (disorder)'),
    ('preeclampsia','SNOMED','199002002','Pre-eclampsia or eclampsia with pre-existing hypertension - not delivered (disorder)'),
    ('preeclampsia','SNOMED','198997005','Pre-eclampsia or eclampsia with pre-existing hypertension (disorder)'),
    ('preeclampsia','SNOMED','199003007','Pre-eclampsia or eclampsia with pre-existing hypertension with postnatal complication (disorder)'),
    ('preeclampsia','SNOMED','398254007','Pre-eclampsia'),
    ('preeclampsia','SNOMED','41114007','Mild pre-eclampsia (disorder)'),
    ('preeclampsia','SNOMED','765182005','Pre-eclampsia in puerperium (disorder)'),
    ('preeclampsia','SNOMED','46764007','Severe pre-eclampsia (disorder)'),
    ('preeclampsia','SNOMED','198983002','Severe pre-eclampsia - delivered (disorder)'),
    ('preeclampsia','SNOMED','198984008','Severe pre-eclampsia - delivered with postnatal complication'),
    ('preeclampsia','SNOMED','198985009','Severe pre-eclampsia - not delivered (disorder)'),
    ('preeclampsia','SNOMED','198986005','Severe pre-eclampsia with postnatal complication')    
  ],
  ['name', 'terminology', 'code', 'term']  
)

# 20220520 commented out copy and paste error
# ('preeclampsia','SNOMED','237281009','Moderate proteinuric hypertension of pregnancy (disorder)')


# polycystic ovary syndrome (PCOS)
codelist_PCOS = spark.createDataFrame(
  [
    ('PCOS','ICD10','E282','Polycystic ovarian syndrome'),
    ('PCOS','SNOMED','237055002','Polycystic ovary syndrome'),
    ('PCOS','SNOMED','271199009','(Polycystic ovaries) or (Stein-Leventhal syndrome) (disorder)'),
    ('PCOS','SNOMED','426635002','Endoscopic drilling of ovary (procedure)'),
    ('PCOS','SNOMED','830053004','Polycystic ovary syndrome of left ovary (disorder)'),
    ('PCOS','SNOMED','830054005','Polycystic ovary syndrome of bilateral ovaries (disorder)'),
    ('PCOS','SNOMED','830052009','Polycystic ovary syndrome of right ovary (disorder)'),
    ('PCOS','SNOMED','69878008','Polycystic ovaries (disorder)')
  ],
  ['name', 'terminology', 'code', 'term']  
)

# COMMAND ----------

tmp_multi_gest = """
name,code,terminology,term
multi_gest,R07,OPCS4,Therapeutic endoscopic operations for twin to twin transfusion syndrome
multi_gest,R072,OPCS4,Endoscopic serial drainage of amniotic fluid for twin to twin transfusion syndrome
multi_gest,R078,OPCS4,Other specified therapeutic endoscopic operations for twin to twin transfusion syndrome
multi_gest,R079,OPCS4,Unspecified therapeutic endoscopic operations for twin to twin transfusion syndrome
multi_gest,R08,OPCS4,Therapeutic percutaneous operations for twin to twin transfusion syndrome
multi_gest,R082,OPCS4,Percutaneous serial drainage of amniotic fluid for twin to twin transfusion syndrome
multi_gest,R088,OPCS4,Other specified therapeutic percutaneous operations for twin to twin transfusion syndrome
multi_gest,R089,OPCS4,Unspecified therapeutic percutaneous operations for twin to twin transfusion syndrome
multi_gest,Z383 ,ICD10,"Twin liveborn infant, born in hospital"
multi_gest,Z384,ICD10,"Twin liveborn infant, born outside hospital"
multi_gest,Z385,ICD10,"Twin liveborn infant, unspecified as to place of birth"
multi_gest,Z386,ICD10,"Other multiple liveborn infant, born in hospital"
multi_gest,Z387,ICD10,"Other multiple liveborn infant, born outside hospital"
multi_gest,Z388,ICD10,"Other multiple liveborn infant, unspecified as to place of birth"
multi_gest,O30,ICD10,Multiple gestation
multi_gest,O31,ICD10,Complications specific to multiple gestation
multi_gest,O632 ,ICD10,"Delayed delivery of second twin, triplet, etc."
multi_gest,102876002,SNOMED,multigravida
multi_gest,10760661000119109,SNOMED,triplets  some live born
multi_gest,10760701000119102,SNOMED,quadruplets  all live born
multi_gest,10760741000119100,SNOMED,quadruplets  some live born
multi_gest,10760781000119105,SNOMED,quintuplets  all live born
multi_gest,10760821000119100,SNOMED,quintuplets  some live born
multi_gest,1148801000000108,SNOMED,monochorionic monoamniotic triplet pregnancy
multi_gest,1148811000000105,SNOMED,trichorionic triamniotic triplet pregnancy
multi_gest,1148821000000104,SNOMED,dichorionic triamniotic triplet pregnancy
multi_gest,1148841000000106,SNOMED,dichorionic diamniotic triplet pregnancy
multi_gest,1149411000000103,SNOMED,monochorionic diamniotic triplet pregnancy
multi_gest,1149421000000109,SNOMED,monochorionic triamniotic triplet pregnancy
multi_gest,13404009,SNOMED,twin-to-twin blood transfer
multi_gest,13859001,SNOMED,premature birth of newborn twins
multi_gest,151441000119105,SNOMED,twin live born in hospital by vaginal delivery
multi_gest,15467003,SNOMED,term birth of identical twins  both living
multi_gest,16356006,SNOMED,multiple pregnancy
multi_gest,169576004,SNOMED,antenatal care of multipara
multi_gest,169605007,SNOMED,antenatal care: multiparous  older than 35 years
multi_gest,169828005,SNOMED,twins - both live born
multi_gest,169829002,SNOMED,twins - one still and one live born
multi_gest,169830007,SNOMED,twins - both stillborn
multi_gest,169831006,SNOMED,triplets - all live born
multi_gest,169832004,SNOMED,triplets - two live and one stillborn
multi_gest,169833009,SNOMED,triplets - one live and two stillborn
multi_gest,169834003,SNOMED,triplets - three stillborn
multi_gest,17333005,SNOMED,term birth of multiple newborns
multi_gest,199305006,SNOMED,complications specific to multiple gestation
multi_gest,199317008,SNOMED,twin pregnancy - delivered
multi_gest,199318003,SNOMED,twin pregnancy with antenatal problem
multi_gest,199321001,SNOMED,triplet pregnancy - delivered
multi_gest,199322008,SNOMED,triplet pregnancy with antenatal problem
multi_gest,199325005,SNOMED,quadruplet pregnancy - delivered
multi_gest,199326006,SNOMED,quadruplet pregnancy with antenatal problem
multi_gest,199329004,SNOMED,multiple delivery  all spontaneous
multi_gest,199330009,SNOMED,multiple delivery  all by forceps and vacuum extractor
multi_gest,199331008,SNOMED,multiple delivery  all by cesarean section
multi_gest,199378009,SNOMED,multiple pregnancy with malpresentation
multi_gest,199380003,SNOMED,multiple pregnancy with malpresentation - delivered
multi_gest,199381004,SNOMED,multiple pregnancy with malpresentation with antenatal problem
multi_gest,199787003,SNOMED,locked twins - delivered
multi_gest,199788008,SNOMED,locked twins with antenatal problem
multi_gest,199860006,SNOMED,delayed delivery of second twin  triplet etc
multi_gest,199862003,SNOMED,delayed delivery second twin - delivered
multi_gest,199863008,SNOMED,delayed delivery second twin with antenatal problem
multi_gest,20272009,SNOMED,premature birth of multiple newborns
multi_gest,21987001,SNOMED,delayed delivery of second of multiple births
multi_gest,22514005,SNOMED,term birth of fraternal twins  one living  one stillborn
multi_gest,237236005,SNOMED,undiagnosed multiple pregnancy
multi_gest,237237001,SNOMED,undiagnosed twin
multi_gest,237312008,SNOMED,multiple delivery  function
multi_gest,237321009,SNOMED,delayed delivery of triplet
multi_gest,238820002,SNOMED,erythema multiforme of pregnancy
multi_gest,24146004,SNOMED,premature birth of newborn quintuplets
multi_gest,25192009,SNOMED,premature birth of fraternal twins  both living
multi_gest,275429002,SNOMED,delayed delivery of second twin
multi_gest,28030000,SNOMED,twin birth
multi_gest,281052005,SNOMED,triplet birth
multi_gest,29997008,SNOMED,premature birth of newborn triplets
multi_gest,30165006,SNOMED,premature birth of fraternal twins  one living  one stillborn
multi_gest,33340004,SNOMED,multiple conception
multi_gest,34089005,SNOMED,premature birth of newborn quadruplets
multi_gest,34100008,SNOMED,premature birth of identical twins  both living
multi_gest,35381000119101,SNOMED,quadruplet pregnancy with loss of one or more fetuses
multi_gest,36801000119105,SNOMED,continuing triplet pregnancy after spontaneous abortion of one or more fetuses
multi_gest,3798002,SNOMED,premature birth of identical twins  both stillborn
multi_gest,38257001,SNOMED,term birth of identical twins  one living  one stillborn
multi_gest,39213008,SNOMED,premature birth of identical twins  one living  one stillborn
multi_gest,417006004,SNOMED,twin reversal arterial perfusion syndrome
multi_gest,428511009,SNOMED,multiple pregnancy with one fetal loss
multi_gest,429187001,SNOMED,continuing pregnancy after intrauterine death of twin fetus
multi_gest,442478007,SNOMED,multiple pregnancy involving intrauterine pregnancy and tubal pregnancy
multi_gest,443460007,SNOMED,multigravida of advanced maternal age
multi_gest,445866007,SNOMED,ultrasonography of multiple pregnancy for fetal anomaly
multi_gest,446810002,SNOMED,ultrasonography of multiple pregnancy for fetal nuchal translucency
multi_gest,45384004,SNOMED,multiple birth
multi_gest,459166009,SNOMED,dichorionic diamniotic twin pregnancy
multi_gest,459167000,SNOMED,monochorionic twin pregnancy
multi_gest,459168005,SNOMED,monochorionic diamniotic twin pregnancy
multi_gest,459169002,SNOMED,monochorionic diamniotic twin pregnancy with similar amniotic fluid volumes
multi_gest,459170001,SNOMED,monochorionic diamniotic twin pregnancy with dissimilar amniotic fluid volumes
multi_gest,459171002,SNOMED,monochorionic monoamniotic twin pregnancy
multi_gest,472321009,SNOMED,continuing pregnancy after intrauterine death of one twin with intrauterine retention of dead twin
multi_gest,50758004,SNOMED,term birth of newborn twins
multi_gest,52942000,SNOMED,term birth of stillborn twins
multi_gest,54650005,SNOMED,premature birth of stillborn twins
multi_gest,60810003,SNOMED,quadruplet pregnancy
multi_gest,64254006,SNOMED,triplet pregnancy
multi_gest,65147003,SNOMED,twin pregnancy
multi_gest,69777007,SNOMED,interlocked twins
multi_gest,702452007,SNOMED,quadruplet birth
multi_gest,702453002,SNOMED,quintuplet birth
multi_gest,702741002,SNOMED,supervision of high risk pregnancy for multigravida
multi_gest,702743004,SNOMED,supervision of high risk pregnancy for multigravida age 15 years or younger
multi_gest,713575004,SNOMED,dizygotic twin pregnancy
multi_gest,713576003,SNOMED,monozygotic twin pregnancy
multi_gest,75697004,SNOMED,term birth of identical twins  both stillborn
multi_gest,762612009,SNOMED,quadruplets with all four stillborn
multi_gest,762613004,SNOMED,quintuplets with all five stillborn
multi_gest,7888004,SNOMED,term birth of newborn quadruplets
multi_gest,80224003,SNOMED,multiple gestation with one or more fetal malpresentations
multi_gest,80997009,SNOMED,quintuplet pregnancy
multi_gest,816148008,SNOMED,disproportion between fetus and pelvis due to conjoined twins
multi_gest,83121003,SNOMED,term birth of fraternal twins  both stillborn
multi_gest,8333008,SNOMED,term birth of newborn triplets
multi_gest,84382006,SNOMED,premature birth of fraternal twins  both stillborn
multi_gest,855021000000107,SNOMED,ultrasonography of multiple pregnancy
multi_gest,855031000000109,SNOMED,doppler ultrasonography of multiple pregnancy
multi_gest,86803008,SNOMED,term birth of newborn quintuplets
multi_gest,87662006,SNOMED,term birth of fraternal twins  both living
"""
codelist_multi_gest = spark.createDataFrame(pd.DataFrame(pd.read_csv(io.StringIO(tmp_multi_gest))))

# COMMAND ----------

# preterm
tmp_preterm = """
name,code,terminology,term
preterm,102503000,SNOMED,well premature newborn
preterm,102504006,SNOMED,well premature male newborn
preterm,102505007,SNOMED,well premature female newborn
preterm,10761141000119107,SNOMED,preterm labor in second trimester with preterm delivery in second trimester
preterm,10761191000119104,SNOMED,preterm labor in third trimester with preterm delivery in third trimester
preterm,10761241000119104,SNOMED,preterm labor with preterm delivery
preterm,10761341000119105,SNOMED,preterm labor without delivery
preterm,112075006,SNOMED,premature birth of newborn sextuplets
preterm,13859001,SNOMED,premature birth of newborn twins
preterm,20272009,SNOMED,premature birth of multiple newborns
preterm,24146004,SNOMED,premature birth of newborn quintuplets
preterm,25192009,SNOMED,premature birth of fraternal twins  both living
preterm,282020008,SNOMED,premature delivery
preterm,289733005,SNOMED,premature uterine contraction
preterm,28996007,SNOMED,stillbirth of premature female (1000-2499 gms.)
preterm,29997008,SNOMED,premature birth of newborn triplets
preterm,30165006,SNOMED,premature birth of fraternal twins  one living  one stillborn
preterm,34089005,SNOMED,premature birth of newborn quadruplets
preterm,34100008,SNOMED,premature birth of identical twins  both living
preterm,367494004,SNOMED,premature birth of newborn
preterm,3798002,SNOMED,premature birth of identical twins  both stillborn
preterm,39213008,SNOMED,premature birth of identical twins  one living  one stillborn
preterm,4886009,SNOMED,premature birth of newborn male
preterm,49550006,SNOMED,premature pregnancy delivered
preterm,54650005,SNOMED,premature birth of stillborn twins
preterm,59403008,SNOMED,premature birth of newborn female
preterm,6383007,SNOMED,premature labor
preterm,698716002,SNOMED,preterm spontaneous labor with preterm delivery
preterm,698717006,SNOMED,preterm spontaneous labor with term delivery
preterm,724488005,SNOMED,preterm delivery following induction of labor
preterm,724489002,SNOMED,preterm delivery following cesarean section
preterm,77814006,SNOMED,stillbirth of premature male (1000-2499 gms.)
preterm,84382006,SNOMED,premature birth of fraternal twins  both stillborn
"""
codelist_preterm = spark.createDataFrame(pd.DataFrame(pd.read_csv(io.StringIO(tmp_preterm))))

# COMMAND ----------

# stillbirth

# v1
tmp_stillbirth_v1 = spark.createDataFrame(
  [
    ('stillbirth','ICD10','Z371','Single stillbirth'),
    ('stillbirth','ICD10','Z373','Twins, one liveborn and one stillborn'),
    ('stillbirth','ICD10','Z374','Twins, both stillborn'),
    ('stillbirth','ICD10','Z377','Other multiple births, all stillborn'),
    ('stillbirth','ICD10','P95','Stillbirth'),
    ('stillbirth','SNOMED','713202001','Antepartum stillbirth'),
    ('stillbirth','SNOMED','237365001','Fresh stillbirth'),
    ('stillbirth','SNOMED','237366000','Macerated stillbirth'),
    ('stillbirth','SNOMED','762612009','Quadruplets with all four stillborn'),
    ('stillbirth','SNOMED','762613004','Quintuplets with all five stillborn'),
    ('stillbirth','SNOMED','762614005','Sextuplets with all six stillborn'),
    ('stillbirth','SNOMED','169827000','Single stillbirth'),
    ('stillbirth','SNOMED','408796008','Stillbirth - unknown if fetal death intrapartum or prior to labor'),
    ('stillbirth','SNOMED','68509000','Stillbirth of immature female (500-999 gms.)'),
    ('stillbirth','SNOMED','72543004','Stillbirth of immature fetus, sex undetermined (500-999 gms.)'),
    ('stillbirth','SNOMED','35608009','Stillbirth of immature male (500-999 gms.)'),
    ('stillbirth','SNOMED','70112005','Stillbirth of mature female (2500 gms. or more)'),
    ('stillbirth','SNOMED','7860005','Stillbirth of mature male (2500 gms. or more)'),
    ('stillbirth','SNOMED','28996007','Stillbirth of premature female (1000-2499 gms.)'),
    ('stillbirth','SNOMED','77814006','Stillbirth of premature male (1000-2499 gms.)'),
    ('stillbirth','SNOMED','921611000000101','Intrapartum stillbirth'),
    ('stillbirth','SNOMED','3561000000102','Form 6 - certificate of stillbirth'),
    ('stillbirth','SNOMED','169833009','Triplets - one live and two stillborn'),
    ('stillbirth','SNOMED','169834003','Triplets - three stillborn'),
    ('stillbirth','SNOMED','169832004','Triplets - two live and one stillborn'),
    ('stillbirth','SNOMED','169830007','Twins - both stillborn'),
    ('stillbirth','SNOMED','169829002','Twins - one still and one live born'),
    ('stillbirth','SNOMED','237361005','Antepartum fetal death'),
    ('stillbirth','SNOMED','445868008','Fetal death due to anoxia'),
    ('stillbirth','SNOMED','445862009','Fetal death due to asphyxia'),
    ('stillbirth','SNOMED','67313008','Fetal death due to termination of pregnancy'),
    ('stillbirth','SNOMED','17766007','Fetal death from asphyxia AND/OR anoxia, not clear if noted before OR after onset of labor'),
    ('stillbirth','SNOMED','237362003','Fetal intrapartum death'),
    ('stillbirth','SNOMED','199607009','Intrauterine death - delivered'),
    ('stillbirth','SNOMED','237363008','Intrauterine death of one twin'),
    ('stillbirth','SNOMED','199608004','Intrauterine death with antenatal problem')
  ],
  ['name', 'terminology', 'code', 'term']  
)


# v2
# Source
# CVD-COVID-UK Consortium Shared Folder\Projects\CCU018 Pregnancy\6. Analysis and results\pregnancy_codes.xlsx
tmp_stillbirth_v2 = """
name,code,terminology,term
stillbirth,Z371,ICD10,Single stillbirth
stillbirth,Z373,ICD10,"Twins, one liveborn and one stillborn"
stillbirth,Z374,ICD10,"Twins, both stillborn"
stillbirth,Z377,ICD10,"Other multiple births, all stillborn"
stillbirth,P95,ICD10,Stillbirth
stillbirth,14022007,SNOMED,"fetal death, affecting management of mother"
stillbirth,169827000,SNOMED,single stillbirth
stillbirth,169829002,SNOMED,twins - one still and one live born
stillbirth,169830007,SNOMED,twins - both stillborn
stillbirth,169832004,SNOMED,triplets - two live and one stillborn
stillbirth,169833009,SNOMED,triplets - one live and two stillborn
stillbirth,169834003,SNOMED,triplets - three stillborn
stillbirth,199307003,SNOMED,continuing pregnancy after intrauterine death of one or more fetuses
stillbirth,22514005,SNOMED,"term birth of fraternal twins, one living, one stillborn"
stillbirth,237364002,SNOMED,stillbirth
stillbirth,237365001,SNOMED,fresh stillbirth
stillbirth,237366000,SNOMED,macerated stillbirth
stillbirth,28996007,SNOMED,stillbirth of premature female (1000-2499 gms.)
stillbirth,30165006,SNOMED,"premature birth of fraternal twins, one living, one stillborn"
stillbirth,35608009,SNOMED,stillbirth of immature male (500-999 gms.)
stillbirth,3798002,SNOMED,"premature birth of identical twins, both stillborn"
stillbirth,38257001,SNOMED,"term birth of identical twins, one living, one stillborn"
stillbirth,39213008,SNOMED,"premature birth of identical twins, one living, one stillborn"
stillbirth,399363000,SNOMED,late fetal death affecting management of mother
stillbirth,408796008,SNOMED,stillbirth - unknown if fetal death intrapartum or prior to labor
stillbirth,429187001,SNOMED,continuing pregnancy after intrauterine death of twin fetus
stillbirth,445548006,SNOMED,dead fetus in utero
stillbirth,472321009,SNOMED,continuing pregnancy after intrauterine death of one twin with intrauterine retention of dead twin
stillbirth,522101000000109,SNOMED,fetal death before 24 weeks with retention of dead fetus
stillbirth,52942000,SNOMED,term birth of stillborn twins
stillbirth,54650005,SNOMED,premature birth of stillborn twins
stillbirth,68509000,SNOMED,stillbirth of immature female (500-999 gms.)
stillbirth,70112005,SNOMED,stillbirth of mature female (2500 gms. or more)
stillbirth,713202001,SNOMED,antepartum stillbirth
stillbirth,72543004,SNOMED,"stillbirth of immature fetus, sex undetermined (500-999 gms.)"
stillbirth,75697004,SNOMED,"term birth of identical twins, both stillborn"
stillbirth,762612009,SNOMED,quadruplets with all four stillborn
stillbirth,762613004,SNOMED,quintuplets with all five stillborn
stillbirth,762614005,SNOMED,sextuplets with all six stillborn
stillbirth,77814006,SNOMED,stillbirth of premature male (1000-2499 gms.)
stillbirth,7860005,SNOMED,stillbirth of mature male (2500 gms. or more)
stillbirth,83121003,SNOMED,"term birth of fraternal twins, both stillborn"
stillbirth,84382006,SNOMED,"premature birth of fraternal twins, both stillborn"
stillbirth,921611000000101,SNOMED,intrapartum stillbirth
"""
tmp_stillbirth_v2 = spark.createDataFrame(pd.DataFrame(pd.read_csv(io.StringIO(tmp_stillbirth_v2))))


# compare files
file1, file2, file3, file3_differences = compare_files(tmp_stillbirth_v1, tmp_stillbirth_v2, ['name', 'terminology', 'code'], warningError=0); print()


# append and deduplicate (prioritising v2)
tmp_stillbirth_v2 = tmp_stillbirth_v2\
  .withColumn('_v', f.lit(2))
tmp_stillbirth = tmp_stillbirth_v1\
  .withColumn('_v', f.lit(1))\
  .unionByName(tmp_stillbirth_v2)\
  .orderBy('name', 'terminology', 'code', '_v')
_win = Window\
  .partitionBy(['name', 'terminology', 'code'])\
  .orderBy(f.desc('_v'))
_win_rownummax = Window\
  .partitionBy(['name', 'terminology', 'code'])
tmp_stillbirth = tmp_stillbirth\
  .withColumn('_rownum', f.row_number().over(_win))\
  .withColumn('_rownummax', f.count('name').over(_win_rownummax))\
  .where(f.col('_rownum') == 1)\
  .select(['name', 'terminology', 'code', 'term'])

# check
count_var(tmp_stillbirth, 'code')

# save
codelist_stillbirth = tmp_stillbirth

# COMMAND ----------

# pregnancy

# v1
tmp_pregnancy_v1 = spark.createDataFrame(
  [
    ("171057006","Pregnancy alcohol education (procedure)"),
    ("72301000119103","Asthma in pregnancy (disorder)"),
    ("10742121000119104","Asthma in mother complicating childbirth (disorder)"),
    ("10745291000119103","Malignant neoplastic disease in mother complicating childbirth (disorder)"),
    ("10749871000119100","Malignant neoplastic disease in pregnancy (disorder)"),
    ("20753005","Hypertensive heart disease complicating AND/OR reason for care during pregnancy (disorder)"),
    ("237227006","Congenital heart disease in pregnancy (disorder)"),
    ("169501005","Pregnant, diaphragm failure (finding)"),
    ("169560008","Pregnant - urine test confirms (finding)"),
    ("169561007","Pregnant - blood test confirms (finding)"),
    ("169562000","Pregnant - vaginal examination confirms (finding)"),
    ("169565003","Pregnant - planned (finding)"),
    ("169566002","Pregnant - unplanned - wanted (finding)"),
    ("413567003","Aplastic anemia associated with pregnancy (disorder)"),
    ("91948008","Asymptomatic human immunodeficiency virus infection in pregnancy (disorder)"),
    ("169488004","Contraceptive intrauterine device failure - pregnant (finding)"),
    ("169508004","Pregnant, sheath failure (finding)"),
    ("169564004","Pregnant - on abdominal palpation (finding)"),
    ("77386006","Pregnant (finding)"),
    ("10746341000119109","Acquired immune deficiency syndrome complicating childbirth (disorder)"),
    ("10759351000119103","Sickle cell anemia in mother complicating childbirth (disorder)"),
    ("10757401000119104","Pre-existing hypertensive heart and chronic kidney disease in mother complicating childbirth (disorder)"),
    ("10757481000119107","Pre-existing hypertensive heart and chronic kidney disease in mother complicating pregnancy (disorder)"),
    ("10757441000119102","Pre-existing hypertensive heart disease in mother complicating childbirth (disorder)"),
    ("10759031000119106","Pre-existing hypertensive heart disease in mother complicating pregnancy (disorder)"),
    ("1474004","Hypertensive heart AND renal disease complicating AND/OR reason for care during childbirth (disorder)"),
    ("199006004","Pre-existing hypertensive heart disease complicating pregnancy, childbirth and the puerperium (disorder)"),
    ("199007008","Pre-existing hypertensive heart and renal disease complicating pregnancy, childbirth and the puerperium (disorder)"),
    ("22966008","Hypertensive heart AND renal disease complicating AND/OR reason for care during pregnancy (disorder)"),
    ("59733002","Hypertensive heart disease complicating AND/OR reason for care during childbirth (disorder)"),
    ("171054004","Pregnancy diet education (procedure)"),
    ("106281000119103","Pre-existing diabetes mellitus in mother complicating childbirth (disorder)"),
    ("10754881000119104","Diabetes mellitus in mother complicating childbirth (disorder)"),
    ("199225007","Diabetes mellitus during pregnancy - baby delivered (disorder)"),
    ("237627000","Pregnancy and type 2 diabetes mellitus (disorder)"),
    ("609563008","Pre-existing diabetes mellitus in pregnancy (disorder)"),
    ("609566000","Pregnancy and type 1 diabetes mellitus (disorder)"),
    ("609567009","Pre-existing type 2 diabetes mellitus in pregnancy (disorder)"),
    ("199223000","Diabetes mellitus during pregnancy, childbirth and the puerperium (disorder)"),
    ("199227004","Diabetes mellitus during pregnancy - baby not yet delivered (disorder)"),
    ("609564002","Pre-existing type 1 diabetes mellitus in pregnancy (disorder)"),
    ("76751001","Diabetes mellitus in mother complicating pregnancy, childbirth AND/OR puerperium (disorder)"),
    ("526961000000105","Pregnancy advice for patients with epilepsy (procedure)"),
    ("527041000000108","Pregnancy advice for patients with epilepsy not indicated (situation)"),
    ("527131000000100","Pregnancy advice for patients with epilepsy declined (situation)"),
    ("10753491000119101","Gestational diabetes mellitus in childbirth (disorder)"),
    ("40801000119106","Gestational diabetes mellitus complicating pregnancy (disorder)"),
    ("10562009","Malignant hypertension complicating AND/OR reason for care during childbirth (disorder)"),
    ("198944004","Benign essential hypertension complicating pregnancy, childbirth and the puerperium - delivered (disorder)"),
    ("198945003","Benign essential hypertension complicating pregnancy, childbirth and the puerperium - delivered with postnatal complication (disorder)"),
    ("198946002","Benign essential hypertension complicating pregnancy, childbirth and the puerperium - not delivered (disorder)"),
    ("198949009","Renal hypertension complicating pregnancy, childbirth and the puerperium (disorder)"),
    ("198951008","Renal hypertension complicating pregnancy, childbirth and the puerperium - delivered (disorder)"),
    ("198954000","Renal hypertension complicating pregnancy, childbirth and the puerperium with postnatal complication (disorder)"),
    ("199005000","Pre-existing hypertension complicating pregnancy, childbirth and puerperium (disorder)"),
    ("23717007","Benign essential hypertension complicating AND/OR reason for care during pregnancy (disorder)"),
    ("26078007","Hypertension secondary to renal disease complicating AND/OR reason for care during childbirth (disorder)"),
    ("29259002","Malignant hypertension complicating AND/OR reason for care during pregnancy (disorder)"),
    ("65402008","Pre-existing hypertension complicating AND/OR reason for care during pregnancy (disorder)"),
    ("8218002","Chronic hypertension complicating AND/OR reason for care during childbirth (disorder)"),
    ("10752641000119102","Eclampsia with pre-existing hypertension in childbirth (disorder)"),
    ("118781000119108","Pre-existing hypertensive chronic kidney disease in mother complicating pregnancy (disorder)"),
    ("18416000","Essential hypertension complicating AND/OR reason for care during childbirth (disorder)"),
    ("198942000","Benign essential hypertension complicating pregnancy, childbirth and the puerperium (disorder)"),
    ("198947006","Benign essential hypertension complicating pregnancy, childbirth and the puerperium with postnatal complication (disorder)"),
    ("198952001","Renal hypertension complicating pregnancy, childbirth and the puerperium - delivered with postnatal complication (disorder)"),
    ("198953006","Renal hypertension complicating pregnancy, childbirth and the puerperium - not delivered (disorder)"),
    ("199008003","Pre-existing secondary hypertension complicating pregnancy, childbirth and puerperium (disorder)"),
    ("34694006","Pre-existing hypertension complicating AND/OR reason for care during childbirth (disorder)"),
    ("37618003","Chronic hypertension complicating AND/OR reason for care during pregnancy (disorder)"),
    ("48552006","Hypertension secondary to renal disease complicating AND/OR reason for care during pregnancy (disorder)"),
    ("71874008","Benign essential hypertension complicating AND/OR reason for care during childbirth (disorder)"),
    ("78808002","Essential hypertension complicating AND/OR reason for care during pregnancy (disorder)"),
    ("91923005","Acquired immunodeficiency syndrome virus infection associated with pregnancy (disorder)"),
    ("10755671000119100","Human immunodeficiency virus in mother complicating childbirth (disorder)"),
    ("721166000","Human immunodeficiency virus complicating pregnancy childbirth and the puerperium (disorder)"),
    ("449369001","Stopped smoking before pregnancy (finding)"),
    ("449345000","Smoked before confirmation of pregnancy (finding)"),
    ("449368009","Stopped smoking during pregnancy (finding)"),
    ("88144003","Removal of ectopic interstitial uterine pregnancy requiring total hysterectomy (procedure)"),
    ("240154002","Idiopathic osteoporosis in pregnancy (disorder)"),
    ("956951000000104","Pertussis vaccination in pregnancy (procedure)"),
    ("866641000000105","Pertussis vaccination in pregnancy declined (situation)"),
    ("956971000000108","Pertussis vaccination in pregnancy given by other healthcare provider (finding)"),
    ("169563005","Pregnant - on history (finding)"),
    ("10231000132102","In-vitro fertilization pregnancy (finding)"),
    ("134781000119106","High risk pregnancy due to recurrent miscarriage (finding)"),
    ("16356006","Multiple pregnancy (disorder)"),
    ("237239003","Low risk pregnancy (finding)"),
    ("276367008","Wanted pregnancy (finding)"),
    ("314204000","Early stage of pregnancy (finding)"),
    ("439311009","Intends to continue pregnancy (finding)"),
    ("713575004","Dizygotic twin pregnancy (disorder)"),
    ("80997009","Quintuplet pregnancy (disorder)"),
    ("1109951000000101","Pregnancy insufficiently advanced for reliable antenatal screening (finding)"),
    ("1109971000000105","Pregnancy too advanced for reliable antenatal screening (finding)"),
    ("237238006","Pregnancy with uncertain dates (finding)"),
    ("444661007","High risk pregnancy due to history of preterm labor (finding)"),
    ("459166009","Dichorionic diamniotic twin pregnancy (disorder)"),
    ("459167000","Monochorionic twin pregnancy (disorder)"),
    ("459168005","Monochorionic diamniotic twin pregnancy (disorder)"),
    ("459171002","Monochorionic monoamniotic twin pregnancy (disorder)"),
    ("47200007","High risk pregnancy (finding)"),
    ("60810003","Quadruplet pregnancy (disorder)"),
    ("64254006","Triplet pregnancy (disorder)"),
    ("65147003","Twin pregnancy (disorder)"),
    ("713576003","Monozygotic twin pregnancy (disorder)"),
    ("171055003","Pregnancy smoking education (procedure)"),
    ("10809101000119109","Hypothyroidism in childbirth (disorder)"),
    ("428165003","Hypothyroidism in pregnancy (disorder)")
  ],
  ['code', 'term']  
)
tmp_pregnancy_v1 = tmp_pregnancy_v1\
  .withColumn('name', f.lit('pregnancy'))\
  .withColumn('terminology', f.lit('SNOMED'))\
  .select(['name', 'terminology', 'code', 'term'])


# v2
# Source
# CVD-COVID-UK Consortium Shared Folder\Projects\CCU018 Pregnancy\6. Analysis and results\pregnancy_codes.xlsx
tmp_pregnancy_v2 = """
name,code,terminology,term
pregnancy,R01,OPCS4,Therapeutic endoscopic operations on fetus
pregnancy,R011,OPCS4,Fetoscopic blood transfusion of fetus
pregnancy,R012,OPCS4,Fetoscopic insertion of tracheal plug for congenital diaphragmatic hernia
pregnancy,R018,OPCS4,Other specified therapeutic endoscopic operations on fetus
pregnancy,R019,OPCS4,Unspecified therapeutic endoscopic operations on fetus
pregnancy,R02,OPCS4,Diagnostic endoscopic examination of fetus
pregnancy,R021,OPCS4,Fetoscopic examination of fetus and fetoscopic biopsy of fetus
pregnancy,R022,OPCS4,Fetoscopic examination of fetus and fetoscopic sampling of fetal blood
pregnancy,R028,OPCS4,Other specified diagnostic endoscopic examination of fetus
pregnancy,R029,OPCS4,Unspecified diagnostic endoscopic examination of fetus
pregnancy,R03,OPCS4,Selective destruction of fetus
pregnancy,R031,OPCS4,Early selective feticide
pregnancy,R032,OPCS4,Late selective feticide
pregnancy,R038,OPCS4,Other specified selective destruction of fetus
pregnancy,R039,OPCS4,Unspecified selective destruction of fetus
pregnancy,R04,OPCS4,Therapeutic percutaneous operations on fetus
pregnancy,R041,OPCS4,Percutaneous insertion of fetal vesicoamniotic shunt
pregnancy,R042,OPCS4,Percutaneous insertion of fetal pleuroamniotic shunt
pregnancy,R043,OPCS4,Percutaneous blood transfusion of fetus
pregnancy,R044,OPCS4,Percutaneous insertion of fetal pleural drain
pregnancy,R045,OPCS4,Percutaneous insertion of fetal bladder drain
pregnancy,R046,OPCS4,Percutaneous insertion of fetal tracheal plug for congenital diaphragmatic hernia
pregnancy,R047,OPCS4,Percutaneous laser ablation of lesion of fetus
pregnancy,R048,OPCS4,Other specified therapeutic percutaneous operations on fetus
pregnancy,R049,OPCS4,Unspecified therapeutic percutaneous operations on fetus
pregnancy,R05,OPCS4,Diagnostic percutaneous examination of fetus
pregnancy,R051,OPCS4,Percutaneous biopsy of fetus
pregnancy,R052,OPCS4,Percutaneous sampling of fetal blood
pregnancy,R053,OPCS4,Percutaneous sampling of chorionic villus
pregnancy,R058,OPCS4,Other specified diagnostic percutaneous examination of fetus
pregnancy,R059,OPCS4,Unspecified diagnostic percutaneous examination of fetus
pregnancy,R06,OPCS4,Destruction of fetus
pregnancy,R061,OPCS4,Selective feticide
pregnancy,R062,OPCS4,Feticide NEC
pregnancy,R068,OPCS4,Other specified
pregnancy,R069,OPCS4,Unspecified
pregnancy,R07,OPCS4,Therapeutic endoscopic operations for twin to twin transfusion syndrome
pregnancy,R071,OPCS4,Endoscopic laser ablation of placental arteriovenous anastomosis
pregnancy,R072,OPCS4,Endoscopic serial drainage of amniotic fluid for twin to twin transfusion syndrome
pregnancy,R078,OPCS4,Other specified therapeutic endoscopic operations for twin to twin transfusion syndrome
pregnancy,R079,OPCS4,Unspecified therapeutic endoscopic operations for twin to twin transfusion syndrome
pregnancy,R08,OPCS4,Therapeutic percutaneous operations for twin to twin transfusion syndrome
pregnancy,R081,OPCS4,Percutaneous laser ablation of placental arteriovenous anastomosis
pregnancy,R082,OPCS4,Percutaneous serial drainage of amniotic fluid for twin to twin transfusion syndrome
pregnancy,R088,OPCS4,Other specified therapeutic percutaneous operations for twin to twin transfusion syndrome
pregnancy,R089,OPCS4,Unspecified therapeutic percutaneous operations for twin to twin transfusion syndrome
pregnancy,R10,OPCS4,Other operations on amniotic cavity
pregnancy,R101,OPCS4,Drainage of amniotic cavity
pregnancy,R102,OPCS4,Diagnostic amniocentesis
pregnancy,R103,OPCS4,Amnioscopy
pregnancy,R104,OPCS4,Sampling of chorionic villus NEC
pregnancy,R105,OPCS4,Biopsy of placenta NEC
pregnancy,R108,OPCS4,Other specified other operations on amniotic cavity
pregnancy,R109,OPCS4,Unspecified other operations on amniotic cavity
pregnancy,R12,OPCS4,Operations on gravid uterus
pregnancy,R121,OPCS4,Cerclage of cervix of gravid uterus
pregnancy,R122,OPCS4,Removal of cerclage from cervix of gravid uterus
pregnancy,R123,OPCS4,Repositioning of retroverted gravid uterus
pregnancy,R124,OPCS4,External cephalic version
pregnancy,R128,OPCS4,Other specified operations on gravid uterus
pregnancy,R129,OPCS4,Unspecified operations on gravid uterus
pregnancy,R14,OPCS4,Surgical induction of labour
pregnancy,R141,OPCS4,Forewater rupture of amniotic membrane
pregnancy,R142,OPCS4,Hindwater rupture of amniotic membrane
pregnancy,R148,OPCS4,Other specified surgical induction of labour
pregnancy,R149,OPCS4,Unspecified surgical induction of labour
pregnancy,R15,OPCS4,Other induction of labour
pregnancy,R151,OPCS4,Medical induction of labour
pregnancy,R158,OPCS4,Other specified other induction of labour
pregnancy,R159,OPCS4,Unspecified other induction of labour
pregnancy,R17,OPCS4,Elective caesarean delivery
pregnancy,R171,OPCS4,Elective upper uterine segment caesarean delivery
pregnancy,R172,OPCS4,Elective lower uterine segment caesarean delivery
pregnancy,R178,OPCS4,Other specified elective caesarean delivery
pregnancy,R179,OPCS4,Unspecified elective caesarean delivery
pregnancy,R18,OPCS4,Other caesarean delivery
pregnancy,R181,OPCS4,Upper uterine segment caesarean delivery NEC
pregnancy,R182,OPCS4,Lower uterine segment caesarean delivery NEC
pregnancy,R188,OPCS4,Other specified other caesarean delivery
pregnancy,R189,OPCS4,Unspecified other caesarean delivery
pregnancy,R19,OPCS4,Breech extraction delivery
pregnancy,R191,OPCS4,Breech extraction delivery with version
pregnancy,R198,OPCS4,Other specified breech extraction delivery
pregnancy,R199,OPCS4,Unspecified breech extraction delivery
pregnancy,R20,OPCS4,Other breech delivery
pregnancy,R201,OPCS4,Spontaneous breech delivery
pregnancy,R202,OPCS4,Assisted breech delivery
pregnancy,R208,OPCS4,Other specified other breech delivery
pregnancy,R209,OPCS4,Unspecified other breech delivery
pregnancy,R21,OPCS4,Forceps cephalic delivery
pregnancy,R211,OPCS4,High forceps cephalic delivery with rotation
pregnancy,R212,OPCS4,High forceps cephalic delivery NEC
pregnancy,R213,OPCS4,Mid forceps cephalic delivery with rotation
pregnancy,R214,OPCS4,Mid forceps cephalic delivery NEC
pregnancy,R215,OPCS4,Low forceps cephalic delivery
pregnancy,R218,OPCS4,Other specified forceps cephalic delivery
pregnancy,R219,OPCS4,Unspecified forceps cephalic delivery
pregnancy,R22,OPCS4,Vacuum delivery
pregnancy,R221,OPCS4,High vacuum delivery
pregnancy,R222,OPCS4,Low vacuum delivery
pregnancy,R223,OPCS4,Vacuum delivery before full dilation of cervix
pregnancy,R228,OPCS4,Other specified vacuum delivery
pregnancy,R229,OPCS4,Unspecified vacuum delivery
pregnancy,R23,OPCS4,Cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,R231,OPCS4,Manipulative cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,R232,OPCS4,Non-manipulative cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,R238,OPCS4,Other specified cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,R239,OPCS4,Unspecified cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,R24,OPCS4,Normal delivery
pregnancy,R249,OPCS4,All normal delivery
pregnancy,R25,OPCS4,Other methods of delivery
pregnancy,R251,OPCS4,Caesarean hysterectomy
pregnancy,R252,OPCS4,Destructive operation to facilitate delivery
pregnancy,R258,OPCS4,Other specified other methods of delivery
pregnancy,R259,OPCS4,Unspecified other methods of delivery
pregnancy,R27,OPCS4,Other operations to facilitate delivery
pregnancy,R271,OPCS4,Episiotomy to facilitate delivery
pregnancy,R272,OPCS4,Deinfibulation of vulva to facilitate delivery
pregnancy,R278,OPCS4,Other specified other operations to facilitate delivery
pregnancy,R279,OPCS4,Unspecified other operations to facilitate delivery
pregnancy,R28,OPCS4,Instrumental removal of products of conception from delivered uterus
pregnancy,R281,OPCS4,Curettage of delivered uterus
pregnancy,R288,OPCS4,Other specified instrumental removal of products of conception from delivered uterus
pregnancy,R289,OPCS4,Unspecified instrumental removal of products of conception from delivered uterus
pregnancy,R29,OPCS4,Manual removal of products of conception from delivered uterus
pregnancy,R291,OPCS4,Manual removal of placenta from delivered uterus
pregnancy,R298,OPCS4,Other specified manual removal of products of conception from delivered uterus
pregnancy,R299,OPCS4,Unspecified manual removal of products of conception from delivered uterus
pregnancy,R30,OPCS4,Other operations on delivered uterus
pregnancy,R301,OPCS4,Repositioning of inverted delivered uterus
pregnancy,R302,OPCS4,Expression of placenta
pregnancy,R303,OPCS4,Instrumental exploration of delivered uterus NEC
pregnancy,R304,OPCS4,Manual exploration of delivered uterus NEC
pregnancy,R308,OPCS4,Other specified other operations on delivered uterus
pregnancy,R309,OPCS4,Unspecified other operations on delivered uterus
pregnancy,R32,OPCS4,Repair of obstetric laceration
pregnancy,R321,OPCS4,Repair of obstetric laceration of uterus or cervix uteri
pregnancy,R322,OPCS4,Repair of obstetric laceration of perineum and sphincter of anus
pregnancy,R323,OPCS4,Repair of obstetric laceration of vagina and floor of pelvis
pregnancy,R324,OPCS4,Repair of minor obstetric laceration
pregnancy,R325,OPCS4,Repair of obstetric laceration of perineum and sphincter and mucosa of anus
pregnancy,R328,OPCS4,Other specified repair of obstetric laceration
pregnancy,R329,OPCS4,Unspecified repair of obstetric laceration
pregnancy,R34,OPCS4,Other obstetric operations
pregnancy,R348,OPCS4,Other specified other obstetric operations
pregnancy,R349,OPCS4,Unspecified other obstetric operations
pregnancy,R36,OPCS4,Routine obstetric scan
pregnancy,R361,OPCS4,Dating scan
pregnancy,R362,OPCS4,Viability scan
pregnancy,R363,OPCS4,Mid trimester scan
pregnancy,R368,OPCS4,Other specified routine obstetric scan
pregnancy,R369,OPCS4,Unspecified routine obstetric scan
pregnancy,R37,OPCS4,Non-routine obstetric scan for fetal observations
pregnancy,R371,OPCS4,Biophysical profile
pregnancy,R372,OPCS4,Detailed structural scan
pregnancy,R373,OPCS4,Fetal biometry
pregnancy,R374,OPCS4,Nuchal translucency scan
pregnancy,R375,OPCS4,Fetal ascites scan
pregnancy,R376,OPCS4,Rhesus detailed scan
pregnancy,R378,OPCS4,Other specified non-routine obstetric scan for fetal observations
pregnancy,R379,OPCS4,Unspecified non-routine obstetric scan for fetal observations
pregnancy,R38,OPCS4,Other non-routine obstetric scan
pregnancy,R381,OPCS4,Placental localisation scan
pregnancy,R382,OPCS4,Liquor volume scan
pregnancy,R388,OPCS4,Other specified other non-routine obstetric scan
pregnancy,R389,OPCS4,Unspecified other non-routine obstetric scan
pregnancy,R40,OPCS4,Other maternal physiological assessments
pregnancy,R401,OPCS4,Maternal cervical assessment
pregnancy,R402,OPCS4,Cervical length scanning at 24 weeks
pregnancy,R408,OPCS4,Other specified other maternal physiological assessments
pregnancy,R409,OPCS4,Unspecified other maternal physiological assessments
pregnancy,R42,OPCS4,Obstetric doppler ultrasound
pregnancy,R421,OPCS4,Doppler ultrasound scan of umbilical artery
pregnancy,R422,OPCS4,Doppler ultrasound scan of uterine artery
pregnancy,R423,OPCS4,Doppler ultrasound scan of middle cerebral artery of fetus
pregnancy,R428,OPCS4,Other specified obstetric doppler ultrasound
pregnancy,R429,OPCS4,Unspecified obstetric doppler ultrasound
pregnancy,R43,OPCS4,Ultrasound monitoring
pregnancy,R432,OPCS4,Ultrasound monitoring of early pregnancy
pregnancy,R438,OPCS4,Other specified ultrasound monitoring
pregnancy,R439,OPCS4,Unspecified ultrasound monitoring
pregnancy,A34,ICD10,Obstetrical tetanus
pregnancy,N96,ICD10,Recurrent pregnancy loss
pregnancy,O00,ICD10,Ectopic pregnancy
pregnancy,O01,ICD10,Hydatidiform mole
pregnancy,O02,ICD10,Other abnormal products of conception
pregnancy,O03,ICD10,Spontaneous abortion
pregnancy,O04,ICD10,Complications following (induced) termination of pregnancy
pregnancy,O07,ICD10,Failedattemptedterminationofpregnancy
pregnancy,O08,ICD10,Complications following ectopic and molar pregnancy
pregnancy,O09,ICD10,Supervision of high risk pregnancy
pregnancy,O10,ICD10,"Pre-existing hypertension complicating pregnancy, childbirth and the puerperium"
pregnancy,O11,ICD10,Pre-existinghypertensionwithpre-eclampsia
pregnancy,O12,ICD10,Gestational [pregnancy-induced] edema and proteinuria without hypertension
pregnancy,O13,ICD10,Gestational [pregnancy-induced] hypertension without significant proteinuria
pregnancy,O14,ICD10,Pre-eclampsia
pregnancy,O15,ICD10,Eclampsia
pregnancy,O16,ICD10,Unspecified maternal hypertension
pregnancy,O20,ICD10,Hemorrhage in early pregnancy
pregnancy,O21,ICD10,Excessive vomiting in pregnancy
pregnancy,O22,ICD10,Venous complications and hemorrhoids in pregnancy
pregnancy,O23,ICD10,Infections of genitourinary tract in pregnancy
pregnancy,O24,ICD10,"Diabetes mellitus in pregnancy, childbirth, and the puerperium"
pregnancy,O25,ICD10,"Malnutrition in pregnancy, childbirth and the puerperium"
pregnancy,O26,ICD10,Maternal care for other conditions predominantly related to pregnancy
pregnancy,O28,ICD10,Abnormal findings on antenatal screening of mother
pregnancy,O29,ICD10,Complications of anesthesia during pregnancy
pregnancy,O30,ICD10,Multiple gestation
pregnancy,O31,ICD10,Complications specific to multiple gestation
pregnancy,O32,ICD10,Maternal care for malpresentation of fetus
pregnancy,O33,ICD10,Maternal care for disproportion
pregnancy,O34,ICD10,Maternal care for abnormality of pelvic organs
pregnancy,O35,ICD10,Maternal care for known or suspected fetal abnormality and damage
pregnancy,O36,ICD10,Maternal care for other fetal problems
pregnancy,O40,ICD10,Polyhydramnios
pregnancy,O41,ICD10,Other disorders of amniotic fluid and membranes
pregnancy,O42,ICD10,Premature rupture of membranes
pregnancy,O43,ICD10,Placental disorders
pregnancy,O44,ICD10,Placenta previa
pregnancy,O45,ICD10,Premature separation of placenta [abruptio placentae]
pregnancy,O46,ICD10,"Antepartum hemorrhage, not elsewhere classified"
pregnancy,O47,ICD10,False labor
pregnancy,O48,ICD10,Late pregnancy
pregnancy,O60,ICD10,Preterm labor
pregnancy,O61,ICD10,Failed induction of labor
pregnancy,O62,ICD10,Abnormalities of forces of labor
pregnancy,O63,ICD10,Long labor
pregnancy,O64,ICD10,Obstructed labor due to malposition and malpresentation of fetus
pregnancy,O65,ICD10,Obstructed labor due to maternal pelvic abnormality
pregnancy,O66,ICD10,Other obstructed labor
pregnancy,O67,ICD10,"Labor and delivery complicated by intrapartum hemorrhage, not elsewhere classified"
pregnancy,O68,ICD10,Labor and delivery complicated by abnormality of fetal acid-base balance
pregnancy,O69,ICD10,Labor and delivery complicated by umbilical cord complications
pregnancy,O70,ICD10,Perineal laceration during delivery
pregnancy,O71,ICD10,Other obstetric trauma
pregnancy,O72,ICD10,Postpartum hemorrhage
pregnancy,O73,ICD10,"Retained placenta and membranes, without hemorrhage"
pregnancy,O74,ICD10,Complications of anesthesia during labor and delivery
pregnancy,O75,ICD10,"Other complications of labor and delivery, not elsewhere classified"
pregnancy,O76,ICD10,Abnormality in fetal heart rate and rhythm complicating labor and delivery
pregnancy,O77,ICD10,Other fetal stress complicating labor and delivery
pregnancy,O80,ICD10,Encounter for full-term uncomplicated delivery
pregnancy,O82,ICD10,Encounter for cesarean delivery without indication
pregnancy,O85,ICD10,Puerperal sepsis
pregnancy,O86,ICD10,Other puerperal infections
pregnancy,O87,ICD10,Venous complications and hemorrhoids in the puerperium
pregnancy,O88,ICD10,Obstetric embolism
pregnancy,O89,ICD10,Complications of anesthesia during the puerperium
pregnancy,O90,ICD10,"Complications of the puerperium, not elsewhere classified"
pregnancy,O91,ICD10,"Infections of breast associated with pregnancy, the puerperium and lactation"
pregnancy,O92,ICD10,Other disorders of breast and disorders of lactation associated with pregnancy and the puerperium
pregnancy,O94,ICD10,"Sequelae of complication of pregnancy, childbirth, and the puerperium"
pregnancy,O98,ICD10,"Maternal infectious and parasitic diseases classifiable elsewhere but complicating pregnancy, childbirth and the puerperium"
pregnancy,O99,ICD10,"Other maternal diseases classifiable elsewhere but complicating pregnancy, childbirth and the puerperium"
pregnancy,O9A,ICD10,"Maternal malignant neoplasms, traumatic injuries and abuse classifiable elsewhere but complicating pregnancy, childbirth and the puerperium"
pregnancy,P95,ICD10,Stillbirth
pregnancy,Z322,ICD10,Encounter for childbirth instruction
pregnancy,Z33,ICD10,Pregnant state
pregnancy,Z34,ICD10,Encounter for supervision of normal pregnancy
pregnancy,Z36,ICD10,Encounter for antenatal screening of mother
pregnancy,Z37,ICD10,Outcome of delivery
pregnancy,Z38,ICD10,Liveborn infants according to place of birth and type of delivery
pregnancy,Z39,ICD10,Encounter for maternal postpartum care and examination
pregnancy,10058006,SNOMED,Miscarriage with amniotic fluid embolism
pregnancy,100801000119107,SNOMED,Maternal tobacco use in pregnancy
pregnancy,10217006,SNOMED,Third degree perineal laceration
pregnancy,10231000132102,SNOMED,In-vitro fertilization pregnancy
pregnancy,102500002,SNOMED,Good neonatal condition at birth
pregnancy,102501003,SNOMED,Well male newborn
pregnancy,102502005,SNOMED,Well female newborn
pregnancy,102503000,SNOMED,Well premature newborn
pregnancy,102504006,SNOMED,Well premature male newborn
pregnancy,102505007,SNOMED,Well premature female newborn
pregnancy,102872000,SNOMED,Pregnancy on oral contraceptive
pregnancy,102875003,SNOMED,Surrogate pregnancy
pregnancy,102876002,SNOMED,Multigravida
pregnancy,102879009,SNOMED,Post-term delivery
pregnancy,102882004,SNOMED,Abnormal placental secretion of chorionic gonadotropin
pregnancy,102885002,SNOMED,Absence of placental secretion of chorionic gonadotropin
pregnancy,102886001,SNOMED,Increased amniotic fluid production
pregnancy,102887005,SNOMED,Decreased amniotic fluid production
pregnancy,102955006,SNOMED,Contraception failure
pregnancy,1031000119109,SNOMED,Insufficient prenatal care
pregnancy,10423003,SNOMED,Braun von Fernwald's sign
pregnancy,10455003,SNOMED,Removal of ectopic cervical pregnancy by evacuation
pregnancy,104851000119103,SNOMED,Postpartum major depression in remission
pregnancy,10573002,SNOMED,Infection of amniotic cavity
pregnancy,106004004,SNOMED,Hemorrhagic complication of pregnancy
pregnancy,106007006,SNOMED,Maternal AND/OR fetal condition affecting labor AND/OR delivery
pregnancy,106008001,SNOMED,Delivery AND/OR maternal condition affecting management
pregnancy,106009009,SNOMED,Fetal condition affecting obstetrical care of mother
pregnancy,106010004,SNOMED,Pelvic dystocia AND/OR uterine disorder
pregnancy,106111002,SNOMED,Clinical sign related to pregnancy
pregnancy,10629511000119102,SNOMED,Rhinitis of pregnancy
pregnancy,10697004,SNOMED,Miscarriage complicated by renal failure
pregnancy,10741871000119101,SNOMED,Alcohol dependence in pregnancy
pregnancy,10743651000119105,SNOMED,Inflammation of cervix in pregnancy
pregnancy,10743831000119100,SNOMED,Neoplasm of uterus affecting pregnancy
pregnancy,10743881000119104,SNOMED,Suspected fetal abnormality affecting management of mother
pregnancy,10745001,SNOMED,Delivery of transverse presentation
pregnancy,10745231000119102,SNOMED,Cardiac arrest due to administration of anesthesia for obstetric procedure in pregnancy
pregnancy,10749691000119103,SNOMED,Obstetric anesthesia with cardiac complication in childbirth
pregnancy,10749811000119108,SNOMED,Obstetric anesthesia with central nervous system complication in childbirth
pregnancy,10749871000119100,SNOMED,Malignant neoplastic disease in pregnancy
pregnancy,10750111000119108,SNOMED,Breast lump in pregnancy
pregnancy,10750161000119106,SNOMED,Cholestasis of pregnancy complicating childbirth
pregnancy,10750411000119102,SNOMED,Nonpurulent mastitis associated with lactation
pregnancy,10750991000119101,SNOMED,Cyst of ovary in pregnancy
pregnancy,10751511000119105,SNOMED,Obstructed labor due to incomplete rotation of fetal head
pregnancy,10751581000119104,SNOMED,Obstructed labor due to disproportion between fetus and pelvis
pregnancy,10751631000119101,SNOMED,Obstructed labor due to abnormality of maternal pelvis
pregnancy,10751701000119102,SNOMED,Spontaneous onset of labor between 37 and 39 weeks gestation with planned cesarean section
pregnancy,10751771000119107,SNOMED,Placental abruption due to afibrinogenemia
pregnancy,10752251000119103,SNOMED,Galactorrhea in pregnancy
pregnancy,10753491000119101,SNOMED,Gestational diabetes mellitus in childbirth
pregnancy,10754331000119108,SNOMED,Air embolism in childbirth
pregnancy,10755951000119102,SNOMED,Heart murmur in mother in childbirth
pregnancy,10756261000119102,SNOMED,Physical abuse complicating childbirth
pregnancy,10756301000119105,SNOMED,Physical abuse complicating pregnancy
pregnancy,10759191000119106,SNOMED,Postpartum septic thrombophlebitis
pregnancy,10759231000119102,SNOMED,Salpingo-oophoritis in pregnancy
pregnancy,10760221000119101,SNOMED,Uterine prolapse in pregnancy
pregnancy,10760261000119106,SNOMED,Uterine laceration during delivery
pregnancy,10760541000119109,SNOMED,Traumatic injury to vulva during pregnancy
pregnancy,10760581000119104,SNOMED,Pain in round ligament in pregnancy
pregnancy,10760661000119109,SNOMED,Triplets  some live born
pregnancy,10760701000119102,SNOMED,Quadruplets  all live born
pregnancy,10760741000119100,SNOMED,Quadruplets  some live born
pregnancy,10760781000119105,SNOMED,Quintuplets  all live born
pregnancy,10760821000119100,SNOMED,Quintuplets  some live born
pregnancy,10760861000119105,SNOMED,Sextuplets  all live born
pregnancy,10760901000119104,SNOMED,Sextuplets  some live born
pregnancy,10760981000119107,SNOMED,Psychological abuse complicating pregnancy
pregnancy,10761021000119102,SNOMED,Sexual abuse complicating childbirth
pregnancy,10761061000119107,SNOMED,Sexual abuse complicating pregnancy
pregnancy,10761101000119105,SNOMED,Vacuum assisted vaginal delivery
pregnancy,10761141000119107,SNOMED,Preterm labor in second trimester with preterm delivery in second trimester
pregnancy,10761191000119104,SNOMED,Preterm labor in third trimester with preterm delivery in third trimester
pregnancy,10761241000119104,SNOMED,Preterm labor with preterm delivery
pregnancy,10761341000119105,SNOMED,Preterm labor without delivery
pregnancy,10761391000119102,SNOMED,Tobacco use in mother complicating childbirth
pregnancy,10763001,SNOMED,Therapeutic abortion by insertion of laminaria
pregnancy,1076861000000103,SNOMED,Ultrasonography of retained products of conception
pregnancy,1079101000000101,SNOMED,Antenatal screening shows higher risk of Down syndrome
pregnancy,1079111000000104,SNOMED,Antenatal screening shows lower risk of Down syndrome
pregnancy,1079121000000105,SNOMED,Antenatal screening shows higher risk of Edwards and Patau syndromes
pregnancy,1079131000000107,SNOMED,Antenatal screening shows lower risk of Edwards and Patau syndromes
pregnancy,10807061000119103,SNOMED,Liver disorder in mother complicating childbirth
pregnancy,10808861000119105,SNOMED,Maternal distress during childbirth
pregnancy,10812041000119103,SNOMED,Urinary tract infection due to incomplete miscarriage
pregnancy,10835571000119102,SNOMED,Antepartum hemorrhage due to disseminated intravascular coagulation
pregnancy,10835781000119105,SNOMED,Amniotic fluid embolism in childbirth
pregnancy,10835971000119109,SNOMED,Anti-A sensitization in pregnancy
pregnancy,10836071000119101,SNOMED,Dislocation of symphysis pubis in pregnancy
pregnancy,10836111000119108,SNOMED,Dislocation of symphysis pubis in labor and delivery
pregnancy,109562007,SNOMED,Abnormal chorion
pregnancy,109891004,SNOMED,Detached products of conception
pregnancy,109893001,SNOMED,Detached trophoblast
pregnancy,109894007,SNOMED,Retained placenta
pregnancy,109895008,SNOMED,Retained placental fragment
pregnancy,110081000119109,SNOMED,Bacterial vaginosis in pregnancy
pregnancy,11026009,SNOMED,Miscarriage with renal tubular necrosis
pregnancy,1105781000000106,SNOMED,Misoprostol induction of labour
pregnancy,1105791000000108,SNOMED,Mifepristone induction of labour
pregnancy,1105801000000107,SNOMED,Clitoral tear during delivery
pregnancy,11082009,SNOMED,Abnormal pregnancy
pregnancy,1109951000000101,SNOMED,Pregnancy insufficiently advanced for reliable antenatal screening
pregnancy,1109971000000105,SNOMED,Pregnancy too advanced for reliable antenatal screening
pregnancy,11109001,SNOMED,Miscarriage with uremia
pregnancy,111208003,SNOMED,Melasma gravidarum
pregnancy,111424000,SNOMED,Retained products of conception not following abortion
pregnancy,111431001,SNOMED,Legal abortion complicated by damage to pelvic organs AND/OR tissues
pregnancy,111432008,SNOMED,Legal abortion with air embolism
pregnancy,111447004,SNOMED,Placental condition affecting management of mother
pregnancy,111453004,SNOMED,Retained placenta  without hemorrhage
pregnancy,111454005,SNOMED,Retained portions of placenta AND/OR membranes without hemorrhage
pregnancy,111458008,SNOMED,Postpartum venous thrombosis
pregnancy,111459000,SNOMED,Infection of nipple  associated with childbirth
pregnancy,112070001,SNOMED,Onset of labor induced
pregnancy,112071002,SNOMED,Postmature pregnancy delivered
pregnancy,112075006,SNOMED,Premature birth of newborn sextuplets
pregnancy,1125006,SNOMED,Sepsis during labor
pregnancy,112925006,SNOMED,Repair of obstetric laceration of vulva
pregnancy,112926007,SNOMED,Suture of obstetric laceration of vagina
pregnancy,112927003,SNOMED,Manual rotation of fetal head
pregnancy,11337002,SNOMED,Quickening of fetus
pregnancy,11373009,SNOMED,Illegal abortion with sepsis
pregnancy,11454006,SNOMED,Failed attempted abortion with amniotic fluid embolism
pregnancy,11466000,SNOMED,Cesarean section
pregnancy,1147951000000100,SNOMED,Delivery following termination of pregnancy
pregnancy,1147961000000102,SNOMED,Breech delivery following termination of pregnancy
pregnancy,1147971000000109,SNOMED,Cephalic delivery following termination of pregnancy
pregnancy,1148801000000108,SNOMED,Monochorionic monoamniotic triplet pregnancy
pregnancy,1148811000000105,SNOMED,Trichorionic triamniotic triplet pregnancy
pregnancy,1148821000000104,SNOMED,Dichorionic triamniotic triplet pregnancy
pregnancy,1148841000000106,SNOMED,Dichorionic diamniotic triplet pregnancy
pregnancy,1149411000000103,SNOMED,Monochorionic diamniotic triplet pregnancy
pregnancy,1149421000000109,SNOMED,Monochorionic triamniotic triplet pregnancy
pregnancy,1167981000000101,SNOMED,Ultrasound scan for chorionicity
pregnancy,11687002,SNOMED,Gestational diabetes mellitus
pregnancy,11718971000119100,SNOMED,Diarrhea in pregnancy
pregnancy,118180006,SNOMED,Finding related to amniotic fluid function
pregnancy,118181005,SNOMED,Finding related to amniotic fluid production
pregnancy,118182003,SNOMED,Finding related to amniotic fluid turnover
pregnancy,118185001,SNOMED,Finding related to pregnancy
pregnancy,118189007,SNOMED,Prenatal finding
pregnancy,118215003,SNOMED,Delivery finding
pregnancy,118216002,SNOMED,Labor finding
pregnancy,11914001,SNOMED,Transverse OR oblique presentation of fetus
pregnancy,11942004,SNOMED,Perineal laceration involving pelvic floor
pregnancy,119901000119109,SNOMED,Hemorrhage co-occurrent and due to partial placenta previa
pregnancy,12062007,SNOMED,Puerperal pelvic cellulitis
pregnancy,12095001,SNOMED,Legal abortion with cardiac arrest AND/OR failure
pregnancy,12296009,SNOMED,Legal abortion with intravascular hemolysis
pregnancy,12349003,SNOMED,Danforth's sign
pregnancy,12394009,SNOMED,Miscarriage with cerebral anoxia
pregnancy,124735008,SNOMED,Late onset of labor
pregnancy,125586008,SNOMED,Disorder of placenta
pregnancy,127009,SNOMED,Miscarriage with laceration of cervix
pregnancy,127363001,SNOMED,Number of pregnancies  currently pregnant
pregnancy,127364007,SNOMED,Primigravida
pregnancy,127365008,SNOMED,Gravida 2
pregnancy,127366009,SNOMED,Gravida 3
pregnancy,127367000,SNOMED,Gravida 4
pregnancy,127368005,SNOMED,Gravida 5
pregnancy,127369002,SNOMED,Gravida 6
pregnancy,127370001,SNOMED,Gravida 7
pregnancy,127371002,SNOMED,Gravida 8
pregnancy,127372009,SNOMED,Gravida 9
pregnancy,127373004,SNOMED,Gravida 10
pregnancy,127374005,SNOMED,Gravida more than 10
pregnancy,12803000,SNOMED,High maternal weight gain
pregnancy,128077009,SNOMED,Trauma to vagina during delivery
pregnancy,12867002,SNOMED,Fetal distress affecting management of mother
pregnancy,129597002,SNOMED,Moderate hyperemesis gravidarum
pregnancy,129598007,SNOMED,Severe hyperemesis gravidarum
pregnancy,12983003,SNOMED,Failed attempted abortion with septic shock
pregnancy,130958001,SNOMED,Disorder of placental circulatory function
pregnancy,130959009,SNOMED,Disorder of amniotic fluid turnover
pregnancy,130960004,SNOMED,Disorder of amniotic fluid production
pregnancy,13100007,SNOMED,Legal abortion with laceration of uterus
pregnancy,1323351000000104,SNOMED,First trimester bleeding
pregnancy,13384007,SNOMED,Miscarriage complicated by metabolic disorder
pregnancy,133906008,SNOMED,Postpartum care
pregnancy,133907004,SNOMED,Episiotomy care
pregnancy,13404009,SNOMED,Twin-to-twin blood transfer
pregnancy,1343000,SNOMED,Deep transverse arrest
pregnancy,134435003,SNOMED,Routine antenatal care
pregnancy,134781000119106,SNOMED,High risk pregnancy due to recurrent miscarriage
pregnancy,135881001,SNOMED,Pregnancy review
pregnancy,13763000,SNOMED,Gestation period  34 weeks
pregnancy,13798002,SNOMED,Gestation period  38 weeks
pregnancy,13842006,SNOMED,Sub-involution of uterus
pregnancy,13859001,SNOMED,Premature birth of newborn twins
pregnancy,13866000,SNOMED,Fetal acidemia affecting management of mother
pregnancy,13943000,SNOMED,Failed attempted abortion complicated by embolism
pregnancy,14022007,SNOMED,Fetal death  affecting management of mother
pregnancy,14049007,SNOMED,Chaussier's sign
pregnancy,14094001,SNOMED,Excessive vomiting in pregnancy
pregnancy,14119008,SNOMED,Braxton Hicks obstetrical version with extraction
pregnancy,14136000,SNOMED,Miscarriage with perforation of broad ligament
pregnancy,14418008,SNOMED,Precocious pregnancy
pregnancy,1469007,SNOMED,Miscarriage with urinary tract infection
pregnancy,151441000119105,SNOMED,Twin live born in hospital by vaginal delivery
pregnancy,15196006,SNOMED,Intraplacental hematoma
pregnancy,15230009,SNOMED,Liver disorder in pregnancy
pregnancy,1538006,SNOMED,Central nervous system malformation in fetus affecting obstetrical care
pregnancy,15400003,SNOMED,Obstetrical air embolism
pregnancy,15406009,SNOMED,Retromammary abscess associated with childbirth
pregnancy,15413009,SNOMED,High forceps delivery with episiotomy
pregnancy,15467003,SNOMED,Term birth of identical twins  both living
pregnancy,15504009,SNOMED,Rupture of gravid uterus
pregnancy,15511008,SNOMED,Legal abortion with parametritis
pregnancy,15539009,SNOMED,Hydrops fetalis due to isoimmunization
pregnancy,15592002,SNOMED,Previous pregnancy 1
pregnancy,156072005,SNOMED,Incomplete miscarriage
pregnancy,156073000,SNOMED,Complete miscarriage
pregnancy,15633004,SNOMED,Gestation period  16 weeks
pregnancy,15643101000119103,SNOMED,Gastroesophageal reflux disease in pregnancy
pregnancy,15663008,SNOMED,Placenta previa centralis
pregnancy,15809008,SNOMED,Miscarriage with perforation of uterus
pregnancy,15898009,SNOMED,Third stage of labor
pregnancy,1592005,SNOMED,Failed attempted abortion with uremia
pregnancy,16038004,SNOMED,Abortion due to Leptospira
pregnancy,161000119101,SNOMED,Placental abnormality  antepartum
pregnancy,16101000119107,SNOMED,Incomplete induced termination of pregnancy with complication
pregnancy,16111000119105,SNOMED,Incomplete spontaneous abortion due to hemorrhage
pregnancy,16271000119108,SNOMED,Pyelonephritis in pregnancy
pregnancy,16320551000119109,SNOMED,Bleeding from female genital tract co-occurrent with pregnancy
pregnancy,163403004,SNOMED,On examination - vaginal examination - gravid uterus
pregnancy,163498004,SNOMED,On examination - gravid uterus size
pregnancy,163499007,SNOMED,On examination - fundus 12-16 week size
pregnancy,163500003,SNOMED,On examination - fundus 16-20 week size
pregnancy,163501004,SNOMED,On examination - fundus 20-24 week size
pregnancy,163502006,SNOMED,On examination - fundus 24-28 week size
pregnancy,163504007,SNOMED,On examination - fundus 28-32 week size
pregnancy,163505008,SNOMED,On examination - fundus 32-34 week size
pregnancy,163506009,SNOMED,On examination - fundus 34-36 week size
pregnancy,163507000,SNOMED,On examination - fundus 36-38 week size
pregnancy,163508005,SNOMED,On examination - fundus 38 weeks-term size
pregnancy,163509002,SNOMED,On examination - fundus = term size
pregnancy,163510007,SNOMED,On examination - fundal size = dates
pregnancy,163515002,SNOMED,On examination - oblique lie
pregnancy,163516001,SNOMED,On examination - transverse lie
pregnancy,16356006,SNOMED,Multiple pregnancy
pregnancy,163563004,SNOMED,On examination - vaginal examination - cervical dilatation
pregnancy,163564005,SNOMED,On examination - vaginal examination - cervical os closed
pregnancy,163565006,SNOMED,On examination - vaginal examination - os 0-1 cm dilated
pregnancy,163566007,SNOMED,On examination - vaginal examination - os 1-2cm dilated
pregnancy,163567003,SNOMED,On examination - vaginal examination - os 2-4 cm dilated
pregnancy,163568008,SNOMED,On examination - vaginal examination - os 4-6cm dilated
pregnancy,163569000,SNOMED,On examination - vaginal examination - os 6-8cm dilated
pregnancy,163570004,SNOMED,On examination - vaginal examination - os 8-10cm dilated
pregnancy,163571000,SNOMED,On examination - vaginal examination - os fully dilated
pregnancy,1639007,SNOMED,Abnormality of organs AND/OR soft tissues of pelvis affecting pregnancy
pregnancy,16437531000119105,SNOMED,Ultrasonography for qualitative deepest pocket amniotic fluid volume
pregnancy,164817009,SNOMED,Placental localization
pregnancy,16607004,SNOMED,Missed abortion
pregnancy,16714009,SNOMED,Miscarriage with laceration of bowel
pregnancy,16819009,SNOMED,Delivery of face presentation
pregnancy,16836261000119107,SNOMED,Ectopic pregnancy of left ovary
pregnancy,16836351000119100,SNOMED,Ectopic pregnancy of right ovary
pregnancy,16836391000119105,SNOMED,Pregnancy of left fallopian tube
pregnancy,16836431000119100,SNOMED,Ruptured tubal pregnancy of right fallopian tube
pregnancy,16836571000119103,SNOMED,Ruptured tubal pregnancy of left fallopian tube
pregnancy,16836891000119104,SNOMED,Pregnancy of right fallopian tube
pregnancy,169224002,SNOMED,Ultrasound scan for fetal cephalometry
pregnancy,169225001,SNOMED,Ultrasound scan for fetal maturity
pregnancy,169228004,SNOMED,Ultrasound scan for fetal presentation
pregnancy,169229007,SNOMED,Dating/booking ultrasound scan
pregnancy,169230002,SNOMED,Ultrasound scan for fetal viability
pregnancy,169470007,SNOMED,Combined oral contraceptive pill failure
pregnancy,169471006,SNOMED,Progestogen-only pill failure
pregnancy,169488004,SNOMED,Contraceptive intrauterine device failure - pregnant
pregnancy,16950007,SNOMED,Fourth degree perineal laceration involving anal mucosa
pregnancy,169501005,SNOMED,Pregnant  diaphragm failure
pregnancy,169508004,SNOMED,Pregnant  sheath failure
pregnancy,169524003,SNOMED,Depot contraceptive failure
pregnancy,169533001,SNOMED,Contraceptive sponge failure
pregnancy,169539002,SNOMED,Symptothermal contraception failure
pregnancy,169544009,SNOMED,Postcoital oral contraceptive pill failure
pregnancy,169548007,SNOMED,Vasectomy failure
pregnancy,169550004,SNOMED,Female sterilization failure
pregnancy,169560008,SNOMED,Pregnant - urine test confirms
pregnancy,169561007,SNOMED,Pregnant - blood test confirms
pregnancy,169562000,SNOMED,Pregnant - vaginal examination confirms
pregnancy,169563005,SNOMED,Pregnant - on history
pregnancy,169564004,SNOMED,Pregnant - on abdominal palpation
pregnancy,169565003,SNOMED,Pregnant - planned
pregnancy,169566002,SNOMED,Pregnancy unplanned but wanted
pregnancy,169567006,SNOMED,Pregnancy unplanned and unwanted
pregnancy,169568001,SNOMED,Unplanned pregnancy unknown if child is wanted
pregnancy,169569009,SNOMED,Questionable if pregnancy was planned
pregnancy,169572002,SNOMED,Antenatal care categorized by gravida number
pregnancy,169573007,SNOMED,Antenatal care of primigravida
pregnancy,169574001,SNOMED,Antenatal care of second pregnancy
pregnancy,169575000,SNOMED,Antenatal care of third pregnancy
pregnancy,169576004,SNOMED,Antenatal care of multipara
pregnancy,169578003,SNOMED,Antenatal care: obstetric risk
pregnancy,169579006,SNOMED,Antenatal care: uncertain dates
pregnancy,169582001,SNOMED,Antenatal care: history of stillbirth
pregnancy,169583006,SNOMED,Antenatal care: history of perinatal death
pregnancy,169584000,SNOMED,Antenatal care: poor obstetric history
pregnancy,169585004,SNOMED,Antenatal care: history of trophoblastic disease
pregnancy,169587007,SNOMED,Antenatal care: precious pregnancy
pregnancy,169588002,SNOMED,Antenatal care: elderly primiparous
pregnancy,169589005,SNOMED,Antenatal care: history of infertility
pregnancy,169591002,SNOMED,Antenatal care: social risk
pregnancy,169595006,SNOMED,Antenatal care: history of child abuse
pregnancy,169597003,SNOMED,Antenatal care: medical risk
pregnancy,169598008,SNOMED,Antenatal care: gynecological risk
pregnancy,169600002,SNOMED,Antenatal care: under 5ft tall
pregnancy,169602005,SNOMED,Antenatal care: 10 years plus since last pregnancy
pregnancy,169603000,SNOMED,Antenatal care: primiparous  under 17 years
pregnancy,169604006,SNOMED,Antenatal care: primiparous  older than 30 years
pregnancy,169605007,SNOMED,Antenatal care: multiparous  older than 35 years
pregnancy,169613008,SNOMED,Antenatal care provider
pregnancy,169614002,SNOMED,Antenatal care from general practitioner
pregnancy,169615001,SNOMED,Antenatal care from consultant
pregnancy,169616000,SNOMED,Antenatal - shared care
pregnancy,169620001,SNOMED,Delivery: no place booked
pregnancy,169628008,SNOMED,Delivery booking - length of stay
pregnancy,169646002,SNOMED,Antenatal amniocentesis
pregnancy,169650009,SNOMED,Antenatal amniocentesis wanted
pregnancy,169651008,SNOMED,Antenatal amniocentesis - awaited
pregnancy,169652001,SNOMED,Antenatal amniocentesis - normal
pregnancy,169653006,SNOMED,Antenatal amniocentesis - abnormal
pregnancy,169657007,SNOMED,Antenatal ultrasound scan status
pregnancy,169661001,SNOMED,Antenatal ultrasound scan wanted
pregnancy,169662008,SNOMED,Antenatal ultrasound scan awaited
pregnancy,169667002,SNOMED,Antenatal ultrasound scan for slow growth
pregnancy,169668007,SNOMED,Antenatal ultrasound scan 4-8 weeks
pregnancy,169669004,SNOMED,Antenatal ultrasound scan at 9-16 weeks
pregnancy,169670003,SNOMED,Antenatal ultrasound scan at 17-22 weeks
pregnancy,169711001,SNOMED,Antenatal booking examination
pregnancy,169712008,SNOMED,Antenatal 12 weeks examination
pregnancy,169713003,SNOMED,Antenatal 16 week examination
pregnancy,169714009,SNOMED,Antenatal 20 week examination
pregnancy,169715005,SNOMED,Antenatal 24 week examination
pregnancy,169716006,SNOMED,Antenatal 28 week examination
pregnancy,169717002,SNOMED,Antenatal 30 week examination
pregnancy,169718007,SNOMED,Antenatal 32 week examination
pregnancy,169719004,SNOMED,Antenatal 34 week examination
pregnancy,169720005,SNOMED,Antenatal 35 week examination
pregnancy,169721009,SNOMED,Antenatal 36 week examination
pregnancy,169722002,SNOMED,Antenatal 37 week examination
pregnancy,169723007,SNOMED,Antenatal 38 week examination
pregnancy,169724001,SNOMED,Antenatal 39 week examination
pregnancy,169725000,SNOMED,Antenatal 40 week examination
pregnancy,169726004,SNOMED,Antenatal 41 week examination
pregnancy,169727008,SNOMED,Antenatal 42 week examination
pregnancy,169762003,SNOMED,Postnatal visit
pregnancy,169763008,SNOMED,Postnatal - first day visit
pregnancy,169764002,SNOMED,Postnatal - second day visit
pregnancy,169765001,SNOMED,Postnatal - third day visit
pregnancy,169766000,SNOMED,Postnatal - fourth day visit
pregnancy,169767009,SNOMED,Postnatal - fifth day visit
pregnancy,169768004,SNOMED,Postnatal - sixth day visit
pregnancy,169769007,SNOMED,Postnatal - seventh day visit
pregnancy,169770008,SNOMED,Postnatal - eighth day visit
pregnancy,169771007,SNOMED,Postnatal - ninth day visit
pregnancy,169772000,SNOMED,Postnatal - tenth day visit
pregnancy,169826009,SNOMED,Single live birth
pregnancy,169827000,SNOMED,Single stillbirth
pregnancy,169828005,SNOMED,Twins - both live born
pregnancy,169829002,SNOMED,Twins - one still and one live born
pregnancy,169830007,SNOMED,Twins - both stillborn
pregnancy,169831006,SNOMED,Triplets - all live born
pregnancy,169832004,SNOMED,Triplets - two live and one stillborn
pregnancy,169833009,SNOMED,Triplets - one live and two stillborn
pregnancy,169834003,SNOMED,Triplets - three stillborn
pregnancy,169836001,SNOMED,Birth of child
pregnancy,169952004,SNOMED,Placental finding
pregnancy,169954003,SNOMED,Placenta normal O/E
pregnancy,169960003,SNOMED,Labor details
pregnancy,169961004,SNOMED,Normal birth
pregnancy,17285009,SNOMED,Intraperitoneal pregnancy
pregnancy,173300003,SNOMED,Disorder of pregnancy
pregnancy,17333005,SNOMED,Term birth of multiple newborns
pregnancy,17335003,SNOMED,Illegal abortion with laceration of bowel
pregnancy,17369002,SNOMED,Miscarriage
pregnancy,17380002,SNOMED,Failed attempted abortion with acute renal failure
pregnancy,17382005,SNOMED,Cervical incompetence
pregnancy,17433009,SNOMED,Ruptured ectopic pregnancy
pregnancy,17532001,SNOMED,Breech malpresentation successfully converted to cephalic presentation
pregnancy,17588005,SNOMED,Abnormal yolk sac
pregnancy,17594002,SNOMED,Fetal bradycardia affecting management of mother
pregnancy,176827002,SNOMED,Dilation of cervix uteri and vacuum aspiration of products of conception from uterus
pregnancy,176849008,SNOMED,Introduction of abortifacient into uterine cavity
pregnancy,176854004,SNOMED,Insertion of prostaglandin abortifacient suppository
pregnancy,177128002,SNOMED,Induction and delivery procedures
pregnancy,177129005,SNOMED,Surgical induction of labor
pregnancy,177135005,SNOMED,Oxytocin induction of labor
pregnancy,177136006,SNOMED,Prostaglandin induction of labor
pregnancy,177141003,SNOMED,Elective cesarean section
pregnancy,177142005,SNOMED,Elective upper segment cesarean section
pregnancy,177143000,SNOMED,Elective lower segment cesarean section
pregnancy,177152009,SNOMED,Breech extraction delivery with version
pregnancy,177157003,SNOMED,Spontaneous breech delivery
pregnancy,177158008,SNOMED,Assisted breech delivery
pregnancy,177161009,SNOMED,Forceps cephalic delivery
pregnancy,177162002,SNOMED,High forceps cephalic delivery with rotation
pregnancy,177164001,SNOMED,Midforceps cephalic delivery with rotation
pregnancy,177167008,SNOMED,Barton forceps cephalic delivery with rotation
pregnancy,177168003,SNOMED,DeLee forceps cephalic delivery with rotation
pregnancy,177170007,SNOMED,Piper forceps delivery
pregnancy,177173009,SNOMED,High vacuum delivery
pregnancy,177174003,SNOMED,Low vacuum delivery
pregnancy,177175002,SNOMED,Vacuum delivery before full dilation of cervix
pregnancy,177176001,SNOMED,Trial of vacuum delivery
pregnancy,177179008,SNOMED,Cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,177180006,SNOMED,Manipulative cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,177181005,SNOMED,Non-manipulative cephalic vaginal delivery with abnormal presentation of head at delivery without instrument
pregnancy,177184002,SNOMED,Normal delivery procedure
pregnancy,177185001,SNOMED,Water birth delivery
pregnancy,177200004,SNOMED,Instrumental removal of products of conception from delivered uterus
pregnancy,177203002,SNOMED,Manual removal of products of conception from delivered uterus
pregnancy,177204008,SNOMED,Manual removal of placenta from delivered uterus
pregnancy,177208006,SNOMED,Repositioning of inverted uterus
pregnancy,177212000,SNOMED,Normal delivery of placenta
pregnancy,177217006,SNOMED,Immediate repair of obstetric laceration
pregnancy,177218001,SNOMED,Immediate repair of obstetric laceration of uterus or cervix uteri
pregnancy,177219009,SNOMED,Immediate repair of obstetric laceration of perineum and sphincter of anus
pregnancy,177220003,SNOMED,Immediate repair of obstetric laceration of vagina and floor of pelvis
pregnancy,177221004,SNOMED,Immediate repair of minor obstetric laceration
pregnancy,177227000,SNOMED,Secondary repair of obstetric laceration
pregnancy,17787002,SNOMED,Peripheral neuritis in pregnancy
pregnancy,178280004,SNOMED,Postnatal infection
pregnancy,17860005,SNOMED,Low forceps delivery with episiotomy
pregnancy,18114009,SNOMED,Prenatal examination and care of mother
pregnancy,18122002,SNOMED,Normal uterine contraction wave
pregnancy,18237006,SNOMED,Parenchymatous mastitis associated with childbirth
pregnancy,18260003,SNOMED,Postpartum psychosis
pregnancy,18302006,SNOMED,Therapeutic abortion by hysterotomy
pregnancy,18391007,SNOMED,Elective abortion
pregnancy,184339009,SNOMED,Miscarriage at 8 to 28 weeks
pregnancy,18606002,SNOMED,Dyscoordinate labor
pregnancy,18613002,SNOMED,Miscarriage with septic shock
pregnancy,18625004,SNOMED,Low forceps delivery
pregnancy,18684002,SNOMED,Illegal abortion complicated by metabolic disorder
pregnancy,18872002,SNOMED,Illegal abortion with laceration of cervix
pregnancy,18894005,SNOMED,Legal abortion with cerebral anoxia
pregnancy,19099008,SNOMED,Failed attempted abortion with laceration of bladder
pregnancy,19169002,SNOMED,Miscarriage in first trimester
pregnancy,19228003,SNOMED,Failed attempted abortion with perforation of bladder
pregnancy,19363005,SNOMED,Failed attempted abortion with blood-clot embolism
pregnancy,19390001,SNOMED,Partial breech delivery with forceps to aftercoming head
pregnancy,195005,SNOMED,Illegal abortion with endometritis
pregnancy,19564003,SNOMED,Illegal abortion with laceration of periurethral tissue
pregnancy,19569008,SNOMED,Mild hyperemesis gravidarum
pregnancy,19729005,SNOMED,Prolapse of anterior lip of cervix obstructing labor
pregnancy,19773009,SNOMED,Infection of the breast AND/OR nipple associated with childbirth
pregnancy,198332009,SNOMED,Old uterine laceration due to obstetric cause
pregnancy,198347000,SNOMED,Cicatrix (postpartum) of cervix
pregnancy,198617006,SNOMED,Delivery of viable fetus in abdominal pregnancy
pregnancy,198620003,SNOMED,Ruptured tubal pregnancy
pregnancy,198624007,SNOMED,Membranous pregnancy
pregnancy,198626009,SNOMED,Mesenteric pregnancy
pregnancy,198627000,SNOMED,Angular pregnancy
pregnancy,198644001,SNOMED,Incomplete miscarriage with genital tract or pelvic infection
pregnancy,198645000,SNOMED,Incomplete miscarriage with delayed or excessive hemorrhage
pregnancy,198646004,SNOMED,Incomplete miscarriage with damage to pelvic organs or tissues
pregnancy,198647008,SNOMED,Incomplete miscarriage with renal failure
pregnancy,198648003,SNOMED,Incomplete miscarriage with metabolic disorder
pregnancy,198649006,SNOMED,Incomplete miscarriage with shock
pregnancy,198650006,SNOMED,Incomplete miscarriage with embolism
pregnancy,198655001,SNOMED,Complete miscarriage with genital tract or pelvic infection
pregnancy,198656000,SNOMED,Complete miscarriage with delayed or excessive hemorrhage
pregnancy,198657009,SNOMED,Complete miscarriage with damage to pelvic organs or tissues
pregnancy,198659007,SNOMED,Complete miscarriage with renal failure
pregnancy,198660002,SNOMED,Complete miscarriage with metabolic disorder
pregnancy,19866007,SNOMED,Previous operation to cervix affecting pregnancy
pregnancy,198661003,SNOMED,Complete miscarriage with shock
pregnancy,198663000,SNOMED,Complete miscarriage with embolism
pregnancy,198705001,SNOMED,Incomplete legal abortion with genital tract or pelvic infection
pregnancy,198706000,SNOMED,Incomplete legal abortion with delayed or excessive hemorrhage
pregnancy,198707009,SNOMED,Incomplete legal abortion with damage to pelvic organs or tissues
pregnancy,198708004,SNOMED,Incomplete legal abortion with renal failure
pregnancy,198709007,SNOMED,Incomplete legal abortion with metabolic disorder
pregnancy,198710002,SNOMED,Incomplete legal abortion with shock
pregnancy,198711003,SNOMED,Incomplete legal abortion with embolism
pregnancy,198718009,SNOMED,Complete legal abortion with genital tract or pelvic infection
pregnancy,198719001,SNOMED,Complete legal abortion with delayed or excessive hemorrhage
pregnancy,198720007,SNOMED,Complete legal abortion with damage to pelvic organs or tissues
pregnancy,198721006,SNOMED,Complete legal abortion with renal failure
pregnancy,198722004,SNOMED,Complete legal abortion with metabolic disorder
pregnancy,198723009,SNOMED,Complete legal abortion with shock
pregnancy,198724003,SNOMED,Complete legal abortion with embolism
pregnancy,198743003,SNOMED,Illegal abortion incomplete
pregnancy,198744009,SNOMED,Incomplete illegal abortion with genital tract or pelvic infection
pregnancy,198745005,SNOMED,Incomplete illegal abortion with delayed or excessive hemorrhage
pregnancy,198746006,SNOMED,Incomplete illegal abortion with damage to pelvic organs or tissues
pregnancy,198747002,SNOMED,Incomplete illegal abortion with renal failure
pregnancy,198748007,SNOMED,Incomplete illegal abortion with metabolic disorder
pregnancy,198749004,SNOMED,Incomplete illegal abortion with shock
pregnancy,198750004,SNOMED,Incomplete illegal abortion with embolism
pregnancy,198755009,SNOMED,Illegal abortion complete
pregnancy,198757001,SNOMED,Complete illegal abortion with delayed or excessive hemorrhage
pregnancy,198758006,SNOMED,Complete illegal abortion with damage to pelvic organs or tissues
pregnancy,198759003,SNOMED,Complete illegal abortion with renal failure
pregnancy,198760008,SNOMED,Complete illegal abortion with metabolic disorder
pregnancy,198761007,SNOMED,Complete illegal abortion with shock
pregnancy,198762000,SNOMED,Complete illegal abortion with embolism
pregnancy,198806007,SNOMED,Failed attempted abortion with genital tract or pelvic infection
pregnancy,198807003,SNOMED,Failed attempted abortion with delayed or excessive hemorrhage
pregnancy,198808008,SNOMED,Failed attempted abortion with damage to pelvic organs or tissues
pregnancy,198809000,SNOMED,Failed attempted abortion with renal failure
pregnancy,198810005,SNOMED,Failed attempted abortion with metabolic disorder
pregnancy,198811009,SNOMED,Failed attempted abortion with shock
pregnancy,198812002,SNOMED,Failed attempted abortion with embolism
pregnancy,198832001,SNOMED,Damage to pelvic organs or tissues following abortive pregnancy
pregnancy,198861000,SNOMED,Readmission for retained products of conception  spontaneous abortion
pregnancy,198862007,SNOMED,Readmission for retained products of conception  legal abortion
pregnancy,198863002,SNOMED,Readmission for retained products of conception  illegal abortion
pregnancy,198874006,SNOMED,Failed medical abortion  complicated by genital tract and pelvic infection
pregnancy,198875007,SNOMED,Failed medical abortion  complicated by delayed or excessive hemorrhage
pregnancy,198876008,SNOMED,Failed medical abortion  complicated by embolism
pregnancy,198878009,SNOMED,Failed medical abortion  without complication
pregnancy,198899007,SNOMED,Placenta previa without hemorrhage - delivered
pregnancy,198900002,SNOMED,Placenta previa without hemorrhage - not delivered
pregnancy,198903000,SNOMED,Placenta previa with hemorrhage
pregnancy,198905007,SNOMED,Placenta previa with hemorrhage - delivered
pregnancy,198906008,SNOMED,Placenta previa with hemorrhage - not delivered
pregnancy,198910006,SNOMED,Placental abruption - delivered
pregnancy,198911005,SNOMED,Placental abruption - not delivered
pregnancy,198912003,SNOMED,Premature separation of placenta with coagulation defect
pregnancy,198917009,SNOMED,Antepartum hemorrhage with coagulation defect - delivered
pregnancy,198918004,SNOMED,Antepartum hemorrhage with coagulation defect - not delivered
pregnancy,198920001,SNOMED,Antepartum hemorrhage with trauma
pregnancy,198922009,SNOMED,Antepartum hemorrhage with trauma - delivered
pregnancy,198923004,SNOMED,Antepartum hemorrhage with trauma - not delivered
pregnancy,198925006,SNOMED,Antepartum hemorrhage with uterine leiomyoma
pregnancy,198927003,SNOMED,Antepartum hemorrhage with uterine leiomyoma - delivered
pregnancy,198928008,SNOMED,Antepartum hemorrhage with uterine leiomyoma - not delivered
pregnancy,198991006,SNOMED,Eclampsia - delivered with postnatal complication
pregnancy,198992004,SNOMED,Eclampsia in pregnancy
pregnancy,198993009,SNOMED,Eclampsia with postnatal complication
pregnancy,199022003,SNOMED,Mild hyperemesis-delivered
pregnancy,199023008,SNOMED,Mild hyperemesis-not delivered
pregnancy,199025001,SNOMED,Hyperemesis gravidarum with metabolic disturbance
pregnancy,199027009,SNOMED,Hyperemesis gravidarum with metabolic disturbance - delivered
pregnancy,199028004,SNOMED,Hyperemesis gravidarum with metabolic disturbance - not delivered
pregnancy,199032005,SNOMED,Late pregnancy vomiting - delivered
pregnancy,199063009,SNOMED,Post-term pregnancy - delivered
pregnancy,199064003,SNOMED,Post-term pregnancy - not delivered
pregnancy,199087006,SNOMED,Habitual aborter - delivered
pregnancy,199088001,SNOMED,Habitual aborter - not delivered
pregnancy,199093003,SNOMED,Peripheral neuritis in pregnancy - delivered
pregnancy,199095005,SNOMED,Peripheral neuritis in pregnancy - not delivered
pregnancy,199096006,SNOMED,Peripheral neuritis in pregnancy with postnatal complication
pregnancy,199099004,SNOMED,Asymptomatic bacteriuria in pregnancy - delivered
pregnancy,199100007,SNOMED,Asymptomatic bacteriuria in pregnancy - delivered with postnatal complication
pregnancy,199101006,SNOMED,Asymptomatic bacteriuria in pregnancy - not delivered
pregnancy,199102004,SNOMED,Asymptomatic bacteriuria in pregnancy with postnatal complication
pregnancy,199110003,SNOMED,Infections of kidney in pregnancy
pregnancy,199112006,SNOMED,Infections of the genital tract in pregnancy
pregnancy,199117000,SNOMED,Liver disorder in pregnancy - delivered
pregnancy,199118005,SNOMED,Liver disorder in pregnancy - not delivered
pregnancy,199121007,SNOMED,Fatigue during pregnancy - delivered
pregnancy,199122000,SNOMED,Fatigue during pregnancy - delivered with postnatal complication
pregnancy,199123005,SNOMED,Fatigue during pregnancy - not delivered
pregnancy,199124004,SNOMED,Fatigue during pregnancy with postnatal complication
pregnancy,199127006,SNOMED,Herpes gestationis - delivered
pregnancy,199128001,SNOMED,Herpes gestationis - delivered with postnatal complication
pregnancy,199129009,SNOMED,Herpes gestationis - not delivered
pregnancy,199139003,SNOMED,Pregnancy-induced edema and proteinuria without hypertension
pregnancy,199141002,SNOMED,Gestational edema with proteinuria
pregnancy,199192005,SNOMED,Maternal rubella during pregnancy - baby delivered
pregnancy,199194006,SNOMED,Maternal rubella during pregnancy - baby not yet delivered
pregnancy,199225007,SNOMED,Diabetes mellitus during pregnancy - baby delivered
pregnancy,199227004,SNOMED,Diabetes mellitus during pregnancy - baby not yet delivered
pregnancy,199244000,SNOMED,Anemia during pregnancy - baby delivered
pregnancy,199246003,SNOMED,Anemia during pregnancy - baby not yet delivered
pregnancy,199252002,SNOMED,Drug dependence during pregnancy - baby delivered
pregnancy,199254001,SNOMED,Drug dependence during pregnancy - baby not yet delivered
pregnancy,199266007,SNOMED,Congenital cardiovascular disorder during pregnancy - baby delivered
pregnancy,199305006,SNOMED,Complications specific to multiple gestation
pregnancy,199306007,SNOMED,Continuing pregnancy after abortion of one fetus or more
pregnancy,199307003,SNOMED,Continuing pregnancy after intrauterine death of one or more fetuses
pregnancy,199314001,SNOMED,Normal delivery but ante- or post- natal conditions present
pregnancy,199317008,SNOMED,Twin pregnancy - delivered
pregnancy,199318003,SNOMED,Twin pregnancy with antenatal problem
pregnancy,199321001,SNOMED,Triplet pregnancy - delivered
pregnancy,199322008,SNOMED,Triplet pregnancy with antenatal problem
pregnancy,199325005,SNOMED,Quadruplet pregnancy - delivered
pregnancy,199326006,SNOMED,Quadruplet pregnancy with antenatal problem
pregnancy,199329004,SNOMED,Multiple delivery  all spontaneous
pregnancy,199330009,SNOMED,Multiple delivery  all by forceps and vacuum extractor
pregnancy,199331008,SNOMED,Multiple delivery  all by cesarean section
pregnancy,199344003,SNOMED,Unstable lie - delivered
pregnancy,199345002,SNOMED,Unstable lie with antenatal problem
pregnancy,199358001,SNOMED,Oblique lie - delivered
pregnancy,199359009,SNOMED,Oblique lie with antenatal problem
pregnancy,199362007,SNOMED,Transverse lie - delivered
pregnancy,199363002,SNOMED,Transverse lie with antenatal problem
pregnancy,199375007,SNOMED,High head at term - delivered
pregnancy,199378009,SNOMED,Multiple pregnancy with malpresentation
pregnancy,199380003,SNOMED,Multiple pregnancy with malpresentation - delivered
pregnancy,199381004,SNOMED,Multiple pregnancy with malpresentation with antenatal problem
pregnancy,199384007,SNOMED,Prolapsed arm - delivered
pregnancy,199385008,SNOMED,Prolapsed arm with antenatal problem
pregnancy,199397009,SNOMED,Cephalopelvic disproportion
pregnancy,199405005,SNOMED,Generally contracted pelvis - delivered
pregnancy,199406006,SNOMED,Generally contracted pelvis with antenatal problem
pregnancy,199409004,SNOMED,Inlet pelvic contraction - delivered
pregnancy,199410009,SNOMED,Inlet pelvic contraction with antenatal problem
pregnancy,199413006,SNOMED,Outlet pelvic contraction - delivered
pregnancy,199414000,SNOMED,Outlet pelvic contraction with antenatal problem
pregnancy,199416003,SNOMED,Mixed feto-pelvic disproportion
pregnancy,199418002,SNOMED,Mixed feto-pelvic disproportion - delivered
pregnancy,199419005,SNOMED,Mixed feto-pelvic disproportion with antenatal problem
pregnancy,199422007,SNOMED,Large fetus causing disproportion - delivered
pregnancy,199423002,SNOMED,Large fetus causing disproportion with antenatal problem
pregnancy,199425009,SNOMED,Hydrocephalic disproportion
pregnancy,199427001,SNOMED,Hydrocephalic disproportion - delivered
pregnancy,199428006,SNOMED,Hydrocephalic disproportion with antenatal problem
pregnancy,199466009,SNOMED,Retroverted incarcerated gravid uterus
pregnancy,199468005,SNOMED,Retroverted incarcerated gravid uterus - delivered
pregnancy,199469002,SNOMED,Retroverted incarcerated gravid uterus - delivered with postnatal complication
pregnancy,199470001,SNOMED,Retroverted incarcerated gravid uterus with antenatal problem
pregnancy,199471002,SNOMED,Retroverted incarcerated gravid uterus with postnatal complication
pregnancy,199482005,SNOMED,Cervical incompetence - delivered
pregnancy,199483000,SNOMED,Cervical incompetence - delivered with postnatal complication
pregnancy,199484006,SNOMED,Cervical incompetence with antenatal problem
pregnancy,199485007,SNOMED,Cervical incompetence with postnatal complication
pregnancy,199577000,SNOMED,Fetal-maternal hemorrhage - delivered
pregnancy,199578005,SNOMED,Fetal-maternal hemorrhage with antenatal problem
pregnancy,199582007,SNOMED,Rhesus isoimmunization - delivered
pregnancy,199583002,SNOMED,Rhesus isoimmunization with antenatal problem
pregnancy,199595002,SNOMED,Labor and delivery complication by meconium in amniotic fluid
pregnancy,199596001,SNOMED,Labor and delivery complicated by fetal heart rate anomaly with meconium in amniotic fluid
pregnancy,199597005,SNOMED,Labor and delivery complicated by biochemical evidence of fetal stress
pregnancy,199625002,SNOMED,Placental transfusion syndromes
pregnancy,199646006,SNOMED,Polyhydramnios - delivered
pregnancy,199647002,SNOMED,Polyhydramnios with antenatal problem
pregnancy,199653002,SNOMED,Oligohydramnios - delivered
pregnancy,199654008,SNOMED,Oligohydramnios with antenatal problem
pregnancy,199677008,SNOMED,Amniotic cavity infection - delivered
pregnancy,199678003,SNOMED,Amniotic cavity infection with antenatal problem
pregnancy,199694005,SNOMED,Failed mechanical induction - delivered
pregnancy,199695006,SNOMED,Failed mechanical induction with antenatal problem
pregnancy,199710008,SNOMED,Sepsis during labor  delivered
pregnancy,199711007,SNOMED,Sepsis during labor with antenatal problem
pregnancy,199718001,SNOMED,Elderly primigravida - delivered
pregnancy,199719009,SNOMED,Elderly primigravida with antenatal problem
pregnancy,199732004,SNOMED,Abnormal findings on antenatal screening of mother
pregnancy,199733009,SNOMED,Abnormal hematologic finding on antenatal screening of mother
pregnancy,199734003,SNOMED,Abnormal biochemical finding on antenatal screening of mother
pregnancy,199735002,SNOMED,Abnormal cytological finding on antenatal screening of mother
pregnancy,199737005,SNOMED,Abnormal radiological finding on antenatal screening of mother
pregnancy,199738000,SNOMED,Abnormal chromosomal and genetic finding on antenatal screening of mother
pregnancy,199741009,SNOMED,Malnutrition in pregnancy
pregnancy,199745000,SNOMED,Complication occurring during labor and delivery
pregnancy,199746004,SNOMED,Obstructed labor
pregnancy,199747008,SNOMED,Obstructed labor due to fetal malposition
pregnancy,199749006,SNOMED,Obstructed labor due to fetal malposition - delivered
pregnancy,199750006,SNOMED,Obstructed labor due to fetal malposition with antenatal problem
pregnancy,199751005,SNOMED,Obstructed labor due to breech presentation
pregnancy,199752003,SNOMED,Obstructed labor due to face presentation
pregnancy,199753008,SNOMED,Obstructed labor due to brow presentation
pregnancy,199754002,SNOMED,Obstructed labor due to shoulder presentation
pregnancy,199755001,SNOMED,Obstructed labor due to compound presentation
pregnancy,199757009,SNOMED,Obstructed labor caused by bony pelvis
pregnancy,199759007,SNOMED,Obstructed labor caused by bony pelvis - delivered
pregnancy,199760002,SNOMED,Obstructed labor caused by bony pelvis with antenatal problem
pregnancy,199761003,SNOMED,Obstructed labor due to deformed pelvis
pregnancy,199762005,SNOMED,Obstructed labor due to generally contracted pelvis
pregnancy,199763000,SNOMED,Obstructed labor due to pelvic inlet contraction
pregnancy,199764006,SNOMED,Obstructed labor due to pelvic outlet and mid-cavity contraction
pregnancy,199765007,SNOMED,Obstructed labor due to abnormality of maternal pelvic organs
pregnancy,199767004,SNOMED,Obstructed labor caused by pelvic soft tissues
pregnancy,199769001,SNOMED,Obstructed labor caused by pelvic soft tissues - delivered
pregnancy,199770000,SNOMED,Obstructed labor caused by pelvic soft tissues with antenatal problem
pregnancy,199774009,SNOMED,Deep transverse arrest - delivered
pregnancy,199775005,SNOMED,Deep transverse arrest with antenatal problem
pregnancy,199783004,SNOMED,Shoulder dystocia - delivered
pregnancy,199784005,SNOMED,Shoulder dystocia with antenatal problem
pregnancy,199787003,SNOMED,Locked twins - delivered
pregnancy,199788008,SNOMED,Locked twins with antenatal problem
pregnancy,199806003,SNOMED,Obstructed labor due to unusually large fetus
pregnancy,199819004,SNOMED,Primary uterine inertia - delivered
pregnancy,199821009,SNOMED,Primary uterine inertia with antenatal problem
pregnancy,199824001,SNOMED,Secondary uterine inertia - delivered
pregnancy,199825000,SNOMED,Secondary uterine inertia with antenatal problem
pregnancy,199833004,SNOMED,Precipitate labor - delivered
pregnancy,199834005,SNOMED,Precipitate labor with antenatal problem
pregnancy,199838008,SNOMED,Hypertonic uterine inertia - delivered
pregnancy,199839000,SNOMED,Hypertonic uterine inertia with antenatal problem
pregnancy,199847000,SNOMED,Prolonged first stage - delivered
pregnancy,199848005,SNOMED,Prolonged first stage with antenatal problem
pregnancy,199857004,SNOMED,Prolonged second stage - delivered
pregnancy,199858009,SNOMED,Prolonged second stage with antenatal problem
pregnancy,199860006,SNOMED,Delayed delivery of second twin  triplet etc
pregnancy,199862003,SNOMED,Delayed delivery second twin - delivered
pregnancy,199863008,SNOMED,Delayed delivery second twin with antenatal problem
pregnancy,199889008,SNOMED,Short cord - delivered
pregnancy,199890004,SNOMED,Short cord with antenatal problem
pregnancy,199916005,SNOMED,First degree perineal tear during delivery - delivered
pregnancy,199917001,SNOMED,First degree perineal tear during delivery with postnatal problem
pregnancy,199925004,SNOMED,Second degree perineal tear during delivery - delivered
pregnancy,199926003,SNOMED,Second degree perineal tear during delivery with postnatal problem
pregnancy,199930000,SNOMED,Third degree perineal tear during delivery - delivered
pregnancy,199931001,SNOMED,Third degree perineal tear during delivery with postnatal problem
pregnancy,199934009,SNOMED,Fourth degree perineal tear during delivery - delivered
pregnancy,199935005,SNOMED,Fourth degree perineal tear during delivery with postnatal problem
pregnancy,199958008,SNOMED,Ruptured uterus before labor
pregnancy,199960005,SNOMED,Rupture of uterus before labor - delivered
pregnancy,199961009,SNOMED,Rupture of uterus before labor with antenatal problem
pregnancy,199964001,SNOMED,Rupture of uterus during and after labor - delivered
pregnancy,199965000,SNOMED,Rupture of uterus during and after labor - delivered with postnatal problem
pregnancy,199969006,SNOMED,Obstetric inversion of uterus - delivered with postnatal problem
pregnancy,199970007,SNOMED,Obstetric inversion of uterus with postnatal problem
pregnancy,199972004,SNOMED,Laceration of cervix - obstetric
pregnancy,199974003,SNOMED,Obstetric laceration of cervix - delivered
pregnancy,199975002,SNOMED,Obstetric laceration of cervix with postnatal problem
pregnancy,199977005,SNOMED,Obstetric high vaginal laceration
pregnancy,199979008,SNOMED,Obstetric high vaginal laceration - delivered
pregnancy,199980006,SNOMED,Obstetric high vaginal laceration with postnatal problem
pregnancy,199990003,SNOMED,Obstetric damage to pelvic joints and ligaments - delivered
pregnancy,199991004,SNOMED,Obstetric damage to pelvic joints and ligaments with postnatal problem
pregnancy,199997000,SNOMED,Obstetric pelvic hematoma with postnatal problem
pregnancy,200025008,SNOMED,Secondary postpartum hemorrhage - delivered with postnatal problem
pregnancy,200030007,SNOMED,Postpartum coagulation defects - delivered with postnatal problem
pregnancy,200031006,SNOMED,Postpartum coagulation defects with postnatal problem
pregnancy,200038000,SNOMED,Retained placenta with no hemorrhage with postnatal problem
pregnancy,200040005,SNOMED,Retained portion of placenta or membranes with no hemorrhage
pregnancy,200043007,SNOMED,Retained products with no hemorrhage with postnatal problem
pregnancy,200046004,SNOMED,Complications of anesthesia during labor and delivery
pregnancy,200049006,SNOMED,Obstetric anesthesia with pulmonary complications - delivered
pregnancy,200050006,SNOMED,Obstetric anesthesia with pulmonary complications - delivered with postnatal problem
pregnancy,200052003,SNOMED,Obstetric anesthesia with pulmonary complications with postnatal problem
pregnancy,200054002,SNOMED,Obstetric anesthesia with cardiac complications
pregnancy,200056000,SNOMED,Obstetric anesthesia with cardiac complications - delivered
pregnancy,200057009,SNOMED,Obstetric anesthesia with cardiac complications - delivered with postnatal problem
pregnancy,200059007,SNOMED,Obstetric anesthesia with cardiac complications with postnatal problem
pregnancy,200061003,SNOMED,Obstetric anesthesia with central nervous system complications
pregnancy,200063000,SNOMED,Obstetric anesthesia with central nervous system complications - delivered
pregnancy,200064006,SNOMED,Obstetric anesthesia with central nervous system complication - delivered with postnatal problem
pregnancy,200066008,SNOMED,Obstetric anesthesia with central nervous system complication with postnatal problem
pregnancy,200068009,SNOMED,Obstetric toxic reaction caused by local anesthesia
pregnancy,200070000,SNOMED,Toxic reaction caused by local anesthesia during puerperium
pregnancy,200075005,SNOMED,Toxic reaction caused by local anesthetic during labor and delivery
pregnancy,200077002,SNOMED,Cardiac complications of anesthesia during labor and delivery
pregnancy,200099005,SNOMED,Maternal distress - delivered
pregnancy,200100002,SNOMED,Maternal distress - delivered with postnatal problem
pregnancy,200102005,SNOMED,Maternal distress with postnatal problem
pregnancy,200105007,SNOMED,Obstetric shock - delivered
pregnancy,200106008,SNOMED,Obstetric shock - delivered with postnatal problem
pregnancy,200108009,SNOMED,Obstetric shock with postnatal problem
pregnancy,200111005,SNOMED,Maternal hypotension syndrome - delivered
pregnancy,200112003,SNOMED,Maternal hypotension syndrome - delivered with postnatal problem
pregnancy,200114002,SNOMED,Maternal hypotension syndrome with postnatal problem
pregnancy,200118004,SNOMED,Post-delivery acute renal failure with postnatal problem
pregnancy,200125006,SNOMED,Infection of obstetric surgical wound
pregnancy,200130005,SNOMED,Forceps delivery - delivered
pregnancy,200133007,SNOMED,Delivery by combination of forceps and vacuum extractor
pregnancy,200134001,SNOMED,Delivered by mid-cavity forceps with rotation
pregnancy,200138003,SNOMED,Vacuum extractor delivery - delivered
pregnancy,200142000,SNOMED,Breech extraction - delivered
pregnancy,200144004,SNOMED,Deliveries by cesarean
pregnancy,200146002,SNOMED,Cesarean delivery - delivered
pregnancy,200147006,SNOMED,Cesarean section - pregnancy at term
pregnancy,200148001,SNOMED,Delivery by elective cesarean section
pregnancy,200149009,SNOMED,Delivery by emergency cesarean section
pregnancy,200150009,SNOMED,Delivery by cesarean hysterectomy
pregnancy,200151008,SNOMED,Cesarean section following previous cesarean section
pregnancy,200154000,SNOMED,Deliveries by destructive operation
pregnancy,200164009,SNOMED,Exhaustion during labor
pregnancy,200173001,SNOMED,Intrapartum hemorrhage with coagulation defect
pregnancy,200181000,SNOMED,Puerperal endometritis - delivered with postnatal complication
pregnancy,200182007,SNOMED,Puerperal endometritis with postnatal complication
pregnancy,200185009,SNOMED,Puerperal salpingitis - delivered with postnatal complication
pregnancy,200187001,SNOMED,Puerperal salpingitis with postnatal complication
pregnancy,200190007,SNOMED,Puerperal peritonitis - delivered with postnatal complication
pregnancy,200191006,SNOMED,Puerperal peritonitis with postnatal complication
pregnancy,200195002,SNOMED,Puerperal septicemia - delivered with postnatal complication
pregnancy,200196001,SNOMED,Puerperal sepsis with postnatal complication
pregnancy,200237000,SNOMED,Postnatal deep vein thrombosis - delivered with postnatal complication
pregnancy,200238005,SNOMED,Postnatal deep vein thrombosis with postnatal complication
pregnancy,200255009,SNOMED,Hemorrhoids in the puerperium
pregnancy,200277008,SNOMED,Puerperal pyrexia of unknown origin
pregnancy,200280009,SNOMED,Puerperal pyrexia of unknown origin - delivered with postnatal complication
pregnancy,200281008,SNOMED,Puerperal pyrexia of unknown origin with postnatal complication
pregnancy,200286003,SNOMED,Obstetric air pulmonary embolism
pregnancy,200288002,SNOMED,Obstetric air pulmonary embolism - delivered
pregnancy,200289005,SNOMED,Obstetric air pulmonary embolism - delivered with postnatal complication
pregnancy,200290001,SNOMED,Obstetric air pulmonary embolism with antenatal complication
pregnancy,200291002,SNOMED,Obstetric air pulmonary embolism with postnatal complication
pregnancy,200297003,SNOMED,Amniotic fluid pulmonary embolism with postnatal complication
pregnancy,200330000,SNOMED,Puerperal cerebrovascular disorder - delivered
pregnancy,200331001,SNOMED,Puerperal cerebrovascular disorder - delivered with postnatal complication
pregnancy,200332008,SNOMED,Puerperal cerebrovascular disorder with antenatal complication
pregnancy,200333003,SNOMED,Puerperal cerebrovascular disorder with postnatal complication
pregnancy,200337002,SNOMED,Cesarean wound disruption - delivered with postnatal complication
pregnancy,200338007,SNOMED,Cesarean wound disruption with postnatal complication
pregnancy,200342005,SNOMED,Obstetric perineal wound disruption - delivered with postnatal complication
pregnancy,200343000,SNOMED,Obstetric perineal wound disruption with postnatal complication
pregnancy,200351002,SNOMED,Placental polyp - delivered with postnatal complication
pregnancy,200352009,SNOMED,Placental polyp with postnatal complication
pregnancy,200444007,SNOMED,Galactorrhea in pregnancy and the puerperium
pregnancy,200446009,SNOMED,Galactorrhea in pregnancy and the puerperium - delivered
pregnancy,200447000,SNOMED,Galactorrhea in pregnancy and the puerperium - delivered with postnatal complication
pregnancy,200448005,SNOMED,Galactorrhea in pregnancy and the puerperium with antenatal complication
pregnancy,200449002,SNOMED,Galactorrhea in pregnancy and the puerperium with postnatal complication
pregnancy,201134009,SNOMED,Alopecia of pregnancy
pregnancy,20216003,SNOMED,Legal abortion with perforation of uterus
pregnancy,20236002,SNOMED,Labor established
pregnancy,20259008,SNOMED,Finding of ballottement of fetal parts
pregnancy,20272009,SNOMED,Premature birth of multiple newborns
pregnancy,20286008,SNOMED,Gorissenne's sign
pregnancy,20391007,SNOMED,Amniotic cyst
pregnancy,20483002,SNOMED,Legal abortion with renal tubular necrosis
pregnancy,206057002,SNOMED,Fetal or neonatal effect of transverse lie before labor
pregnancy,206070006,SNOMED,Fetal or neonatal effect of placental damage caused by cesarean section
pregnancy,206112008,SNOMED,Fetal or neonatal effect of maternal bony pelvis abnormality during labor and delivery
pregnancy,206113003,SNOMED,Fetal or neonatal effect of maternal contracted pelvis during labor and delivery
pregnancy,206114009,SNOMED,Fetal or neonatal effect of persistent occipito-posterior malposition during labor and delivery
pregnancy,206115005,SNOMED,Fetal or neonatal effect of shoulder presentation during labor and delivery
pregnancy,206116006,SNOMED,Fetal or neonatal effect of transverse lie during labor and delivery
pregnancy,206117002,SNOMED,Fetal or neonatal effect of face presentation during labor and delivery
pregnancy,206118007,SNOMED,Fetal or neonatal effect of disproportion during labor and delivery
pregnancy,206121009,SNOMED,Fetal or neonatal effect of forceps delivery
pregnancy,206122002,SNOMED,Fetal or neonatal effect of vacuum extraction delivery
pregnancy,206123007,SNOMED,Fetal or neonatal effect of cesarean section
pregnancy,206134005,SNOMED,Fetal or neonatal effect of precipitate delivery
pregnancy,206137003,SNOMED,Fetal or neonatal effect of maternal contraction ring
pregnancy,206138008,SNOMED,Fetal or neonatal effect of hypertonic labor
pregnancy,206139000,SNOMED,Fetal or neonatal effect of hypertonic uterine dysfunction
pregnancy,206146009,SNOMED,Fetal or neonatal effect of induction of labor
pregnancy,206148005,SNOMED,Fetal or neonatal effect of long labor
pregnancy,206149002,SNOMED,Fetal or neonatal effect of instrumental delivery
pregnancy,206242003,SNOMED,Liver subcapsular hematoma due to birth trauma
pregnancy,206244002,SNOMED,Vulval hematoma due to birth trauma
pregnancy,20625004,SNOMED,Obstruction caused by position of fetus at onset of labor
pregnancy,206365006,SNOMED,Clostridial intra-amniotic fetal infection
pregnancy,206538000,SNOMED,Idiopathic hydrops fetalis
pregnancy,20753005,SNOMED,Hypertensive heart disease complicating AND/OR reason for care during pregnancy
pregnancy,20845005,SNOMED,Meconium in amniotic fluid affecting management of mother
pregnancy,20932005,SNOMED,Puerperal salpingitis
pregnancy,21127004,SNOMED,Term birth of newborn male
pregnancy,21243004,SNOMED,Term birth of newborn
pregnancy,21280005,SNOMED,Legal abortion with postoperative shock
pregnancy,21334005,SNOMED,Failed attempted abortion with oliguria
pregnancy,21346009,SNOMED,Double uterus affecting pregnancy
pregnancy,21360006,SNOMED,Miscarriage with afibrinogenemia
pregnancy,21504004,SNOMED,Pressure collapse of lung after anesthesia AND/OR sedation in labor AND/OR delivery
pregnancy,21604008,SNOMED,Illegal abortion with blood-clot embolism
pregnancy,21623001,SNOMED,Fetal biophysical profile
pregnancy,21737000,SNOMED,Retained fetal tissue
pregnancy,21987001,SNOMED,Delayed delivery of second of multiple births
pregnancy,22046002,SNOMED,Illegal abortion with salpingitis
pregnancy,22173004,SNOMED,Excessive fetal growth affecting management of mother
pregnancy,22271007,SNOMED,Abnormal fetal heart beat first noted during labor AND/OR delivery in liveborn infant
pregnancy,22281000119101,SNOMED,Post-term pregnancy of 40 to 42 weeks
pregnancy,22288000,SNOMED,Tumor of cervix affecting pregnancy
pregnancy,223003,SNOMED,Tumor of body of uterus affecting pregnancy
pregnancy,2239004,SNOMED,Previous pregnancies 6
pregnancy,22399000,SNOMED,Puerperal endometritis
pregnancy,22514005,SNOMED,Term birth of fraternal twins  one living  one stillborn
pregnancy,225245001,SNOMED,Rubbing up a contraction
pregnancy,22633006,SNOMED,Vaginal delivery  medical personnel present
pregnancy,22753004,SNOMED,Fetal AND/OR placental disorder affecting management of mother
pregnancy,22758008,SNOMED,Spalding-Horner sign
pregnancy,22846003,SNOMED,Hepatorenal syndrome following delivery
pregnancy,228471000000102,SNOMED,Routine obstetric scan
pregnancy,228531000000103,SNOMED,Cervical length scanning at 24 weeks
pregnancy,228551000000105,SNOMED,Mid trimester scan
pregnancy,228691000000101,SNOMED,Non routine obstetric scan for fetal observations
pregnancy,228701000000101,SNOMED,Fetal ascites scan
pregnancy,228711000000104,SNOMED,Rhesus detailed scan
pregnancy,22879003,SNOMED,Previous pregnancies 2
pregnancy,228881000000102,SNOMED,Ultrasound monitoring of early pregnancy
pregnancy,228921000000108,SNOMED,Obstetric ultrasound monitoring
pregnancy,22966008,SNOMED,Hypertensive heart AND renal disease complicating AND/OR reason for care during pregnancy
pregnancy,229761000000107,SNOMED,Vacuum aspiration of products of conception from uterus using flexible cannula
pregnancy,229801000000102,SNOMED,Vacuum aspiration of products of conception from uterus using rigid cannula
pregnancy,23001005,SNOMED,Stenosis AND/OR stricture of cervix affecting pregnancy
pregnancy,23128002,SNOMED,Cervical dilatation  5cm
pregnancy,23171006,SNOMED,Delayed AND/OR secondary postpartum hemorrhage
pregnancy,23177005,SNOMED,Crepitus uteri
pregnancy,232671000000100,SNOMED,Fetal measurement scan
pregnancy,23332002,SNOMED,Failed trial of labor
pregnancy,23401002,SNOMED,Illegal abortion with perforation of bowel
pregnancy,234058009,SNOMED,Genital varices in pregnancy
pregnancy,23431000119106,SNOMED,Rupture of uterus during labor
pregnancy,234380002,SNOMED,Kell isoimmunization of the newborn
pregnancy,23464008,SNOMED,Gestation period  20 weeks
pregnancy,23508005,SNOMED,Uterine incoordination  first degree
pregnancy,235888006,SNOMED,Cholestasis of pregnancy
pregnancy,23652008,SNOMED,Cervical dilatation  3cm
pregnancy,23667007,SNOMED,Term pregnancy delivered
pregnancy,236883005,SNOMED,Evacuation of retained product of conception
pregnancy,236958009,SNOMED,Induction of labor
pregnancy,236971007,SNOMED,Stabilizing induction
pregnancy,236973005,SNOMED,Delivery procedure
pregnancy,236974004,SNOMED,Instrumental delivery
pregnancy,236975003,SNOMED,Nonrotational forceps delivery
pregnancy,236976002,SNOMED,Outlet forceps delivery
pregnancy,236977006,SNOMED,Forceps delivery  face to pubes
pregnancy,236978001,SNOMED,Forceps delivery to the aftercoming head
pregnancy,236979009,SNOMED,Pajot's maneuver
pregnancy,236980007,SNOMED,Groin traction at breech delivery
pregnancy,236981006,SNOMED,Lovset's maneuver
pregnancy,236982004,SNOMED,Delivery of the after coming head
pregnancy,236983009,SNOMED,Burns Marshall maneuver
pregnancy,236984003,SNOMED,Mauriceau Smellie Veit maneuver
pregnancy,236985002,SNOMED,Emergency lower segment cesarean section
pregnancy,236986001,SNOMED,Emergency upper segment cesarean section
pregnancy,236987005,SNOMED,Emergency cesarean hysterectomy
pregnancy,236988000,SNOMED,Elective cesarean hysterectomy
pregnancy,236989008,SNOMED,Abdominal delivery for shoulder dystocia
pregnancy,236990004,SNOMED,Postmortem cesarean section
pregnancy,236991000,SNOMED,Operation to facilitate delivery
pregnancy,236992007,SNOMED,Right mediolateral episiotomy
pregnancy,236994008,SNOMED,Placental delivery procedure
pregnancy,237000000,SNOMED,Modification of uterine activity
pregnancy,237001001,SNOMED,Augmentation of labor
pregnancy,237002008,SNOMED,Stimulation of labor
pregnancy,237003003,SNOMED,Tocolysis for hypertonicity of uterus
pregnancy,237004009,SNOMED,Prophylactic oxytocic administration
pregnancy,237005005,SNOMED,Manual procedure for malpresentation or position
pregnancy,237006006,SNOMED,Internal conversion of face to vertex
pregnancy,237007002,SNOMED,Reposition of a prolapsed arm
pregnancy,237008007,SNOMED,Maneuvers for delivery in shoulder dystocia
pregnancy,237009004,SNOMED,McRoberts maneuver
pregnancy,237010009,SNOMED,Suprapubic pressure on fetal shoulder
pregnancy,237011008,SNOMED,Wood's screw maneuver
pregnancy,237012001,SNOMED,Freeing the posterior arm
pregnancy,237014000,SNOMED,Postpartum obstetric operation
pregnancy,237015004,SNOMED,Surgical control of postpartum hemorrhage
pregnancy,237021000,SNOMED,Hydrostatic replacement of inverted uterus
pregnancy,237022007,SNOMED,Manual replacement of inverted uterus
pregnancy,237023002,SNOMED,Haultain's operation
pregnancy,237024008,SNOMED,Kustner's operation
pregnancy,23717007,SNOMED,Benign essential hypertension complicating AND/OR reason for care during pregnancy
pregnancy,237198001,SNOMED,Rectocele complicating postpartum care - baby delivered during previous episode of care
pregnancy,237201006,SNOMED,Rectocele complicating antenatal care - baby not yet delivered
pregnancy,237202004,SNOMED,Rectocele - delivered with postpartum complication
pregnancy,237205002,SNOMED,Rectocele affecting obstetric care
pregnancy,237206001,SNOMED,Rectocele - baby delivered
pregnancy,237230004,SNOMED,Uremia in pregnancy without hypertension
pregnancy,237233002,SNOMED,Concealed pregnancy
pregnancy,237234008,SNOMED,Undiagnosed pregnancy
pregnancy,237235009,SNOMED,Undiagnosed breech
pregnancy,237236005,SNOMED,Undiagnosed multiple pregnancy
pregnancy,237237001,SNOMED,Undiagnosed twin
pregnancy,237238006,SNOMED,Pregnancy with uncertain dates
pregnancy,237239003,SNOMED,Low risk pregnancy
pregnancy,237240001,SNOMED,Teenage pregnancy
pregnancy,237241002,SNOMED,Viable pregnancy
pregnancy,237242009,SNOMED,Non-viable pregnancy
pregnancy,237243004,SNOMED,Biochemical pregnancy
pregnancy,237244005,SNOMED,Single pregnancy
pregnancy,237247003,SNOMED,Continuing pregnancy after abortion of sibling fetus
pregnancy,237249000,SNOMED,Complete hydatidiform mole
pregnancy,237250000,SNOMED,Incomplete hydatidiform mole
pregnancy,237252008,SNOMED,Placental site trophoblastic tumor
pregnancy,237253003,SNOMED,Viable fetus in abdominal pregnancy
pregnancy,237254009,SNOMED,Unruptured tubal pregnancy
pregnancy,237256006,SNOMED,Disorder of pelvic size and disproportion
pregnancy,237257002,SNOMED,Midpelvic contraction
pregnancy,237259004,SNOMED,Variation of placental position
pregnancy,237261008,SNOMED,Long umbilical cord
pregnancy,237270006,SNOMED,Antepartum hemorrhage with hypofibrinogenemia
pregnancy,237271005,SNOMED,Antepartum hemorrhage with hyperfibrinolysis
pregnancy,237272003,SNOMED,Antepartum hemorrhage with afibrinogenemia
pregnancy,237273008,SNOMED,Revealed accidental hemorrhage
pregnancy,237274002,SNOMED,Concealed accidental hemorrhage
pregnancy,237275001,SNOMED,Mixed accidental hemorrhage
pregnancy,237276000,SNOMED,Indeterminate antepartum hemorrhage
pregnancy,237277009,SNOMED,Marginal placental hemorrhage
pregnancy,237284001,SNOMED,Symptomatic disorders in pregnancy
pregnancy,237285000,SNOMED,Gestational edema
pregnancy,237286004,SNOMED,Vulval varices in pregnancy
pregnancy,237288003,SNOMED,Abnormal weight gain in pregnancy
pregnancy,237292005,SNOMED,Placental insufficiency
pregnancy,237294006,SNOMED,Retained placenta and membranes
pregnancy,237298009,SNOMED,Maternofetal transfusion
pregnancy,237300009,SNOMED,Pregnancy with isoimmunization
pregnancy,237302001,SNOMED,Kell isoimmunization in pregnancy
pregnancy,237303006,SNOMED,Duffy isoimmunization in pregnancy
pregnancy,237304000,SNOMED,Lewis isoimmunization in pregnancy
pregnancy,237311001,SNOMED,Breech delivery
pregnancy,237312008,SNOMED,Multiple delivery  function
pregnancy,237313003,SNOMED,Vaginal delivery following previous cesarean section
pregnancy,237319004,SNOMED,Failure to progress in labor
pregnancy,237320005,SNOMED,Failure to progress in first stage of labor
pregnancy,237321009,SNOMED,Delayed delivery of triplet
pregnancy,237324001,SNOMED,Cervical dystocia
pregnancy,237325000,SNOMED,Head entrapment during breech delivery
pregnancy,237327008,SNOMED,Obstetric pelvic ligament damage
pregnancy,237328003,SNOMED,Obstetric pelvic joint damage
pregnancy,237329006,SNOMED,Urethra injury - obstetric
pregnancy,237336007,SNOMED,Fibrinolysis - postpartum
pregnancy,237337003,SNOMED,Afibrinogenemia - postpartum
pregnancy,237338008,SNOMED,Blood dyscrasia puerperal
pregnancy,237343001,SNOMED,Puerperal phlebitis
pregnancy,237348005,SNOMED,Puerperal pyrexia
pregnancy,237349002,SNOMED,Mild postnatal depression
pregnancy,237350002,SNOMED,Severe postnatal depression
pregnancy,237351003,SNOMED,Mild postnatal psychosis
pregnancy,237352005,SNOMED,Severe postnatal psychosis
pregnancy,237357004,SNOMED,Vascular engorgement of breast
pregnancy,237364002,SNOMED,Stillbirth
pregnancy,237365001,SNOMED,Fresh stillbirth
pregnancy,237366000,SNOMED,Macerated stillbirth
pregnancy,23793007,SNOMED,Miscarriage without complication
pregnancy,238613007,SNOMED,Generalized pustular psoriasis of pregnancy
pregnancy,238820002,SNOMED,Erythema multiforme of pregnancy
pregnancy,23885003,SNOMED,Inversion of uterus during delivery
pregnancy,239101008,SNOMED,Pregnancy eruption
pregnancy,239102001,SNOMED,Pruritus of pregnancy
pregnancy,239103006,SNOMED,Prurigo of pregnancy
pregnancy,239104000,SNOMED,Pruritic folliculitis of pregnancy
pregnancy,239105004,SNOMED,Transplacental herpes gestationis
pregnancy,240160002,SNOMED,Transient osteoporosis of hip in pregnancy
pregnancy,24095001,SNOMED,Placenta previa partialis
pregnancy,24146004,SNOMED,Premature birth of newborn quintuplets
pregnancy,241491007,SNOMED,Ultrasound scan of fetus
pregnancy,241493005,SNOMED,Ultrasound scan for fetal growth
pregnancy,241494004,SNOMED,Ultrasound scan for amniotic fluid volume
pregnancy,24258008,SNOMED,Damage to pelvic joints AND/OR ligaments during delivery
pregnancy,243826008,SNOMED,Antenatal care status
pregnancy,243827004,SNOMED,Antenatal RhD antibody status
pregnancy,24444009,SNOMED,Failed attempted abortion with sepsis
pregnancy,24699006,SNOMED,Arrested active phase of labor
pregnancy,247421008,SNOMED,Removal of secundines by aspiration curettage
pregnancy,248896000,SNOMED,Products of conception in vagina
pregnancy,248937008,SNOMED,Products of conception at uterine os cervix
pregnancy,248985009,SNOMED,Presentation of pregnancy
pregnancy,248996001,SNOMED,Transversely contracted pelvis
pregnancy,249013004,SNOMED,Pregnant abdomen finding
pregnancy,249014005,SNOMED,Finding of shape of pregnant abdomen
pregnancy,249017003,SNOMED,Pregnant uterus displaced laterally
pregnancy,249018008,SNOMED,Irritable uterus
pregnancy,249020006,SNOMED,Cervical observation during pregnancy and labor
pregnancy,249032005,SNOMED,Finding of speed of delivery
pregnancy,249037004,SNOMED,Caul membrane over baby's head at delivery
pregnancy,249064003,SNOMED,Oblique lie  head in iliac fossa
pregnancy,249065002,SNOMED,Oblique lie  breech in iliac fossa
pregnancy,249089009,SNOMED,Fetal mouth presenting
pregnancy,249090000,SNOMED,Fetal ear presenting
pregnancy,249091001,SNOMED,Fetal nose presenting
pregnancy,249098007,SNOMED,Knee presentation
pregnancy,249099004,SNOMED,Single knee presentation
pregnancy,249100007,SNOMED,Double knee presentation
pregnancy,249104003,SNOMED,Dorsoanterior shoulder presentation
pregnancy,249105002,SNOMED,Dorsoposterior shoulder presentation
pregnancy,249122000,SNOMED,Baby overdue
pregnancy,249142009,SNOMED,Slow progress in first stage of labor
pregnancy,249144005,SNOMED,Rapid first stage of labor
pregnancy,249145006,SNOMED,Uterine observation in labor
pregnancy,249146007,SNOMED,Segments of uterus distinguishable in labor
pregnancy,249147003,SNOMED,State of upper segment retraction during labor
pregnancy,249148008,SNOMED,Hypertonic lower uterine segment during labor
pregnancy,249149000,SNOMED,Contraction of uterus during labor
pregnancy,249150000,SNOMED,Transmission of uterine contraction wave
pregnancy,249151001,SNOMED,Reversal of uterine contraction wave
pregnancy,249161008,SNOMED,Maternal effort during second stage of labor
pregnancy,249162001,SNOMED,Desire to push in labor
pregnancy,249166003,SNOMED,Failure to progress in second stage of labor
pregnancy,249170006,SNOMED,Complete placenta at delivery
pregnancy,249172003,SNOMED,Finding of consistency of placenta
pregnancy,249173008,SNOMED,Placenta gritty
pregnancy,249174002,SNOMED,Placenta calcified
pregnancy,249175001,SNOMED,Placental vessel finding
pregnancy,249177009,SNOMED,Retroplacental clot
pregnancy,249189006,SNOMED,Wharton's jelly excessive
pregnancy,249195007,SNOMED,Maternal condition during labor
pregnancy,249196008,SNOMED,Distress from pain in labor
pregnancy,249205008,SNOMED,Placental fragments in uterus
pregnancy,249206009,SNOMED,Placental fragments at cervical os
pregnancy,249207000,SNOMED,Membrane at cervical os
pregnancy,249218000,SNOMED,Postpartum vulval hematoma
pregnancy,249219008,SNOMED,Genital tear resulting from childbirth
pregnancy,249220002,SNOMED,Vaginal tear resulting from childbirth
pregnancy,25026004,SNOMED,Gestation period  18 weeks
pregnancy,25032009,SNOMED,Failed attempted abortion with laceration of cervix
pregnancy,25053000,SNOMED,Obstetrical central nervous system complication of anesthesia AND/OR sedation
pregnancy,25113000,SNOMED,Chorea gravidarum
pregnancy,25192009,SNOMED,Premature birth of fraternal twins  both living
pregnancy,25296001,SNOMED,Delivery by Scanzoni maneuver
pregnancy,25404008,SNOMED,Illegal abortion with electrolyte imbalance
pregnancy,25519006,SNOMED,Illegal abortion with perforation of vagina
pregnancy,25691001,SNOMED,Legal abortion with septic shock
pregnancy,25749005,SNOMED,Disproportion between fetus and pelvis
pregnancy,25825004,SNOMED,Hemorrhage in early pregnancy
pregnancy,25828002,SNOMED,Mid forceps delivery with episiotomy
pregnancy,25922000,SNOMED,Major depressive disorder  single episode with postpartum onset
pregnancy,26010008,SNOMED,Legal abortion with pelvic peritonitis
pregnancy,26050006,SNOMED,Suture of old obstetrical laceration of vagina
pregnancy,26158002,SNOMED,Uterine inertia
pregnancy,26224003,SNOMED,Failed attempted abortion complicated by genital-pelvic infection
pregnancy,26313002,SNOMED,Delivery by vacuum extraction with episiotomy
pregnancy,265062002,SNOMED,Dilation of cervix uteri and curettage of products of conception from uterus
pregnancy,265639000,SNOMED,Midforceps delivery without rotation
pregnancy,265640003,SNOMED,Crede placental expression
pregnancy,26623000,SNOMED,Failed attempted termination of pregnancy complicated by delayed and/or excessive hemorrhage
pregnancy,266784003,SNOMED,Manual replacement of inverted postnatal uterus
pregnancy,26690008,SNOMED,Gestation period  8 weeks
pregnancy,267193004,SNOMED,Incomplete legal abortion
pregnancy,267194005,SNOMED,Complete legal abortion
pregnancy,267197003,SNOMED,Antepartum hemorrhage  abruptio placentae and placenta previa
pregnancy,267199000,SNOMED,Antepartum hemorrhage with coagulation defect
pregnancy,267257007,SNOMED,Labor and delivery complicated by fetal heart rate anomaly
pregnancy,267265005,SNOMED,Hypertonic uterine inertia
pregnancy,267268007,SNOMED,Trauma to perineum and/or vulva during delivery
pregnancy,267269004,SNOMED,Vulval and perineal hematoma during delivery
pregnancy,267271004,SNOMED,Obstetric trauma damaging pelvic joints and ligaments
pregnancy,267272006,SNOMED,Postpartum coagulation defects
pregnancy,267273001,SNOMED,Retained placenta or membranes with no hemorrhage
pregnancy,267276009,SNOMED,Obstetric anesthesia with pulmonary complications
pregnancy,267278005,SNOMED,Deliveries by vacuum extractor
pregnancy,267335003,SNOMED,Fetoplacental problems
pregnancy,267340006,SNOMED,Maternal pyrexia in labor
pregnancy,26741000,SNOMED,Abnormal amnion
pregnancy,26743002,SNOMED,Illegal abortion complicated by embolism
pregnancy,26828006,SNOMED,Rectocele affecting pregnancy
pregnancy,268445003,SNOMED,Ultrasound scan - obstetric
pregnancy,268475008,SNOMED,Outcome of delivery
pregnancy,268479002,SNOMED,Incomplete placenta at delivery
pregnancy,268585006,SNOMED,Placental infarct
pregnancy,268809007,SNOMED,Fetal or neonatal effect of malposition or disproportion during labor or delivery
pregnancy,268811003,SNOMED,Fetal or neonatal effect of abnormal uterine contractions
pregnancy,268812005,SNOMED,Fetal or neonatal effect of uterine inertia or dysfunction
pregnancy,268813000,SNOMED,Fetal or neonatal effect of destructive operation to aid delivery
pregnancy,268865003,SNOMED,Had short umbilical cord
pregnancy,27015006,SNOMED,Legal abortion with amniotic fluid embolism
pregnancy,270498000,SNOMED,Malposition and malpresentation of fetus
pregnancy,27068000,SNOMED,Failed attempted abortion with afibrinogenemia
pregnancy,271368004,SNOMED,Delivered by low forceps delivery
pregnancy,271369007,SNOMED,Delivered by mid-cavity forceps delivery
pregnancy,271370008,SNOMED,Deliveries by breech extraction
pregnancy,271373005,SNOMED,Deliveries by spontaneous breech delivery
pregnancy,271403007,SNOMED,Placenta infarcted
pregnancy,271442007,SNOMED,Fetal anatomy study
pregnancy,27152008,SNOMED,Hyperemesis gravidarum before end of 22 week gestation with carbohydrate depletion
pregnancy,27169005,SNOMED,Legal abortion complicated by shock
pregnancy,271954000,SNOMED,Failed attempted medical abortion
pregnancy,27215002,SNOMED,Uterine inversion
pregnancy,27388005,SNOMED,Partial placenta previa with intrapartum hemorrhage
pregnancy,273982004,SNOMED,Obstetric problem affecting fetus
pregnancy,273984003,SNOMED,Delivery problem for fetus
pregnancy,274117006,SNOMED,Pregnancy and infectious disease
pregnancy,274118001,SNOMED,Venereal disease in pregnancy
pregnancy,274119009,SNOMED,Rubella in pregnancy
pregnancy,274121004,SNOMED,Cardiac disease in pregnancy
pregnancy,274122006,SNOMED,Gravid uterus - retroverted
pregnancy,274125008,SNOMED,Elderly primiparous with labor
pregnancy,274127000,SNOMED,Abnormal delivery
pregnancy,274128005,SNOMED,Brow delivery
pregnancy,274129002,SNOMED,Face delivery
pregnancy,274130007,SNOMED,Emergency cesarean section
pregnancy,274514009,SNOMED,Delivery conduct  function
pregnancy,274972007,SNOMED,Dilatation of cervix and curettage of retained products of conception
pregnancy,274973002,SNOMED,Dilation of cervix uteri and curettage for termination of pregnancy
pregnancy,275168001,SNOMED,Neville-Barnes forceps delivery
pregnancy,275169009,SNOMED,Simpson's forceps delivery
pregnancy,275306006,SNOMED,Postnatal data
pregnancy,275412000,SNOMED,Cystitis of pregnancy
pregnancy,275421004,SNOMED,Miscarriage with heavy bleeding
pregnancy,275425008,SNOMED,Retained products after miscarriage
pregnancy,275426009,SNOMED,Pelvic disproportion
pregnancy,275427000,SNOMED,Retained membrane without hemorrhage
pregnancy,275429002,SNOMED,Delayed delivery of second twin
pregnancy,275434003,SNOMED,Stroke in the puerperium
pregnancy,276367008,SNOMED,Wanted pregnancy
pregnancy,276445008,SNOMED,Antenatal risk factors
pregnancy,276479009,SNOMED,Retained membrane
pregnancy,276508000,SNOMED,Hydrops fetalis
pregnancy,276509008,SNOMED,Non-immune hydrops fetalis
pregnancy,276580005,SNOMED,Atypical isoimmunization of newborn
pregnancy,276641008,SNOMED,Intrauterine asphyxia
pregnancy,276642001,SNOMED,Antepartum fetal asphyxia
pregnancy,276881003,SNOMED,Secondary abdominal pregnancy
pregnancy,27696007,SNOMED,True knot of umbilical cord
pregnancy,278056007,SNOMED,Uneffaced cervix
pregnancy,278058008,SNOMED,Partially effaced cervix
pregnancy,278094007,SNOMED,Rapid rate of delivery
pregnancy,278095008,SNOMED,Normal rate of delivery
pregnancy,278096009,SNOMED,Slow rate of delivery
pregnancy,279225001,SNOMED,Maternity blues
pregnancy,28030000,SNOMED,Twin birth
pregnancy,280732008,SNOMED,Obstetric disorder of uterus
pregnancy,281050002,SNOMED,Livebirth
pregnancy,281052005,SNOMED,Triplet birth
pregnancy,281307002,SNOMED,Uncertain viability of pregnancy
pregnancy,281687006,SNOMED,Face to pubes birth
pregnancy,282020008,SNOMED,Premature delivery
pregnancy,284075002,SNOMED,Spotting per vagina in pregnancy
pregnancy,285409006,SNOMED,Medical termination of pregnancy
pregnancy,28542003,SNOMED,Wigand-Martin maneuver
pregnancy,285434006,SNOMED,Insertion of abortifacient suppository
pregnancy,2858002,SNOMED,Puerperal sepsis
pregnancy,286996009,SNOMED,Blighted ovum and/or carneous mole
pregnancy,28701003,SNOMED,Low maternal weight gain
pregnancy,287976008,SNOMED,Breech/instrumental delivery operations
pregnancy,287977004,SNOMED,Dilation/incision of cervix - delivery aid
pregnancy,288039005,SNOMED,Internal cephalic version and extraction
pregnancy,288042004,SNOMED,Hysterectomy and fetus removal
pregnancy,288045002,SNOMED,Injection of amnion for termination of pregnancy
pregnancy,288141009,SNOMED,Fetal head - manual flexion
pregnancy,288189000,SNOMED,Induction of labor by intravenous injection
pregnancy,288190009,SNOMED,Induction of labor by intravenous drip
pregnancy,288191008,SNOMED,Induction of labor by injection into upper limb
pregnancy,288193006,SNOMED,Supervision - normal delivery
pregnancy,288194000,SNOMED,Routine episiotomy and repair
pregnancy,288209009,SNOMED,Normal delivery - occipitoanterior
pregnancy,288210004,SNOMED,Abnormal head presentation delivery
pregnancy,288261009,SNOMED,Was malpresentation pre-labor
pregnancy,288265000,SNOMED,Born after precipitate delivery
pregnancy,288266004,SNOMED,Born after induced labor
pregnancy,28860009,SNOMED,Prague maneuver
pregnancy,289203002,SNOMED,Finding of pattern of pregnancy
pregnancy,289204008,SNOMED,Finding of quantity of pregnancy
pregnancy,289205009,SNOMED,Finding of measures of pregnancy
pregnancy,289208006,SNOMED,Finding of viability of pregnancy
pregnancy,289209003,SNOMED,Pregnancy problem
pregnancy,289210008,SNOMED,Finding of first stage of labor
pregnancy,289211007,SNOMED,First stage of labor established
pregnancy,289212000,SNOMED,First stage of labor not established
pregnancy,289213005,SNOMED,Normal length of first stage of labor
pregnancy,289214004,SNOMED,Normal first stage of labor
pregnancy,289215003,SNOMED,First stage of labor problem
pregnancy,289216002,SNOMED,Finding of second stage of labor
pregnancy,289217006,SNOMED,Second stage of labor established
pregnancy,289218001,SNOMED,Second stage of labor not established
pregnancy,289219009,SNOMED,Rapid second stage of labor
pregnancy,289220003,SNOMED,Progressing well in second stage
pregnancy,289221004,SNOMED,Normal length of second stage of labor
pregnancy,289222006,SNOMED,Second stage of labor problem
pregnancy,289223001,SNOMED,Normal second stage of labor
pregnancy,289224007,SNOMED,Finding of delivery push in labor
pregnancy,289226009,SNOMED,Pushing effectively in labor
pregnancy,289227000,SNOMED,Not pushing well in labor
pregnancy,289228005,SNOMED,Urge to push in labor
pregnancy,289229002,SNOMED,Reluctant to push in labor
pregnancy,289230007,SNOMED,Pushing voluntarily in labor
pregnancy,289231006,SNOMED,Pushing involuntarily in labor
pregnancy,289232004,SNOMED,Finding of third stage of labor
pregnancy,289233009,SNOMED,Normal length of third stage of labor
pregnancy,289234003,SNOMED,Prolonged third stage of labor
pregnancy,289235002,SNOMED,Rapid expulsion of placenta
pregnancy,289236001,SNOMED,Delayed expulsion of placenta
pregnancy,289237005,SNOMED,Normal rate of expulsion of placenta
pregnancy,289238000,SNOMED,Finding of pattern of labor
pregnancy,289239008,SNOMED,Finding of duration of labor
pregnancy,289240005,SNOMED,Long duration of labor
pregnancy,289241009,SNOMED,Short duration of labor
pregnancy,289242002,SNOMED,Finding of blood loss in labor
pregnancy,289243007,SNOMED,Maternal blood loss minimal
pregnancy,289244001,SNOMED,Maternal blood loss within normal limits
pregnancy,289245000,SNOMED,Maternal blood loss moderate
pregnancy,289246004,SNOMED,Maternal blood loss heavy
pregnancy,289247008,SNOMED,Finding of measures of labor
pregnancy,289252003,SNOMED,Estimated maternal blood loss
pregnancy,289253008,SNOMED,Device-associated finding of labor
pregnancy,289254002,SNOMED,Expulsion of intrauterine contraceptive device during third stage of labor
pregnancy,289256000,SNOMED,Mother delivered
pregnancy,289257009,SNOMED,Mother not delivered
pregnancy,289258004,SNOMED,Finding of pattern of delivery
pregnancy,289259007,SNOMED,Vaginal delivery
pregnancy,289261003,SNOMED,Delivery problem
pregnancy,289263000,SNOMED,Large placenta
pregnancy,289264006,SNOMED,Small placenta
pregnancy,289265007,SNOMED,Placenta normal size
pregnancy,289267004,SNOMED,Finding of completeness of placenta
pregnancy,289269001,SNOMED,Lesion of placenta
pregnancy,289270000,SNOMED,Fresh retroplacental clot
pregnancy,289271001,SNOMED,Old retroplacental clot
pregnancy,289272008,SNOMED,Placenta infected
pregnancy,289273003,SNOMED,Placenta fatty deposits
pregnancy,289275005,SNOMED,Placenta offensive odor
pregnancy,289276006,SNOMED,Finding of color of placenta
pregnancy,289277002,SNOMED,Placenta pale
pregnancy,289278007,SNOMED,Finding of measures of placenta
pregnancy,289279004,SNOMED,Placenta healthy
pregnancy,289280001,SNOMED,Placenta unhealthy
pregnancy,289349009,SNOMED,Presenting part ballottable
pregnancy,289350009,SNOMED,Ballottement of fetal head abdominally
pregnancy,289351008,SNOMED,Ballottement of fetal head at fundus
pregnancy,289352001,SNOMED,Ballottement of fetal head in suprapubic area
pregnancy,289353006,SNOMED,Ballottement of fetal head vaginally
pregnancy,289354000,SNOMED,Presenting part not ballottable
pregnancy,289355004,SNOMED,Oblique lie head in right iliac fossa
pregnancy,289356003,SNOMED,Oblique lie head in left iliac fossa
pregnancy,289357007,SNOMED,Oblique lie breech in right iliac fossa
pregnancy,289358002,SNOMED,Oblique lie breech in left iliac fossa
pregnancy,289572007,SNOMED,No liquor observed vaginally
pregnancy,289573002,SNOMED,Finding of passing of operculum
pregnancy,289574008,SNOMED,Operculum passed
pregnancy,289575009,SNOMED,Operculum not passed
pregnancy,289606001,SNOMED,Maternity pad damp with liquor
pregnancy,289607005,SNOMED,Maternity pad wet with liquor
pregnancy,289608000,SNOMED,Maternity pad soaked with liquor
pregnancy,289675001,SNOMED,Finding of gravid uterus
pregnancy,289676000,SNOMED,Gravid uterus present
pregnancy,289677009,SNOMED,Gravid uterus absent
pregnancy,289678004,SNOMED,Gravid uterus not observed
pregnancy,289679007,SNOMED,Finding of size of gravid uterus
pregnancy,289680005,SNOMED,Gravid uterus large-for-dates
pregnancy,289681009,SNOMED,Gravid uterus small-for-dates
pregnancy,289682002,SNOMED,Finding of height of gravid uterus
pregnancy,289683007,SNOMED,Fundal height high for dates
pregnancy,289684001,SNOMED,Fundal height equal to dates
pregnancy,289685000,SNOMED,Fundal height low for dates
pregnancy,289686004,SNOMED,Ovoid pregnant abdomen
pregnancy,289687008,SNOMED,Rounded pregnant abdomen
pregnancy,289688003,SNOMED,Transversely enlarged pregnant abdomen
pregnancy,289689006,SNOMED,Finding of arrangement of gravid uterus
pregnancy,289690002,SNOMED,Gravid uterus central
pregnancy,289691003,SNOMED,Gravid uterus deviated to left
pregnancy,289692005,SNOMED,Gravid uterus deviated to right
pregnancy,289693000,SNOMED,Normal position of gravid uterus
pregnancy,289694006,SNOMED,Finding of sensation of gravid uterus
pregnancy,289699001,SNOMED,Finding of uterine contractions
pregnancy,289700000,SNOMED,Uterine contractions present
pregnancy,289701001,SNOMED,Uterine contractions absent
pregnancy,289702008,SNOMED,Uterine contractions ceased
pregnancy,289703003,SNOMED,Finding of pattern of uterine contractions
pregnancy,289704009,SNOMED,Finding of frequency of uterine contraction
pregnancy,289705005,SNOMED,Intermittent uterine contractions
pregnancy,289706006,SNOMED,Occasional uterine tightenings
pregnancy,289707002,SNOMED,Finding of regularity of uterine contraction
pregnancy,289708007,SNOMED,Regular uterine contractions
pregnancy,289709004,SNOMED,Finding of quantity of uterine contraction
pregnancy,289710009,SNOMED,Excessive uterine contraction
pregnancy,289711008,SNOMED,Reduced uterine contraction
pregnancy,289712001,SNOMED,Niggling uterine contractions
pregnancy,289714000,SNOMED,Finding of strength of uterine contraction
pregnancy,289715004,SNOMED,Mild uterine contractions
pregnancy,289716003,SNOMED,Moderate uterine contractions
pregnancy,289717007,SNOMED,Fair uterine contractions
pregnancy,289718002,SNOMED,Good uterine contractions
pregnancy,289719005,SNOMED,Strong uterine contractions
pregnancy,289720004,SNOMED,Very strong uterine contractions
pregnancy,289721000,SNOMED,Variable strength uterine contractions
pregnancy,289722007,SNOMED,Normal strength uterine contractions
pregnancy,289723002,SNOMED,Finding of duration of uterine contraction
pregnancy,289724008,SNOMED,Continuous contractions
pregnancy,289725009,SNOMED,Hypertonic contractions
pregnancy,289726005,SNOMED,Finding of effectiveness of uterine contraction
pregnancy,289727001,SNOMED,Effective uterine contractions
pregnancy,289728006,SNOMED,Non-effective uterine contractions
pregnancy,289729003,SNOMED,Expulsive uterine contractions
pregnancy,289730008,SNOMED,Finding of painfulness of uterine contraction
pregnancy,289731007,SNOMED,Painless uterine contractions
pregnancy,289732000,SNOMED,Painful uterine contractions
pregnancy,289733005,SNOMED,Premature uterine contraction
pregnancy,289734004,SNOMED,Delayed uterine contraction
pregnancy,289735003,SNOMED,Persistent uterine contraction
pregnancy,289736002,SNOMED,Inappropriate uterine contraction
pregnancy,289737006,SNOMED,Finding of measures of uterine contractions
pregnancy,289738001,SNOMED,Uterine contractions normal
pregnancy,289739009,SNOMED,Uterine contractions problem
pregnancy,289740006,SNOMED,Finding of contraction state of uterus
pregnancy,289741005,SNOMED,Uterus contracted
pregnancy,289742003,SNOMED,Uterus relaxed
pregnancy,289743008,SNOMED,Finding of upper segment retraction
pregnancy,289744002,SNOMED,Poor retraction of upper segment
pregnancy,289745001,SNOMED,Excessive retraction of upper segment
pregnancy,289746000,SNOMED,Normal retraction of upper segment
pregnancy,289747009,SNOMED,Finding of measures of gravid uterus
pregnancy,289748004,SNOMED,Gravid uterus normal
pregnancy,289749007,SNOMED,Gravid uterus problem
pregnancy,289761004,SNOMED,Finding of cervical dilatation
pregnancy,289762006,SNOMED,Cervix dilated
pregnancy,289763001,SNOMED,Rim of cervix palpable
pregnancy,289764007,SNOMED,Cervix undilated
pregnancy,289765008,SNOMED,Ripe cervix
pregnancy,289766009,SNOMED,Finding of thickness of cervix
pregnancy,289768005,SNOMED,Cervix thick
pregnancy,289769002,SNOMED,Cervix thinning
pregnancy,289770001,SNOMED,Cervix thin
pregnancy,289771002,SNOMED,Cervix paper thin
pregnancy,289827009,SNOMED,Finding of cervical cerclage suture
pregnancy,289828004,SNOMED,Cervical cerclage suture absent
pregnancy,28996007,SNOMED,Stillbirth of premature female (1000-2499 gms.)
pregnancy,290653008,SNOMED,Postpartum hypopituitarism
pregnancy,291665000,SNOMED,Postpartum intrapituitary hemorrhage
pregnancy,29171003,SNOMED,Trapped placenta
pregnancy,29397004,SNOMED,Repair of old obstetric urethral laceration
pregnancy,29399001,SNOMED,Elderly primigravida
pregnancy,29421008,SNOMED,Failed attempted abortion complicated by renal failure
pregnancy,29583006,SNOMED,Illegal abortion with laceration of broad ligament
pregnancy,29613008,SNOMED,Delivery by double application of forceps
pregnancy,29682007,SNOMED,Dilation and curettage for termination of pregnancy
pregnancy,29851005,SNOMED,Halo sign
pregnancy,29950007,SNOMED,Legal abortion with septic embolism
pregnancy,29995000,SNOMED,Illegal abortion with fat embolism
pregnancy,29997008,SNOMED,Premature birth of newborn triplets
pregnancy,300927001,SNOMED,Episiotomy infection
pregnancy,30165006,SNOMED,Premature birth of fraternal twins  one living  one stillborn
pregnancy,301801008,SNOMED,Finding of position of pregnancy
pregnancy,302080006,SNOMED,Finding of birth outcome
pregnancy,302253005,SNOMED,Delivered by cesarean section - pregnancy at term
pregnancy,302254004,SNOMED,Delivered by cesarean delivery following previous cesarean delivery
pregnancy,302375005,SNOMED,Operative termination of pregnancy
pregnancy,302382009,SNOMED,Breech extraction with internal podalic version
pregnancy,302383004,SNOMED,Forceps delivery
pregnancy,302384005,SNOMED,Controlled cord traction of placenta
pregnancy,302644007,SNOMED,Abnormal immature chorion
pregnancy,303063000,SNOMED,Eclampsia in puerperium
pregnancy,30476003,SNOMED,Barton's forceps delivery
pregnancy,30479005,SNOMED,Legal abortion with afibrinogenemia
pregnancy,30653008,SNOMED,Previous pregnancies 5
pregnancy,306727001,SNOMED,Breech presentation  delivery  no version
pregnancy,307337003,SNOMED,Duffy isoimmunization of the newborn
pregnancy,307338008,SNOMED,Kidd isoimmunization of the newborn
pregnancy,307534009,SNOMED,Urinary tract infection in pregnancy
pregnancy,307733001,SNOMED,Inevitable abortion complete
pregnancy,307734007,SNOMED,Complete inevitable abortion complicated by genital tract and pelvic infection
pregnancy,307735008,SNOMED,Complete inevitable abortion complicated by delayed or excessive hemorrhage
pregnancy,307737000,SNOMED,Inevitable abortion incomplete
pregnancy,307738005,SNOMED,Complete inevitable abortion without complication
pregnancy,307746006,SNOMED,Incomplete inevitable abortion complicated by embolism
pregnancy,307748007,SNOMED,Incomplete inevitable abortion without complication
pregnancy,307749004,SNOMED,Incomplete inevitable abortion complicated by genital tract and pelvic infection
pregnancy,307750004,SNOMED,Complete inevitable abortion complicated by embolism
pregnancy,307752007,SNOMED,Incomplete inevitable abortion complicated by delayed or excessive hemorrhage
pregnancy,307813007,SNOMED,Antenatal ultrasound scan at 22-40 weeks
pregnancy,308037008,SNOMED,Syntocinon induction of labor
pregnancy,30806007,SNOMED,Miscarriage with endometritis
pregnancy,308135003,SNOMED,Genital varices in the puerperium
pregnancy,308137006,SNOMED,Superficial thrombophlebitis in puerperium
pregnancy,308140006,SNOMED,Hemorrhoids in pregnancy
pregnancy,308187004,SNOMED,Vaginal varices in the puerperium
pregnancy,30850008,SNOMED,Hemorrhage in early pregnancy  antepartum
pregnancy,309469004,SNOMED,Spontaneous vertex delivery
pregnancy,310248000,SNOMED,Antenatal care midwifery led
pregnancy,31026002,SNOMED,Obstetrical complication of sedation
pregnancy,31041000119106,SNOMED,Obstructed labor due to deep transverse arrest and persistent occipitoposterior position
pregnancy,310592002,SNOMED,Pregnancy prolonged - 41 weeks
pregnancy,310594001,SNOMED,Pregnancy prolonged - 42 weeks
pregnancy,310602001,SNOMED,Expression of retained products of conception
pregnancy,310603006,SNOMED,Digital evacuation of retained products of conception
pregnancy,310604000,SNOMED,Suction evacuation of retained products of conception
pregnancy,31159001,SNOMED,Bluish discoloration of cervix
pregnancy,31207002,SNOMED,Self-induced abortion
pregnancy,31208007,SNOMED,Medical induction of labor
pregnancy,313017000,SNOMED,Anhydramnios
pregnancy,313178001,SNOMED,Gestation less than 24 weeks
pregnancy,313179009,SNOMED,Gestation period  24 weeks
pregnancy,313180007,SNOMED,Gestation greater than 24 weeks
pregnancy,31383003,SNOMED,Tumor of vagina affecting pregnancy
pregnancy,314204000,SNOMED,Early stage of pregnancy
pregnancy,31481000,SNOMED,Vascular anomaly of umbilical cord
pregnancy,315307003,SNOMED,Repair of obstetric laceration of lower urinary tract
pregnancy,315308008,SNOMED,Dilatation of cervix for delivery
pregnancy,31563000,SNOMED,Asymptomatic bacteriuria in pregnancy
pregnancy,31601007,SNOMED,Combined pregnancy
pregnancy,31805001,SNOMED,Fetal disproportion
pregnancy,31821006,SNOMED,Uterine scar from previous surgery affecting pregnancy
pregnancy,31939001,SNOMED,Repair of obstetric laceration of cervix
pregnancy,31998007,SNOMED,Echography  scan B-mode for fetal growth rate
pregnancy,320003,SNOMED,Cervical dilatation  1cm
pregnancy,3230006,SNOMED,Illegal abortion with afibrinogenemia
pregnancy,32999002,SNOMED,Complication of obstetrical surgical wound
pregnancy,33046001,SNOMED,Illegal abortion with urinary tract infection
pregnancy,33340004,SNOMED,Multiple conception
pregnancy,33348006,SNOMED,Previous pregnancies 7
pregnancy,33370009,SNOMED,Hyperemesis gravidarum before end of 22 week gestation with electrolyte imbalance
pregnancy,33490001,SNOMED,Failed attempted abortion with fat embolism
pregnancy,33552005,SNOMED,Anomaly of placenta
pregnancy,33561005,SNOMED,Illegal abortion with acute renal failure
pregnancy,33627001,SNOMED,Prolonged first stage of labor
pregnancy,33654003,SNOMED,Repair of old obstetric laceration of urinary bladder
pregnancy,33807004,SNOMED,Internal and combined version with extraction
pregnancy,34089005,SNOMED,Premature birth of newborn quadruplets
pregnancy,34100008,SNOMED,Premature birth of identical twins  both living
pregnancy,34262005,SNOMED,Fourth degree perineal laceration involving rectal mucosa
pregnancy,34270000,SNOMED,Miscarriage complicated by shock
pregnancy,34327003,SNOMED,Parturient hemorrhage associated with hyperfibrinolysis
pregnancy,34367002,SNOMED,Failed attempted abortion with perforation of bowel
pregnancy,34478009,SNOMED,Failed attempted abortion with defibrination syndrome
pregnancy,34500003,SNOMED,Legal abortion without complication
pregnancy,34530006,SNOMED,Failed attempted abortion with electrolyte imbalance
pregnancy,34614007,SNOMED,Miscarriage with complication
pregnancy,34701001,SNOMED,Illegal abortion with postoperative shock
pregnancy,34801009,SNOMED,Ectopic pregnancy
pregnancy,34842007,SNOMED,Antepartum hemorrhage
pregnancy,34981006,SNOMED,Hypertonic uterine dysfunction
pregnancy,35347003,SNOMED,Delayed delivery after artificial rupture of membranes
pregnancy,35381000119101,SNOMED,Quadruplet pregnancy with loss of one or more fetuses
pregnancy,35537000,SNOMED,Ladin's sign
pregnancy,35574001,SNOMED,Delivery of shoulder presentation  function
pregnancy,35608009,SNOMED,Stillbirth of immature male (500-999 gms.)
pregnancy,35656003,SNOMED,Intraligamentous pregnancy
pregnancy,35716006,SNOMED,Cracked nipple associated with childbirth
pregnancy,35746009,SNOMED,Uterine fibroids affecting pregnancy
pregnancy,35874009,SNOMED,Normal labor
pregnancy,35882009,SNOMED,Abnormality of forces of labor
pregnancy,359937006,SNOMED,Repair of obstetric laceration of anus
pregnancy,359940006,SNOMED,Partial breech extraction
pregnancy,359943008,SNOMED,Partial breech delivery
pregnancy,359946000,SNOMED,Repair of current obstetric laceration of rectum and sphincter ani
pregnancy,35999006,SNOMED,Blighted ovum
pregnancy,361095003,SNOMED,Dehiscence AND/OR disruption of uterine wound in the puerperium
pregnancy,361096002,SNOMED,Disruption of cesarean wound in the puerperium
pregnancy,36144008,SNOMED,McClintock's sign
pregnancy,36248000,SNOMED,Repair of obstetric laceration of urethra
pregnancy,36297009,SNOMED,Septate vagina affecting pregnancy
pregnancy,362972006,SNOMED,Disorder of labor / delivery
pregnancy,362973001,SNOMED,Disorder of puerperium
pregnancy,3634007,SNOMED,Legal abortion complicated by metabolic disorder
pregnancy,363681007,SNOMED,Pregnancy with abortive outcome
pregnancy,36428009,SNOMED,Gestation period  42 weeks
pregnancy,364587008,SNOMED,Birth outcome
pregnancy,36473002,SNOMED,Legal abortion with oliguria
pregnancy,364738009,SNOMED,Finding of outcome of delivery
pregnancy,36497006,SNOMED,Traumatic extension of episiotomy
pregnancy,366323009,SNOMED,Finding of length of gestation
pregnancy,366325002,SNOMED,Finding of progress of labor - first stage
pregnancy,366327005,SNOMED,Finding of progress of second stage of labor
pregnancy,366328000,SNOMED,Finding related to ability to push in labor
pregnancy,366329008,SNOMED,Finding of speed of delivery of placenta
pregnancy,366330003,SNOMED,Finding of size of placenta
pregnancy,366332006,SNOMED,Finding of odor of placenta
pregnancy,36664002,SNOMED,Failed attempted abortion with air embolism
pregnancy,36697001,SNOMED,False knot of umbilical cord
pregnancy,367494004,SNOMED,Premature birth of newborn
pregnancy,36801000119105,SNOMED,Continuing triplet pregnancy after spontaneous abortion of one or more fetuses
pregnancy,36813001,SNOMED,Placenta previa
pregnancy,36854009,SNOMED,Inlet contraction of pelvis
pregnancy,37005007,SNOMED,Gestation period  5 weeks
pregnancy,370352001,SNOMED,Antenatal screening finding
pregnancy,371091008,SNOMED,Postpartum infection of vulva
pregnancy,371106008,SNOMED,Idiopathic maternal thrombocytopenia
pregnancy,371374003,SNOMED,Retained products of conception
pregnancy,371375002,SNOMED,Retained secundines
pregnancy,371380006,SNOMED,Amniotic fluid leaking
pregnancy,371614003,SNOMED,Hematoma of obstetric wound
pregnancy,372043009,SNOMED,Hydrops of allantois
pregnancy,372456005,SNOMED,Repair of obstetric laceration
pregnancy,373663005,SNOMED,Perinatal period
pregnancy,373896005,SNOMED,Miscarriage complicated by genital-pelvic infection
pregnancy,373901007,SNOMED,Legal abortion complicated by genital-pelvic infection
pregnancy,373902000,SNOMED,Illegal abortion complicated by genital-pelvic infection
pregnancy,37762002,SNOMED,Face OR brow presentation of fetus
pregnancy,37787009,SNOMED,Legal abortion with blood-clot embolism
pregnancy,3798002,SNOMED,Premature birth of identical twins  both stillborn
pregnancy,38010008,SNOMED,Intrapartum hemorrhage
pregnancy,38039008,SNOMED,Gestation period  10 weeks
pregnancy,38099005,SNOMED,Failed attempted abortion with endometritis
pregnancy,382341000000101,SNOMED,Total number of registerable births at delivery
pregnancy,38250004,SNOMED,Arrested labor
pregnancy,38257001,SNOMED,Term birth of identical twins  one living  one stillborn
pregnancy,38451000119105,SNOMED,Failed attempted vaginal birth after previous cesarean section
pregnancy,384729004,SNOMED,Delivery of vertex presentation
pregnancy,384730009,SNOMED,Delivery of cephalic presentation
pregnancy,38479009,SNOMED,Frank breech delivery
pregnancy,38534008,SNOMED,Aspiration of stomach contents after anesthesia AND/OR sedation in labor AND/OR delivery
pregnancy,386235000,SNOMED,Childbirth preparation
pregnancy,386322007,SNOMED,High risk pregnancy care
pregnancy,386343008,SNOMED,Labor suppression
pregnancy,386639001,SNOMED,Termination of pregnancy
pregnancy,386641000,SNOMED,Therapeutic abortion procedure
pregnancy,38720006,SNOMED,Septuplet pregnancy
pregnancy,387692004,SNOMED,Hypotonic uterine inertia
pregnancy,387696001,SNOMED,Primary hypotonic uterine dysfunction
pregnancy,387699008,SNOMED,Primary uterine inertia
pregnancy,387700009,SNOMED,Prolonged latent phase of labor
pregnancy,387709005,SNOMED,Removal of ectopic fetus from abdominal cavity
pregnancy,387710000,SNOMED,Removal of extrauterine ectopic fetus
pregnancy,387711001,SNOMED,Pubiotomy to assist delivery
pregnancy,38784004,SNOMED,Illegal abortion complicated by damage to pelvic organs AND/OR tissues
pregnancy,3885002,SNOMED,ABO isoimmunization affecting pregnancy
pregnancy,38905006,SNOMED,Illegal abortion with amniotic fluid embolism
pregnancy,38951007,SNOMED,Failed attempted abortion with laceration of bowel
pregnancy,39101000119109,SNOMED,Pregnancy with isoimmunization from irregular blood group incompatibility
pregnancy,391131000119106,SNOMED,Ultrasonography for qualitative amniotic fluid volume
pregnancy,39120007,SNOMED,Failed attempted abortion with salpingo-oophoritis
pregnancy,39121000119100,SNOMED,Pelvic mass in pregnancy
pregnancy,391896006,SNOMED,Therapeutic abortion by aspiration curettage
pregnancy,391897002,SNOMED,Aspiration curettage of uterus for termination of pregnancy
pregnancy,39199000,SNOMED,Miscarriage with laceration of bladder
pregnancy,391997001,SNOMED,Operation for retained placenta
pregnancy,391998006,SNOMED,Dilation and curettage of uterus after delivery
pregnancy,392000009,SNOMED,Hysterotomy for retained placenta
pregnancy,39208009,SNOMED,Fetal hydrops causing disproportion
pregnancy,39213008,SNOMED,Premature birth of identical twins  one living  one stillborn
pregnancy,39239006,SNOMED,Legal abortion with electrolyte imbalance
pregnancy,39246002,SNOMED,Legal abortion with sepsis
pregnancy,39249009,SNOMED,Placentitis
pregnancy,39333005,SNOMED,Cervical dilatation  8cm
pregnancy,39406005,SNOMED,Legally induced abortion
pregnancy,394211000119109,SNOMED,Ultrasonography for fetal biophysical profile with non-stress testing
pregnancy,394221000119102,SNOMED,Ultrasonography for fetal biophysical profile without non-stress testing
pregnancy,3944006,SNOMED,Placental sulfatase deficiency (X-linked steryl-sulfatase deficiency) in a female
pregnancy,3950001,SNOMED,Birth
pregnancy,396544001,SNOMED,Cesarean wound disruption
pregnancy,397752008,SNOMED,Obstetric perineal wound disruption
pregnancy,397949005,SNOMED,Poor fetal growth affecting management
pregnancy,397952002,SNOMED,Trauma to perineum during delivery
pregnancy,398019008,SNOMED,Perineal laceration during delivery
pregnancy,39804004,SNOMED,Abnormal products of conception
pregnancy,398262004,SNOMED,Disruption of episiotomy wound in the puerperium
pregnancy,398307005,SNOMED,Low cervical cesarean section
pregnancy,399031001,SNOMED,Fourth degree perineal laceration
pregnancy,399363000,SNOMED,Late fetal death affecting management of mother
pregnancy,400170001,SNOMED,Hypocalcemia of puerperium
pregnancy,4006006,SNOMED,Fetal tachycardia affecting management of mother
pregnancy,40219000,SNOMED,Delivery by Malstrom's extraction with episiotomy
pregnancy,4026005,SNOMED,Interstitial mastitis associated with childbirth
pregnancy,402836009,SNOMED,Spider telangiectasis in association with pregnancy
pregnancy,403528000,SNOMED,Pregnancy-related exacerbation of dermatosis
pregnancy,40405003,SNOMED,Illegal abortion with cerebral anoxia
pregnancy,40444006,SNOMED,Miscarriage with acute necrosis of liver
pregnancy,40500006,SNOMED,Illegal abortion with laceration of uterus
pregnancy,40521000119100,SNOMED,Postpartum pregnancy-induced hypertension
pregnancy,405256006,SNOMED,Parturient paresis
pregnancy,405733001,SNOMED,Hypocalcemia of late pregnancy or lactation
pregnancy,405736009,SNOMED,Accidental antepartum hemorrhage
pregnancy,40704000,SNOMED,Wright's obstetrical version with extraction
pregnancy,40791000119105,SNOMED,Postpartum gestational diabetes mellitus
pregnancy,40792007,SNOMED,Kristeller maneuver
pregnancy,40801000119106,SNOMED,Gestational diabetes mellitus complicating pregnancy
pregnancy,408783007,SNOMED,Antenatal Anti-D prophylaxis status
pregnancy,408796008,SNOMED,Stillbirth - unknown if fetal death intrapartum or prior to labor
pregnancy,408814002,SNOMED,Ultrasound scan for fetal anomaly
pregnancy,408815001,SNOMED,Ultrasound scan for fetal nuchal translucency
pregnancy,408818004,SNOMED,Induction of labor by artificial rupture of membranes
pregnancy,408819007,SNOMED,Delivery of placenta by maternal effort
pregnancy,408823004,SNOMED,Antenatal hepatitis B blood screening test status
pregnancy,408825006,SNOMED,Antenatal hepatitis B blood screening test sent
pregnancy,408827003,SNOMED,Antenatal human immunodeficiency virus blood screening test status
pregnancy,408828008,SNOMED,Antenatal human immunodeficiency virus blood screening test sent
pregnancy,408831009,SNOMED,Antenatal thalassemia blood screening test status
pregnancy,408833007,SNOMED,Antenatal thalassemia blood screening test sent
pregnancy,408840008,SNOMED,Extra-amniotic termination of pregnancy
pregnancy,408842000,SNOMED,Antenatal human immunodeficiency virus blood screening test requested
pregnancy,408843005,SNOMED,Antenatal thalassemia blood screening test requested
pregnancy,408883002,SNOMED,Breastfeeding support
pregnancy,41059002,SNOMED,Cesarean hysterectomy
pregnancy,41215002,SNOMED,Congenital abnormality of uterus  affecting pregnancy
pregnancy,413338003,SNOMED,Incomplete miscarriage with complication
pregnancy,413339006,SNOMED,Failed trial of labor - delivered
pregnancy,41438001,SNOMED,Gestation period  21 weeks
pregnancy,414880004,SNOMED,Nuchal ultrasound scan
pregnancy,415105001,SNOMED,Placental abruption
pregnancy,41587001,SNOMED,Third trimester pregnancy
pregnancy,416055001,SNOMED,Total breech extraction
pregnancy,416402001,SNOMED,Gestational trophoblastic disease
pregnancy,416413003,SNOMED,Advanced maternal age gravida
pregnancy,416669000,SNOMED,Invasive hydatidiform mole
pregnancy,417006004,SNOMED,Twin reversal arterial perfusion syndrome
pregnancy,417044008,SNOMED,Hydatidiform mole  benign
pregnancy,417121007,SNOMED,Breech extraction
pregnancy,417150000,SNOMED,Exaggerated placental site
pregnancy,417364008,SNOMED,Placental site nodule
pregnancy,417570003,SNOMED,Gestational choriocarcinoma
pregnancy,41806003,SNOMED,Legal abortion with laceration of broad ligament
pregnancy,418090003,SNOMED,Ultrasound obstetric doppler
pregnancy,42102002,SNOMED,Pre-admission observation  undelivered mother
pregnancy,42170009,SNOMED,Abnormal amniotic fluid
pregnancy,422808006,SNOMED,Prenatal continuous visit
pregnancy,423445003,SNOMED,Difficulty following prenatal diet
pregnancy,423834007,SNOMED,Difficulty with prenatal rest pattern
pregnancy,42390009,SNOMED,Repair of obstetric laceration of bladder and urethra
pregnancy,424037008,SNOMED,Difficulty following prenatal exercise routine
pregnancy,424441002,SNOMED,Prenatal initial visit
pregnancy,424525001,SNOMED,Antenatal care
pregnancy,424619006,SNOMED,Prenatal visit
pregnancy,42537006,SNOMED,Secondary perineal tear in the puerperium
pregnancy,42553009,SNOMED,Deficiency of placental barrier function
pregnancy,425551008,SNOMED,Antenatal ultrasound scan for possible abnormality
pregnancy,425708006,SNOMED,Placental aromatase deficiency
pregnancy,42571002,SNOMED,Failed induction of labor
pregnancy,42599006,SNOMED,Trauma from instrument during delivery
pregnancy,426295007,SNOMED,Obstetric uterine artery Doppler
pregnancy,426403007,SNOMED,Late entry into prenatal care
pregnancy,426840007,SNOMED,Fetal biometry using ultrasound
pregnancy,42686001,SNOMED,Chromosomal abnormality in fetus affecting obstetrical care
pregnancy,426997005,SNOMED,Traumatic injury during pregnancy
pregnancy,427013000,SNOMED,Alcohol consumption during pregnancy
pregnancy,427139004,SNOMED,Third trimester bleeding
pregnancy,427623005,SNOMED,Obstetric umbilical artery Doppler
pregnancy,42783002,SNOMED,Spontaneous placental expulsion  Schultz mechanism
pregnancy,428017002,SNOMED,Condyloma acuminata of vulva in pregnancy
pregnancy,428058009,SNOMED,Gestation less than 9 weeks
pregnancy,428164004,SNOMED,Mitral valve disorder in pregnancy
pregnancy,428230005,SNOMED,Trichomonal vaginitis in pregnancy
pregnancy,428252001,SNOMED,Vaginitis in pregnancy
pregnancy,428508008,SNOMED,Abortion due to Brucella abortus
pregnancy,428511009,SNOMED,Multiple pregnancy with one fetal loss
pregnancy,428566005,SNOMED,Gestation less than 20 weeks
pregnancy,428567001,SNOMED,Gestation 14 - 20 weeks
pregnancy,42857002,SNOMED,Second stage of labor
pregnancy,428930004,SNOMED,Gestation 9- 13 weeks
pregnancy,429187001,SNOMED,Continuing pregnancy after intrauterine death of twin fetus
pregnancy,429240000,SNOMED,Third trimester pregnancy less than 36 weeks
pregnancy,429715006,SNOMED,Gestation greater than 20 weeks
pregnancy,430063002,SNOMED,Transvaginal nuchal ultrasonography
pregnancy,430064008,SNOMED,Transvaginal obstetric ultrasonography
pregnancy,430881000,SNOMED,Second trimester bleeding
pregnancy,430933008,SNOMED,Gravid uterus size for dates discrepancy
pregnancy,431868002,SNOMED,Initiation of breastfeeding
pregnancy,432246004,SNOMED,Transvaginal obstetric doppler ultrasonography
pregnancy,43293004,SNOMED,Failed attempted abortion with septic embolism
pregnancy,43306002,SNOMED,Miscarriage complicated by embolism
pregnancy,433153009,SNOMED,Chorionic villus sampling using obstetric ultrasound guidance
pregnancy,43651009,SNOMED,Acquired stenosis of vagina affecting pregnancy
pregnancy,43673002,SNOMED,Fetal souffle
pregnancy,43697006,SNOMED,Gestation period  37 weeks
pregnancy,43715006,SNOMED,Secondary uterine inertia
pregnancy,43808007,SNOMED,Cervical dilatation  4cm
pregnancy,439311009,SNOMED,Intends to continue pregnancy
pregnancy,43970002,SNOMED,Congenital stenosis of vagina affecting pregnancy
pregnancy,43990006,SNOMED,Sextuplet pregnancy
pregnancy,44101005,SNOMED,Illegal abortion with air embolism
pregnancy,441619002,SNOMED,Repair of obstetric laceration of perineum and anal sphincter and mucosa of rectum
pregnancy,441697004,SNOMED,Thrombophilia associated with pregnancy
pregnancy,441924001,SNOMED,Gestational age unknown
pregnancy,44216000,SNOMED,Retained products of conception  following delivery with hemorrhage
pregnancy,442478007,SNOMED,Multiple pregnancy involving intrauterine pregnancy and tubal pregnancy
pregnancy,443006,SNOMED,Cystocele affecting pregnancy
pregnancy,443460007,SNOMED,Multigravida of advanced maternal age
pregnancy,44398003,SNOMED,Gestation period  4 weeks
pregnancy,444661007,SNOMED,High risk pregnancy due to history of preterm labor
pregnancy,445086005,SNOMED,Chemical pregnancy
pregnancy,4451004,SNOMED,Illegal abortion with renal tubular necrosis
pregnancy,445149007,SNOMED,Residual trophoblastic disease
pregnancy,445548006,SNOMED,Dead fetus in utero
pregnancy,445866007,SNOMED,Ultrasonography of multiple pregnancy for fetal anomaly
pregnancy,445912000,SNOMED,Excision of fallopian tube and surgical removal of ectopic pregnancy
pregnancy,446208007,SNOMED,Ultrasonography in second trimester
pregnancy,446353007,SNOMED,Ultrasonography in third trimester
pregnancy,44640004,SNOMED,Failed attempted abortion with parametritis
pregnancy,446522006,SNOMED,Ultrasonography in first trimester
pregnancy,446810002,SNOMED,Ultrasonography of multiple pregnancy for fetal nuchal translucency
pregnancy,446920006,SNOMED,Transvaginal ultrasonography to determine the estimated date of confinement
pregnancy,44772007,SNOMED,Maternal obesity syndrome
pregnancy,44782008,SNOMED,Molar pregnancy
pregnancy,44795003,SNOMED,Rhesus isoimmunization affecting pregnancy
pregnancy,447972007,SNOMED,Medical termination of pregnancy using prostaglandin
pregnancy,44814008,SNOMED,Miscarriage with laceration of periurethral tissue
pregnancy,44979007,SNOMED,Carneous mole
pregnancy,449807005,SNOMED,Type 3a third degree laceration of perineum
pregnancy,449808000,SNOMED,Type 3b third degree laceration of perineum
pregnancy,449809008,SNOMED,Type 3c third degree laceration of perineum
pregnancy,44992005,SNOMED,Failed attempted abortion with intravascular hemolysis
pregnancy,4504004,SNOMED,Potter's obstetrical version with extraction
pregnancy,450483001,SNOMED,Cesarean section through inverted T shaped incision of uterus
pregnancy,450484007,SNOMED,Cesarean section through J shaped incision of uterus
pregnancy,450640001,SNOMED,Open removal of products of conception from uterus
pregnancy,450678004,SNOMED,Intraamniotic injection of abortifacient
pregnancy,450679007,SNOMED,Extraamniotic injection of abortifacient
pregnancy,450798003,SNOMED,Delivery of occipitoposterior presentation
pregnancy,451014004,SNOMED,Curettage of products of conception from uterus
pregnancy,45139008,SNOMED,Gestation period  29 weeks
pregnancy,45307008,SNOMED,Extrachorial pregnancy
pregnancy,45384004,SNOMED,Multiple birth
pregnancy,45718005,SNOMED,Vaginal delivery with forceps including postpartum care
pregnancy,45757002,SNOMED,Labor problem
pregnancy,45759004,SNOMED,Rigid perineum affecting pregnancy
pregnancy,4576001,SNOMED,Legal abortion complicated by renal failure
pregnancy,459166009,SNOMED,Dichorionic diamniotic twin pregnancy
pregnancy,459167000,SNOMED,Monochorionic twin pregnancy
pregnancy,459168005,SNOMED,Monochorionic diamniotic twin pregnancy
pregnancy,459169002,SNOMED,Monochorionic diamniotic twin pregnancy with similar amniotic fluid volumes
pregnancy,459170001,SNOMED,Monochorionic diamniotic twin pregnancy with dissimilar amniotic fluid volumes
pregnancy,459171002,SNOMED,Monochorionic monoamniotic twin pregnancy
pregnancy,46022004,SNOMED,Bearing down reflex
pregnancy,46230007,SNOMED,Gestation period  40 weeks
pregnancy,46273003,SNOMED,Abscess of nipple associated with childbirth
pregnancy,46311005,SNOMED,Perineal laceration involving fourchette
pregnancy,46365005,SNOMED,Failed attempted abortion with perforation of periurethral tissue
pregnancy,46502006,SNOMED,Hematoma of vagina during delivery
pregnancy,46894009,SNOMED,Gestational diabetes mellitus  class A>2<
pregnancy,46906003,SNOMED,Gestation period  27 weeks
pregnancy,47161002,SNOMED,Failed attempted abortion with perforation of cervix
pregnancy,47200007,SNOMED,High risk pregnancy
pregnancy,472321009,SNOMED,Continuing pregnancy after intrauterine death of one twin with intrauterine retention of dead twin
pregnancy,47236005,SNOMED,Third stage hemorrhage
pregnancy,47267007,SNOMED,Fetal or neonatal effect of destructive operation on live fetus to facilitate delivery
pregnancy,472839005,SNOMED,Termination of pregnancy after first trimester
pregnancy,47537002,SNOMED,Miscarriage with postoperative shock
pregnancy,4787007,SNOMED,Fetal or neonatal effect of breech delivery and extraction
pregnancy,480571000119102,SNOMED,Doppler ultrasound velocimetry of umbilical artery of fetus
pregnancy,48204000,SNOMED,Spontaneous unassisted delivery  medical personnel present
pregnancy,48433002,SNOMED,Legal abortion with complication
pregnancy,48485000,SNOMED,Miscarriage in third trimester
pregnancy,48688005,SNOMED,Gestation period  26 weeks
pregnancy,48739004,SNOMED,Miscarriage with cardiac arrest and/or cardiac failure
pregnancy,48775002,SNOMED,Repair of obstetric laceration of pelvic floor
pregnancy,48782003,SNOMED,Delivery normal
pregnancy,4886009,SNOMED,Premature birth of newborn male
pregnancy,48888007,SNOMED,Placenta previa found before labor AND delivery by cesarean section without hemorrhage
pregnancy,48975005,SNOMED,Stimulated labor
pregnancy,4907004,SNOMED,Non-involution of uterus
pregnancy,49177006,SNOMED,Postpartum coagulation defect with hemorrhage
pregnancy,49279000,SNOMED,Slow slope active phase of labor
pregnancy,49342001,SNOMED,Stricture of vagina affecting pregnancy
pregnancy,49364005,SNOMED,Subareolar abscess associated with childbirth
pregnancy,49416000,SNOMED,Failed attempted abortion
pregnancy,49550006,SNOMED,Premature pregnancy delivered
pregnancy,49561003,SNOMED,Rupture of gravid uterus before onset of labor
pregnancy,49632008,SNOMED,Illegally induced abortion
pregnancy,49815007,SNOMED,Damage to coccyx during delivery
pregnancy,49964003,SNOMED,Ectopic fetus
pregnancy,50258003,SNOMED,Fetal or neonatal effect of hypotonic uterine dysfunction
pregnancy,50367001,SNOMED,Gestation period  11 weeks
pregnancy,50557007,SNOMED,Healed pelvic floor repair affecting pregnancy
pregnancy,50726009,SNOMED,Failed attempted abortion with perforation of uterus
pregnancy,50758004,SNOMED,Term birth of newborn twins
pregnancy,50770000,SNOMED,Miscarriage with defibrination syndrome
pregnancy,50844007,SNOMED,Failed attempted abortion with pelvic peritonitis
pregnancy,51096002,SNOMED,Legal abortion with pulmonary embolism
pregnancy,51154004,SNOMED,Obstetrical pulmonary complication of anesthesia AND/OR sedation
pregnancy,51195001,SNOMED,Placental polyp
pregnancy,51495008,SNOMED,Cerebral anoxia following anesthesia AND/OR sedation in labor AND/OR delivery
pregnancy,51519001,SNOMED,Marginal insertion of umbilical cord
pregnancy,51707007,SNOMED,Legal abortion with salpingo-oophoritis
pregnancy,51885006,SNOMED,Morning sickness
pregnancy,51920004,SNOMED,Precipitate labor
pregnancy,51953009,SNOMED,Legal abortion with perforation of broad ligament
pregnancy,51954003,SNOMED,Miscarriage with perforation of periurethral tissue
pregnancy,521000119104,SNOMED,Acute cystitis in pregnancy  antepartum
pregnancy,522101000000109,SNOMED,Fetal death before 24 weeks with retention of dead fetus
pregnancy,5231000179108,SNOMED,Three dimensional obstetric ultrasonography
pregnancy,52327008,SNOMED,Fetal myelomeningocele causing disproportion
pregnancy,52342006,SNOMED,Legal abortion with acute renal failure
pregnancy,5241000179100,SNOMED,Three dimensional obstetric ultrasonography in third trimester
pregnancy,52483005,SNOMED,Term birth of newborn sextuplets
pregnancy,52588004,SNOMED,Robert's sign
pregnancy,52660002,SNOMED,Induced abortion following intra-amniotic injection with hysterotomy
pregnancy,52772002,SNOMED,Postpartum thyroiditis
pregnancy,52942000,SNOMED,Term birth of stillborn twins
pregnancy,53024001,SNOMED,Insufficient weight gain of pregnancy
pregnancy,53098006,SNOMED,Legal abortion with salpingitis
pregnancy,53111003,SNOMED,Failed attempted abortion with postoperative shock
pregnancy,53183006,SNOMED,Miscarriage with intravascular hemolysis
pregnancy,53212003,SNOMED,Postobstetric urethral stricture
pregnancy,53247006,SNOMED,Illegal abortion with laceration of bladder
pregnancy,53443007,SNOMED,Prolonged labor
pregnancy,53638009,SNOMED,Therapeutic abortion
pregnancy,53881005,SNOMED,Gravida 0
pregnancy,54044001,SNOMED,Legal abortion with laceration of vagina
pregnancy,54155004,SNOMED,Illegal abortion with uremia
pregnancy,54212005,SNOMED,Irregular uterine contractions
pregnancy,54213000,SNOMED,Oligohydramnios without rupture of membranes
pregnancy,54318006,SNOMED,Gestation period  19 weeks
pregnancy,54449002,SNOMED,Forced uterine inversion
pregnancy,54529009,SNOMED,Illegal abortion with cardiac arrest AND/OR failure
pregnancy,54559004,SNOMED,Uterine souffle
pregnancy,54650005,SNOMED,Premature birth of stillborn twins
pregnancy,54812001,SNOMED,Delivery of brow presentation  function
pregnancy,54844002,SNOMED,Prolapse of gravid uterus
pregnancy,54973000,SNOMED,Total breech delivery with forceps to aftercoming head
pregnancy,55052008,SNOMED,Diagnostic ultrasound of gravid uterus
pregnancy,55187005,SNOMED,Tarnier's sign
pregnancy,55466006,SNOMED,Missed labor
pregnancy,55472006,SNOMED,Submammary abscess associated with childbirth
pregnancy,55543007,SNOMED,Previous pregnancies 3
pregnancy,5556001,SNOMED,Manually assisted spontaneous delivery
pregnancy,55581002,SNOMED,Meconium in amniotic fluid first noted during labor AND/OR delivery in liveborn infant
pregnancy,55589000,SNOMED,Illegal abortion with pulmonary embolism
pregnancy,55613002,SNOMED,Engorgement of breasts associated with childbirth
pregnancy,55639004,SNOMED,Failed attempted abortion with laceration of vagina
pregnancy,55669006,SNOMED,Repair of obstetrical laceration of perineum
pregnancy,55704005,SNOMED,Abscess of breast  associated with childbirth
pregnancy,5577003,SNOMED,Legal abortion with laceration of bowel
pregnancy,55933000,SNOMED,Legal abortion with perforation of bowel
pregnancy,55976003,SNOMED,Miscarriage with blood-clot embolism
pregnancy,56160003,SNOMED,Hydrorrhea gravidarum
pregnancy,56272000,SNOMED,Postpartum deep phlebothrombosis
pregnancy,56425003,SNOMED,Placenta edematous
pregnancy,56451001,SNOMED,Failed attempted abortion with perforation of broad ligament
pregnancy,56462001,SNOMED,Failed attempted abortion with soap embolism
pregnancy,56620000,SNOMED,Delivery of placenta following delivery of infant outside of hospital
pregnancy,57271003,SNOMED,Extraperitoneal cesarean section
pregnancy,57296000,SNOMED,Incarcerated gravid uterus
pregnancy,5740008,SNOMED,Pelvic hematoma during delivery
pregnancy,57411006,SNOMED,Colpoperineorrhaphy following delivery
pregnancy,57420002,SNOMED,Listeria abortion
pregnancy,57469000,SNOMED,Miscarriage with acute renal failure
pregnancy,57576007,SNOMED,Retroverted gravid uterus
pregnancy,57630001,SNOMED,First trimester pregnancy
pregnancy,57734001,SNOMED,Legal abortion complicated by embolism
pregnancy,57759005,SNOMED,First degree perineal laceration
pregnancy,57797005,SNOMED,Induced termination of pregnancy
pregnancy,57907009,SNOMED,Gestation period  36 weeks
pregnancy,58123006,SNOMED,Failed attempted abortion with pulmonary embolism
pregnancy,58289000,SNOMED,Prodromal stage labor
pregnancy,58532003,SNOMED,Unwanted pregnancy
pregnancy,58699001,SNOMED,Cervical dilatation  2cm
pregnancy,58703003,SNOMED,Postpartum depression
pregnancy,58705005,SNOMED,Bracht maneuver
pregnancy,58881007,SNOMED,Polyp of cervix affecting pregnancy
pregnancy,58990004,SNOMED,Miscarriage complicated by damage to pelvic organs and/or tissues
pregnancy,59204004,SNOMED,Miscarriage with septic embolism
pregnancy,59363009,SNOMED,Inevitable abortion
pregnancy,5939002,SNOMED,Failed attempted abortion complicated by metabolic disorder
pregnancy,59403008,SNOMED,Premature birth of newborn female
pregnancy,5945005,SNOMED,Legal abortion with urinary tract infection
pregnancy,59466002,SNOMED,Second trimester pregnancy
pregnancy,59566000,SNOMED,Oligohydramnios
pregnancy,59795007,SNOMED,Short cord
pregnancy,5984000,SNOMED,Fetal or neonatal effect of malpresentation  malposition and/or disproportion during labor and/or delivery
pregnancy,59859005,SNOMED,Legal abortion with laceration of bladder
pregnancy,59919008,SNOMED,Failed attempted abortion with complication
pregnancy,60000008,SNOMED,Mesometric pregnancy
pregnancy,60265009,SNOMED,Miscarriage with air embolism
pregnancy,60328005,SNOMED,Previous pregnancies 4
pregnancy,60401000119104,SNOMED,Postpartum psychosis in remission
pregnancy,60755004,SNOMED,Persistent hymen affecting pregnancy
pregnancy,60810003,SNOMED,Quadruplet pregnancy
pregnancy,609133009,SNOMED,Short cervical length in pregnancy
pregnancy,609204004,SNOMED,Subchorionic hematoma
pregnancy,609441001,SNOMED,Fetal or neonatal effect of breech delivery
pregnancy,609442008,SNOMED,Antenatal care for woman with history of recurrent miscarriage
pregnancy,609443003,SNOMED,Retained products of conception following induced termination of pregnancy
pregnancy,609446006,SNOMED,Induced termination of pregnancy with complication
pregnancy,609447002,SNOMED,Induced termination of pregnancy complicated by damage to pelvic organs and/or tissues
pregnancy,609449004,SNOMED,Induced termination of pregnancy complicated by embolism
pregnancy,609450004,SNOMED,Induced termination of pregnancy complicated by genital-pelvic infection
pregnancy,609451000,SNOMED,Induced termination of pregnancy complicated by metabolic disorder
pregnancy,609452007,SNOMED,Induced termination of pregnancy complicated by renal failure
pregnancy,609453002,SNOMED,Induced termination of pregnancy complicated by shock
pregnancy,609454008,SNOMED,Induced termination of pregnancy complicated by acute necrosis of liver
pregnancy,609455009,SNOMED,Induced termination of pregnancy complicated by acute renal failure
pregnancy,609456005,SNOMED,Induced termination of pregnancy complicated by afibrinogenemia
pregnancy,609457001,SNOMED,Induced termination of pregnancy complicated by air embolism
pregnancy,609458006,SNOMED,Induced termination of pregnancy complicated by amniotic fluid embolism
pregnancy,609459003,SNOMED,Induced termination of pregnancy complicated by blood-clot embolism
pregnancy,609460008,SNOMED,Induced termination of pregnancy complicated by cardiac arrest and/or failure
pregnancy,609461007,SNOMED,Induced termination of pregnancy complicated by cerebral anoxia
pregnancy,609462000,SNOMED,Induced termination of pregnancy complicated by defibrination syndrome
pregnancy,609463005,SNOMED,Induced termination of pregnancy complicated by electrolyte imbalance
pregnancy,609464004,SNOMED,Induced termination of pregnancy complicated by endometritis
pregnancy,609465003,SNOMED,Induced termination of pregnancy complicated by fat embolism
pregnancy,609466002,SNOMED,Induced termination of pregnancy complicated by intravascular hemolysis
pregnancy,609467006,SNOMED,Induced termination of pregnancy complicated by laceration of bowel
pregnancy,609468001,SNOMED,Induced termination of pregnancy complicated by laceration of broad ligament
pregnancy,609469009,SNOMED,Induced termination of pregnancy complicated by laceration of cervix
pregnancy,609470005,SNOMED,Induced termination of pregnancy complicated by laceration of uterus
pregnancy,609471009,SNOMED,Induced termination of pregnancy complicated by laceration of vagina
pregnancy,609472002,SNOMED,Induced termination of pregnancy complicated by acute renal failure with oliguria
pregnancy,609473007,SNOMED,Induced termination of pregnancy complicated by parametritis
pregnancy,609474001,SNOMED,Induced termination of pregnancy complicated by pelvic peritonitis
pregnancy,609475000,SNOMED,Induced termination of pregnancy complicated by perforation of bowel
pregnancy,609476004,SNOMED,Induced termination of pregnancy complicated by perforation of cervix
pregnancy,609477008,SNOMED,Induced termination of pregnancy complicated by perforation of uterus
pregnancy,609478003,SNOMED,Induced termination of pregnancy complicated by perforation of vagina
pregnancy,609479006,SNOMED,Induced termination of pregnancy complicated by postoperative shock
pregnancy,609480009,SNOMED,Induced termination of pregnancy complicated by pulmonary embolism
pregnancy,609482001,SNOMED,Induced termination of pregnancy complicated by renal tubular necrosis
pregnancy,609483006,SNOMED,Induced termination of pregnancy complicated by salpingitis
pregnancy,609484000,SNOMED,Induced termination of pregnancy complicated by salpingo-oophoritis
pregnancy,609485004,SNOMED,Induced termination of pregnancy complicated by sepsis
pregnancy,609486003,SNOMED,Induced termination of pregnancy complicated by septic embolism
pregnancy,609487007,SNOMED,Induced termination of pregnancy complicated by septic shock
pregnancy,609489005,SNOMED,Induced termination of pregnancy complicated by soap embolism
pregnancy,609490001,SNOMED,Induced termination of pregnancy complicated by uremia
pregnancy,609491002,SNOMED,Induced termination of pregnancy complicated by urinary tract infection
pregnancy,609492009,SNOMED,Induced termination of pregnancy without complication
pregnancy,609493004,SNOMED,Induced termination of pregnancy complicated by tetanus
pregnancy,609494005,SNOMED,Induced termination of pregnancy complicated by pelvic disorder
pregnancy,609496007,SNOMED,Complication occurring during pregnancy
pregnancy,609497003,SNOMED,Venous complication in the puerperium
pregnancy,609498008,SNOMED,Induced termination of pregnancy complicated by laceration of bladder
pregnancy,609499000,SNOMED,Induced termination of pregnancy complicated by laceration of periurethral tissue
pregnancy,609500009,SNOMED,Induced termination of pregnancy complicated by perforation of bladder
pregnancy,609501008,SNOMED,Induced termination of pregnancy complicated by perforation of broad ligament
pregnancy,609502001,SNOMED,Induced termination of pregnancy complicated by perforation of periurethral tissue
pregnancy,609503006,SNOMED,Induced termination of pregnancy complicated by bladder damage
pregnancy,609504000,SNOMED,Induced termination of pregnancy complicated by bowel damage
pregnancy,609505004,SNOMED,Induced termination of pregnancy complicated by broad ligament damage
pregnancy,609506003,SNOMED,Induced termination of pregnancy complicated by cardiac arrest
pregnancy,609507007,SNOMED,Induced termination of pregnancy complicated by cardiac failure
pregnancy,609508002,SNOMED,Induced termination of pregnancy complicated by cervix damage
pregnancy,609510000,SNOMED,Induced termination of pregnancy complicated by infectious disease
pregnancy,609511001,SNOMED,Induced termination of pregnancy complicated by periurethral tissue damage
pregnancy,609512008,SNOMED,Induced termination of pregnancy complicated by oliguria
pregnancy,609513003,SNOMED,Induced termination of pregnancy complicated by uterus damage
pregnancy,609514009,SNOMED,Induced termination of pregnancy complicated by vaginal damage
pregnancy,609515005,SNOMED,Epithelioid trophoblastic tumor
pregnancy,609516006,SNOMED,Gestational trophoblastic lesion
pregnancy,609519004,SNOMED,Gestational trophoblastic neoplasia
pregnancy,609525000,SNOMED,Miscarriage of tubal ectopic pregnancy
pregnancy,61007003,SNOMED,Separation of symphysis pubis during delivery
pregnancy,6134000,SNOMED,Illegal abortion with oliguria
pregnancy,61353001,SNOMED,Repair of obstetric laceration of bladder
pregnancy,61452007,SNOMED,Failed attempted abortion with laceration of uterus
pregnancy,61568004,SNOMED,Miscarriage with salpingo-oophoritis
pregnancy,61586001,SNOMED,Delivery by vacuum extraction
pregnancy,61714007,SNOMED,Metabolic disturbance in labor AND/OR delivery
pregnancy,61752008,SNOMED,Illegal abortion complicated by shock
pregnancy,61810006,SNOMED,Illegal abortion with defibrination syndrome
pregnancy,61881000,SNOMED,Osiander's sign
pregnancy,61893009,SNOMED,Laparoscopic treatment of ectopic pregnancy with oophorectomy
pregnancy,61951009,SNOMED,Failed attempted abortion with laceration of periurethral tissue
pregnancy,62129004,SNOMED,Contraction ring dystocia
pregnancy,62131008,SNOMED,Couvelaire uterus
pregnancy,62333002,SNOMED,Gestation period  13 weeks
pregnancy,6234006,SNOMED,Second degree perineal laceration
pregnancy,62377009,SNOMED,Postpartum cardiomyopathy
pregnancy,62410004,SNOMED,Postpartum fibrinolysis with hemorrhage
pregnancy,62472004,SNOMED,Cervix fully dilated
pregnancy,62508004,SNOMED,Mid forceps delivery
pregnancy,6251000119101,SNOMED,Induced termination of pregnancy in first trimester
pregnancy,62531004,SNOMED,Placenta previa marginalis
pregnancy,62583006,SNOMED,Puerperal phlegmasia alba dolens
pregnancy,6261000119104,SNOMED,Induced termination of pregnancy in second trimester
pregnancy,62612003,SNOMED,Fibrosis of perineum affecting pregnancy
pregnancy,62657007,SNOMED,Cardiac arrest AND/OR failure following anesthesia AND/OR sedation in labor AND/OR delivery
pregnancy,62774004,SNOMED,Cervical dilatation  9cm
pregnancy,62888008,SNOMED,Legal abortion with perforation of bladder
pregnancy,63110000,SNOMED,Gestation period  7 weeks
pregnancy,63407004,SNOMED,Episioproctotomy
pregnancy,63503002,SNOMED,Gestation period  41 weeks
pregnancy,63596003,SNOMED,Laparoscopic treatment of ectopic pregnancy with salpingectomy
pregnancy,63637002,SNOMED,Failed attempted abortion with laceration of broad ligament
pregnancy,63662002,SNOMED,Purulent mastitis associated with childbirth
pregnancy,63750008,SNOMED,Oblique lie
pregnancy,6383007,SNOMED,Premature labor
pregnancy,64171002,SNOMED,Obstetrical cardiac complication of anesthesia AND/OR sedation
pregnancy,64181003,SNOMED,Failed attempted abortion with perforation of vagina
pregnancy,64229006,SNOMED,Traumatic lesion during delivery
pregnancy,64254006,SNOMED,Triplet pregnancy
pregnancy,6473009,SNOMED,Suture of old obstetrical laceration of uterus
pregnancy,64814003,SNOMED,Miscarriage with electrolyte imbalance
pregnancy,64920003,SNOMED,Gestation period  31 weeks
pregnancy,64954002,SNOMED,Avulsion of inner symphyseal cartilage during delivery
pregnancy,65035007,SNOMED,Gestation period  22 weeks
pregnancy,65147003,SNOMED,Twin pregnancy
pregnancy,65243006,SNOMED,Delivery by midwife
pregnancy,65377004,SNOMED,Polygalactia
pregnancy,65409004,SNOMED,Obstetrical complication of anesthesia
pregnancy,65539006,SNOMED,Impetigo herpetiformis
pregnancy,65683006,SNOMED,Gestation period  17 weeks
pregnancy,65727000,SNOMED,Intrauterine pregnancy
pregnancy,6594005,SNOMED,Cerebrovascular disorder in the puerperium
pregnancy,66119008,SNOMED,Disruption of perineal laceration repair in the puerperium
pregnancy,66131005,SNOMED,Miscarriage with fat embolism
pregnancy,66231000,SNOMED,Fetal OR intrauterine asphyxia  not clear if noted before OR after onset of labor in liveborn infant
pregnancy,66294006,SNOMED,Obstruction by abnormal pelvic soft tissues
pregnancy,6647006,SNOMED,Legal abortion with defibrination syndrome
pregnancy,6678005,SNOMED,Gestation period  15 weeks
pregnancy,66892003,SNOMED,Failed attempted abortion with urinary tract infection
pregnancy,66895001,SNOMED,Cervical dilatation  6cm
pregnancy,66958002,SNOMED,Isoimmunization from non-ABO  non-Rh blood-group incompatibility affecting pregnancy
pregnancy,67042008,SNOMED,Failed attempted abortion complicated by shock
pregnancy,67229002,SNOMED,Spontaneous uterine inversion
pregnancy,67465009,SNOMED,Miscarriage with sepsis
pregnancy,67486009,SNOMED,Postpartum pelvic thrombophlebitis
pregnancy,67802002,SNOMED,Malpresentation other than breech  successfully converted to cephalic presentation
pregnancy,6802007,SNOMED,Illegal abortion with perforation of broad ligament
pregnancy,68189005,SNOMED,Illegal abortion with perforation of periurethral tissue
pregnancy,68214002,SNOMED,Lymphangitis of breast associated with childbirth
pregnancy,6825008,SNOMED,Perineal laceration involving rectovaginal septum
pregnancy,68509000,SNOMED,Stillbirth of immature female (500-999 gms.)
pregnancy,68635007,SNOMED,Deficiency of placental function
pregnancy,6891008,SNOMED,Uterine incoordination  second degree
pregnancy,6893006,SNOMED,First stage of labor
pregnancy,69162008,SNOMED,Cleidotomy
pregnancy,69217004,SNOMED,Outlet contraction of pelvis
pregnancy,69270005,SNOMED,Rupture of uterus during AND/OR after labor
pregnancy,69302000,SNOMED,Abortion on demand
pregnancy,69338007,SNOMED,Kanter's sign
pregnancy,69422002,SNOMED,Trial forceps delivery
pregnancy,69777007,SNOMED,Interlocked twins
pregnancy,69802008,SNOMED,Cervical cerclage suture present
pregnancy,698414000,SNOMED,Fetal or neonatal effect of complication of labor
pregnancy,698415004,SNOMED,Fetal or neonatal effect of complication of delivery
pregnancy,698497008,SNOMED,Fetal or neonatal effect of disproportion during labor
pregnancy,698498003,SNOMED,Fetal or neonatal effect of disproportion during delivery
pregnancy,698554000,SNOMED,Fetal or neonatal effect of malposition during labor
pregnancy,698555004,SNOMED,Fetal or neonatal effect of malposition during delivery
pregnancy,698586005,SNOMED,Complete legal abortion complicated by excessive hemorrhage
pregnancy,698587001,SNOMED,Complete illegal abortion complicated by excessive hemorrhage
pregnancy,698632006,SNOMED,Pregnancy induced edema
pregnancy,698636009,SNOMED,Complete legal abortion with complication
pregnancy,698702007,SNOMED,Deep transverse arrest with persistent occipitoposterior position
pregnancy,698708006,SNOMED,Antepartum hemorrhage due to placenta previa type I
pregnancy,698709003,SNOMED,Antepartum hemorrhage due to placenta previa type II
pregnancy,698710008,SNOMED,Antepartum hemorrhage due to placenta previa type III
pregnancy,698711007,SNOMED,Antepartum hemorrhage due to placenta previa type IV
pregnancy,698712000,SNOMED,Antepartum hemorrhage due to cervical polyp
pregnancy,698713005,SNOMED,Antepartum haemorrhage due to cervical erosion
pregnancy,698716002,SNOMED,Preterm spontaneous labor with preterm delivery
pregnancy,698717006,SNOMED,Preterm spontaneous labor with term delivery
pregnancy,698791008,SNOMED,Fetal or neonatal effect of malpresentation during labor
pregnancy,698795004,SNOMED,Fetal or neonatal effect of malpresentation during delivery
pregnancy,699240001,SNOMED,Combined intrauterine and ovarian pregnancy
pregnancy,699949009,SNOMED,Retained placenta due to morbidly adherent placenta
pregnancy,699950009,SNOMED,Anti-D isoimmunization affecting pregnancy
pregnancy,699999008,SNOMED,Obstetrical version with extraction
pregnancy,700000006,SNOMED,Vaginal delivery of fetus
pregnancy,700038005,SNOMED,Mastitis associated with lactation
pregnancy,700041001,SNOMED,Induced termination of pregnancy under unsafe conditions
pregnancy,700442004,SNOMED,Ultrasonography of fetal ductus venosus
pregnancy,70068004,SNOMED,Persistent occipitoposterior position
pregnancy,70112005,SNOMED,Stillbirth of mature female (2500 gms. or more)
pregnancy,70137000,SNOMED,Deficiency of placental endocrine function
pregnancy,702452007,SNOMED,Quadruplet birth
pregnancy,702453002,SNOMED,Quintuplet birth
pregnancy,702454008,SNOMED,Sextuplet birth
pregnancy,702736005,SNOMED,Supervision of high risk pregnancy with history of previous cesarean section
pregnancy,702737001,SNOMED,Supervision of high risk pregnancy with history of gestational diabetes mellitus
pregnancy,702738006,SNOMED,Supervision of high risk pregnancy
pregnancy,702739003,SNOMED,Supervision of high risk pregnancy with history of previous molar pregnancy
pregnancy,702740001,SNOMED,Supervision of high risk pregnancy with history of previous precipitate labor
pregnancy,702741002,SNOMED,Supervision of high risk pregnancy for multigravida
pregnancy,702742009,SNOMED,Supervision of high risk pregnancy for social problem
pregnancy,702743004,SNOMED,Supervision of high risk pregnancy for multigravida age 15 years or younger
pregnancy,702744005,SNOMED,Supervision of high risk pregnancy for primigravida age 15 years or younger
pregnancy,702985005,SNOMED,Ultrasonography of fetal shunt
pregnancy,70425008,SNOMED,Piskacek's sign
pregnancy,70537007,SNOMED,Hegar's sign
pregnancy,70651004,SNOMED,Calkin's sign
pregnancy,707089008,SNOMED,Genital tract infection in puerperium
pregnancy,707207004,SNOMED,Incomplete induced termination of pregnancy
pregnancy,707254000,SNOMED,Amniotic adhesion
pregnancy,70823006,SNOMED,Illegal abortion with intravascular hemolysis
pregnancy,709004006,SNOMED,Emergency lower segment cesarean section with inverted T incision
pregnancy,70964000,SNOMED,Postparturient hemoglobinuria
pregnancy,710165007,SNOMED,Ultrasonography of fetal head
pregnancy,71028008,SNOMED,Fetal-maternal hemorrhage
pregnancy,710911000000102,SNOMED,Infant feeding antenatal checklist completed
pregnancy,71096001,SNOMED,Inversion of uterine contraction
pregnancy,71166009,SNOMED,Forceps delivery with rotation of fetal head
pregnancy,71216006,SNOMED,Legal abortion with laceration of periurethral tissue
pregnancy,712653003,SNOMED,Delivery by cesarean section for footling breech presentation
pregnancy,712654009,SNOMED,Delivery by cesarean section for breech presentation
pregnancy,712655005,SNOMED,Delivery by cesarean section for flexed breech presentation
pregnancy,713187004,SNOMED,Polyhydramnios due to maternal disease
pregnancy,713191009,SNOMED,Polyhydramnios due to placental anomaly
pregnancy,713192002,SNOMED,Oligohydramnios due to rupture of membranes
pregnancy,713202001,SNOMED,Antepartum stillbirth
pregnancy,713232009,SNOMED,Prolonged second stage of labor due to poor maternal effort
pregnancy,713233004,SNOMED,Supervision of high risk pregnancy with history of previous neonatal death
pregnancy,713234005,SNOMED,Supervision of high risk pregnancy with history of previous intrauterine death
pregnancy,713235006,SNOMED,Supervision of high risk pregnancy with history of previous antepartum hemorrhage
pregnancy,713237003,SNOMED,Supervision of high risk pregnancy with history of previous big baby
pregnancy,713238008,SNOMED,Supervision of high risk pregnancy with history of previous abnormal baby
pregnancy,713239000,SNOMED,Supervision of high risk pregnancy with history of previous primary postpartum hemorrhage
pregnancy,713240003,SNOMED,Supervision of high risk pregnancy with poor obstetric history
pregnancy,713241004,SNOMED,Supervision of high risk pregnancy with history of previous fetal distress
pregnancy,713242006,SNOMED,Supervision of high risk pregnancy with poor reproductive history
pregnancy,713249002,SNOMED,Pyogenic granuloma of gingiva co-occurrent and due to pregnancy
pregnancy,713386003,SNOMED,Supervision of high risk pregnancy for maternal short stature
pregnancy,713387007,SNOMED,Supervision of high risk pregnancy with family history of diabetes mellitus
pregnancy,71355009,SNOMED,Gestation period  30 weeks
pregnancy,713575004,SNOMED,Dizygotic twin pregnancy
pregnancy,713576003,SNOMED,Monozygotic twin pregnancy
pregnancy,71362000,SNOMED,Illegal abortion with septic embolism
pregnancy,714812005,SNOMED,Induced termination of pregnancy
pregnancy,715880002,SNOMED,Obstructed labor due to fetal abnormality
pregnancy,71612002,SNOMED,Postpartum uterine hypertrophy
pregnancy,716379000,SNOMED,Acute fatty liver of pregnancy
pregnancy,71639005,SNOMED,Galactorrhea associated with childbirth
pregnancy,7166002,SNOMED,Legal abortion with laceration of cervix
pregnancy,717794008,SNOMED,Supervision of pregnancy with history of infertility
pregnancy,717795009,SNOMED,Supervision of pregnancy with history of insufficient antenatal care
pregnancy,717797001,SNOMED,Antenatal care of elderly primigravida
pregnancy,717809003,SNOMED,Immediate postpartum care
pregnancy,717810008,SNOMED,Routine postpartum follow-up
pregnancy,717816002,SNOMED,Infection of nipple associated with childbirth with attachment difficulty
pregnancy,717817006,SNOMED,Abscess of breast associated with childbirth with attachment difficulty
pregnancy,717818001,SNOMED,Nonpurulent mastitis associated with childbirth with attachment difficulty
pregnancy,717819009,SNOMED,Retracted nipple associated with childbirth with attachment difficulty
pregnancy,717820003,SNOMED,Cracked nipple associated with childbirth with attachment difficulty
pregnancy,717959008,SNOMED,Cardiac complication of anesthesia during the puerperium
pregnancy,717960003,SNOMED,Central nervous system complication of anesthesia during the puerperium
pregnancy,718475004,SNOMED,Ultrasonography for amniotic fluid index
pregnancy,71848002,SNOMED,Bolt's sign
pregnancy,71901000,SNOMED,Congenital contracted pelvis
pregnancy,72014004,SNOMED,Abnormal fetal duplication
pregnancy,72059007,SNOMED,Destructive procedure on fetus to facilitate delivery
pregnancy,721022000,SNOMED,Complication of anesthesia during the puerperium
pregnancy,721177006,SNOMED,Injury complicating pregnancy
pregnancy,72161000119100,SNOMED,Antiphospholipid syndrome in pregnancy
pregnancy,722570003,SNOMED,Fetal or neonatal effect of meconium passage during delivery
pregnancy,723541004,SNOMED,Disease of respiratory system complicating pregnancy
pregnancy,723665008,SNOMED,Vaginal bleeding complicating early pregnancy
pregnancy,72417002,SNOMED,Tumultuous uterine contraction
pregnancy,724483001,SNOMED,Concern about body image related to pregnancy
pregnancy,724484007,SNOMED,Incomplete legal abortion without complication
pregnancy,724486009,SNOMED,Venous disorder co-occurrent with pregnancy
pregnancy,724488005,SNOMED,Preterm delivery following induction of labor
pregnancy,724489002,SNOMED,Preterm delivery following Cesarean section
pregnancy,724490006,SNOMED,Intrapartum hemorrhage co-occurrent and due to obstructed labor with uterine rupture
pregnancy,724496000,SNOMED,Postpartum hemorrhage co-occurrent and due to uterine rupture following obstructed labor
pregnancy,7245003,SNOMED,Fetal dystocia
pregnancy,72492007,SNOMED,Footling breech delivery
pregnancy,72543004,SNOMED,Stillbirth of immature fetus  sex undetermined (500-999 gms.)
pregnancy,72544005,SNOMED,Gestation period  25 weeks
pregnancy,72613009,SNOMED,Miscarriage with oliguria
pregnancy,7266006,SNOMED,Total placenta previa with intrapartum hemorrhage
pregnancy,72846000,SNOMED,Gestation period  14 weeks
pregnancy,72860003,SNOMED,Disorder of amniotic cavity AND/OR membrane
pregnancy,72892002,SNOMED,Normal pregnancy
pregnancy,73161006,SNOMED,Transverse lie
pregnancy,73280003,SNOMED,Illegal abortion with laceration of vagina
pregnancy,733142005,SNOMED,Sepsis following obstructed labor
pregnancy,73341009,SNOMED,Removal of ectopic fetus from ovary without oophorectomy
pregnancy,733839001,SNOMED,Postpartum acute renal failure
pregnancy,734275002,SNOMED,Delivery by outlet vacuum extraction
pregnancy,734276001,SNOMED,Delivery by mid-vacuum extraction
pregnancy,735492001,SNOMED,Obstructed labor due to shoulder dystocia
pregnancy,736018001,SNOMED,Elective upper segment cesarean section with bilateral tubal ligation
pregnancy,736020003,SNOMED,Emergency upper segment cesarean section with bilateral tubal ligation
pregnancy,736026009,SNOMED,Elective lower segment cesarean section with bilateral tubal ligation
pregnancy,736118004,SNOMED,Emergency lower segment cesarean section with bilateral tubal ligation
pregnancy,737318003,SNOMED,Delayed hemorrhage due to and following miscarriage
pregnancy,737321001,SNOMED,Excessive hemorrhage due to and following molar pregnancy
pregnancy,737331008,SNOMED,Disorder of vein following miscarriage
pregnancy,73837001,SNOMED,Failed attempted abortion with cardiac arrest AND/OR failure
pregnancy,73972002,SNOMED,Postpartum neurosis
pregnancy,740597009,SNOMED,Umbilical cord complication during labor and delivery
pregnancy,74369005,SNOMED,Miscarriage with perforation of cervix
pregnancy,74437002,SNOMED,Ahlfeld's sign
pregnancy,74522004,SNOMED,Cervical dilatation  7cm
pregnancy,74952004,SNOMED,Gestation period  3 weeks
pregnancy,74955002,SNOMED,Retracted nipple associated with childbirth
pregnancy,74978008,SNOMED,Illegal abortion with complication
pregnancy,749781000000109,SNOMED,Incomplete termination of pregnancy
pregnancy,75013000,SNOMED,Legal abortion with perforation of vagina
pregnancy,75022004,SNOMED,Gestational diabetes mellitus  class A>1<
pregnancy,7504005,SNOMED,Trauma to vulva during delivery
pregnancy,75094005,SNOMED,Hydrops of placenta
pregnancy,75697004,SNOMED,Term birth of identical twins  both stillborn
pregnancy,75825001,SNOMED,Legal abortion with fat embolism
pregnancy,75928003,SNOMED,Pinard maneuver
pregnancy,75933004,SNOMED,Threatened abortion in second trimester
pregnancy,75947000,SNOMED,Legal abortion with endometritis
pregnancy,76012002,SNOMED,Fetal or neonatal effect of complication of labor and/or delivery
pregnancy,76037007,SNOMED,Rigid cervix uteri affecting pregnancy
pregnancy,762612009,SNOMED,Quadruplets with all four stillborn
pregnancy,762613004,SNOMED,Quintuplets with all five stillborn
pregnancy,762614005,SNOMED,Sextuplets with all six stillborn
pregnancy,76472002,SNOMED,Legal abortion with perforation of periurethral tissue
pregnancy,76771005,SNOMED,Parturient hemorrhage associated with hypofibrinogenemia
pregnancy,76871004,SNOMED,Previous surgery to vagina affecting pregnancy
pregnancy,76889003,SNOMED,Failed attempted abortion with cerebral anoxia
pregnancy,7707000,SNOMED,Gestation period  32 weeks
pregnancy,77099001,SNOMED,Illegal abortion with perforation of uterus
pregnancy,77186001,SNOMED,Failed attempted abortion with renal tubular necrosis
pregnancy,77206006,SNOMED,Puerperal pelvic sepsis
pregnancy,77259008,SNOMED,Prolonged second stage of labor
pregnancy,77285007,SNOMED,Placental infarct affecting management of mother
pregnancy,77376005,SNOMED,Gestational edema without hypertension
pregnancy,77386006,SNOMED,Pregnant
pregnancy,77563000,SNOMED,Obstruction by bony pelvis
pregnancy,7768008,SNOMED,Failure of cervical dilation
pregnancy,77814006,SNOMED,Stillbirth of premature male (1000-2499 gms.)
pregnancy,77854008,SNOMED,Failed medical induction of labor
pregnancy,77913004,SNOMED,Submammary mastitis associated with childbirth
pregnancy,7792000,SNOMED,Placenta previa without hemorrhage
pregnancy,7802000,SNOMED,Illegal abortion without complication
pregnancy,7809009,SNOMED,Miscarriage with laceration of uterus
pregnancy,7822001,SNOMED,Failed attempted abortion complicated by damage to pelvic organs AND/OR tissues
pregnancy,78395001,SNOMED,Gestation period  33 weeks
pregnancy,785341006,SNOMED,Intrapartum hemorrhage due to leiomyoma
pregnancy,785867009,SNOMED,Excessive hemorrhage due to and following induced termination of pregnancy
pregnancy,785868004,SNOMED,Secondary hemorrhage due to and following induced termination of pregnancy
pregnancy,785869007,SNOMED,Secondary hemorrhage due to and following illegally induced termination of pregnancy
pregnancy,785870008,SNOMED,Secondary hemorrhage due to and following legally induced termination of pregnancy
pregnancy,785871007,SNOMED,Excessive hemorrhage due to and following legally induced termination of pregnancy
pregnancy,785872000,SNOMED,Excessive hemorrhage due to and following illegally induced termination of pregnancy
pregnancy,7860005,SNOMED,Stillbirth of mature male (2500 gms. or more)
pregnancy,786067000,SNOMED,Intramural ectopic pregnancy of myometrium
pregnancy,78697003,SNOMED,Nonpurulent mastitis associated with childbirth
pregnancy,78808002,SNOMED,Essential hypertension complicating AND/OR reason for care during pregnancy
pregnancy,788180009,SNOMED,Lower uterine segment cesarean section
pregnancy,788290007,SNOMED,Hemangioma of skin in pregnancy
pregnancy,788728009,SNOMED,Hematoma of surgical wound following cesarean section
pregnancy,7888004,SNOMED,Term birth of newborn quadruplets
pregnancy,7910003,SNOMED,Miscarriage with salpingitis
pregnancy,79179003,SNOMED,Desultory labor
pregnancy,79255005,SNOMED,Mentum presentation of fetus
pregnancy,79290002,SNOMED,Cervical pregnancy
pregnancy,79414005,SNOMED,Retraction ring dystocia
pregnancy,79586000,SNOMED,Tubal pregnancy
pregnancy,796731000000105,SNOMED,Extra-amniotic injection of abortifacient
pregnancy,796741000000101,SNOMED,Intra-amniotic injection of abortifacient
pregnancy,79748007,SNOMED,Maternal dystocia
pregnancy,79839005,SNOMED,Perineal laceration involving vulva
pregnancy,79992004,SNOMED,Gestation period  12 weeks
pregnancy,80002007,SNOMED,Malpresentation of fetus
pregnancy,80113008,SNOMED,Complication of the puerperium
pregnancy,80224003,SNOMED,Multiple gestation with one OR more fetal malpresentations
pregnancy,80228000,SNOMED,Lightening of fetus
pregnancy,80256005,SNOMED,Intervillous thrombosis
pregnancy,80438008,SNOMED,Illegal abortion with perforation of bladder
pregnancy,80487005,SNOMED,Gestation period  39 weeks
pregnancy,8071005,SNOMED,Miscarriage with perforation of bowel
pregnancy,80722003,SNOMED,Surgical correction of inverted pregnant uterus
pregnancy,80818002,SNOMED,Previous surgery to perineum AND/OR vulva affecting pregnancy
pregnancy,80997009,SNOMED,Quintuplet pregnancy
pregnancy,81130000,SNOMED,Removal of intraligamentous ectopic pregnancy
pregnancy,81328008,SNOMED,Fundal dominance of uterine contraction
pregnancy,813541000000100,SNOMED,Pregnancy resulting from assisted conception
pregnancy,81448000,SNOMED,Hemorrhage in early pregnancy  delivered
pregnancy,81521003,SNOMED,Failed attempted abortion with salpingitis
pregnancy,816148008,SNOMED,Disproportion between fetus and pelvis due to conjoined twins
pregnancy,81677009,SNOMED,Lactation tetany
pregnancy,816966004,SNOMED,Augmentation of labor using oxytocin
pregnancy,816967008,SNOMED,Fetal distress due to augmentation of labor with oxytocin
pregnancy,816969006,SNOMED,Inefficient uterine activity with oxytocin augmentation
pregnancy,820947007,SNOMED,Third stage hemorrhage due to retention of placenta
pregnancy,82118009,SNOMED,Gestation period  2 weeks
pregnancy,82153002,SNOMED,Miscarriage with pulmonary embolism
pregnancy,82204006,SNOMED,Illegal abortion with parametritis
pregnancy,82338001,SNOMED,Illegal abortion with septic shock
pregnancy,82661006,SNOMED,Abdominal pregnancy
pregnancy,82688001,SNOMED,Removal of ectopic fetus
pregnancy,82897000,SNOMED,Spontaneous placental expulsion  Duncan mechanism
pregnancy,83074005,SNOMED,Unplanned pregnancy
pregnancy,83094001,SNOMED,Perineal hematoma during delivery
pregnancy,83121003,SNOMED,Term birth of fraternal twins  both stillborn
pregnancy,83243004,SNOMED,Rigid pelvic floor affecting pregnancy
pregnancy,8333008,SNOMED,Term birth of newborn triplets
pregnancy,83916000,SNOMED,Postpartum thrombophlebitis
pregnancy,83922009,SNOMED,Miscarriage with parametritis
pregnancy,8393005,SNOMED,Tetanic contractions of uterus
pregnancy,84007008,SNOMED,Shock during AND/OR following labor AND/OR delivery
pregnancy,84032005,SNOMED,Halban's sign
pregnancy,840448004,SNOMED,Cystic hygroma in fetus co-occurrent with hydrops
pregnancy,840625002,SNOMED,Gravid uterus at 12-16 weeks size
pregnancy,840626001,SNOMED,Gravid uterus at 16-20 weeks size
pregnancy,840627005,SNOMED,Gravid uterus at 20-24 weeks size
pregnancy,840628000,SNOMED,Gravid uterus at 24-28 weeks size
pregnancy,840629008,SNOMED,Gravid uterus at 28-32 weeks size
pregnancy,840630003,SNOMED,Gravid uterus at 32-34 weeks size
pregnancy,840631004,SNOMED,Gravid uterus at 34-36 weeks size
pregnancy,840632006,SNOMED,Gravid uterus at 36-38 weeks size
pregnancy,840633001,SNOMED,Gravid uterus at term size
pregnancy,84132007,SNOMED,Gestation period  35 weeks
pregnancy,84143004,SNOMED,Illegal abortion with perforation of cervix
pregnancy,84195007,SNOMED,Classical cesarean section
pregnancy,84235001,SNOMED,Cephalic version
pregnancy,84275009,SNOMED,Obstetrical hysterotomy
pregnancy,84382006,SNOMED,Premature birth of fraternal twins  both stillborn
pregnancy,8445003,SNOMED,Tumor of vulva affecting pregnancy
pregnancy,84457005,SNOMED,Spontaneous onset of labor
pregnancy,8468007,SNOMED,Legal abortion with uremia
pregnancy,84693004,SNOMED,Intervillous hemorrhage of placenta
pregnancy,85039006,SNOMED,Postpartum amenorrhea-galactorrhea syndrome
pregnancy,85116003,SNOMED,Miscarriage in second trimester
pregnancy,85331004,SNOMED,Miscarriage with laceration of broad ligament
pregnancy,85403009,SNOMED,Delivery  medical personnel present
pregnancy,854611000000109,SNOMED,Medically induced evacuation of retained products of conception using prostaglandin
pregnancy,85467007,SNOMED,Miscarriage with perforation of bladder
pregnancy,855021000000107,SNOMED,Ultrasonography of multiple pregnancy
pregnancy,855031000000109,SNOMED,Doppler ultrasonography of multiple pregnancy
pregnancy,85542007,SNOMED,Perineal laceration involving skin
pregnancy,85548006,SNOMED,Episiotomy
pregnancy,85632001,SNOMED,Miscarriage with laceration of vagina
pregnancy,85652000,SNOMED,Legal abortion with perforation of cervix
pregnancy,858901000000108,SNOMED,Pregnancy of unknown location
pregnancy,85991008,SNOMED,Miscarriage with pelvic peritonitis
pregnancy,860602007,SNOMED,Postpartum excision of uterus
pregnancy,86081009,SNOMED,Herpes gestationis
pregnancy,861281000000109,SNOMED,Antenatal 22 week examination
pregnancy,861301000000105,SNOMED,Antenatal 25 week examination
pregnancy,861321000000101,SNOMED,Antenatal 31 week examination
pregnancy,86196005,SNOMED,Disorder of breast associated with childbirth
pregnancy,86203003,SNOMED,Polyhydramnios
pregnancy,86356004,SNOMED,Unstable lie
pregnancy,863897005,SNOMED,Failed attempted termination of pregnancy complicated by acute necrosis of liver
pregnancy,86599005,SNOMED,Echography  scan B-mode for placental localization
pregnancy,866229004,SNOMED,Trauma to urethra and bladder during delivery
pregnancy,866481000000104,SNOMED,Ultrasonography to determine estimated date of confinement
pregnancy,8670007,SNOMED,Illegal abortion with salpingo-oophoritis
pregnancy,86801005,SNOMED,Gestation period  6 weeks
pregnancy,86803008,SNOMED,Term birth of newborn quintuplets
pregnancy,86883006,SNOMED,Gestation period  23 weeks
pregnancy,87038002,SNOMED,Postpartum alopecia
pregnancy,871005,SNOMED,Contracted pelvis
pregnancy,87178007,SNOMED,Gestation period  1 week
pregnancy,87383005,SNOMED,Maternal distress
pregnancy,87527008,SNOMED,Term pregnancy
pregnancy,87605005,SNOMED,Cornual pregnancy
pregnancy,87621000,SNOMED,Hyperemesis gravidarum before end of 22 week gestation with dehydration
pregnancy,87662006,SNOMED,Term birth of fraternal twins  both living
pregnancy,87814002,SNOMED,Marginal placenta previa with intrapartum hemorrhage
pregnancy,87840008,SNOMED,Galactocele associated with childbirth
pregnancy,87967003,SNOMED,Miscarriage with perforation of vagina
pregnancy,87968008,SNOMED,Repair of old obstetrical laceration of cervix
pregnancy,88144003,SNOMED,Removal of ectopic interstitial uterine pregnancy requiring total hysterectomy
pregnancy,88178009,SNOMED,Puerperal peritonitis
pregnancy,88201000119101,SNOMED,Failure of cervical dilation due to primary uterine inertia
pregnancy,88362001,SNOMED,Removal of ectopic fetus from fallopian tube without salpingectomy
pregnancy,88697005,SNOMED,Papular dermatitis of pregnancy
pregnancy,8884000,SNOMED,Fetal ascites causing disproportion
pregnancy,88887003,SNOMED,Maternal hypotension syndrome
pregnancy,88895004,SNOMED,Fatigue during pregnancy
pregnancy,89053004,SNOMED,Vaginal cesarean section
pregnancy,89346004,SNOMED,Delivery by Kielland rotation
pregnancy,893721000000103,SNOMED,Defibulation of vulva to facilitate delivery
pregnancy,89672000,SNOMED,Retromammary mastitis associated with childbirth
pregnancy,89700002,SNOMED,Shoulder girdle dystocia
pregnancy,89849000,SNOMED,High forceps delivery
pregnancy,89934007,SNOMED,Crowning
pregnancy,8996006,SNOMED,Illegal abortion complicated by renal failure
pregnancy,90009001,SNOMED,Abnormal umbilical cord
pregnancy,90188009,SNOMED,Failed mechanical induction
pregnancy,90306000,SNOMED,Trial labor
pregnancy,904002,SNOMED,Pinard's sign
pregnancy,90438006,SNOMED,Delivery by Malstrom's extraction
pregnancy,90450000,SNOMED,Illegal abortion with pelvic peritonitis
pregnancy,90532005,SNOMED,Central laceration during delivery
pregnancy,90645002,SNOMED,Failed attempted abortion without complication
pregnancy,90797000,SNOMED,Gestation period  28 weeks
pregnancy,90968009,SNOMED,Prolonged pregnancy
pregnancy,91162000,SNOMED,Necrosis of liver of pregnancy
pregnancy,9121000119106,SNOMED,Low back pain in pregnancy
pregnancy,91271004,SNOMED,Superfetation
pregnancy,91484005,SNOMED,Failure of induction of labor by oxytocic drugs
pregnancy,91957002,SNOMED,Back pain complicating pregnancy
pregnancy,921611000000101,SNOMED,Intrapartum stillbirth
pregnancy,9221009,SNOMED,Surgical treatment of septic abortion
pregnancy,92297008,SNOMED,Benign neoplasm of placenta
pregnancy,925561000000100,SNOMED,Gestation less than 28 weeks
pregnancy,92684002,SNOMED,Carcinoma in situ of placenta
pregnancy,9279009,SNOMED,Extra-amniotic pregnancy
pregnancy,9293002,SNOMED,Atony of uterus
pregnancy,9297001,SNOMED,Uterus bicornis affecting pregnancy
pregnancy,931004,SNOMED,Gestation period  9 weeks
pregnancy,9343003,SNOMED,Term birth of newborn female
pregnancy,9442009,SNOMED,Parturient hemorrhage associated with afibrinogenemia
pregnancy,95606005,SNOMED,Maternal drug exposure
pregnancy,95607001,SNOMED,Maternal drug use
pregnancy,95608006,SNOMED,Necrosis of placenta
pregnancy,9686009,SNOMED,Goodell's sign
pregnancy,9720009,SNOMED,Cicatrix of cervix affecting pregnancy
pregnancy,9724000,SNOMED,Repair of current obstetric laceration of uterus
pregnancy,9780006,SNOMED,Presentation of prolapsed arm of fetus
pregnancy,9899009,SNOMED,Ovarian pregnancy
"""
tmp_pregnancy_v2 = spark.createDataFrame(pd.DataFrame(pd.read_csv(io.StringIO(tmp_pregnancy_v2))))


# compare files
file1, file2, file3, file3_differences = compare_files(tmp_pregnancy_v1, tmp_pregnancy_v2, ['name', 'terminology', 'code'], warningError=0); print()


# append and deduplicate (prioritising v2)
tmp_pregnancy_v2 = tmp_pregnancy_v2\
  .withColumn('_v', f.lit(2))
tmp_pregnancy = tmp_pregnancy_v1\
  .withColumn('_v', f.lit(1))\
  .unionByName(tmp_pregnancy_v2)\
  .orderBy('name', 'terminology', 'code', '_v')
_win = Window\
  .partitionBy(['name', 'terminology', 'code'])\
  .orderBy(f.desc('_v'))
_win_rownummax = Window\
  .partitionBy(['name', 'terminology', 'code'])
tmp_pregnancy = tmp_pregnancy\
  .withColumn('_rownum', f.row_number().over(_win))\
  .withColumn('_rownummax', f.count('name').over(_win_rownummax))\
  .where(f.col('_rownum') == 1)\
  .select(['name', 'terminology', 'code', 'term'])

# check
count_var(tmp_pregnancy, 'code')

# save
codelist_pregnancy = tmp_pregnancy

# COMMAND ----------

# MAGIC %md # 4 Combine

# COMMAND ----------

# append (union) codelists defined above
# harmonise columns before appending
clistm = []
for indx, clist in enumerate([clist for clist in globals().keys() if (bool(re.match('^codelist_.*', clist))) & (clist not in ['codelist_match', 'codelist_match_summ'])]):
  print(f'{0 if indx<10 else ""}' + str(indx) + ' ' + clist)
  tmp = globals()[clist]
  if(indx == 0):
    clistm = tmp
  else:
    # pre unionByName
    for col in [col for col in tmp.columns if col not in clistm.columns]:
      # print('  M - adding column: ' + col)
      clistm = clistm.withColumn(col, f.lit(None))
    for col in [col for col in clistm.columns if col not in tmp.columns]:
      # print('  C - adding column: ' + col)
      tmp = tmp.withColumn(col, f.lit(None))
    clistm = clistm.unionByName(tmp)
  
clistm = clistm\
  .orderBy('name', 'terminology', 'code')

# COMMAND ----------

# MAGIC %md # 5 Reformat

# COMMAND ----------

# check
display(clistm.where((f.col('terminology') == 'ICD10') & (f.col('code').rlike('X$'))))

# COMMAND ----------

display(clistm.where((f.col('terminology') == 'ICD10') & (f.col('code').rlike('[\.\-\s]'))))

# COMMAND ----------

# remove trailing X's, decimal points, dashes, and spaces
clistmr = clistm\
  .withColumn('code', f.when(f.col('terminology') == 'ICD10', f.regexp_replace('code', r'X$', '')).otherwise(f.col('code')))\
  .withColumn('code', f.when(f.col('terminology') == 'ICD10', f.regexp_replace('code', r'[\.\-\s]', '')).otherwise(f.col('code')))

# COMMAND ----------

# MAGIC %md # 6 Check

# COMMAND ----------

# check
tmpt = tab(clistmr, 'name', 'terminology', var2_unstyled=1)

# COMMAND ----------

# check
display(clistmr)

# COMMAND ----------

# output ICD10 codelist for comparison purposes with SAIL
tmp = clistmr\
  .where(f.col('terminology') == 'ICD10')
display(tmp)

# COMMAND ----------

# MAGIC %md # 7 Save

# COMMAND ----------

# save name
outName = f'{proj}_out_codelist'.lower()

# save previous version for comparison purposes
_datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
outName_pre = f'{outName}_pre{_datetimenow}'.lower()
print(outName_pre)
spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')

# save
clistmr.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

# COMMAND ----------

# EXCLUDED for the moment due to 5-character ICD10 codes

# need to include snomed codes
# small_gest_age
#  = spark.createDataFrame(
#   [
#     ('small_gest_age','ICD10','P0511','Newborn small for gestational age, less than 500 grams'),
#     ('small_gest_age','ICD10','P0512','Newborn small for gestational age, 500-749 grams'),
#     ('small_gest_age','ICD10','P0513','Newborn small for gestational age, 750-999 grams'),
#     ('small_gest_age','ICD10','P0514','Newborn small for gestational age, 1000-1249 grams'),
#     ('small_gest_age','ICD10','P0515','Newborn small for gestational age, 1250-1499 grams'),
#     ('small_gest_age','ICD10','P0516','Newborn small for gestational age, 1500-1749 grams'),
#     ('small_gest_age','ICD10','P0517','Newborn small for gestational age, 1750-1999 grams'),
#     ('small_gest_age','ICD10','P052','Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age')
#   ],
#   ['name', 'terminology', 'code', 'term']  
# )   

# need to include snomed codes
# preterm 
#  = spark.createDataFrame(
#   [
#     ('preterm','ICD10','P0726','Extreme immaturity of newborn, gestational age 27 completed weeks'),
#     ('preterm','ICD10','P0730','Preterm newborn, unspecified weeks of gestation'),
#     ('preterm','ICD10','P0731','Preterm newborn, gestational age 28 completed weeks'),
#     ('preterm','ICD10','P0732','Preterm newborn, gestational age 29 completed weeks'),
#     ('preterm','ICD10','P0733','Preterm newborn, gestational age 30 completed weeks'),
#     ('preterm','ICD10','P0734','Preterm newborn, gestational age 31 completed weeks'),
#     ('preterm','ICD10','P0735','Preterm newborn, gestational age 32 completed weeks'),
#     ('preterm','ICD10','P0736','Preterm newborn, gestational age 33 completed weeks'),
#     ('preterm','ICD10','P0737','Preterm newborn, gestational age 34 completed weeks'),
#     ('preterm','ICD10','P0738','Preterm newborn, gestational age 35 completed weeks'),
#     ('preterm','ICD10','P0739','Preterm newborn, gestational age 36 completed weeks')
#   ],
#   ['name', 'terminology', 'code', 'term']  

# need to include snomed codes
# very_preterm 
#  = spark.createDataFrame(
#   [
#     ('very_preterm','ICD10','P0726','Extreme immaturity of newborn, gestational age 27 completed weeks'),
#     ('very_preterm','ICD10','P0731','Preterm newborn, gestational age 28 completed weeks'),
#     ('very_preterm','ICD10','P0732','Preterm newborn, gestational age 29 completed weeks'),
#     ('very_preterm','ICD10','P0733','Preterm newborn, gestational age 30 completed weeks'),
#     ('very_preterm','ICD10','P0734','Preterm newborn, gestational age 31 completed weeks'),
#   ],
#   ['name', 'terminology', 'code', 'term'] 

# need to include snomed codes
# extremely_preterm 
#  = spark.createDataFrame(
#   [
#     ('extremely_preterm','ICD10','P0726','Extreme immaturity of newborn, gestational age 27 completed weeks','','',0),
#   ],
#   ['name', 'terminology', 'code', 'term']

################################################################################
## File: Precard data Imputation for Type I Data
## Purpose: Load and impute Type 1 data 
## Author: Tasos Mangelis
## Last update: 15.05.2022
################################################################################

# Libraries ----

library("readxl")
library("tidyverse")
library("dplyr")
library("lubridate")
library("mice")

# Load dataset ----

# Define file location
Dataload_file_location <- "E:\\Precard project\\Data\\Initial_merge\\"
Datasave_file_location <- "E:\\Precard project\\Data\\Precard_eGFR_files\\"

# Load data and keep Type I patients only
Precard_Data_merge<-readRDS(paste0(Dataload_file_location,"Precard_Data_merge.rds"))

precard_DTI<-Precard_Data_merge$precard_merged%>%filter(DM_TYPE_ID=="DT_I")

# impute missing values ----

# Step1: Check missingness xclude variables with more than 40% missing

precard_DTI<-precard_DTI%>%
  select(-c(PCR, Smoking, DM_TYPE_ID, HDL, LDL, RM, R, M))

# Step 2: - define variable that need to be imputed, 
#         - variables not to be imputed but used as predictors and 
#         - variables to be completely excluded from the process
#         - impute missing values using pmm method from mice package

prediction<-mice(precard_DTI%>%
                   select(R50_ID, TEST_DATE, Age, Gender, Ethnicity_groups, eGFR, 
                          Weight, ACR, Triglycerids, BMI, SYSTOLIC, DIASTOLIC, 
                          Cholesterol, Hba1c), 
                 m=5, maxit = 15, method = c('pmm'), seed = 500, 
                 predictorMatrix = quickpred(precard_DTI%>%
                                               select(R50_ID, TEST_DATE, Age, Gender, Ethnicity_groups, eGFR, 
                                                      Weight, ACR, Triglycerids, BMI, SYSTOLIC, DIASTOLIC, 
                                                      Cholesterol, Hba1c), 
                                   include = c("Age", "Gender", "Ethnicity_groups", "eGFR", "Weight", "ACR", "Triglycerids", 
                                               "BMI", "SYSTOLIC", "DIASTOLIC", "Cholesterol", "Hba1c"),
                                   exclude = c("R50_ID", "TEST_DATE")))

# Summary of the imputation and density plot for checking new distribution against the old in 04a

# Assignment of the iteration for the imputed dataset
precard_DTI_full<-mice::complete(prediction, 5)
precard_DTI_full<-precard_DTI_full%>%
  inner_join(precard_DTI%>%select(R50_ID, TEST_DATE, TEST_DATE_bsl, eGFR_bsl, YODeath, YOBirth, DIAGNOSIS_YEAR, Diabetes_Dur, ACR_G ), by = c("R50_ID", "TEST_DATE"))%>%
  distinct(R50_ID, TEST_DATE, eGFR, .keep_all = T)

write.csv(precard_DTI_full, paste0(Datasave_file_location, "Precard_Type1_full.csv"))
# End of code ----

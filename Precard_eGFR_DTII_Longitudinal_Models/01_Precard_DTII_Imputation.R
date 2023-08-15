################################################################################
## File: Precard data loading and Imputation for Type 2 Latent class models (LCM)
## Purpose: Load and impute Type 2 data 
## Author: Tasos Mangelis
## Last update: 04.08.2022
################################################################################

# Libraries ----

Loadlibraries()

Loadlibraries <- function() {
  library(tidyverse)
  library(readxl)
  library(ggplot2)
  library(lubridate)
  library(LCTMtools)
  library(lcmm)
  library(mice)
}

# Data Loading ----

# Define data location to draw the data and save all initial eGFR datasets
Dataload_file_location <- "E:\\Precard project\\Data\\Initial_merge\\"
Datasave_file_location <- "E:\\Precard project\\Data\\Precard_LatentClassModels_files\\"
# Load data of merged eGFR dataset 
Precard_Data_Merge<-readRDS(paste0(Dataload_file_location,"Precard_Data_merge.rds"))

# load raw data from the excel file in the project dir

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # straight data.frames - tibble = F 
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  #x<- lapply(sheets, function(Y) names(Y)[names(Y) == 'TEST_DATE'] <- paste0("test_date_", sheets))
  x
}
raw_data <- read_excel_allsheets(paste0(Dataload_file_location,"Precardc_Full.xlsx"))

# Dataset with eGFR values, including patients with >45 eGFR baseline and after test date 2004 and demographics

precard_T2<-Precard_Data_Merge$precard_merged%>%
  filter(DM_TYPE_ID=="DT_II")%>%
  rename("test_date.bsl"="TEST_DATE_bsl", 
         "test_year.bsl"="YEAR_TEST_bsl",
         "test_date"="TEST_DATE", 
         "test_year"="YEAR_TEST")
  

# Imputation of dataset ----

## Step1: Check missingness and exclude variables with more than 40% missing

precard_T2<-precard_T2%>%
  select(-c(PCR, Smoking, DM_TYPE_ID, LDL, Weight))

# Step 2: - define variable that need to be imputed, 
#         - variables not to be imputed but used as predictors and 
#         - variables to be completely excluded from the process
#         - impute missing values using pmm method from mice package

prediction<-mice(precard_T2%>%
                   select(R50_ID, test_date, Age, Gender, Ethnicity_groups, eGFR, 
                          ACR, Triglycerids, BMI, SYSTOLIC, DIASTOLIC, 
                          Cholesterol, Hba1c, HDL), 
                 m=5, maxit = 15, method = c('pmm'), seed = 500, 
                 predictorMatrix = quickpred(precard_T2%>%
                                               select(R50_ID, test_date, Age, Gender, Ethnicity_groups, eGFR, 
                                                      ACR, Triglycerids, BMI, SYSTOLIC, DIASTOLIC, 
                                                      Cholesterol, Hba1c, HDL), 
                                             include = c("Age", "Gender", "Ethnicity_groups", "eGFR", "HDL", "ACR", "Triglycerids", 
                                                         "BMI", "SYSTOLIC", "DIASTOLIC", "Cholesterol", "Hba1c"),
                                             exclude = c("R50_ID", "test_date")))

# Summary of the imputation and density plot for checking new distribution against the old in 04a

# Assignment of the iteration for the imputed dataset
precard_T2_full<-mice::complete(prediction, 5)
precard_T2_full<-precard_T2_full%>%
  inner_join(precard_T2%>%select(R50_ID, test_date, test_date.bsl, eGFR_bsl, YODeath, YOBirth, DIAGNOSIS_YEAR, Diabetes_Dur, ACR_G ), by = c("R50_ID", "test_date"))%>%
  distinct(R50_ID, test_date, eGFR, .keep_all = T)

write.csv(precard_T2_full, paste0(Datasave_file_location, "Precard_Type2_full.csv"))

# End of code ----



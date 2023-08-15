################################################################################
## File: Precard data cleaning
## Purpose: Import and combine raw data, prepare datasets for analysis
## Author: Tasos Mangelis
## Last update: 22.04.2022
################################################################################

# Libraries ----

# Load all necessary libraries

library("readxl")
library("readr")
library("tidyverse")
library("dplyr")
library("lubridate")
library("mice")

# data loading ----

# Define data location to draw the data and save all initial eGFR datasets
Datasave_file_location <- "E:\\Precard project\\Data\\Initial_merge\\"

# load data from the excel file in the project dir
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # straight data.frames - tibble = F 
  sheets <- readxl::excel_sheets(filename)
  # apply readxl to all sheets of the excel file
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  # name each dataset after the corresponding sheet
  names(x) <- sheets
  x
}

# Apply function to file in specified location
imported_data <- read_excel_allsheets(paste0(Datasave_file_location,"Precardc_Full.xlsx"))


# Calculation of eGFR values according to the CKD-EPI equation ----

# Function of CKD-epi equation calculation. This equation is based on creatinine 
# and calculation is dependent on gender, age and ethnicity, the combination of 
# which define the values of k and a parameters

calculate_eGFR <- function(creat, age, gender, ethn) {
  if (ethn != "BLACK") {
    if (gender != "FEMALE") {
      k <- 61.9
      a <- -0.329
    } else {
      k <- 79.6
      a <- -0.411
    }
  } else {
    if (gender != "FEMALE") {
      k <- 61.9
      a <- -0.329
    } else {
      k <- 79.6
      a <- -0.411
    }
  }
    
    egfr <- 141 * ((min(creat / k, 1)) ** a) * ((max(creat / k, 1)) ** (-1.209)) * (0.993 ** age)
    return(egfr)
  }

# Create a main dataset based on creatinine, including necessary info for eGFR 
# calculation to store eGFR extracted values and include demographics
precard_main<-imported_data$raw_creat%>%
  left_join(imported_data$raw_demogs, by = 'R50_ID')%>% # join demographics
  filter(!is.na(Ethnicity_groups))%>% # filter out patients without Ethnicity
  mutate(Age=as.numeric(as_date(YEAR_TEST-YOBirth))) # Create new variables 

# Initialize a vector to store eGFR values
gfr <- numeric(length(precard_main$Creatinine))  # Pre-allocate space for efficiency

# extraction of total CKD_EPI eGFR using calculate_eGFR() function
for (i in 1:length(precard_main$Creatinine)) {
  creat_val <- precard_main$Creatinine[i]
  age_val <- precard_main$Age[i]
  ethn_val <- precard_main$Ethnicity_groups[i]
  gender_val <- precard_main$Gender[i]
  
  gfr[i] <- calculate_eGFR(creat_val, age_val, gender_val, ethn_val)
}

# Store values in precard main dataset
precard_main$eGFR <- round(gfr, 1)

# Shape and store dataset
precard_main<-precard_main%>%
  mutate(TEST_DATE=as.Date(TEST_DATE,format="%Y-%m-%d", origin = '1970-01-01'), 
         Diabetes_Dur=as.numeric(YEAR_TEST-DIAGNOSIS_YEAR))%>%
  select(-c(Creatinine))%>%
  filter(YEAR_TEST>=2004)%>% # keep measurements after 2004 (Data not reliable before)
  distinct(R50_ID, TEST_DATE, eGFR, .keep_all = T) # make sure to remove duplicates

# Save data to csv file
write_csv(precard_main, file = paste0(Data_file_location,"eGFR_full.csv"))

# Extract baseline EGFR data ----

# Order and keep only earliest measurements. In addition, Clinical changes to data set: 
# Exclude all patients with baseline eGFR below 45

Baseline_dataset<- precard_main%>%
  group_by(R50_ID)%>%
  arrange(TEST_DATE)%>%slice(1)%>% # group and keep earliest date measurements
  ungroup()%>%
  rename("TEST_DATE_bsl"="TEST_DATE", "YEAR_TEST_bsl"="YEAR_TEST", "eGFR_bsl"="eGFR",
         "Diabetes_Dur_bsl"="Diabetes_Dur")%>%
  mutate(TEST_DATE_bsl=as.Date(TEST_DATE_bsl))%>%
  filter(eGFR_bsl>=45) # exclude patients with baseline eGFR < 45 
 

# Add baseline information to main dataset for later calculation of duration, timet to event etc

precard_bsl<-precard_main%>%
  inner_join(Baseline_dataset%>%select(R50_ID, TEST_DATE_bsl, YEAR_TEST_bsl, eGFR_bsl), by = "R50_ID")

# Save dataset of eGFR measurements (CKD-EPI Eq) excluding patients with bs date<=2004

write_csv(precard_bsl, file = paste0(Data_file_location,"eGFR_bsl_full_2004.csv"))

# Merge baseline eGFR with all other data and variables to identify baseline values ----

precard_merged<-precard_bsl  # Dataset to store values
eGFR_ID_TD<-precard_bsl%>%   # Dataset including all IDs and baseline test dates of eGFR
  select(R50_ID, TEST_DATE_bsl)%>%
  mutate(TEST_DATE_bsl= as.Date(TEST_DATE_bsl))

# Exclude raw_demogs (demographic data), medication data (due to missingness) and
# creatinine (we include eGFR) from the list of datasets as not needed

datasets_loop<-imported_data
datasets_loop[["raw_demogs"]]<-NULL
datasets_loop[["raw_med"]]<-NULL
datasets_loop[["raw_creat"]]<-NULL

# Loop for locating the "within 2 years measurements" of other variables based on eGFR baseline
for (i in 1:length(datasets_loop)) {
  print(i)
  df<-datasets_loop[[i]]%>% # load data
    inner_join(eGFR_ID_TD, by = "R50_ID", all = TRUE)%>% # join with baseline eGFR
    mutate(TEST_DATE=as.Date(TEST_DATE),  # Calculate difference
           date_diff=round((as.Date(TEST_DATE)-as.Date(TEST_DATE_bsl))/365, 1))%>%
    filter(abs(date_diff)<= 2)%>% # filter difference less than 2 years
    group_by(R50_ID)%>%           # Isolate the first value
    arrange(TEST_DATE)%>%
    slice(1)%>%
    select(-c(TEST_DATE, TEST_DATE_bsl, date_diff, YEAR_TEST))
  precard_merged<-precard_merged%>%left_join(df, by = "R50_ID") # merge 
}

# Additional changes on dataset following visual and data exploration

precard_merged<-precard_merged%>%
  mutate(RM=ifelse(RM=="U", NA, RM),                     
         RM=ifelse(RM=="R1"|RM=="R3", NA, RM),
         ACR=ifelse(ACR>200, 200, ACR),                 
         Cholesterol=ifelse(Cholesterol>10, NA, Cholesterol),
         eGFR=ifelse(eGFR>160, 160, eGFR), 
         Triglycerids=ifelse(Triglycerids>10, NA, Triglycerids),
         LDL=ifelse(LDL>8, NA, LDL), 
         Diabetes_Dur=ifelse(Diabetes_Dur<0, 0, Diabetes_Dur),
         RM=as.factor(RM),
         DM_TYPE_ID=as.factor(DM_TYPE_ID), 
         Ethnicity_groups=as.factor(Ethnicity_groups), 
         Gender=as.factor(Gender), 
         ACR_G=ifelse(ACR<3,"A1", ifelse(ACR>=30,"A3","A2")))%>%
  distinct(R50_ID, TEST_DATE, eGFR, .keep_all = T)

write_csv(precard_merged, file = paste0(Data_file_location,"eGFR_Full_Inclusive_Bsl.csv"))

# Save all data to RDS file to ensure proper class of the variables
data_store_list<- list(precard_bsl=precard_bsl, precard_merged=precard_merged, 
                       precard_main=precard_main, Baseline_dataset=Baseline_dataset)
saveRDS(data_store_list, paste0(Datasave_file_location,"Precard_Data_merge.rds"))

# End of code ##################################################################
################################################################################
## File: Precard data preparation for Type 2 Latent class models
## Purpose: Load and impute Type 2 data 
## Author: Tasos Mangelis
## Last update: 04.08.2022
################################################################################

# Libraries ----

Loadlibraries()

Loadlibraries <- function() {
  library("tidyverse")
  library("readxl")
  library("ggplot2")
  library("lubridate")
  library("LCTMtools")
  library("lcmm")
  library("mice")
  library("splines")
}

# Data Loading ----

# Define data location to draw the data and save all initial eGFR datasets
Dataload_file_location <- "E:\\Precard project\\Data\\Initial_merge\\"
Datasave_file_location <- "E:\\Precard project\\Data\\Precard_LatentClassModels_files\\"

# Load dataset precard_T2_full including imputed variables and baseline and eGFR data

precard_T2_full<-read.csv(precard_T2_full, paste0(Datasave_file_location, "Precard_Type2_full.csv"))

# Dataset for longitudinal analysis ----

# Create dataset based on the full Type 2 data but with standardised yearly eGFR measurements

prec_long_T2<-precard_T2_full%>%
  # Create time difference variable
  mutate(timeDiff=round(as.numeric(difftime(test_date, test_date.bsl, units = "days")/365), 2))%>%
  # Group in order to extract mean per year below
  group_by(across(c(R50_ID,timeDiff)))%>%
  # Extract mean per year
  mutate(egfr_year= mean(eGFR, na.rm = TRUE))%>%
  # Keep one of the multiple measurements that only differ in eGFR values 
  # (but with same eGFR mean value)
  distinct(R50_ID, timeDiff, .keep_all = TRUE)%>%
  ungroup(timeDiff)%>%
  # Indicate number of measurements and exclude those that have less than 3 and 
  # more than 10
  mutate(n=max(timeDiff, na.rm = T),
         timeDiff=as.factor(timeDiff))%>%
  filter(n>2|n<11)%>%
  # Add further changes to dataset
  rename("mEGFR"="egfr_year")%>%
  # convert ID to numeric
  mutate(R50_ID=as.numeric(str_sub(R50_ID, 2, -1)))%>%
  # Keep only adult population
  filter(Age>=18)

write.csv(prec_long_T2, paste0(Datasave_file_location, "Precard_Type2_long.csv"))


#### End of code ###############################################################

################################################################################
## File: Precard Renal DTI
## Purpose: Data preparation for Type I Data Analysis
## Author: Tasos Mangelis
## Last update: 15.05.2022
################################################################################

# Libraries ----

library("readxl")
library("tidyverse")
library("dplyr")

# Load dataset ----

# Define file location
Dataload_file_location <- "E:\\Precard project\\Data\\Initial_merge\\"
Datasave_file_location <- "E:\\Precard project\\Data\\Precard_eGFR_files\\"

precard_DTI_full<-read.csv(paste0(Datasave_file_location, "Precard_Type1_full.csv"))

# Create a dataset suited for survival analysis, including eGFR events for 30, 40 and 50% decrease with eGFR < 35 ml/min
# In addition, including time-to-event and event variables for the above decreases
# Also, slopes for comparison

DTI_Sanalysis<-precard_DTI_full%>%
  # Calculate the 30, 40 and 50% decrease of eGFR for all measurements
  mutate(eGFR_drop=round(((eGFR-eGFR_bsl)/eGFR_bsl)*100, 2), 
         eGFR_drop30=ifelse(eGFR_drop<=-30 & eGFR<=35,1,0),
         eGFR_drop40=ifelse(eGFR_drop<=-40 & eGFR<=35,1,0), 
         eGFR_drop50=ifelse(eGFR_drop<=-50 & eGFR<=35,1,0),
         # Additional data modifications
         Duration_diabetes_groups=ifelse(Diabetes_Dur==0, "0",
                                                ifelse(Diabetes_Dur>0&Diabetes_Dur<10, "1-9",
                                                       ifelse(Diabetes_Dur>9&Diabetes_Dur<20, "10-20", "20+"))),
         Age_groups=as.factor(ifelse(Age<30, "0-30", ifelse(Age>29&Age<60, "31-60", "60+"))),
         Afrocaribbean_Ethnicity=as.factor(ifelse(Ethnicity_groups=="BLACK", "afro-caribbean", "other")))
# Change level of ethnicity for survival analytics 
DTI_Sanalysis <- within(DTI_Sanalysis, Afrocaribbean_Ethnicity<-relevel(Afrocaribbean_Ethnicity, ref = 2))

# Files for survival analysis -------------------------------------------------


drop_list<-list("eGFR_drop30", "eGFR_drop40", "eGFR_drop50")
Precard_eGFRdrop_datasets<-list()

for (i in drop_list) {
  print(i)
  # isolate the dataset of interest from drop_list
  exclude_names<-drop_list[drop_list !=i]
  df<-DTI_Sanalysis%>%
    # keep only the column of interest
    select(-c(exclude_names[[1]], exclude_names[[2]]))%>%
    group_by(R50_ID)%>%
    # calculate the eGFR slopes 
    mutate(TEST_DATE_final=max(TEST_DATE), 
           # eGFR value for the latest available test date
           eGFR_final=eGFR[which.max(TEST_DATE)],
           # slope calculation 
           slope_time=as.numeric(difftime(TEST_DATE_final, TEST_DATE_bsl, units = "days")/365), 
           eGFR_slope=round((eGFR_final-eGFR_bsl)/slope_time, 3),
           # characterise slopes according to the b coefficient
           eGFR_slope_c=ifelse(eGFR_slope> 1, "incline", 
                               ifelse(eGFR_slope<1 & eGFR_slope >-1, "stable",
                                      ifelse(eGFR_slope>=-3 & eGFR_slope <=-1, "slow_decline", 
                                             ifelse(eGFR_slope>-5 & eGFR_slope < -3, "moderate_decline", "fast_decline")))))%>%
    # Isolate the event that happened for the first time
    arrange(desc(get(i)), TEST_DATE)%>%slice(1)%>%
    rename("TEST_DATE_event"="TEST_DATE")%>%
    ungroup()%>%
    # Identify the events, censored and pre-event death
    mutate(TEST_DATE_bsl=as.POSIXct(TEST_DATE_bsl,format="%Y-%m-%d", origin = "1970-01-01"), 
           TEST_DATE_event=as.POSIXct(TEST_DATE_event,format="%Y-%m-%d", origin = "1970-01-01"),
           TEST_DATE_final=as.POSIXct(TEST_DATE_final,format="%Y-%m-%d", origin = "1970-01-01"),
           # Event: 0 = No event, 1=Event, 2=Pre event death
           Event=ifelse(get(i)==0 & is.na(YODeath), 0,
                        ifelse(get(i)==0 & !is.na(YODeath), 2, 1)),
           # Time to event calculation
           T2E=round(ifelse(get(i)==0 & is.na(YODeath), difftime(TEST_DATE_final, TEST_DATE_bsl, units = "days")/365,
                         ifelse(get(i)==0 & !is.na(YODeath), YODeath - year(TEST_DATE_bsl), 
                                difftime(TEST_DATE_event, TEST_DATE_bsl, units = "days")/365)), 1), 
           ## EventCox: 1= No Event, 2=Event ()
           EventCox=ifelse(Event==0, 1, ifelse(Event==1, 2, NA)))
  # Assign df to a list and write to csv
  Precard_eGFRdrop_datasets[[i]]<-df
  write.csv(df, paste0(Datasave_file_location,i,".csv"))
}

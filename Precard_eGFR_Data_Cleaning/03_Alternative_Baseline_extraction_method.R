################################################################################
## File: Alternative Baseline extraction method
## Purpose: Alternative method for baseline dataset calulation 
##          based on the moving window method
## Author: Tasos Mangelis
## Last update: 05.04.2022
################################################################################# 

# Load all necessary libraries

library("readxl")
library("readr")
library("tidyverse")
library("dplyr")

# Define data location to draw the data and save all initial eGFR datasets
Data_file_location <- "E:\\Precard project\\Data\\Initial_merge\\"

# Create an id vector 
id<-unique(precard_merged$R50_ID)

Baseline_eGFR_MovingW<-data.frame()

for (r in 1:length(id)) {
  
  # eGFR dataset to use as a starting point
  df<-precard_merged[precard_merged$R50_ID==id[r], c("R50_ID", "TEST_DATE")]
  
  # start from the first available eGFR measurement.
  # Pick a time window (chosen on the average differences of the measurements)
  # define the window
  
  t.ini <-min(df$TEST_DATE, na.rm = T) #initial date
  t.last<-max(df$TEST_DATE, na.rm = T) #final date
  t.tot<-as.numeric(difftime(t.last, t.ini, units = "days"))
  t.ints<-round(t.tot/30, digits = 0)-1 # estimate optimal number of iterations (ie. advances of an ONE MONTH window)
  
  # length of window - 4 months
  
  Dates_summary<-data.frame()
  
  for (i in 1:t.ints){
    
    t.upper<- t.ini+i*180                # upper limit of window 
    t.lower<- t.ini+(i-1)*180            # lower limit of window
    
    # eGFR
    win.eGFR<-df%>%filter((TEST_DATE<t.upper&TEST_DATE>t.lower))  # Find instances within the window
    mark.eGFR<-ifelse(length(win.eGFR$TEST_DATE)>=1, 1, 0)        # create eGFR indicator (window includes a date (1) or not (0))
    
    # HbA1c 
    win.HbA1c<-raw_HBA1C[raw_HBA1C$R50_ID==id[r], ]%>%            # Same as above for all data in raw_datasets
      filter((TEST_DATE_HbA1c<t.upper&TEST_DATE_HbA1c>t.lower)) 
    mark.HbA1c<-ifelse(length(win.HbA1c$TEST_DATE_HbA1c)>=1, 1, 0)
    
    # ACR
    win.acr<-raw_acr[raw_acr$R50_ID==id[r], ]%>%
      filter((TEST_DATE_acr<t.upper&TEST_DATE_acr>t.lower)) 
    mark.acr<-ifelse(length(win.acr$TEST_DATE_acr)>=1, 1, 0)
    
    # BP
    win.bp<-raw_BP[raw_BP$R50_ID==id[r], ]%>%
      filter((TEST_DATE_BP<t.upper&TEST_DATE_BP>t.lower)) 
    mark.bp<-ifelse(length(win.bp$TEST_DATE_BP)>=1, 1, 0)
    
    # ret
    win.ret<-raw_ret[raw_ret$R50_ID==id[r], ]%>%
      filter((TEST_DATE_ret<t.upper&TEST_DATE_ret>t.lower)) 
    mark.ret<-ifelse(length(win.ret$TEST_DATE_ret)>=1, 1, 0)
    
    # triglycerides
    win.trig<-raw_trig[raw_trig$R50_ID==id[r], ]%>%
      filter((TEST_DATE_trig<t.upper&TEST_DATE_trig>t.lower)) 
    mark.trig<-ifelse(length(win.trig$TEST_DATE_trig)>=1, 1, 0)
    
    # BMI
    win.BMI<-raw_BMI[raw_BMI$R50_ID==id[r], ]%>%
      filter((TEST_DATE_BMI<t.upper&TEST_DATE_BMI>t.lower)) 
    mark.BMI<-ifelse(length(win.BMI$TEST_DATE_BMI)>=1, 1, 0)
    
    # cholesterol
    win.chol<-raw_chol[raw_chol$R50_ID==id[r], ]%>%
      filter((TEST_DATE_chol<t.upper&TEST_DATE_chol>t.lower)) 
    mark.chol<-ifelse(length(win.chol$TEST_DATE_chol)>=1, 1, 0)
    
    # HDL
    win.HDL<-raw_HDL[raw_HDL$R50_ID==id[r], ]%>%
      filter((TEST_DATE_HDL<t.upper&TEST_DATE_HDL>t.lower)) 
    mark.HDL<-ifelse(length(win.HDL$TEST_DATE_HDL)>=1, 1, 0)
    
    # LDL
    win.LDL<-raw_LDL[raw_LDL$R50_ID==id[r], ]%>%
      filter((TEST_DATE_LDL<t.upper&TEST_DATE_LDL>t.lower)) 
    mark.LDL<-ifelse(length(win.LDL$TEST_DATE_LDL)>=1, 1, 0)
    
    # Total score for sorting out potential solutions
    total.score<-mark.eGFR+mark.HbA1c+mark.acr+mark.bp+mark.BMI+mark.chol+
      mark.HDL+mark.LDL+mark.ret+mark.trig
    
    # If we have solutions, document them and take the first available
    Dates_summary<-rbind(Dates_summary, 
                         data.frame(i, total.score, win.eGFR[1, ], win.acr[1, -1], win.BMI[1, -1], win.bp[1, -1], 
                                    win.chol[1, -1], win.HbA1c[1, -1], win.HDL[1, -1], win.LDL[1, -1], 
                                    win.ret[1, -1], win.trig[1, -1], stringsAsFactors = F))
    
  }
  Dates_summary<-Dates_summary%>%filter(!is.na(R50_ID))%>%
    arrange(TEST_DATE, total.score)%>%slice(1)
  # Add solutions to the initial dataset
  Baseline_eGFR_MovingW<-rbind(Baseline_eGFR_MovingW, Dates_summary)
  
}

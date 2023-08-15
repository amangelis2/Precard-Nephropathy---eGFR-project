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

# Load dataset 

prec_long_T2<-read.csv(paste0(Datasave_file_location, "Precard_Type2_long.csv"))


# Step 1: Select the form of the random effect structure ------------------

# To determine the initial working model structure of random effects, the rationale 
# of Verbeke and Molenbergh can be followed to examined the shape of standardised 
# residual plots for each of the K classes in a model with no random effects.

# If the residual profile could be approximated by a flat, straight line or a curve, 
# then a random intercept, slope or quadratic term, respectively, were considered.

# To fit a latent class model with no random effects, the lcmm R package this can 
# be used with the specification of random=~-1


model1 <- lcmm::hlme(fixed = meGFR ~ time_diff + I( time_diff^2)+ I( time_diff^3),
                     mixture = meGFR ~   time_diff + I( time_diff^2)+ I( time_diff^3),
                     random= ~ -1,
                     subject="R50_ID",
                     ng=4, # number of predefined classes
                     nwg=T, # allow for variance to vary between classes
                     data = data.frame(prec_long_T2))

# We then feed the fitted model to the step1 function in LCTMtools to examine 
# the class specific residuals.

residualplot_step1(model1, 
                   nameofoutcome="eGFR",  nameofage = "time_diff",
                   data = prec_long_T2) 

# Step 2 ------------------------------------------------------------------

# Refine the preliminary working model from step 1 to determine the optimal number 
# of classes, testing K=1,...5
# The number of classes chosen may be chosen based on the lowest Bayesian information criteria (BIC).

set.seed(100)
m.1 <- lcmm::hlme(fixed = meGFR ~ time_diff + I( time_diff^2)+ I( time_diff^3),
                  random = ~ time_diff ,
                  ng = 1,
                  idiag = FALSE, 
                  data = data.frame(prec_long_T2), subject = "R50_ID")
lin <- c(m.1$ng, m.1$BIC, m.1$AIC, m.1$loglik)

for (i in 2:5) {
  mi <- lcmm::hlme(fixed = meGFR ~ time_diff + I( time_diff^2)+ I( time_diff^3),
                   mixture = meGFR ~   time_diff + I( time_diff^2)+ I( time_diff^3),
                   random= ~ time_diff,
                   ng = i, nwg = TRUE, 
                   idiag = FALSE, 
                   data = data.frame(prec_long_T2), subject = "R50_ID")
  
  lin <- rbind(lin, c(i, mi$BIC, mi$AIC, mi$loglik))
}

modelout <- knitr::kable(lin, col.names = c("k", "BIC", "AIC", "Log-likelihood"), row.names = FALSE, align = "c")


# Step 3 ------------------------------------------------------------------

# Further refine the model using the favoured K derived in step 2, 
# testing for the optimal model structure. 

# model B
model_b <- hlme(fixed = meGFR ~time_diff + I( time_diff^2)+ I( time_diff^3),
                mixture = ~time_diff + I( time_diff^2)+ I( time_diff^3),
                random = ~time_diff,
                ng = 5, nwg = F, 
                idiag = FALSE, 
                data = data.frame(prec_long_T2),
                subject = "R50_ID")

model_b$BIC

# model c with variations ie different number of classes 

model_c2 <- hlme(fixed = meGFR ~time_diff + I( time_diff^2)+ I( time_diff^3),
                 mixture = ~time_diff + I( time_diff^2)+ I( time_diff^3),
                 random = ~time_diff,
                 ng = 2, nwg = T, 
                 idiag = FALSE, 
                 data = data.frame(prec_long_T2), subject = "R50_ID")
model_c2$BIC


model_c3 <- hlme(fixed = meGFR ~time_diff + I( time_diff^2)+ I( time_diff^3),
                 mixture = ~time_diff + I( time_diff^2)+ I( time_diff^3),
                 random = ~time_diff,
                 ng = 3, nwg = T, 
                 idiag = FALSE, 
                 data = data.frame(prec_long_T2), subject = "R50_ID")
model_c3$BIC

model_c4 <- hlme(fixed = meGFR ~time_diff + I( time_diff^2)+ I( time_diff^3),
                 mixture = ~time_diff + I( time_diff^2)+ I( time_diff^3),
                 random = ~time_diff,
                 ng = 4, nwg = T, 
                 idiag = FALSE, 
                 data = data.frame(prec_long_T2), subject = "R50_ID")
model_c4$BIC

# Step 4: Model adequacy assessments --------------------------------------

#Perform a number of model adequacy assessments. First, for each participant, 
#calculate the posterior probability of being assigned to each trajectory class 
#and assigned the individual to the class with the highest probability. 
#An averYEAR_TEST of these maximum posterior probability of assignments (APPA) above 70%, 
#in all classes, is regarded as acceptable. Further assess model adequacy using 
#odds of correct classification, mismatch.

LCTMtoolkit(model_c3)

# Step 5: Graphical representations ---------------------------------------

# Plot mean trajectories with time encompassing each class

# Mean trajectory plots with 95% predictive intervals for each class, which displays the predicted random variation within each class

plotpred <- predictY(model_c3, datnew, var.time ="time_diff", draws = T)
plot(plotpred, lty=1, xlab="Time (years)", ylab="mean eGFR", legend.loc = "topleft", cex=0.75) 

# Individual level ‘spaghetti plots’ with time, depending on sample size 
# use a random sample of participants

library(ggplot2)
ggplot(prec_long_T2, aes(x = time_diff, y = meGFR)) + geom_line(aes(color = R50_ID,group = R50_ID), colour = "grey") + xlab("YEAR_TEST") + ylab("meGFR")

#with color
ggplot(prec_long_T2, aes(x = time_diff, y= meGFR)) + 
  geom_line(aes(group = R50_ID)) + xlab("YEAR_TEST") + ylab("meGFR") + 
  labs(color = "Class Assignment")

# Step 6: Assessing clinical characterization --------

# 1. Assessing the clinical meaningfulness of the trajectory patterns, 
# aiming to include classes with at least 1% capture of the population

lcmm::postprob( model_c3 )

# 2. Tabulation of characteristics by latent classes versus conventional categorisations

# Extract class assignments from chosen model 
# Dataset including classes produced by the models, used for ploting and comparisons
# model$pprob[] gives a matrix of all instances (here: patients) 
# including assigned classes and posterior probabilities

Prec_T2_class<-as.data.frame(model_c2$pprob[, 1:2])
Prec_T2_class<-cbind(Prec_T2_class, as.data.frame(model_c3$pprob[ , 2]),
                     as.data.frame(model_c4$pprob[, 2]))
# Define column names
colnames(Prec_T2_class)<-c("R50_ID", "classes_2", "classes_3", "classes_4")


prec_long_T2<-prec_long_T2%>%
  left_join(Prec_T2_class, by="R50_ID")

# and then feed back into main dataset with descriptive variables. 
# Then these can be tabulated as needed.

# Create new data for plotting LCMM prediction function ----

# Data that include time variable within range of training set 
# and other variables that are defined as the mean of the variables they represent 

datnew_t<-data.frame(timeDiff=seq(0, 10, length = 1500), 
                     ACR=10, Diabetes_Dur =5, Age=25, Triglycerids= 1.7, 
                     Hba1c= 67, SYSTOLIC= 135, BMI= 33, Cholesterol= 4.5)



# End of code ----

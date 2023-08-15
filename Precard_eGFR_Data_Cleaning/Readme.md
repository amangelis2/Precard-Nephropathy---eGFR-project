
# Precard eGFR Data Merge and preparation

This repository contains the .R and .Rmd files for the eGFR Data Analysis project, focusing on raw data input, cleaning, creation of new measures, dataset merging, observational analysis, and visualization.

## Project Components

The project files are organized as follows:

### 01_Precard_eGFR_Baseline.R

This script serves as the input point for the raw data of the project. It handles data cleaning, creation of new measures, and the merging of eGFR-based datasets. Further details about the processes are included as comments within the script.

### 02_Precard_eGFR_Observational_Analysis.Rmd

This R Markdown document contains the results of the observational analysis. It includes visualizations of dataset characteristics during the processes described in the previous file (01_Precard_eGFR_Baseline.R). The document provides insights into the characteristics of the data and the effects of the applied processes.

### 03_Alternative_Baseline_Development.R

This script presents an alternative method for developing the baseline dataset. It uses a moving window solution as opposed to the 2-year-block approach used in the first file (01_Precard_eGFR_Baseline.R). However, it's important to note that this method was ultimately not used for the project.

## Project Details

For detailed explanations of the code and processes, please refer to the comments within each file.

For any questions or inquiries about the project, please contact Tasos Mangelis (amangelis@hotmail.com).

---
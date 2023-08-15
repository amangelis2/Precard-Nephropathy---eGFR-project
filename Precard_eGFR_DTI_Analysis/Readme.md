# Precard eGFR Type 1 analysis

This repository contains the code used to load, clean, impute, analyze, and visualize data from the eGFR Data Cleaning project. 
The focus of this project is to prepare data specifically for patients with type 1 diabetes, perform comprehensive analysis, and create visualizations suitable for publication.

# Project Overview

The project is divided into the following main parts:

## 01_Precard_data_Imputation_DTI

In this section, data containing eGFR (estimated Glomerular Filtration Rate) combined with other baseline data is retrieved from the prestored location. The data related to patients with type 1 diabetes are isolated and imputed.

## 01a_Precard_Data_Exploration.Rmd

This section includes the code for creating visualizations that assist in the imputation process. Exploratory data analysis helps understand the characteristics of the data and aids in making informed imputation decisions.

## 02_Precard_eGFR_DTI_Dataset.R

Here, the dataset is prepared for subsequent analysis. Data manipulation and formatting necessary for analysis are performed in this part.

## 02a_Precard_eGFR_Risk_analysis.Rmd

This file contains the analysis portion of the project. It involves the creation of survival models, validation, and calibration using three different R packages. The analysis focuses on understanding the risk associated with eGFR for patients with type 1 diabetes.

## 02b_Precard_Tables_publication.Rmd

In this section, the code generates relevant statistics, model tables, and figures that are suitable for publication. The results are formatted to meet publication standards and provide clear insights into the analysis outcomes.

## 02c_Precard_Supplementary_publication.Rmd

This part of the project focuses on producing supplementary tables and figures for secondary outcomes. These supplementary materials enhance the publication by providing additional context and insights.

# Publication

For more details on the findings and outcomes of this project, please refer to the [publication](https://pubmed.ncbi.nlm.nih.gov/36044663/).

# Project Details

For detailed explanations of the code and processes, please refer to the comments within each file.
For any questions or inquiries about the project, please contact Tasos Mangelis (amangelis@hotmail.com).

---

# 0. Master Script

# Load all required packages
library(tidyverse) # tidyverse includes ggplot2
library(anthro) # For calculation of Z-Scores
library(car) # For VIF Analysis
library(broom) # For tidy function
library(pROC) # For ROC Curves
library(gtsummary) # For individual assignment tables
library(dplyr) # For individual assignment tables

# Run the data cleaning and processing
source('scripts/1_data_cleaning_processing.R')

# Run exploratory analysis codes, including all the tests and their results
source('scripts/2_exploratory_analysis.R')

# Run model building codes
source('scripts/3_statistical_models.R')

# Run the interaction analysis of residence vs. all variables in the final model
source('scripts/4_interaction_analysis.R')

# Visualization codes already in separate scripts

#Run the table and visualisation codes for the individual assignment
source('scripts/5_individual_assignment.R')
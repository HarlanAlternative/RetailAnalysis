# RetailAnalysis

## Project Overview

This repository contains a retail data analysis project for the STATS 765 course.  
The project uses R to process, explore, and analyse retail-related data, with a focus on data preparation, variable relationships, and sales forecasting ideas.

## Repository Structure and Code Description
'''
RetailAnalysis/
├── Reanalyze/
├── sales_forecasting_model/
├── DataProcess.R
├── ForCorr.R
└── README.md
'''

## Code Description

### DataProcess.R

This script is responsible for data processing and initial exploratory analysis and forms the core data preparation stage of the project.  
It works with multiple retail-related data sources and focuses on:

- Cleaning and organising raw data, including handling missing values and variable selection  
- Merging data from different sources based on business logic to construct a structured panel-style dataset  
- Exploring basic trends and patterns in key variables  
- Preparing structured data for subsequent correlation analysis and forecasting tasks  

The emphasis of this script is on data logic and preparation rather than complex modelling.

### ForCorr.R

This script focuses on correlation analysis between variables.  
It is used to explore statistical relationships between retail-related variables and sales indicators, providing insights into potential influencing factors.

### sales_forecasting_model/

This folder contains code and related files for sales forecasting.  
It represents an attempt to model and analyse time-based changes in sales data, with the focus on modelling ideas and comparison of results rather than optimising model performance.

### Reanalyze/

This folder is used for revisiting and refining earlier analyses.  
It may include additional checks, adjustments, or alternative views of previously obtained results.

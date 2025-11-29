# Crash statistics analysis

This project is aimed at exploring the crash statistics and understand where the crash recurrence occurs.

## What each script contains

1.  ***main.R*** : this scripts loads the CAS data and does cleaning. Mainly using latest shapefile to clean region column. All the other scripts reads the cleaned data from main.R
2.  **exploratory_data_analysis.R** : This script uses the cleaned data and runs exploratory data analysis mainly time-series analysis by region, crash types, roadways and meshblock
3.  **regional_analysis.R** : this script also uses the cleaned CAS data and creates Maps and scatter plots
4.  **plot_function.R :** plot functions that has been reused

## Instructions for running

Kindly note that CAS data and statsnz-regional-council-2025-SHP files and renv libraries have been gitignored because they were heavy files. please download the data to run the script

## Project overview

This project uses **renv** to ensure full reproducibility

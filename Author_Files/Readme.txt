# -------------------------------------------------- 
# Readme
# -------------------------------------------------- 

Author: Abhit Bhandari

Title: “Political Determinants of Economic Exchange: Evidence from a Business Experiment in Senegal”

This document describes the contents of the replication archive that reproduces the paper’s reported results. All files were compiled using macOS Catalina (10.15.2) and R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night.”

The packages used in the R scripts are located at the beginning of each script, along with code that will detect missing R packages and install them if necessary.

All data included in this replication archive was originally collected by the author, who has the right to share it.


# -------------------------------------------------- 
# Archive contents
# -------------------------------------------------- 

Source — folder containing the source data and script necessary to produce the final analysis dataset. The folder contains the following files:

	⁃	“data_source.RData” — R data frame containing the source data.
	⁃	“prepare_analysis_dataset.R” — R script that prepares the source data for analysis and generates the data frame “data_analysis.RData.”
 
Analysis — folder containing the analysis dataset and analysis code. The folder contains the following files:

	⁃	“data_analysis.RData” — R data frame containing the analysis data that was produced using “prepare_analysis_dataset.R.”
	⁃	“analysis.R” — R script that produces the results of the paper.

“PAPS RCT Codebook.pdf” — Codebook describing the variables in “data_analysis.RData.”
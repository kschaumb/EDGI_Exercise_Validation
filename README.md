# EDGI_Exercise_Validation

Replication Instructions: 

In order to replicate analysis in the validation paper, complete the following steps -- 

1. Pull/Copy the ENTIRE Repository and set it up on your computer. Pick out a spot in which it is OK to place data 
2. Make a 'data' folder in the head of the repository. Place your RAW data file here. It is important that the the variable names are all totally raw (straight from EDGI data download), and there has not been pre-processing that has removed any of the original variable names. You will need ONLY the following scales in the raw data file (though fine if you have more): ED100k, OCI12, FrostMPS, EDEQ, and CET. 
3. Get your R environment set up. Open the 'EDGI_Exercise_Analyses.RProj' file. The first thing you will want to do is download the 'renv' package `install.packages('renv')`. Once you have the renv package installed, run renv::restore() to restore all of the packages needed for analysis on your computer. You should only need to got through this process once. If `renv::restore()` is getting stuck, you may need to install a package directly:  `(install.packages('package'))` for most things, or `library(remotes)` followed by `install_github(embark-lab/scorekeeper)` for the scorekeeper package. 
4. ALWAYS have the 'EDGI_Exercise_Analyses.RProj' file open! The file paths assume that you are working inside of a project
5. Navigate to and open 'validataion_paper/R/Scoring.R'. Put the name of your raw data file in line 8. The example raw data file that I have here is 'EDGI_USA_raw.sav'. I am using having and the `read_sav` function to read in the data. If you are getting stuck trying to read in the data, ask for help. 
6. Name your cohort in line 11
7. Run the entire scoring script. 
8. Open 'validation_paper/R/Analysis.R' -- name your cohort in line 3. Run the entire analysis script. Analysis should take < 1 minute total. All figures and tables should now be output in the figs/ and tabs/ folders with your cohort name appended to the end of those files

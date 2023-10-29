# EDGI_Exercise_Validation

Replication Instructions: 

In order to replicate analysis in the validation paper, complete the following steps -- 

1. Pull/Copy the ENTIRE Repository and set it up on your computer (either fork and pull from GitHub, or, on the repository page, click on code, download zip, and download and unzip the file). Pick out a spot in which it is OK to place data. Do not change the folder structure. Keep all of the existing folders. For introduction to Git, see [GitHub Demo](https://www.youtube.com/watch?v=wmt7kFKUX-4&list=PLj0MKOezmHuAuiYX9ShaVDlHQLEJMoS_H&index=4&t=532s)


2. Make a 'data' folder in the head of the repository. Place your RAW data file here. It is important that the the variable names are all totally raw (straight from EDGI data download), and there has not been pre-processing that has removed any of the original variable names. You will need ONLY the following scales in the raw data file (though fine if you have more): ED100k, OCI12, FrostMPS, EDEQ, and CET. 


4. ALWAYS have the 'EDGI_Exercise_Analyses.RProj' file open and make sure you are working inside of it. The file paths assume that you are working inside of a project.


5. Navigate to and open 'validataion_paper/R/Scoring.R'. Put the name of your raw data in the file at the correct line. The example raw data file that I have here is 'EDGI_USA_raw.sav'. I am using the `read_sav` function to read in the data. If you are getting stuck trying to read in the data, ask for help. 


6. Name your cohort in the scoring file 


7. Run the entire scoring script. If you get an error - make sure all of your raw variables match the EDGI US raw variables (including response options). If your raw variables do not *perfectly* match the US raw variables, you may need to edit the scoresheet: EDGI_exercise_scoresheet.xlsx file to match your file. Ask for help if you get stuck. 


8. Open 'validation_paper/R/Analysis.R' -- name your cohort in line 3. Run the entire analysis script. Analysis should take < 1 minute total. All figures and tables should now be output in the figs/ and tabs/ folders with your cohort name appended to the end of those files. Look at the figs/ folder to compare USA figures to your cohort's figures to verify that things are running as expected. Once you have updated results in the tabs/ folder and updated figs in the figs/ folder, push your results back to github and send a pull request so that I can integrate changes. (you can also send the results file and figures in a zip file over dropbox or email). 

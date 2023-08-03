# Table of Contents
1. [General Info](#general-info)
2. [Instructions](#instructions)
3. [Data Tabs](#data-tabs)
4. [FAQs](#faqs)

## General Info
- This is an R Shiny App that allows you to tidy Purple Air PM2.5 Data that has been downloaded from an SD card from an offline Purple Air sensor and Download into .csv format 
- This code will only work with data exported from an SD card and not data downloaded from the Purple Air Map.
- Data Columns for SD data are different from Map columns more info here: https://community.purpleair.com/t/sd-card-file-headers/279
- Corrected data uses US EPA's Simple Correction Factor which is described in detail here: https://www.mdpi.com/1424-8220/22/24/9669
- The data will be corrected and only show the columns that are necessary to be compared to FEM PM instruments. 


## Instructions
**Please Note:** App will not work correctly in R studio's native web launcher please change settings in Run App -> set to run External or select open in Browser when running it in R studio. 
1. Download the Repository and Make sure R, R studio, and all packages are installed in order for the App to properly function
2. Run the App in your normal web browser and browse your computer for the Raw data files (multiple files can be selected at one time)
3. Select the Timezone the Data will be in. Additionally, Check off the Smoke Conversion if your data contains high PM2.5 (smoke).
4. Select which Data will be uploaded and it will combined into one data file. Click Download and rename as you would like.

## Data Tabs
This is Information on what has been done in each of the Data Tabs that can be downloaded. 
- Raw Data: Raw Data combined into one file with no changes or corrections to the data
- Tidy Data: Converted to selected Time Zone with Both A & B Channels along with Meterology data but no Corrections
- Average Data: Averages 2 minute Data into 60 minute Averages 
- Corrected Data: Corrects data based on QC Criteria of 70% data completeness using EPA Correction factor using both channels to generate a single PM2.5 Value
- Frequency Data: Shows the number of 2 minute data points taken in each Hour.
- File Names: Shows a Table with the File names that were selected in the Upload Phase. Useful for debugging
- Data Complete: Shows the amount of complete data between the first and last dates contained in the data upload 
- Output: Show Raw Data output from R Console. Useful for debugging 

## FAQs
1. Why does my data produces an error when reading in my raw data files?
> A: There are many sources of error for this problem including: Data not in right file format (ie. not .csv), Contains corruption in the data file, or browser incompability (make sure you are using a web Browser!). If errors mentions a specific file you can look in the File Names tab to identify what file is giving you the error and attempt to correct it.
2. What data format does the App support?
> A: Input data must be in .csv format and download data will be .csv as well
3. Is there a list of exact names for the columns of each dataframe including units?
> A: Yes! please view the Column test.txt file on the what the column names mean and their containing units
4. What is the file limit/size for uploading data?
> A: The file limit size is based on your memory limit of your device. For best results It is recomended to only do a few months at a time when importing data (less than 10,000 measurements) to make viewing data easier.  
5. What is the Recommended Data Table/ Column to get for Data analysis?
> A: I recommend downloading the Corrected Data Frame and using the 'PM_corr_EPA' column along with 'date' to compare to a FEM instrument
6. When would you recommend using the Smoke Conversion option?
> A: I recommend reviewing the EPA Correction Equation Paper on the specifics of what levels have different corrections for Purple Air Data.
7. I have and issue/request/question about the app, how can I contact you?
> A: Please contact me through Github by creating a new issue and labeling it appropriately and I will try to get back to you ASAP.

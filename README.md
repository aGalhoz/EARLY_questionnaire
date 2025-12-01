# Code for the EARLY-ALS questionnaire study

Explanation of what happens in each script:

* Script 00, load of libraries, raw data.
* Script 01, creation of datasets of ALS only questions, CTR only questions and questions in common between ALS and CTR. Creation of summarised info for Supplementary Table 1.
* Script 02, transformation of data for the univariate analyses. In short, basically binary data of yes/no are changed to 0/1, numerical data is saved as numeric, date-time information is changed to amount of years, etc. In addition, imputation of binary questions (0/1) and amount of visits per healthcare specialist.
* Script 03, heatmap visualisations before and after imputation.
* Script 04, univariate analyses with or without adjustment for gender, and stratified by female and male.
* Script 05, creation of final datasets for Supplementary Table 2 with univariate results for sex-adjusted and sex-stratified.
* Script 06, forest plots of univariate analyses per category.
  

To update:
* Script 05, statistics of clinical factors (diff in bulbar/spinal, age, sex, age at onset, ...) for the demographics table. Also relevant for you.
* Script 07, analyses of open answers of non-motor symptoms (think this you mentioned you would not do, since you have a low amount of IDs)
* Script 08, it's called boxplots, but performes both boxplots for continuous/categorical variables, as well as, some of the barplots in the manuscript.
* Script 09, there are two files, one performing the ssame as 06 but stratified by gender, and another one checks for enrichment of answers in ALS bulbar/spinal (prob not relevant for you)
* Script 10, timeline plots of weight, BMI

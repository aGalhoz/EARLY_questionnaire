# Code for the EARLY-ALS questionnaire study

Explanation of what happens in each script:

* Script 00, load of libraries, raw data.
* Script 01, creation of datasets of ALS only questions, CTR only questions and questions in common between ALS and CTR. Creation of summarised info for Supplementary Table 1.
* Script 02, transformation of data for the univariate analyses. In short, basically binary data of yes/no are changed to 0/1, numerical data is saved as numeric, date-time information is changed to amount of years, etc. In addition, imputation of binary questions (0/1) and amount of visits per healthcare specialist.
* Script 03, heatmap visualisations before and after imputation.
* Script 04, univariate analyses with or without adjustment for gender, and stratified by female and male.
* Script 05, creation of final datasets for Supplementary Table 2 with univariate results for sex-adjusted and sex-stratified.
* Script 06, forest plots of univariate analyses per category.
* Script 07, statistics of clinical factors (diff in bulbar/spinal, age, sex, age at onset, ...) for the demographics table.
* Script 08, analyses of open answers of non-motor symptoms.
* Script 09, barplots visualisations for several categories.
* Script 10, timeline plots for healthcare visits and weight.
* Script 11, spinal/bulbar onset univariate analyses.

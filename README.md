# Code for the EARLY-ALS questionnaire study

Explanation of what happens in each script:

* Script 00, load of libraries, raw data, creation of datasets of ALS only questions, CTR only questions and questions in common between ALS and CTR. You can modify this code to your task to be SMA and CTR groups
* Script 01, transformation of data for the univariate analyses. In short, basically binary data of yes/no are changed to 0/1, numerical data is saved as numeric, date-time information is changed to amount of years, etc. This script is necessary for you before you perform univariate analyses
* Script 02, is the imputation of binary questions (0/1) and amount of visits per healthcare specialist. You can use this code as it is, and is also necessary for your analyses
* Script 03, performs univariate analyses without considering subcategories within each category.
* Script 04, same as 03 but stratified by female/male
* Script 05, statistics of clinical factors (diff in bulbar/spinal, age, sex, age at onset, ...) for the demographics table. Also relevant for you.
* Script 06, univariate analyses considering subcategories within each category
* Script 07, analyses of open answers of non-motor symptoms (think this you mentioned you would not do, since you have a low amount of IDs)
* Script 08, it's called boxplots, but performes both boxplots for continuous/categorical variables, as well as, some of the barplots in the manuscript.
* Script 09, there are two files, one performing the ssame as 06 but stratified by gender, and another one checks for enrichment of answers in ALS bulbar/spinal (prob not relevant for you)
* Script 10, timeline plots of weight, BMI
* Script 11, this you can ignore, was a work we ended up not using
* Script 12, final merge of all datasets from 06 and 09 for supplementary tables
* Script 13, forest plots of univariate results

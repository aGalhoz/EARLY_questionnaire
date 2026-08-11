# EARLY-ALS: Prodromal and Presymptomatic Amyotrophic Lateral Sclerosis Questionnaire Analysis

## Description

This repository contains the analysis code for the manuscript *EARLY-ALS: A Multicentre Study on Presymptomatic and Prodromal Amyotrophic Lateral Sclerosis*, submitted to **eBioMedicine**.

The study compares 475 ALS patients with 285 controls recruited across 20 ALS expert centres in Germany and Switzerland, based on a structured digital questionnaire capturing prodromal complaints, healthcare utilisation, comorbidities, lifestyle factors, and weight changes during the years preceding ALS symptom onset.

## Usage

This project was conducted in [R software](https://www.r-project.org). All required packages and dependencies are listed and installed in `00_packages.R`.

Scripts are numbered in the order they should be run, as each builds on objects created by the previous ones:

| Script | Description |
|---|---|
| `00_packages.R` | Installs and loads all required CRAN and GitHub package dependencies. |
| `01_data_preprocessing.R` | Cleans and harmonises the raw questionnaire export (ALS and control datasets): standardises missing-value codes, converts German yes/no responses to binary, and summarises response and completion rates per question. |
| `02_data_transformation.R` | Transforms cleaned variables into their final analysis-ready types (binary, categorical, date/duration), applies missing-value imputation, and assembles the combined ALS/control dataset used by all downstream analyses. |
| `03_heatmap.R` | Generates heatmaps of questionnaire missingness before and after imputation. |
| `04_univariate_by_category.R` | Core univariate analysis functions: fits logistic regression models (GLM) for each questionnaire item against ALS/control status, including sex-stratified and adjusted variants (e.g. age, centre). |
| `05_univariate_datasets.R` | Assembles and formats the univariate results (full sample, female, male) into the final per-category datasets used for plotting. |
| `06_forest_plots.R` | Generates forest plots of odds ratios for questionnaire categories and subcategories, stratified by sex. |
| `07_demographics.R` | Computes demographic and clinical summary statistics, including ALSFRS-R scores and age distributions. |
| `08_open_answers.R` | Processes and categorises free-text responses to the open-ended prodromal-complaints question, and generates the corresponding pie charts. |
| `09_barplots.R` | Generates bar plots of questionnaire responses across categories (e.g. education, physical activity, substance use), including time-course and sex-stratified versions. |
| `10_timeline.R` | Generates timeline plots of healthcare visit frequency (e.g. neurology, speech therapy) across the prodromal period. |
| `11_subanalysis.R` | Subgroup analyses stratified by site of onset (bulbar vs spinal ALS). |
| `12_subanalyses_new_adjustments.R` | Extended univariate analyses with additional adjustment strategies (e.g. sex and centre, age exclusion) as sensitivity analyses. |
| `13_subanalysis_extra.R` | Additional subgroup analyses stratified by age at onset, disease duration, and progression rate, including sensitivity analyses on incomplete survey submissions. |

Plots are written to a local `plots/` directory and intermediate/result tables to `data code output/`, which are created by the scripts as needed.

## Contact

For any inquiries related to this work, please contact ana.galhoz at helmholtz-munich.de.

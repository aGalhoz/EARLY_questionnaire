# data from original until ready for ML

install.packages("colorRamp2")
install.packages("devtools")
install_github("jokergoo/ComplexHeatmap")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
devtools::install_github("NightingaleHealth/ggforestplot")
install.packages("wesanderson")
library(readr)
library(dplyr)
library(colorRamp2)
library(devtools)
library(ComplexHeatmap)
library(readxl)
library(ggplot2)
library(tibble)
library(ggrepel)
library(forestplot)
library(stringr)
library(ggpubr)
library(reshape2)
library(ggforestplot)
library(forcats)
library(wesanderson)
library(magrittr)
library(cowplot)
library(tidyverse)
library(ggpattern)
library(lme4)
library(emmeans)
library(data.table)
library(summarytools)
library(zoo)
library(skimr)

# # -> new data (end of 2022)
# data_patients_new <- read_csv("original data/20220908_results-survey529321-EARLY_ALS_Pat.csv")
# data_control_new <- read_csv("original data/20220908_results-survey333685-EARLY_ALS_Ang.csv")
# 
# # -> new data (2023)
# data_patients_new_2023 <- read_csv("ALS_0803_original_data/EARLY-ALS für Patienten (529321)_results-survey529321.csv")
# data_control_new_2023 <- read_csv("ALS_0803_original_data/EARLY-ALS für Angehörige (333685)_results-survey333685.csv")

# -> newest data (2024)
data_patients_new_2024 <- read_csv("original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Patienten _results-survey529321.csv")
data_control_new_2024 <- read_csv("original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Angehörige_results-survey333685.csv")

# # -> old data
# data_patients_old <- read_csv("original data/20220127-results-survey529321-EARLY_ALS_Pat.csv")
# data_control_old <- read_csv("original data/20220127-results-survey333685-EARLY_ALS_Ang.csv")

# old data with translation
data_patients_old_en <- read_csv("data input/ALS_Patient_Original.csv")
data_control_old_en <- read_csv("data input/ALS_Control_Original.csv",locale=locale(encoding="latin1"))

# -> new data with columns translated 
original_col_ALS <- colnames(data_patients_new_2024)
original_col_CTR <- colnames(data_control_new_2024)
en_col_ALS <- unlist(lapply(colnames(data_patients_old_en) %>%
  strsplit(split = ".",fixed = TRUE),function(x) x[1]))
en_col_CTR <- unlist(lapply(colnames(data_control_old_en) %>%
                              strsplit(split = ".",fixed = TRUE),function(x) x[1])) 
map_col_ALS <- data.frame(col_DE = original_col_ALS,
                          col_EN = en_col_ALS)
map_col_CTR <- data.frame(col_DE = original_col_CTR,
                          col_EN = en_col_CTR)

# save original data
#data_patients_original_new <- data_patients_new 
data_patients_original_new_2024 <- data_patients_new_2024
data_contol_original_new_2024 <- data_control_new_2024

# data with aberrant dates of 1920, 1921, etc. altered to 2020, 2021, etc. 
data_patients_new_2024 <- read_delim("original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Patienten _results-survey529321 copy.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_control_new_2024 <- read_delim("original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Angehörige_results-survey333685 copy.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# new birth date from Tear study
TEAR_study <- read_csv("data input/20240517_TEAR_ALS_results-survey557312.csv")
TEAR_study_tmp <- TEAR_study %>%
  select(Zugangscode,`Geburtsmonat und -jahr des Patienten`,`Geburtsmonat und -jahr des Angehörigen`)
Tokens_EARLY_TEAR <- read_excel("data input/Tokens_EARLY_TEAR.xlsx")
data_patients_new_2024_temp <- data_patients_new_2024 %>%
  left_join(Tokens_EARLY_TEAR %>%
              select(Patienten_Token_EARLY,
                     Patienten_Token_TEAR) %>%
              rename(Zugangscode = Patienten_Token_EARLY)) %>%
  left_join(TEAR_study_tmp %>%
              rename(Patienten_Token_TEAR = Zugangscode))
data_patients_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <- ifelse(is.na(data_patients_new_2024_temp$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`),
                                                                                                   data_patients_new_2024_temp$`Geburtsmonat und -jahr des Patienten`,
                                                                                                   data_patients_new_2024_temp$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`)
TEAR_study[which(TEAR_study$Zugangscode %in% data_patients_new_2024_temp$Patienten_Token_TEAR),"Geburtsmonat und -jahr des Patienten"] # 374 patients in common (107 with not NA date)
na.omit(TEAR_study$`Geburtsmonat und -jahr des Patienten`) #200 patients without NA date

data_control_new_2024_temp <- data_control_new_2024 %>%
  left_join(Tokens_EARLY_TEAR %>%
              select(Angehörige_Token_TEAR,
                     Angehörige_Token_EARLY) %>%
              rename(Zugangscode = Angehörige_Token_EARLY)) %>%
  left_join(TEAR_study_tmp %>%
              rename(Angehörige_Token_TEAR = Zugangscode))
data_control_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <- ifelse(is.na(data_control_new_2024_temp$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`),
                                                                                             data_control_new_2024_temp$`Geburtsmonat und -jahr des Angehörigen`,
                                                                                             data_control_new_2024_temp$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`)

TEAR_study[which(TEAR_study$Zugangscode %in% data_control_new_2024_temp$Angehörige_Token_TEAR),"Geburtsmonat und -jahr des Angehörigen"] #241 patients in common (56 with not NA date)
na.omit(TEAR_study$`Geburtsmonat und -jahr des Angehörigen`) #106 controls without NA date

# new birth date from December 2024 (several centers contacted)
Referenzliste_Rostock <- read_excel("data input/Referenzliste_Rostock.xlsx")
DOB_dates_new <- read_excel("~/Documents/HMGU/ALS_Project/ALS questionnaire/data input/EARLY_DOB_all_Rostock.xlsx")
Referenzliste_Rostock_interest <- Referenzliste_Rostock[Referenzliste_Rostock$`Patienten/Probanden ID` %in% DOB_dates_new$Patienten_Probanden_ID,] %>%
  select(`Patienten/Probanden ID`,`DOB Patient`,`DOB Angehöriger`)
DOB_dates_new[DOB_dates_new$Patienten_Probanden_ID %in% Referenzliste_Rostock$`Patienten/Probanden ID`,]$DOB_Pat_clean <- format(as.Date(Referenzliste_Rostock_interest$`DOB Patient`,"%Y.%m.%d"),"%d.%m.%Y")
DOB_dates_new[DOB_dates_new$Patienten_Probanden_ID %in% Referenzliste_Rostock$`Patienten/Probanden ID`,]$DOB_Angehoerige_clean <- format(as.Date(Referenzliste_Rostock_interest$`DOB Angehöriger`,"%Y.%m.%d"),"%d.%m.%Y")
# -> new dates from Cologne
DOB_dates_Koln = read_excel("data input/Referenzliste EARLY & TEAR-ALS_Cologne.xlsx", 
                            col_types = c("numeric", "date", "date"))
DOB_dates_new <- rbind(DOB_dates_new,
                       data.frame(Center = rep("Köln",21),
                                  Patienten_Probanden_ID = DOB_dates_Koln$`Patienten/Probanden ID`,
                                  DOB_Pat_clean = format(DOB_dates_Koln$`DOB Patient`,"%d.%m.%Y"),
                                  DOB_Angehoerige_clean = format(DOB_dates_Koln$`DOB Angehöriger`,"%d.%m.%Y")))
data_control_new_2024_temp2 <- data_control_new_2024 %>%
  select(`Antwort ID`,Zugangscode,`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`,`Datum Abgeschickt`) %>%
  dplyr::left_join(Tokens_EARLY_TEAR %>%
              select(Angehörige_Token_EARLY,
                     Patienten_ID) %>%
              dplyr::rename(Zugangscode = Angehörige_Token_EARLY)) %>%
  left_join(DOB_dates_new %>%
              dplyr::rename(Patienten_ID = Patienten_Probanden_ID) %>%
              select(Patienten_ID,DOB_Angehoerige_clean))
data_control_new_2024_temp2$DOB_Angehoerige_clean <- format(as.Date(data_control_new_2024_temp2$DOB_Angehoerige_clean,
                                                                    "%d.%m.%Y"),"%m/%Y")
data_control_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <- ifelse(is.na(data_control_new_2024_temp2$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`),
                                                                                             data_control_new_2024_temp2$DOB_Angehoerige_clean,
                                                                                             data_control_new_2024_temp2$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`)

data_patients_new_2024_temp2 <- data_patients_new_2024 %>%
  select(`Antwort ID`,Zugangscode,`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`,`Datum Abgeschickt`) %>%
  dplyr::left_join(Tokens_EARLY_TEAR %>%
                     select(Patienten_Token_EARLY,
                            Patienten_ID) %>%
                     dplyr::rename(Zugangscode = Patienten_Token_EARLY)) %>%
  left_join(DOB_dates_new %>%
              dplyr::rename(Patienten_ID = Patienten_Probanden_ID) %>%
              select(Patienten_ID,DOB_Pat_clean))
data_patients_new_2024_temp2[data_patients_new_2024_temp2$`Antwort ID` == 610,]$DOB_Pat_clean <- "01.01.1973"
data_patients_new_2024_temp2[data_patients_new_2024_temp2$`Antwort ID` == 590,]$DOB_Pat_clean <- "01.01.1974"
data_patients_new_2024_temp2[data_patients_new_2024_temp2$`Antwort ID` == 540,]$DOB_Pat_clean <- "01.01.1956"
data_patients_new_2024_temp2$DOB_Pat_clean <- format(as.Date(data_patients_new_2024_temp2$DOB_Pat_clean,
                                                                    "%d.%m.%Y"),"%m/%Y")
data_patients_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <- ifelse(is.na(data_patients_new_2024_temp2$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`),
                                                                                             data_patients_new_2024_temp2$DOB_Pat_clean,
                                                                                             data_patients_new_2024_temp2$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`)

# -> extract only ALS patients of interest
data_control_new <- data_control_new_2024 # 410 CTR patients
data_patients_new <- data_patients_new_2024 %>%
  filter(`Welche Diagnose wurde bei Ihnen gestellt?` == "Amyotrophe Lateralsklerose (ALS)")
data_patients_new <- data_patients_new[c(1:6,8:514),]  # 513 ALS patients

# Patiens with SMA and HSP for Isabell
data_patients_SMA_HSP <- data_patients_new_2024 %>%
  filter(grepl("HSP|SMA", `Welche Diagnose wurde bei Ihnen gestellt?`))
writexl::write_xlsx(data_patients_SMA_HSP,"data_patients_SMA_HSP.xlsx")
writexl::write_xlsx(data_control_new_2024,"data_controls.xlsx")


# -> extract only CTR patients connected to ALS
data_control_new <- data_control_new[data_control_new$Zugangscode %in% data_patients_new$Zugangscode,] 
#data_patients_new <- data_patients_new[data_patients_new$Zugangscode %in% data_control_new$Zugangscode,]
data_control_new <- data_control_new[c(1:5,7:306),] # 305 CTR patients, there is another repetitive control but has different answers

## check patients that did not finish the questionnaire (ignore this step now)
# # how many patients with NA in Datum abgeschickt
data_patients_new_NA <- data_patients_new %>%
  filter(is.na(`Datum Abgeschickt`)) # 38
data_control_new_NA <- data_control_new %>%
  filter(is.na(`Datum Abgeschickt`)) # 20

# # heatmap of the patients and controls without NA in Datum abgeschickt 
# data_patients_new <- data_patients_new[!data_patients_new$`Antwort ID` %in% data_patients_new_NA$`Antwort ID`,] # 476
# data_control_new <- data_control_new[!data_control_new$`Antwort ID` %in% data_control_new_NA$`Antwort ID`,] # 369

writexl::write_xlsx(data_patients_new,"data code output/data_patients_noimputation_completed.xlsx")
writexl::write_xlsx(data_control_new,"data code output/data_control_noimputation_completed.xlsx")

# more filtering: change entries with aberrant values, imputation, new heatmaps, new final data 17.03.24
# get mapping between original columns in ALS and original columns in CTR
map_col_ALS_CTR <- read_excel("data code output/final_table_questionnaire_new_deprecated.xlsx")
map_col_ALS_CTR <- map_col_ALS_CTR %>%
  dplyr::select(`original question (ALS)`,`original question (CTR)`,
                `question in EN (ALS)`,`question in EN (CTR)`)
map_col_ALS_CTR$`original question (CTR)` <- ifelse(is.na(map_col_ALS_CTR$`original question (CTR)`),
                                                    map_col_ALS_CTR$`original question (ALS)`,
                                                    map_col_ALS_CTR$`original question (CTR)`)
map_col_ALS_CTR$`question in EN (CTR)` <- ifelse(is.na(map_col_ALS_CTR$`question in EN (CTR)`),
                                                 map_col_ALS_CTR$`question in EN (ALS)`,
                                                 map_col_ALS_CTR$`question in EN (CTR)`)

# check common columns between ALS patients and controls (281 common columns)
common_col_ALS <- which(map_col_ALS$col_EN %in% map_col_ALS_CTR$`question in EN (ALS)`)
common_col_CTR <- which(map_col_CTR$col_EN %in% c(map_col_ALS_CTR$`question in EN (CTR)`,"caffeinestop")) 

# separate data by common and only ALS / CTR questions
data_patients_new_common <- data_patients_new[,common_col_ALS]
data_patients_new_ALS_only <- data_patients_new[,which(!map_col_ALS$col_EN %in% map_col_ALS_CTR$`question in EN (ALS)`)]
data_control_new_common <- data_control_new[,common_col_CTR]
data_control_new_CTR_only <- data_control_new[,which(!map_col_CTR$col_EN %in% c(map_col_ALS_CTR$`question in EN (CTR)`,"caffeinestop"))]

# amount of answered questions per cohort
data_patients_new_common_NA <- apply(data_patients_new_common,
                                     2,
                                     function(x) {
                                       ifelse((is.na(x) | x == "N/A" | x == "NA"),0,1)})
data_patients_new_ALS_only_NA <- apply(data_patients_new_ALS_only,
                                       2,
                                       function(x) {
                                         ifelse((is.na(x) | x == "N/A" | x == "NA"),0,1)})
data_control_new_common_NA <- apply(data_control_new_common,
                                    2,
                                    function(x) {
                                      ifelse((is.na(x) | x == "N/A" | x == "NA"),0,1)})
data_control_new_CTR_NA <- apply(data_control_new_CTR_only,
                                    2,
                                 function(x) {
                                   ifelse((is.na(x) | x == "N/A" | x == "NA"),0,1)})
library(glue)
frac_patients_common <- data.frame(frac_answered_ALS = apply(data_patients_new_common_NA, 
                                                2, 
                                                function(x) {
                                                  x <- as.numeric(x)
                                                  glue("{ sum(x !=0) }/{ length(x) }") 
                                                }),
                                   total_answered_ALS = apply(data_patients_new_common_NA, 
                                                          2, 
                                                          function(x) {
                                                            x <- as.numeric(x)
                                                            sum(x!=0) 
                                                          }),
                                   col = colnames(data_patients_new_common_NA)) 
frac_patients_only <- data.frame(frac_answered = apply(data_patients_new_ALS_only_NA, 
                                                2, 
                                                function(x) {
                                                  x <- as.numeric(x)
                                                  glue("{ sum(x !=0) }/{ length(x) }") 
                                                }),
                                 total_answered = apply(data_patients_new_ALS_only_NA, 
                                                        2, 
                                                        function(x) {
                                                          x <- as.numeric(x)
                                                          sum(x!=0) 
                                                        }),
                                 col = colnames(data_patients_new_ALS_only_NA)) 
frac_control_common <- data.frame(frac_answered_CTR = apply(data_control_new_common_NA, 
                                                         2, 
                                                         function(x) {
                                                           x <- as.numeric(x)
                                                           glue("{ sum(x !=0) }/{ length(x) }") 
                                                         }),
                                   total_answered_CTR = apply(data_control_new_common_NA, 
                                                          2, 
                                                          function(x) {
                                                            x <- as.numeric(x)
                                                            sum(x!=0) 
                                                          }),
                                  col = colnames(data_control_new_common_NA)) 
frac_control_only <- data.frame(frac = apply(data_control_new_CTR_NA, 
                                              2, 
                                              function(x) {
                                                x <- as.numeric(x)
                                                glue("{ sum(x !=0) }/{ length(x) }") 
                                              }),
                                 total_answered = apply(data_control_new_CTR_NA, 
                                                        2, 
                                                        function(x) {
                                                          x <- as.numeric(x)
                                                          sum(x!=0) 
                                                        }),
                                col = colnames(data_control_new_CTR_NA))  

# -> make final datasets and save them
frac_both_common <- cbind(frac_patients_common %>%
                            dplyr::mutate(`original question (ALS)` = col) %>%
                            select(-col), 
                          frac_control_common %>%
                            dplyr::mutate(`original question (CTR)` = col) %>%
                            select(-col)) %>%
  arrange(desc(total_answered_ALS),desc(total_answered_CTR))
frac_both_common <- merge(frac_both_common,
                          map_col_ALS,
                          by.x = "original question (ALS)",by.y = "col_DE") %>%
  arrange(desc(total_answered_ALS),desc(total_answered_CTR))
frac_ALS <- merge(frac_patients_only,
                  map_col_ALS,
                  by.x = "col",by.y = "col_DE") %>%
  dplyr::mutate(`original question (ALS)` = col) %>%
  select(-col) %>%
  arrange(desc(total_answered))
frac_CTR <- merge(frac_control_only,
                  map_col_CTR,
                  by.x = "col",by.y = "col_DE") %>%
  dplyr::mutate(`original question (CTR)` = col) %>%
  select(-col) %>%
  arrange(desc(total_answered))

# amount of positive answered questions per cohort (if yes/no Q)
fun_binary <- function(i){
  value <- na.omit(unique(i))
  value <- value[value != "N/A"]
  if(length(value) %in% c(1,2)){
    if(value[1] %in% c("Ja", "Nein")){
      i = ifelse(i == "Ja",1,0)
    }
  }
  return(i)
}
data_patients_new_common_binary <- apply(data_patients_new_common,
                                     2,
                                     function(x) fun_binary(x))
data_patients_new_ALS_only_binary <- apply(data_patients_new_ALS_only,
                                       2,
                                       function(x) fun_binary(x))
data_control_new_common_binary <- apply(data_control_new_common,
                                    2,
                                    function(x) fun_binary(x))
data_control_new_CTR_binary <- apply(data_control_new_CTR_only,
                                 2,
                                 function(x) fun_binary(x))
frac_positive_patients_common <- data.frame(frac_positive_answered_ALS = apply(data_patients_new_common_binary, 
                                                             2, 
                                                             function(x) {
                                                               x <- x[!is.na(x) & x != "N/A"]
                                                               #x <- as.numeric(x)
                                                               glue("{ sum(x == 1) }/{ length(x) }") 
                                                             }),
                                            total_positive_answered_ALS = apply(data_patients_new_common_binary, 
                                                                       2, 
                                                                       function(x) {
                                                                         x <- as.numeric(x)
                                                                         sum(x==1) 
                                                                       }),
                                   col = colnames(data_patients_new_common_binary)) 
frac_positive_patients_only <- data.frame(frac_positive_answered = apply(data_patients_new_ALS_only_binary, 
                                                       2, 
                                                       function(x) {
                                                         x <- as.numeric(x)
                                                         glue("{ sum(x == 1) }/{ length(x) }") 
                                                       }),
                                          total_positive_answered = apply(data_patients_new_ALS_only_binary, 
                                                                              2, 
                                                                              function(x) {
                                                                                x <- as.numeric(x)
                                                                                sum(x==1) 
                                                                              }),
                                 col = colnames(data_patients_new_ALS_only_binary)) 
frac_positive_control_common <- data.frame(frac_positive_answered_CTR = apply(data_control_new_common_binary, 
                                                            2, 
                                                            function(x) {
                                                              x <- as.numeric(x)
                                                              glue("{ sum(x == 1) }/{ length(x) }") 
                                                            }),
                                           total_positive_answered_CTR = apply(data_control_new_common_binary, 
                                                                               2, 
                                                                               function(x) {
                                                                                 x <- as.numeric(x)
                                                                                 sum(x==1) 
                                                                               }),
                                  col = colnames(data_control_new_common_binary)) 
frac_positive_control_only <- data.frame(frac_positive = apply(data_control_new_CTR_binary, 
                                             2, 
                                             function(x) {
                                               x <- as.numeric(x)
                                               glue("{ sum(x == 1) }/{ length(x) }") 
                                             }),
                                         total_positive_answered = apply(data_control_new_CTR_binary, 
                                                                         2, 
                                                                         function(x) {
                                                                           x <- as.numeric(x)
                                                                           sum(x==1) 
                                                                         }),
                                col = colnames(data_control_new_CTR_binary))  

# -> make final datasets and save them
frac_positive_both_common <- cbind(frac_positive_patients_common %>%
                                     dplyr::mutate(`original question (ALS)` = col) %>%
                                     select(-col),
                                   frac_positive_control_common %>%
                                     dplyr::mutate(`original question (CTR)` = col) %>%
                                     select(-col)) 
final_table_frac_both_common <- merge(frac_both_common,
                                      frac_positive_both_common) %>%
  arrange(desc(total_answered_ALS),desc(total_answered_CTR))
final_table_frac_ALS <- merge(frac_ALS,
                              frac_positive_patients_only %>%
                                dplyr::mutate(`original question (ALS)` = col) %>%
                                select(-col)) %>%
  arrange(desc(total_answered),desc(total_positive_answered))
final_table_frac_CTR <- merge(frac_CTR,
                              frac_positive_control_only %>%
                                dplyr::mutate(`original question (CTR)` = col) %>%
                                select(-col)) %>%
  arrange(desc(total_answered),desc(total_positive_answered))

final_table_frac_ALS_not_in_CTR <- final_table_frac_ALS[!final_table_frac_ALS$col_EN %in% final_table_frac_CTR$col_EN,]

final_table_frac_CTR_not_in_ALS <- final_table_frac_CTR[!final_table_frac_CTR$col_EN %in% final_table_frac_ALS$col_EN,]
final_table_frac_CTR_in_ALS <- final_table_frac_CTR[final_table_frac_CTR$col_EN %in% final_table_frac_ALS$col_EN,]

writexl::write_xlsx(final_table_frac_both_common,"data code output/final_table_frac_both_common.xlsx")
writexl::write_xlsx(final_table_frac_ALS,"data code output/final_table_frac_ALS.xlsx")
writexl::write_xlsx(final_table_frac_CTR,"data code output/final_table_frac_CTR.xlsx")
writexl::write_xlsx(final_table_frac_ALS_not_in_CTR,"data code output/final_table_frac_ALS_not_in_CTR_en.xlsx")
writexl::write_xlsx(final_table_frac_CTR_not_in_ALS,"data code output/final_table_frac_CTR_not_in_ALS_en.xlsx")

# updated final table from 03/05/2024
final_ALS_CTR <- final_table_frac_both_common
final_ALS_only <- final_table_frac_ALS
final_CTR_only <- final_table_frac_CTR

final_ALS_CTR <- final_ALS_CTR %>%
  arrange(desc(total_answered_ALS),desc(total_answered_CTR))
final_ALS_only <- final_ALS_only %>%
  arrange(desc(total_answered))
final_CTR_only <- final_CTR_only %>%
  arrange(desc(total_answered))

final_ALS_CTR$`original question (CTR)` <- ifelse(is.na(final_ALS_CTR$`original question (CTR)`),
                                                  final_ALS_CTR$`original question (ALS)`,
                                                  final_ALS_CTR$`original question (CTR)`)

# -> patients and control data
data_patients_common_final <- data_patients_new[,colnames(data_patients_new) %in% final_ALS_CTR$`original question (ALS)`]
data_control_common_final <- data_control_new[,colnames(data_control_new) %in% final_ALS_CTR$`original question (CTR)`]

data_patients_common_final <- apply(data_patients_common_final, 2, function(col){
  ifelse(col == "N/A" | col == "NA",NA,col)
}) %>% as.data.frame()
data_control_common_final <- apply(data_control_common_final, 2, function(col){
  ifelse(col == "N/A" | col == "NA",NA,col)
}) %>% as.data.frame()

# -> summary of answers per question
data_patients_common_final_summary <- data.frame(col_values_ALS = apply(data_patients_common_final,
                                                                        2,
                                                                        function(x) {
                                                                          unique_col <- na.omit(unique(x))
                                                                          final_col <- paste(unique_col,collapse = ",")}))
data_patients_common_final_summary$`original question (ALS)` <- rownames(data_patients_common_final_summary)

data_control_common_final_summary <- data.frame(col_values_CTR = apply(data_control_common_final,
                                                                       2,
                                                                       function(x) {
                                                                         unique_col <- na.omit(unique(x))
                                                                         final_col <- paste(unique_col,collapse = ",")}))
data_control_common_final_summary$`original question (CTR)` <- rownames(data_control_common_final_summary)

final_ALS_CTR_summary <- final_ALS_CTR %>%
  left_join(data_patients_common_final_summary) %>%
  left_join(data_control_common_final_summary) 
  # left_join(map_col_ALS_CTR)
  
# get previous data to get info on using for ML 
final_ALS_CTR_deprecated <- read_excel("data code output/final_ALS_CTR_questionnaire_new_deprecated.xlsx")

final_ALS_CTR_deprecated <- final_ALS_CTR_deprecated %>%
  select(`original question (ALS)`,`question in EN (ALS)`,`question in EN (CTR)`,col_classification,use_ML,Note)

final_ALS_CTR_summary <- final_ALS_CTR_summary %>%
  left_join(final_ALS_CTR_deprecated)

final_ALS_CTR_summary$`question in EN (ALS)` <- ifelse(is.na(final_ALS_CTR_summary$`question in EN (ALS)`),
                                                       final_ALS_CTR_summary$col_EN,
                                                       final_ALS_CTR_summary$`question in EN (ALS)`)

final_ALS_CTR_summary$`question in EN (CTR)` <- ifelse(is.na(final_ALS_CTR_summary$`question in EN (CTR)`),
                                                          final_ALS_CTR_summary$`question in EN (ALS)`,
                                                          final_ALS_CTR_summary$`question in EN (CTR)`)

final_ALS_CTR_summary$total_positive_answered_ALS <- ifelse(is.na(final_ALS_CTR_summary$total_positive_answered_ALS),
                                                            0,
                                                            final_ALS_CTR_summary$total_positive_answered_ALS)

final_ALS_CTR_summary$total_positive_answered_CTR <- ifelse(is.na(final_ALS_CTR_summary$total_positive_answered_CTR),
                                                            0,
                                                            final_ALS_CTR_summary$total_positive_answered_CTR)

writexl::write_xlsx(final_ALS_CTR_summary,"final_ALS_CTR_summary.xlsx")

# ALS only data
final_ALS_only <- final_ALS_only %>%
  dplyr::rename(frac_ALS = frac_answered,
         total_answered_ALS = total_answered,
         `question in EN (ALS)` = col_EN)

data_ALS_final <- data_patients_new[,!colnames(data_patients_new) %in% final_ALS_CTR$`original question (ALS)`]
final_ALS_only_summary <- data.frame(col_values_ALS = apply(data_ALS_final,
                                                                2,
                                                                function(x) {
                                                                  unique_col <- na.omit(unique(x))
                                                                  final_col <- paste(unique_col,collapse = ",")}))
final_ALS_only_summary$`original question (ALS)` <- rownames(final_ALS_only_summary)
final_ALS_only <- final_ALS_only %>%
  left_join(final_ALS_only_summary)

# CTR only data
final_CTR_only <- final_CTR_only %>%
  dplyr::rename(frac_CTR = frac,
         total_answered_CTR = total_answered,
         `question in EN (CTR)` = col_EN) 

data_control_final <- data_control_new[,!colnames(data_control_new) %in% final_ALS_CTR$`original question (CTR)`]
final_control_only_summary <- data.frame(col_values_CTR = apply(data_control_final,
                                                                2,
                                                                function(x) {
                                                                  unique_col <- na.omit(unique(x))
                                                                  final_col <- paste(unique_col,collapse = ",")}))
final_control_only_summary$`original question (CTR)` <- rownames(final_control_only_summary)
final_CTR_only <- final_CTR_only %>%
  left_join(final_control_only_summary)

writexl::write_xlsx(final_ALS_only,"final_ALS.xlsx")
writexl::write_xlsx(final_CTR_only,"final_CTR.xlsx")
## univariate analysis for specific categories (sent by Isabel)

library(readxl)
library(stringr)
library(tibble)
final_ALS_CTR_category <- read_excel("data input/final_ALS_CTR_questionnaire_summary_IC.xlsx", sheet = "common")
map_questions_plot <- read_excel("data input/map_questions_plot.xlsx")

final_ALS_CTR_category_temp <- final_ALS_CTR_category %>%
  filter(.,!is.na(category)) %>%
  select(`original question (ALS)`,`original question (CTR)`,
         `question in EN (ALS)`, `question in EN (CTR)`,
         col_classification,category,`sub-category`,`sub-subcategory`)
final_ALS_CTR_category_temp_age_sex <- final_ALS_CTR_category %>%
  filter(.,`question in EN (ALS)` %in% c("birth","sex")) %>%
  select(`original question (ALS)`,`original question (CTR)`,
         `question in EN (ALS)`, `question in EN (CTR)`,
         col_classification,category,`sub-category`,`sub-subcategory`)

dat_final_age_sex <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_temp_age_sex$`original question (ALS)`))]
dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an. <- as.factor(dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an.)

## Non-motor symptoms
final_ALS_CTR_category_nonmotor <- final_ALS_CTR_category_temp %>%
  filter(category == "non-motor symptoms")

# -> general
final_ALS_CTR_category_nonmotor_general <- final_ALS_CTR_category_nonmotor %>%
  filter(`sub-category` == "general")

dat_final_nonmotor_general <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_nonmotor_general$`original question (ALS)`),ncol(dat_final))]

univariate_nonmotor_general <- univariate_model_new(dat_final_nonmotor_general)[[1]] %>%
  mutate(Variables = "general changes")

univariate_nonmotor_general_age <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_general,dat_final_age_sex[1]),
                                                                   names(dat_final_age_sex)[1])[[1]] %>%
  mutate(Variables = "general changes",
         eff = Estimate)
univariate_nonmotor_general_gender <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_general,dat_final_age_sex[2]),
                                                                   names(dat_final_age_sex)[2])[[1]][2,] %>%
  mutate(Variables = "general changes",
         eff = Estimate)

final_ALS_CTR_category_nonmotor_diff <- final_ALS_CTR_category_nonmotor %>%
  filter(`sub-category` != "general")

# -> different non-motor (all together)
dat_final_nonmotor_diff_all <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_nonmotor_diff$`original question (ALS)`),ncol(dat_final))]
dat_final_nonmotor_diff_all_temp <- apply(dat_final_nonmotor_diff_all[,1:ncol(dat_final_nonmotor_diff_all)-1], 2, function(x) ifelse(!is.na(x),1,0))
dat_final_nonmotor_diff_all <- data.frame(nonmotor_diff_all = apply(dat_final_nonmotor_diff_all_temp, 1, function(x) {
  nonmotor_diff_all <- sum(x)
}),status2 = dat_final_nonmotor_diff_all[,"status2"])

univariate_nonmotor_diff_all <- univariate_model_new(dat_final_nonmotor_diff_all)[[1]]
univariate_nonmotor_diff_all_age <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_diff_all,dat_final_age_sex[1]),
                                                                    names(dat_final_age_sex)[1])[[1]] %>%
  mutate(eff = Estimate)
univariate_nonmotor_diff_all_gender <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_diff_all,dat_final_age_sex[2]),
                                                                    names(dat_final_age_sex)[2])[[1]][2,] %>%
  mutate(Variables = "nonmotor_diff_all",
         eff = Estimate)


# -> different non-motor (each subquestion individually)
subquestions_nonmotor <- final_ALS_CTR_category_nonmotor_diff$`sub-category` %>% unique()

df_subquestions_nonmotor <- list()
df_subquestions_nonmotor_age <- list()
df_subquestions_nonmotor_gender <- list()
for (i in 1:length(subquestions_nonmotor)) {
  subquestion_i <- subquestions_nonmotor[i]
  final_ALS_CTR_category_nonmotor_diff_i <- final_ALS_CTR_category_nonmotor_diff %>%
    filter(`sub-category` == subquestion_i)
  dat_final_nonmotor_diff_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_nonmotor_diff_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_nonmotor_diff_i_temp <- apply(dat_final_nonmotor_diff_i[,1:ncol(dat_final_nonmotor_diff_i)-1], 2, function(x) ifelse(!is.na(x),1,0))
  dat_final_nonmotor_diff_i <- data.frame(subquestion_i = apply(dat_final_nonmotor_diff_i_temp, 1, function(x) {
    nonmotor_diff_all <- sum(x)
  }),status2 = dat_final_nonmotor_diff_i[,"status2"])
  df_subquestions_nonmotor[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i)[[1]]
  df_subquestions_nonmotor_age[[i]] <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_diff_i,dat_final_age_sex[1]),
                                                                       names(dat_final_age_sex)[1])[[1]] %>%
    mutate(eff = Estimate)
  # df_subquestions_nonmotor_gender[[i]] <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_diff_i,dat_final_age_sex[2]),
  #                                                                         names(dat_final_age_sex)[2])[[1]][2,] %>%
  #   mutate(eff = Estimate)
  
}

univariate_nonmotor_diff_subquestion <- do.call("rbind", df_subquestions_nonmotor) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_nonmotor)
univariate_nonmotor_diff_subquestion_age <- do.call("rbind", df_subquestions_nonmotor_age) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_nonmotor)
univariate_nonmotor_diff_subquestion_gender <- do.call("rbind", df_subquestions_nonmotor_gender) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_nonmotor)

# -> all results together
preparate_data_forest <- function(data_univariate){
  data_univariate <- data_univariate %>%
    mutate(lower = log(`2.5 %`),
           upper = log(`97.5 %`),
           fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
    mutate(log10_pval = -log10(`Pr(>|z|)`),
           log10_padj = -log10(fdr)) %>%
    arrange(log10_pval) %>%
    dplyr::mutate(mean = eff,
                  index = 1:nrow(.)) %>%
    left_join(map_questions_plot)
}
univariate_nonmotor <- do.call("rbind", list(univariate_nonmotor_general,univariate_nonmotor_diff_all,univariate_nonmotor_diff_subquestion))
univariate_nonmotor_age <- do.call("bind_rows", list(univariate_nonmotor_general_age,
                                                 univariate_nonmotor_diff_all_age,
                                                 univariate_nonmotor_diff_subquestion_age))
univariate_nonmotor_gender <- do.call("rbind", list(univariate_nonmotor_general_gender,
                                                    univariate_nonmotor_diff_all_gender,
                                                    univariate_nonmotor_diff_subquestion_gender))

univariate_nonmotor <- preparate_data_forest(univariate_nonmotor)
univariate_nonmotor_age <- preparate_data_forest(univariate_nonmotor_age)
univariate_nonmotor_gender <- preparate_data_forest(univariate_nonmotor_gender)

writexl::write_xlsx(univariate_nonmotor,"univariate_nonmotor.xlsx")
writexl::write_xlsx(univariate_nonmotor_age,"univariate_nonmotor_age.xlsx")
writexl::write_xlsx(univariate_nonmotor_gender,"univariate_nonmotor_gender.xlsx")

pdf("plots/univariate_nonmotor.pdf")
odds_ratio_plot(univariate_nonmotor,"Non-motor symptoms")
dev.off()
pdf("plots/volcano_nonmotor_pvalue.pdf")
volcano_plot(univariate_nonmotor,"p-value","Non-motor symptoms (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_padj.pdf")
volcano_plot(univariate_nonmotor,"padj","Non-motor symptoms (p-adjusted)")
dev.off()

pdf("plots/univariate_nonmotor_age.pdf")
odds_ratio_plot(univariate_nonmotor_age,"Non-motor symptoms, age adjusted")
dev.off()
pdf("plots/volcano_nonmotor_age_pvalue.pdf")
volcano_plot(univariate_nonmotor_age,"p-value","Non-motor symptoms, age adjusted  (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_age_padj.pdf")
volcano_plot(univariate_nonmotor_age,"padj","Non-motor symptoms, age adjusted (p-adjusted)")
dev.off()

pdf("plots/univariate_nonmotor_gender.pdf")
odds_ratio_plot(univariate_nonmotor_gender,"Non-motor symptoms, gender adjusted")
dev.off()
pdf("plots/volcano_nonmotor_gender_pvalue.pdf")
volcano_plot(univariate_nonmotor_gender,"p-value","Non-motor symptoms, gender adjusted (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_gender_padj.pdf")
volcano_plot(univariate_nonmotor_gender,"padj","Non-motor symptoms, gender adjusted (p-adjusted)")
dev.off()

## -> Different Non-motor symptoms (each sub-subcategories)
final_ALS_CTR_category_nonmotor_diff <- final_ALS_CTR_category_nonmotor_diff[final_ALS_CTR_category_nonmotor_diff$`original question (ALS)` %in% original_names,]
subsubquestions_nonmotor <- final_ALS_CTR_category_nonmotor_diff$`sub-subcategory` %>% unique()

df_subsubquestions_nonmotor <- list()
df_subsubquestions_nonmotor_age <- list()
df_subsubquestions_nonmotor_gender <- list()
for (i in 1:length(subsubquestions_nonmotor)) {
  subsubquestion_i <- subsubquestions_nonmotor[i]
  final_ALS_CTR_category_nonmotor_diff_i <- final_ALS_CTR_category_nonmotor_diff %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_nonmotor_diff_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_nonmotor_diff_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_nonmotor_diff_i_temp <- ifelse(!is.na(dat_final_nonmotor_diff_i[,1:ncol(dat_final_nonmotor_diff_i)-1]),1,0)
  dat_final_nonmotor_diff_i <- data.frame(subsubquestion_i = dat_final_nonmotor_diff_i_temp,status2 = dat_final_nonmotor_diff_i[,"status2"])
  df_subsubquestions_nonmotor[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i)[[1]]
  df_subsubquestions_nonmotor_age[[i]] <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_diff_i,dat_final_age_sex[1]),
                                                                       names(dat_final_age_sex)[1])[[1]] %>%
    mutate(eff = Estimate)
  # df_subsubquestions_nonmotor_gender[[i]] <- univariate_model_adjustment_new(cbind(dat_final_nonmotor_diff_i,dat_final_age_sex[2]),
  #                                                                         names(dat_final_age_sex)[2])[[1]][2,] %>%
  #   mutate(eff = Estimate)
  
}

univariate_nonmotor_diff_subsubquestion <- do.call("rbind", df_subsubquestions_nonmotor) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_nonmotor)
univariate_nonmotor_diff_subsubquestion_age <- do.call("rbind", df_subsubquestions_nonmotor_age) %>%
  as.data.frame() %>%
  mutate(Variables = subsubquestions_nonmotor)
univariate_nonmotor_diff_subsubquestion_gender <- do.call("rbind", df_subsubquestions_nonmotor_gender) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_nonmotor)

# -> all results together
preparate_data_forest <- function(data_univariate){
  data_univariate <- data_univariate %>%
    mutate(lower = log(`2.5 %`),
           upper = log(`97.5 %`),
           fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
    mutate(log10_pval = -log10(`Pr(>|z|)`),
           log10_padj = -log10(fdr)) %>%
    arrange(log10_pval) %>%
    dplyr::mutate(mean = eff,
                  index = 1:nrow(.)) %>%
    left_join(map_questions_plot)
}
univariate_nonmotor_subsubquestion <- univariate_nonmotor_diff_subsubquestion
univariate_nonmotor_subsubquestion[univariate_nonmotor_subsubquestion$Variables == "Häufig verstopfte Nase",]$Labels_plot <- "Frequent runny nose"
univariate_nonmotor_subsubquestion_age <- univariate_nonmotor_diff_subsubquestion_age
univariate_nonmotor_subsubquestion_age[univariate_nonmotor_subsubquestion_age$Variables == "Häufig verstopfte Nase",]$Labels_plot <- "Frequent runny nose"

univariate_nonmotor_subsubquestion_gender <- univariate_nonmotor_diff_subsubquestion_gender
univariate_nonmotor_subsubquestion_gender[univariate_nonmotor_subsubquestion_gender$Variables == "Häufig verstopfte Nase",]$Labels_plot <- "Frequent runny nose"

univariate_nonmotor_subsubquestion <- preparate_data_forest(univariate_nonmotor_subsubquestion)
univariate_nonmotor_subsubquestion_age <- preparate_data_forest(univariate_nonmotor_subsubquestion_age)
univariate_nonmotor_subsubquestion_gender <- preparate_data_forest(univariate_nonmotor_subsubquestion_gender)

writexl::write_xlsx(univariate_nonmotor_subsubquestion,"univariate_nonmotor_subsubquestion.xlsx")
writexl::write_xlsx(univariate_nonmotor_subsubquestion_age,"univariate_nonmotor_subsubquestion_age.xlsx")
writexl::write_xlsx(univariate_nonmotor_subsubquestion_gender,"univariate_nonmotor_subsubquestion_gender.xlsx")

pdf("plots/univariate_nonmotor_subsubquestion.pdf",width=10,height=12)
odds_ratio_plot(univariate_nonmotor_subsubquestion,"Non-motor symptoms subsubquestion")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_pvalue.pdf",width = 10)
volcano_plot(univariate_nonmotor_subsubquestion,"p-value","Non-motor symptoms subsubquestion (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_padj.pdf",width = 10)
volcano_plot(univariate_nonmotor_subsubquestion,"padj","Non-motor symptoms subsubquestion (p-adjusted)")
dev.off()

pdf("plots/univariate_nonmotor_subsubquestion_age.pdf",width=10,height=12)
odds_ratio_plot(univariate_nonmotor_subsubquestion_age,"Non-motor symptoms subsubquestion, age adjusted")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_age_pvalue.pdf",width = 10)
volcano_plot(univariate_nonmotor_subsubquestion_age,"p-value","Non-motor symptoms subsubquestion, age adjusted (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_age_padj.pdf",width = 10)
volcano_plot(univariate_nonmotor_subsubquestion_age,"padj","Non-motor symptoms subsubquestion, age adjusted (p-adjusted)")
dev.off()

pdf("plots/univariate_nonmotor_subsubquestion_gender.pdf",width=10,height=12)
odds_ratio_plot(univariate_nonmotor_subsubquestion_gender,"Non-motor symptoms subsubquestion, gender adjusted")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_gender_pvalue.pdf",width = 10)
volcano_plot(univariate_nonmotor_subsubquestion_gender,"p-value","Non-motor symptoms subsubquestion, gender adjusted (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_gender_padj.pdf",width = 10)
volcano_plot(univariate_nonmotor_subsubquestion_gender,"padj","Non-motor symptoms subsubquestion, gender adjusted (p-adjusted)")
dev.off()


## Contacts healthcare system
final_ALS_CTR_category_healthcare <- final_ALS_CTR_category_temp %>%
  filter(category == "contacts healthcare system")

# -> different non-motor (each subquestion individually)
subquestions_healthcare <- final_ALS_CTR_category_healthcare$`sub-category` %>% unique()

df_subquestions_healthcare <- list()
df_subquestions_healthcare_age <- list()
df_subquestions_healthcare_gender <- list()
for (i in 1:length(subquestions_healthcare)) {
  subquestion_i <- subquestions_healthcare[i]
  final_ALS_CTR_category_healthcare_i <- final_ALS_CTR_category_healthcare %>%
    filter(`sub-category` == subquestion_i)
  dat_final_healthcare_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_healthcare_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_healthcare_i <- data.frame(subquestion_i = apply(dat_final_healthcare_i[,1:ncol(dat_final_healthcare_i)-1], 1, function(x) {
    healthcare_all <- sum(x)
  }),status2 = dat_final_healthcare_i[,"status2"])
  df_subquestions_healthcare[[i]] <- univariate_model_new(dat_final_healthcare_i)[[1]]
  df_subquestions_healthcare_age[[i]] <- univariate_model_adjustment_new(cbind(dat_final_healthcare_i,dat_final_age_sex[1]),
                                                                       names(dat_final_age_sex)[1])[[1]] %>%
    mutate(eff = Estimate)
  # df_subquestions_healthcare_gender[[i]] <- univariate_model_adjustment_new(cbind(dat_final_healthcare_i,dat_final_age_sex[2]),
  #                                                                         names(dat_final_age_sex)[2])[[1]][2,] %>%
  #   mutate(eff = Estimate)
}

univariate_healthcare <- do.call("rbind", df_subquestions_healthcare) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_healthcare)
univariate_healthcare <- preparate_data_forest(univariate_healthcare)

univariate_healthcare_age <- do.call("rbind", df_subquestions_healthcare_age) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_healthcare)
univariate_healthcare_age <- preparate_data_forest(univariate_healthcare_age)

univariate_healthcare_gender <- do.call("rbind", df_subquestions_healthcare_gender) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_healthcare)
univariate_healthcare_gender <- preparate_data_forest(univariate_healthcare_gender)

writexl::write_xlsx(univariate_healthcare,"univariate_healthcare.xlsx")
writexl::write_xlsx(univariate_healthcare_age,"univariate_healthcare_age.xlsx")
writexl::write_xlsx(univariate_healthcare_gender,"univariate_healthcare_gender.xlsx")

pdf("plots/univariate_healthcare.pdf")
odds_ratio_plot(univariate_healthcare,"Contacts healthcare system")
dev.off()
pdf("plots/volcano_healthcare_pvalue.pdf")
volcano_plot(univariate_healthcare,"p-value","Contacts healthcare system (p-value)")
dev.off()
pdf("plots/volcano_healthcare_padj.pdf")
volcano_plot(univariate_healthcare,"padj","Contacts healthcare system (p-adjusted)")
dev.off()

pdf("plots/univariate_healthcare_age.pdf")
odds_ratio_plot(univariate_healthcare_age,"Contacts healthcare system, age adjusted")
dev.off()
pdf("plots/volcano_healthcare_age_pvalue.pdf")
volcano_plot(univariate_healthcare_age,"p-value","Contacts healthcare system, age adjusted (p-value)")
dev.off()
pdf("plots/volcano_healthcare_age_padj.pdf")
volcano_plot(univariate_healthcare_age,"padj","Contacts healthcare system, age adjusted (p-adjusted)")
dev.off()

pdf("plots/univariate_healthcare_gender.pdf")
odds_ratio_plot(univariate_healthcare_gender,"Contacts healthcare system, gender adjusted")
dev.off()
pdf("plots/volcano_healthcare_gender_pvalue.pdf")
volcano_plot(univariate_healthcare_gender,"p-value","Contacts healthcare system, gender adjusted (p-value)")
dev.off()
pdf("plots/volcano_healthcare_gender_padj.pdf")
volcano_plot(univariate_healthcare_gender,"padj","Contacts healthcare system, gender adjusted (p-adjusted)")
dev.off()

## Pre-existing conditions
final_ALS_CTR_category_preconditions <- final_ALS_CTR_category_temp %>%
  filter(category == "pre-existing conditions")

# -> Pre-existing conditions (all together)
dat_final_preconditions_all <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_preconditions$`original question (ALS)`),ncol(dat_final))]
dat_final_preconditions_all_temp <- apply(dat_final_preconditions_all[,1:ncol(dat_final_preconditions_all)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
dat_final_preconditions_all <- data.frame(preconditions_all = apply(dat_final_preconditions_all_temp, 1, function(x) {
  preconditions_all <- sum(x)
}),status2 = dat_final_preconditions_all[,"status2"])

univariate_preconditions_all <- univariate_model_new(dat_final_preconditions_all)[[1]]
univariate_preconditions_all_age <- univariate_model_adjustment_new(cbind(dat_final_preconditions_all,dat_final_age_sex[1]),
                                                                    names(dat_final_age_sex)[1])[[1]] %>%
  mutate(eff = Estimate)
univariate_preconditions_all_gender <- univariate_model_adjustment_new(cbind(dat_final_preconditions_all,dat_final_age_sex[2]),
                                                                    names(dat_final_age_sex)[2])[[1]][2,] %>%
  mutate(eff = Estimate)

# -> Pre-existing conditions (each subquestion individually)
subquestions_preconditions <- final_ALS_CTR_category_preconditions$`sub-category` %>% unique()

df_subquestions_preconditions <- list()
df_subquestions_preconditions_age <- list()
df_subquestions_preconditions_gender <- list()
for (i in 1:length(subquestions_preconditions)) {
  subquestion_i <- subquestions_preconditions[i]
  final_ALS_CTR_category_preconditions_i <- final_ALS_CTR_category_preconditions %>%
    filter(`sub-category` == subquestion_i)
  dat_final_preconditions_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_preconditions_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_preconditions_i_temp <- apply(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
  dat_final_preconditions_i <- data.frame(subquestion_i = apply(dat_final_preconditions_i_temp, 1, function(x) {
    preconditions_all <- sum(x)
  }),status2 = dat_final_preconditions_i[,"status2"])
  df_subquestions_preconditions[[i]] <- univariate_model_new(dat_final_preconditions_i)[[1]]
  df_subquestions_preconditions_age[[i]] <- univariate_model_adjustment_new(cbind(dat_final_preconditions_i,dat_final_age_sex[1]),
                                                                       names(dat_final_age_sex)[1])[[1]] %>%
    mutate(eff = Estimate)
  # df_subquestions_preconditions_gender[[i]] <- univariate_model_adjustment_new(cbind(dat_final_preconditions_i,dat_final_age_sex[2]),
  #                                                                         names(dat_final_age_sex)[2])[[1]][2,] %>%
  #   mutate(eff = Estimate)
}

univariate_preconditions_subquestion <- do.call("rbind", df_subquestions_preconditions) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_preconditions)
univariate_preconditions_subquestion_age <- do.call("rbind", df_subquestions_preconditions_age) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_preconditions)
univariate_preconditions_subquestion_gender <- do.call("rbind", df_subquestions_preconditions_gender) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_preconditions)

# -> all results together
univariate_preconditions <- do.call("rbind", list(univariate_preconditions_all,univariate_preconditions_subquestion))
univariate_preconditions <- preparate_data_forest(univariate_preconditions)
univariate_preconditions_age <- do.call("bind_rows", list(univariate_preconditions_all_age,univariate_preconditions_subquestion_age))
univariate_preconditions_age <- preparate_data_forest(univariate_preconditions_age)
univariate_preconditions_age$Labels_plot <- ifelse(is.na(univariate_preconditions_age$Labels_plot),
                                                      "All pre-existing conditions",
                                                   univariate_preconditions_age$Labels_plot)
univariate_preconditions_gender <- do.call("rbind", list(univariate_preconditions_all_gender,univariate_preconditions_subquestion_gender))
univariate_preconditions_gender <- preparate_data_forest(univariate_preconditions_gender)
univariate_preconditions_gender$Labels_plot <- ifelse(is.na(univariate_preconditions_gender$Labels_plot),
                                                      "All pre-existing conditions",
                                                      univariate_preconditions_gender$Labels_plot)

writexl::write_xlsx(univariate_preconditions,"univariate_preconditions.xlsx")
writexl::write_xlsx(univariate_preconditions_age,"univariate_preconditions_age.xlsx")
writexl::write_xlsx(univariate_preconditions_gender,"univariate_preconditions_gender.xlsx")

pdf("plots/univariate_preconditions.pdf")
odds_ratio_plot(univariate_preconditions,"Pre-existing conditions")
dev.off()
pdf("plots/volcano_preconditions_pvalue.pdf")
volcano_plot(univariate_preconditions,"p-value","Pre-existing conditions (p-value)")
dev.off()
pdf("plots/volcano_preconditions_padj.pdf")
volcano_plot(univariate_preconditions,"padj","Pre-existing conditions (p-adjusted)")
dev.off()

pdf("plots/univariate_preconditions_age.pdf")
odds_ratio_plot(univariate_preconditions_age,"Pre-existing conditions, age adjusted")
dev.off()
pdf("plots/volcano_preconditions_age_pvalue.pdf")
volcano_plot(univariate_preconditions_age,"p-value","Pre-existing conditions, age adjusted (p-value)")
dev.off()
pdf("plots/volcano_preconditions_age_padj.pdf")
volcano_plot(univariate_preconditions_age,"padj","Pre-existing conditions, age adjusted (p-adjusted)")
dev.off()

pdf("plots/univariate_preconditions_gender.pdf")
odds_ratio_plot(univariate_preconditions_gender,"Pre-existing conditions, gender adjusted")
dev.off()
pdf("plots/volcano_preconditions_gender_pvalue.pdf")
volcano_plot(univariate_preconditions_gender,"p-value","Pre-existing conditions, gender adjusted (p-value)")
dev.off()
pdf("plots/volcano_preconditions_gender_padj.pdf")
volcano_plot(univariate_preconditions_gender,"padj","Pre-existing conditions, gender adjusted (p-adjusted)")
dev.off()

# -> Pre-existing conditions (each subsubquestion individually)
subsubquestions_preconditions <- final_ALS_CTR_category_preconditions$`sub-subcategory` %>% unique()
subsubquestions_preconditions <- subsubquestions_preconditions[c(1:14,16:21,23:25,27:31,33:36,38:41,43:47,49:51,53:61,64:65,
                                                                 67:69,79,81,83,93,99,101)]


df_subsubquestions_preconditions <- list()
df_subsubquestions_preconditions_age <- list()
df_subsubquestions_preconditions_gender <- list()
for (i in 1:length(subsubquestions_preconditions)) {
  subsubquestion_i <- subsubquestions_preconditions[i]
  final_ALS_CTR_category_preconditions_i <- final_ALS_CTR_category_preconditions %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_preconditions_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_preconditions_i$`original question (ALS)`),ncol(dat_final))]
  print(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1])
  print(subsubquestion_i)
  dat_final_preconditions_i_temp <- ifelse(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1] == "Ja",1,0)
  dat_final_preconditions_i <- data.frame(subsubquestion_i = dat_final_preconditions_i_temp,status2 = dat_final_preconditions_i[,"status2"])
  df_subsubquestions_preconditions[[i]] <- univariate_model_new(dat_final_preconditions_i)[[1]]
  df_subsubquestions_preconditions_age[[i]] <- univariate_model_adjustment_new(cbind(dat_final_preconditions_i,dat_final_age_sex[1]),
                                                                                  names(dat_final_age_sex)[1])[[1]] %>%
    mutate(eff = Estimate)
  # df_subsubquestions_preconditions_gender[[i]] <- univariate_model_adjustment_new(cbind(dat_final_preconditions_i,dat_final_age_sex[2]),
  #                                                                              names(dat_final_age_sex)[2])[[1]][2,] %>%
  #   mutate(eff = Estimate)
}

univariate_preconditions_subsubquestion <- do.call("rbind", df_subsubquestions_preconditions) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_preconditions)
univariate_preconditions_subsubquestion_age <- do.call("rbind", df_subsubquestions_preconditions_age) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_preconditions)
univariate_preconditions_subsubquestion_gender <- do.call("rbind", df_subsubquestions_preconditions_gender) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_preconditions)

# -> all results together
univariate_preconditions_subsubquestion <- preparate_data_forest(univariate_preconditions_subsubquestion)
univariate_preconditions_subsubquestion <- univariate_preconditions_subsubquestion[!duplicated(univariate_preconditions_subsubquestion$Labels_plot),]
univariate_preconditions_subsubquestion_age <- preparate_data_forest(univariate_preconditions_subsubquestion_age)
univariate_preconditions_subsubquestion_age <- univariate_preconditions_subsubquestion_age[!duplicated(univariate_preconditions_subsubquestion_age$Labels_plot),]
univariate_preconditions_subsubquestion_gender <- preparate_data_forest(univariate_preconditions_subsubquestion_gender)
univariate_preconditions_subsubquestion_gender <- univariate_preconditions_subsubquestion_gender[!duplicated(univariate_preconditions_subsubquestion_gender$Labels_plot),]

writexl::write_xlsx(univariate_preconditions_subsubquestion,"univariate_preconditions_subquestion.xlsx")
writexl::write_xlsx(univariate_preconditions_subsubquestion_age,"univariate_preconditions_subquestion_age.xlsx")
writexl::write_xlsx(univariate_preconditions_subsubquestion_gender,"univariate_preconditions_subquestion_gender.xlsx")

pdf("plots/univariate_preconditions_subquestion.pdf",width=10,height=12)
odds_ratio_plot(univariate_preconditions_subsubquestion,"Pre-existing conditions subquestion")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_pvalue.pdf")
volcano_plot(univariate_preconditions_subsubquestion,"p-value","Pre-existing conditions subquestion (p-value)")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_padj.pdf")
volcano_plot(univariate_preconditions_subsubquestion,"padj","Pre-existing conditions subquestion (p-adjusted)")
dev.off()

pdf("plots/univariate_preconditions_subsubquestion_age.pdf",width=10,height=12)
odds_ratio_plot(univariate_preconditions_subsubquestion_age,"Pre-existing conditions subquestion, age adjusted")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_age_pvalue.pdf")
volcano_plot(univariate_preconditions_subsubquestion_age,"p-value","Pre-existing conditions subquestion, age adjusted (p-value)")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_age_padj.pdf")
volcano_plot(univariate_preconditions_subsubquestion_age,"padj","Pre-existing conditions subquestion, age adjusted (p-adjusted)")
dev.off()

pdf("plots/univariate_preconditions_subsubquestion_gender.pdf",width=10,height=12)
odds_ratio_plot(univariate_preconditions_subsubquestion_gender,"Pre-existing conditions subquestion, gender adjusted")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_gender_pvalue.pdf")
volcano_plot(univariate_preconditions_subsubquestion_gender,"p-value","Pre-existing conditions subquestion, gender adjusted (p-value)")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_gender_padj.pdf")
volcano_plot(univariate_preconditions_subsubquestion_gender,"padj","Pre-existing conditions subquestion, gender adjusted (p-adjusted)")
dev.off()


## Diet and weight
final_ALS_CTR_category_dietweight <- final_ALS_CTR_category_temp %>%
  filter(category == "diet and weight")

# -> Diet and weight (each subquestion individually)
dat_final_dietweight_all <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_dietweight$`original question (ALS)`),ncol(dat_final))]
# dat_final_dietweight_all[322,]$Bitte.geben.Sie.Ihre.Körpergröße.an..cm..<- "NA"
# dat_final_dietweight_all[114,]$Bitte.geben.Sie.Ihre.Körpergröße.an..cm.. <- "NA"
univariate_dietweight_all <- univariate_model_new(dat_final_dietweight_all)[[1]]
univariate_dietweight_all_age <- univariate_model_adjustment_new(cbind(dat_final_dietweight_all[,c(1:5,8)],dat_final_age_sex[1]),
                                                                 names(dat_final_age_sex)[1])[[1]] %>%
  mutate(eff = Estimate)
univariate_dietweight_all_gender_temp <- univariate_model_adjustment_new(cbind(dat_final_dietweight_all[,c(1:5,8)],dat_final_age_sex[2]),
                                                                             names(dat_final_age_sex)[2])[[1]] %>%
  mutate(eff = Estimate)
univariate_dietweight_all_gender <- cbind(do.call("rbind",univariate_dietweight_all_gender_temp[[1]]),do.call("rbind",univariate_dietweight_all_gender_temp[[2]]))
univariate_dietweight_all_gender$Variables <- rownames(univariate_dietweight_all_gender)
univariate_dietweight_all_gender$t_stat = univariate_dietweight_all_gender$Estimate/univariate_dietweight_all_gender$`Std. Error`

univariate_dietweight_all <- univariate_dietweight_all %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`),
         fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr)) %>%
  arrange(log10_pval)  %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.)) %>%
  left_join(map_questions_plot)

univariate_dietweight_all_age <- univariate_dietweight_all_age %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`),
         fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr)) %>%
  arrange(log10_pval)  %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.)) %>%
  left_join(map_questions_plot)

writexl::write_xlsx(univariate_dietweight_all_age,"univariate_dietweight_age.xlsx")

univariate_dietweight_all_gender <- univariate_dietweight_all_gender %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`),
         fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr)) %>%
  arrange(log10_pval)  %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.)) %>%
  left_join(map_questions_plot)
writexl::write_xlsx(univariate_dietweight_all_gender,"univariate_dietweight_gender.xlsx")

pdf("plots/univariate_dietweight_all.pdf")
odds_ratio_plot(univariate_dietweight_all,"Diet and weight")
dev.off()
pdf("plots/volcano_dietweight_pvalue.pdf")
volcano_plot(univariate_dietweight_all,"p-value","Diet and weight (p-value)")
dev.off()
pdf("plots/volcano_dietweight_padj.pdf")
volcano_plot(univariate_dietweight_all,"padj","Diet and weights (p-adjusted)")
dev.off()

pdf("plots/univariate_dietweight_all_age.pdf")
odds_ratio_plot(univariate_dietweight_all_age,"Diet and weight, age adjusted")
dev.off()
pdf("plots/volcano_dietweight_pvalue_age.pdf")
volcano_plot(univariate_dietweight_all_age,"p-value","Diet and weight, age adjusted (p-value)")
dev.off()
pdf("plots/volcano_dietweight_padj_age.pdf")
volcano_plot(univariate_dietweight_all_age,"padj","Diet and weights, age adjusted (p-adjusted)")
dev.off()

pdf("plots/univariate_dietweight_all_gender.pdf")
odds_ratio_plot(univariate_dietweight_all_gender,"Diet and weight, gender adjusted")
dev.off()
pdf("plots/volcano_dietweight_pvalue_gender.pdf")
volcano_plot(univariate_dietweight_all_gender,"p-value","Diet and weight, gender adjusted (p-value)")
dev.off()
pdf("plots/volcano_dietweight_padj_gender.pdf")
volcano_plot(univariate_dietweight_all_gender,"padj","Diet and weights, gender adjusted (p-adjusted)")
dev.off()

## Lifestyle
final_ALS_CTR_category_lifestyle <- final_ALS_CTR_category_temp %>%
  filter(category == "lifestyle") %>%
  mutate(`sub-category` = ifelse(is.na(`sub-subcategory`),`question in EN (ALS)`,`sub-category`))

# -> Lifestyle (separate the ones with and without subcategories)
# -> first only the ones without subcategories
final_ALS_CTR_category_lifestyle_nosubcategories <- final_ALS_CTR_category_lifestyle %>%
  filter(is.na(`sub-subcategory`))

dat_final_lifestyle_nosubcategories<- dat_final[,c(which(original_names %in% final_ALS_CTR_category_lifestyle_nosubcategories$`original question (ALS)`),ncol(dat_final))]

univariate_lifestyle_nosubcategories_temp <- univariate_model_new(dat_final_lifestyle_nosubcategories)
univariate_lifestyle_nosubcategories <- cbind(univariate_lifestyle_nosubcategories_temp[[1]][,1:9],
                                              univariate_lifestyle_nosubcategories_temp[[2]][,2:3])

# -> the ones with categories all together
final_ALS_CTR_category_lifestyle_subcategories <- final_ALS_CTR_category_lifestyle %>%
  filter(!is.na(`sub-subcategory`))

subquestions_lifestyle <- final_ALS_CTR_category_lifestyle_subcategories$`sub-category` %>% unique()

df_subquestions_lifestyle <- list()
df_subquestions_lifestyle_age <- list()
df_subquestions_lifestyle_gender <- list()
for (i in 1:length(subquestions_lifestyle)) {
  subquestion_i <- subquestions_lifestyle[i]
  final_ALS_CTR_category_subquestions_lifestyle_i <- final_ALS_CTR_category_lifestyle_subcategories %>%
    filter(`sub-category` == subquestion_i)
  dat_final_subquestions_lifestyle_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_subquestions_lifestyle_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_subquestions_lifestyle_i_temp <- apply(dat_final_subquestions_lifestyle_i[,1:ncol(dat_final_subquestions_lifestyle_i)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
  dat_final_subquestions_lifestyle_i <- data.frame(subquestion_i = apply(dat_final_subquestions_lifestyle_i_temp, 1, function(x) {
    lifestyle_all <- sum(x)
    lifestyle_all <- ifelse(lifestyle_all > 0,1,0)
  }),status2 = dat_final_subquestions_lifestyle_i[,"status2"])
  df_subquestions_lifestyle[[i]] <- univariate_model_new(dat_final_subquestions_lifestyle_i)[[1]]
  df_subquestions_lifestyle_age[[i]] <- univariate_model_adjustment_new(cbind(dat_final_subquestions_lifestyle_i,dat_final_age_sex[1]),
                                                                           names(dat_final_age_sex)[1])[[1]] %>%
    mutate(eff = Estimate)
  df_subquestions_lifestyle_gender[[i]] <- univariate_model_adjustment_new(cbind(dat_final_subquestions_lifestyle_i,dat_final_age_sex[2]),
                                                                              names(dat_final_age_sex)[2])[[1]][2,] %>%
    mutate(eff = Estimate)
}

univariate_lifestyle_subquestion <- do.call("rbind", df_subquestions_lifestyle) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_lifestyle)

univariate_lifestyle_subquestion_age <- do.call("rbind", df_subquestions_lifestyle_age) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_lifestyle)
univariate_lifestyle_subquestion_gender <- do.call("rbind", df_subquestions_lifestyle_gender) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_lifestyle)

# -> the ones with categories separate
subsubquestions_lifestyle <- final_ALS_CTR_category_lifestyle_subcategories$`sub-subcategory` %>% unique()
subsubquestions_lifestyle <- subsubquestions_lifestyle[c(1:6,8:14)]

df_subsubquestions_lifestyle <- list()
df_subsubquestions_lifestyle_age <- list()
df_subsubquestions_lifestyle_gender <- list()
for (i in 1:length(subsubquestions_lifestyle)) {
  subsubquestion_i <- subsubquestions_lifestyle[i]
  final_ALS_CTR_category_subsubquestions_lifestyle_i <- final_ALS_CTR_category_lifestyle_subcategories %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_subsubquestions_lifestyle_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_subsubquestions_lifestyle_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_subsubquestions_lifestyle_i_temp <- apply(dat_final_subsubquestions_lifestyle_i[,1:ncol(dat_final_subsubquestions_lifestyle_i)-1], 2, function(x) ifelse(x == "Ja",1,0))
  dat_final_subsubquestions_lifestyle_i <- data.frame(subsubquestion_i = apply(dat_final_subsubquestions_lifestyle_i_temp, 1, function(x) {
    lifestyle_all <- sum(x)
    lifestyle_all <- ifelse(lifestyle_all > 0,1,0)
  }),status2 = dat_final_subsubquestions_lifestyle_i[,"status2"])
  df_subsubquestions_lifestyle[[i]] <- univariate_model_new(dat_final_subsubquestions_lifestyle_i)[[1]]
  df_subsubquestions_lifestyle_age[[i]] <- univariate_model_adjustment_new(cbind(dat_final_subsubquestions_lifestyle_i,dat_final_age_sex[1]),
                                                                         names(dat_final_age_sex)[1])[[1]] %>%
    mutate(eff = Estimate)
  # df_subsubquestions_lifestyle_gender[[i]] <- univariate_model_adjustment_new(cbind(dat_final_subsubquestions_lifestyle_i,dat_final_age_sex[2]),
  #                                                                           names(dat_final_age_sex)[2])[[1]][2,] %>%
  #   mutate(eff = Estimate)
}

univariate_lifestyle_subsubquestion <- do.call("rbind", df_subsubquestions_lifestyle) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_lifestyle)

univariate_lifestyle_subsubquestion_age <- do.call("rbind", df_subsubquestions_lifestyle_age) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_lifestyle)

univariate_lifestyle_subsubquestion_gender <- do.call("rbind", df_subsubquestions_lifestyle_gender) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_lifestyle)

# -> all results together
univariate_lifestyle <- do.call("rbind", list(univariate_lifestyle_nosubcategories,univariate_lifestyle_subquestion,univariate_lifestyle_subsubquestion))
univariate_lifestyle_age <- do.call("bind_rows", list(univariate_lifestyle_subquestion_age,univariate_lifestyle_subsubquestion_age))
univariate_lifestyle_gender <- do.call("rbind", list(univariate_lifestyle_subquestion_gender,univariate_lifestyle_subsubquestion_gender))

univariate_lifestyle <- preparate_data_forest(univariate_lifestyle)
univariate_lifestyle_age <- preparate_data_forest(univariate_lifestyle_age)
univariate_lifestyle_gender <- preparate_data_forest(univariate_lifestyle_gender)

writexl::write_xlsx(univariate_lifestyle,"univariate_lifestyle.xlsx")
writexl::write_xlsx(univariate_lifestyle_age,"univariate_lifestyle_age.xlsx")
writexl::write_xlsx(univariate_lifestyle_gender,"univariate_lifestyle_gender.xlsx")

pdf("plots/univariate_lifestyle.pdf")
odds_ratio_plot(univariate_lifestyle,"Lifestyle")
dev.off()
pdf("plots/volcano_lifestyle_pvalue.pdf")
volcano_plot(univariate_lifestyle,"p-value","Lifestyle (p-value)")
dev.off()
pdf("plots/volcano_lifestyle_padj.pdf")
volcano_plot(univariate_lifestyle,"padj","Lifestyle (p-adjusted)")
dev.off()

pdf("plots/univariate_lifestyle_age.pdf")
odds_ratio_plot(univariate_lifestyle_age,"Lifestyle, age adjusted")
dev.off()
pdf("plots/volcano_lifestyle_age_pvalue.pdf")
volcano_plot(univariate_lifestyle_age,"p-value","Lifestyle, age adjusted (p-value)")
dev.off()
pdf("plots/volcano_lifestyle_age_padj.pdf")
volcano_plot(univariate_lifestyle_age,"padj","Lifestyle, age adjusted (p-adjusted)")
dev.off()

pdf("plots/univariate_lifestyle_gender.pdf")
odds_ratio_plot(univariate_lifestyle_gender,"Lifestyle, gender adjusted")
dev.off()
pdf("plots/volcano_lifestyle_gender_pvalue.pdf")
volcano_plot(univariate_lifestyle_gender,"p-value","Lifestyle, gender adjusted (p-value)")
dev.off()
pdf("plots/volcano_lifestyle_gender_padj.pdf")
volcano_plot(univariate_lifestyle_gender,"padj","Lifestyle, gender adjusted (p-adjusted)")
dev.off()


univariate_model_new <- function(data_final){
  res = list()
  ftr = list()
  eff_CI = list()
  for (i in colnames(data_final)){
    if (i %in% c("status", "status2")){
      next
    }
    if (nlevels(as.factor(data_final[,i])) < 2){
      next
    }
    form = as.formula(paste("status2 ~",i))
    if(length(grep("Nein",data_final[,i]))!=0){
      unique_values <- unique(data_final[,i])
      if(length(unique_values) < 5){
        unique_values_ordered <- unique_values[str_order(unique_values,decreasing = T)]
        col_i <- factor(data_final[,i],levels = unique_values_ordered)
        data_final[,i] <- col_i
      }
    }
    mod = glm(form, data_final, family = "binomial")
    tmp = summary(mod)$coefficients
    rn = rownames(tmp)
    rn = rn[rn != "(Intercept)"]
    rn = sub(i,"",rn,ignore.case = T)
    tmp = tmp[rownames(tmp) != "(Intercept)",, drop = F] %>% as.data.frame()
    rownames(tmp) = rn
    eff = vector("numeric", length = length(rn))
    eff_CI_tmp = exp(confint.default(mod))
    eff_CI_tmp <- eff_CI_tmp[rownames(eff_CI_tmp) != "(Intercept)",, drop = F] %>% as.data.frame()
    if (i %in% colnames_dat_fil_cat){
      if (length(grep(" ",rn)) > 0){
        cl = substr(rn, nchar(rn) - 1, nchar(rn))
      } else{
        cl = substr(rn, nchar(rn), nchar(rn))
      }
      cl = rn
      print(cl)
      for(l in 1:length(cl)){
        dat_tmp = data_final
        dat_tmp[,i] = ifelse(dat_tmp[,i] == cl[l],1,0)
        fisher = fisher.test(x = dat_tmp[,i], y = dat_tmp[,"status2"])
        print(data_final[,i][data_final[,i] == cl[l]], na.rm = T)
        #meandiff = mean(data_final[,i][data_final[,i] == cl[l]], na.rm = T) - mean(data_final[,i][data_final[,i] != cl[l]], na.rm = T)
        eff[l] = log(fisher$estimate)
      }
    } else {
      meandiff = mean(data_final[,i][data_final[,"status2"] == 1], na.rm = T) - mean(data_final[,i][data_final[,"status2"] == 0], na.rm = T)
      eff = meandiff
      eff = exp(coef(mod))
      eff = log(eff[names(eff) != "(Intercept)"])
    }
    tmp$eff = eff
   # tmp$Type = ifelse(i %in% colnames_dat_fil_cat,"Categorical","Continuous")
    res[[i]] = tmp
    ftr[[i]] = rn
    eff_CI[[i]] = eff_CI_tmp
  }
  
  df = do.call("rbind", res) %>% as.data.frame() %>% rownames_to_column("Variables")
  df$Features = do.call("c", ftr)
  df$t_stat = df$Estimate/df$`Std. Error`
  df$fdr = p.adjust(df$`Pr(>|z|)`, "fdr")
  eff_CI_new = do.call("rbind",eff_CI) %>% as.data.frame() %>% rownames_to_column("Variables") 
  colnames(eff_CI_new) <- c("Variables","2.5 %","97.5 %")
  df <- df %>%
    left_join(eff_CI_new)
  return(list(df,eff_CI_new))
}


univariate_model_adjustment_new <- function(data_final,variable_adjust){
  res = list()
  ftr = list()
  eff_CI = list()
  for (i in colnames(data_final)){
    if (i %in% c("status", "status2",variable_adjust)){
      next
    }
    if (nlevels(as.factor(data_final[,i])) < 2){
      next
    }
    form = as.formula(paste("status2 ~",variable_adjust, "+ ", i))
    if(length(grep("Nein",data_final[,i]))!=0){
      unique_values <- unique(data_final[,i])
      if(length(unique_values) < 5){
        unique_values_ordered <- unique_values[str_order(unique_values,decreasing = T)]
        col_i <- factor(data_final[,i],levels = unique_values_ordered)
        data_final[,i] <- col_i
      }
    }
    mod = glm(form, data_final, family = "binomial")
    tmp = summary(mod)$coefficients
    rn = rownames(tmp)
    rn = rn[!rn %in% c("(Intercept)",variable_adjust)]
    rn = sub(i,"",rn,ignore.case = T)
    tmp = tmp[!rownames(tmp) %in% c("(Intercept)",variable_adjust),, drop = F] %>% as.data.frame()
    rownames(tmp) = rn
    eff = vector("numeric", length = length(rn))
    eff_CI_tmp = exp(confint.default(mod))
    eff_CI_tmp <- eff_CI_tmp[rownames(eff_CI_tmp) != "(Intercept)",, drop = F] %>% as.data.frame()
    # if (i %in% colnames_dat_fil_cat){
    #   if (length(grep(" ",rn)) > 0){
    #     cl = substr(rn, nchar(rn) - 1, nchar(rn))
    #   } else{
    #     cl = substr(rn, nchar(rn), nchar(rn))
    #   }
    #   cl = rn
    #   for(l in 1:length(cl)){
    #     dat_tmp = data_final
    #     dat_tmp[,i] = ifelse(dat_tmp[,i] == cl[l],1,0)
    #     fisher = fisher.test(x = dat_tmp[,i], y = dat_tmp[,"status2"])
    #     meandiff = mean(data_final[,i][data_final[,i] == cl[l]], na.rm = T) - mean(data_final[,i][data_final[,i] != cl[l]], na.rm = T)
    #     eff[l] = log(fisher$estimate)
    #   }
    # } else {
    #   meandiff = mean(data_final[,i][data_final[,"status2"] == 1], na.rm = T) - mean(data_final[,i][data_final[,"status2"] == 0], na.rm = T)
    #   eff = meandiff
    # }
    eff = (coef(mod))
    eff = eff[!names(eff) %in% c("(Intercept)",variable_adjust)] %>% as.data.frame()
    # print(eff)
    tmp$eff = eff[1,]
    # # tmp$Type = ifelse(i %in% colnames_dat_fil_cat,"Categorical","Continuous")
    # tmp = tmp[1,]
    eff_CI_tmp = eff_CI_tmp[1,]
    rownames(tmp) <- i
    rownames(eff_CI_tmp) <- i
    # print(tmp)
    # res[[i]] = tmp
    # ftr[[i]] = rn
    # eff_CI[[i]] = eff_CI_tmp
    # tmp$Type = ifelse(i %in% colnames_dat_fil_cat,"Categorical","Continuous")
    res[[i]] = tmp
    ftr[[i]] = rn
    eff_CI[[i]] = eff_CI_tmp
  }
  #print(res)
  print(eff_CI)
  #return(list(res,eff_CI))
  df = do.call("rbind",res) %>% as.data.frame() %>% rownames_to_column("Variables")
  #print(df)
  #df$Features = do.call("c", ftr)
  df$t_stat = df$Estimate/df$`Std. Error`
  df$fdr = p.adjust(df$`Pr(>|z|)`, "fdr")
  eff_CI_new = do.call("rbind",eff_CI) %>% as.data.frame() %>% rownames_to_column("Variables")
  colnames(eff_CI_new) <- c("Variables","2.5 %","97.5 %")
  df$"2.5 %" <- eff_CI_new["2.5 %"]
  df$"97.5 %" <- eff_CI_new[,"97.5 %"]
  return(list(df,eff_CI_new))
}
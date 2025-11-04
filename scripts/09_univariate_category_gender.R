### analysis of categories and subcategories stratified by female/male 

which_female <- dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich"
which_male <- dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich"

### Non-motor symptoms
# -> general
dat_final_nonmotor_general_female = dat_final_nonmotor_general[which_female,]
dat_final_nonmotor_general_male = dat_final_nonmotor_general[which_male,]

univariate_nonmotor_general_female <- univariate_model_new(dat_final_nonmotor_general_female)[[1]] %>%
  mutate(Variables = "general changes")

univariate_nonmotor_general_male <- univariate_model_new(dat_final_nonmotor_general_male)[[1]] %>%
  mutate(Variables = "general changes")

# -> non-motor related
dat_final_nonmotor_diff_all_female = dat_final_nonmotor_diff_all[which_female,]
dat_final_nonmotor_diff_all_male = dat_final_nonmotor_diff_all[which_male,]

univariate_nonmotor_diff_all_female <- univariate_model_new(dat_final_nonmotor_diff_all_female)[[1]]
univariate_nonmotor_diff_all_male <- univariate_model_new(dat_final_nonmotor_diff_all_male)[[1]]

# -> different non-motor (each subquestion individually)
df_subquestions_nonmotor_female <- list()
df_subquestions_nonmotor_male <- list()
aux_function_nonmotor <- function(data,which_rows){
  dat_final_nonmotor_diff_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_nonmotor_diff_i_temp <- apply(dat_final_nonmotor_diff_i[,1:ncol(dat_final_nonmotor_diff_i)-1], 2, function(x) ifelse(!is.na(x),1,0))
  dat_final_nonmotor_diff_i <- data.frame(subquestion_i = apply(dat_final_nonmotor_diff_i_temp, 1, function(x) {
    nonmotor_diff_all <- sum(x)
  }),status2 = dat_final_nonmotor_diff_i[,"status2"])
  return(dat_final_nonmotor_diff_i)
}
for (i in 1:length(subquestions_nonmotor)) {
  subquestion_i <- subquestions_nonmotor[i]
  final_ALS_CTR_category_nonmotor_diff_i <- final_ALS_CTR_category_nonmotor_diff %>%
    filter(`sub-category` == subquestion_i)
  dat_final_nonmotor_diff_i_female = aux_function_nonmotor(final_ALS_CTR_category_nonmotor_diff_i,which_female)
  dat_final_nonmotor_diff_i_male = aux_function_nonmotor(final_ALS_CTR_category_nonmotor_diff_i,which_male)
  df_subquestions_nonmotor_female[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_female)[[1]]
  df_subquestions_nonmotor_male[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_male)[[1]]
}

univariate_nonmotor_diff_subquestion_female <- do.call("rbind", df_subquestions_nonmotor_female) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_nonmotor)
univariate_nonmotor_diff_subquestion_male <- do.call("rbind", df_subquestions_nonmotor_male) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_nonmotor)

univariate_nonmotor_female <- do.call("rbind", list(univariate_nonmotor_general_female,
                                                    univariate_nonmotor_diff_all_female,
                                                    univariate_nonmotor_diff_subquestion_female))
univariate_nonmotor_male <- do.call("rbind", list(univariate_nonmotor_general_male,
                                                    univariate_nonmotor_diff_all_male,
                                                    univariate_nonmotor_diff_subquestion_male))

univariate_nonmotor_female <- preparate_data_forest(univariate_nonmotor_female)
univariate_nonmotor_male <- preparate_data_forest(univariate_nonmotor_male)

writexl::write_xlsx(univariate_nonmotor_female,"univariate_nonmotor_female.xlsx")
writexl::write_xlsx(univariate_nonmotor_male,"univariate_nonmotor_male.xlsx")

univariate_nonmotor_female_male <- rbindx(univariate_nonmotor_female %>%
                                              mutate(type = rep("female",nrow(.))),
                                          univariate_nonmotor_male %>%
                                            select(-index) %>%
                                            mutate(index = match(Labels_plot,
                                                                 univariate_nonmotor_female$Labels_plot),
                                                   type = rep("male",nrow(.))))

pdf("plots/univariate_nonmotor_female_male.pdf")
odds_ratio_plot_female_male(univariate_nonmotor_female_male,"Non-motor female and male")
dev.off()
pdf("plots/volcano_nonmotor_pvalue_female.pdf")
volcano_plot(univariate_nonmotor_female,"p-value","Non-motor symptoms Female (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_pvalue_male.pdf")
volcano_plot(univariate_nonmotor_male,"p-value","Non-motor symptoms Male (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_padj_female.pdf")
volcano_plot(univariate_nonmotor_female,"padj","Non-motor symptoms Female (p-adjusted)")
dev.off()
pdf("plots/volcano_nonmotor_padj_male.pdf")
volcano_plot(univariate_nonmotor_male,"padj","Non-motor symptoms Male (p-adjusted)")
dev.off()

## -> Different Non-motor symptoms (each sub-subcategories)
df_subsubquestions_nonmotor_female <- list()
df_subsubquestions_nonmotor_male <- list()
aux_function_nonmotor_subsubquestion <- function(data,which_rows){
  dat_final_nonmotor_diff_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_nonmotor_diff_i_temp <- ifelse(!is.na(dat_final_nonmotor_diff_i[,1:ncol(dat_final_nonmotor_diff_i)-1]),1,0)
  dat_final_nonmotor_diff_i <- data.frame(subsubquestion_i = dat_final_nonmotor_diff_i_temp,status2 = dat_final_nonmotor_diff_i[,"status2"])
  return(dat_final_nonmotor_diff_i)
}
subsubquestions_nonmotor_temp <- subsubquestions_nonmotor[c(1:69,71:78)]
for (i in 1:length(subsubquestions_nonmotor_temp)) {
  subsubquestion_i <- subsubquestions_nonmotor_temp[i]
  final_ALS_CTR_category_nonmotor_diff_i <- final_ALS_CTR_category_nonmotor_diff %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_nonmotor_diff_i_female <- aux_function_nonmotor_subsubquestion(final_ALS_CTR_category_nonmotor_diff_i,which_female)
  dat_final_nonmotor_diff_i_male <- aux_function_nonmotor_subsubquestion(final_ALS_CTR_category_nonmotor_diff_i,which_male)
  df_subsubquestions_nonmotor_female[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_female)[[1]]
  df_subsubquestions_nonmotor_male[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_male)[[1]]
}

univariate_nonmotor_diff_subsubquestion_female <- do.call("rbind", df_subsubquestions_nonmotor_female) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_nonmotor_temp)
univariate_nonmotor_diff_subsubquestion_male <- do.call("rbind", df_subsubquestions_nonmotor_male) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_nonmotor_temp)

univariate_nonmotor_subsubquestion_female <- preparate_data_forest(univariate_nonmotor_diff_subsubquestion_female)
univariate_nonmotor_subsubquestion_female[univariate_nonmotor_subsubquestion_female$Variables == "Häufig verstopfte Nase",]$Labels_plot <- "Frequent runny nose"
univariate_nonmotor_subsubquestion_male <- preparate_data_forest(univariate_nonmotor_diff_subsubquestion_male)
univariate_nonmotor_subsubquestion_male[univariate_nonmotor_subsubquestion_male$Variables == "Häufig verstopfte Nase",]$Labels_plot <- "Frequent runny nose"

writexl::write_xlsx(univariate_nonmotor_subsubquestion_female,"univariate_nonmotor_subsubquestion_female.xlsx")
writexl::write_xlsx(univariate_nonmotor_subsubquestion_male,"univariate_nonmotor_subsubquestion_male.xlsx")

univariate_nonmotor_subsubquestion_female_male <- rbindx(univariate_nonmotor_subsubquestion_female %>%
                                            mutate(type = rep("female",nrow(.))),
                                            univariate_nonmotor_subsubquestion_male %>%
                                              select(-index) %>%
                                              mutate(index = match(Labels_plot,
                                                                   univariate_nonmotor_subsubquestion_female$Labels_plot),
                                                     type = rep("male",nrow(.))))

pdf("plots/univariate_nonmotor_subsubquestion_female_male.pdf",width=10,height=12)
odds_ratio_plot_female_male(univariate_nonmotor_subsubquestion_female_male,"Non-motor subquestion female and male")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_pvalue_female.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_female,"p-value","Non-motor symptoms Female (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_pvalue_male.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_male,"p-value","Non-motor symptoms Male (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_padj_female.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_female,"padj","Non-motor symptoms Female (p-adjusted)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_padj_male.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_male,"padj","Non-motor symptoms Male (p-adjusted)")
dev.off()

## Contacts healthcare system
df_subquestions_healthcare_female <- list()
df_subquestions_healthcare_male <- list()
aux_function_healthcare <- function(data,which_rows){
  dat_final_healthcare_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_healthcare_i <- data.frame(subquestion_i = apply(dat_final_healthcare_i[,1:ncol(dat_final_healthcare_i)-1], 1, function(x) {
    healthcare_all <- sum(x)
  }),status2 = dat_final_healthcare_i[,"status2"])
  return(dat_final_healthcare_i)
}
for (i in 1:length(subquestions_healthcare)) {
  subquestion_i <- subquestions_healthcare[i]
  final_ALS_CTR_category_healthcare_i <- final_ALS_CTR_category_healthcare %>%
    filter(`sub-category` == subquestion_i)
  dat_final_healthcare_i_female <- aux_function_healthcare(final_ALS_CTR_category_healthcare_i,which_female)
  dat_final_healthcare_i_male <- aux_function_healthcare(final_ALS_CTR_category_healthcare_i,which_male)
  df_subquestions_healthcare_female[[i]] <- univariate_model_new(dat_final_healthcare_i_female)[[1]]
  df_subquestions_healthcare_male[[i]] <- univariate_model_new(dat_final_healthcare_i_male)[[1]]
}

univariate_healthcare_female <- do.call("rbind", df_subquestions_healthcare_female) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_healthcare)
univariate_healthcare_female <- preparate_data_forest(univariate_healthcare_female)
univariate_healthcare_male <- do.call("rbind", df_subquestions_healthcare_male) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_healthcare)
univariate_healthcare_male <- preparate_data_forest(univariate_healthcare_male)

writexl::write_xlsx(univariate_healthcare_female,"univariate_healthcare_female.xlsx")
writexl::write_xlsx(univariate_healthcare_male,"univariate_healthcare_male.xlsx")

univariate_healthcare_female_male <- rbindx(univariate_healthcare_female %>%
                                              mutate(type = rep("female",nrow(.))),
                                            univariate_healthcare_male %>%
                                              select(-index) %>%
                                              mutate(index = match(Labels_plot,
                                                                   univariate_healthcare_female$Labels_plot),
                                                     type = rep("male",nrow(.))))

pdf("plots/univariate_healthcare_female_male.pdf")
odds_ratio_plot_female_male(univariate_healthcare_female_male,"Contacts healthcare female and male")
dev.off()
pdf("plots/volcano_healthcare_pvalue_female.pdf")
volcano_plot(univariate_healthcare_female,"p-value","Contacts healthcare Female (p-value)")
dev.off()
pdf("plots/volcano_healthcare_pvalue_male.pdf")
volcano_plot(univariate_healthcare_male,"p-value","Contacts healthcare Male (p-value)")
dev.off()
pdf("plots/volcano_healthcare_padj_female.pdf")
volcano_plot(univariate_healthcare_female,"padj","Contacts healthcare Female (p-adjusted)")
dev.off()
pdf("plots/volcano_healthcare_padj_male.pdf")
volcano_plot(univariate_healthcare_male,"padj","Contacts healthcare Male (p-adjusted)")
dev.off()

## Pre-existing conditions
dat_final_preconditions_all_female = dat_final_preconditions_all[which_female,]
dat_final_preconditions_all_male = dat_final_preconditions_all[which_male,]

univariate_preconditions_all_female <- univariate_model_new(dat_final_preconditions_all_female)[[1]]
univariate_preconditions_all_male <- univariate_model_new(dat_final_preconditions_all_male)[[1]]

# -> Pre-existing conditions (each subquestion individually)
df_subquestions_preconditions_female <- list()
df_subquestions_preconditions_male <- list()
aux_function_precondition <- function(data,which_rows){
  dat_final_preconditions_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_preconditions_i_temp <- apply(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
  dat_final_preconditions_i <- data.frame(subquestion_i = apply(dat_final_preconditions_i_temp, 1, function(x) {
    preconditions_all <- sum(x)
  }),status2 = dat_final_preconditions_i[,"status2"])
  return(dat_final_preconditions_i)
}
#for (i in 1:length(subquestions_preconditions)) {
for (i in c(1:12,14)) {
  subquestion_i <- subquestions_preconditions[i]
  final_ALS_CTR_category_preconditions_i <- final_ALS_CTR_category_preconditions %>%
    filter(`sub-category` == subquestion_i)
  dat_final_preconditions_i_female <- aux_function_precondition(final_ALS_CTR_category_preconditions_i,which_female)
  dat_final_preconditions_i_male <- aux_function_precondition(final_ALS_CTR_category_preconditions_i,which_male)
  df_subquestions_preconditions_female[[i]] <- univariate_model_new(dat_final_preconditions_i_female)[[1]]
  df_subquestions_preconditions_male[[i]] <- univariate_model_new(dat_final_preconditions_i_male)[[1]]
}

univariate_preconditions_subquestion_female <- do.call("rbind", df_subquestions_preconditions_female) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_preconditions[c(1:12,14)])
univariate_preconditions_subquestion_male <- do.call("rbind", df_subquestions_preconditions_male) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_preconditions[c(1:12,14)])

# -> all results together
univariate_preconditions_female <- do.call("rbind", list(univariate_preconditions_all_female %>% select(-fdr),
                                                         univariate_preconditions_subquestion_female))
univariate_preconditions_female <- preparate_data_forest(univariate_preconditions_female)
univariate_preconditions_male <- do.call("rbind", list(univariate_preconditions_all_male %>% select(-fdr),
                                                         univariate_preconditions_subquestion_male))
univariate_preconditions_male <- preparate_data_forest(univariate_preconditions_male)

writexl::write_xlsx(univariate_preconditions_female,"univariate_preconditions_female.xlsx")
writexl::write_xlsx(univariate_preconditions_male,"univariate_preconditions_male.xlsx")

univariate_preconditions_female <- univariate_preconditions_female[!duplicated(univariate_preconditions_female$Labels_plot),]
univariate_preconditions_male <- univariate_preconditions_male[!duplicated(univariate_preconditions_male$Labels_plot),]

univariate_preconditions_female_male <- rbindx(univariate_preconditions_female %>%
                                                 mutate(type = rep("female",nrow(.))),
                                               univariate_preconditions_male %>%
                                                 select(-index) %>%
                                                 mutate(index = match(Labels_plot,
                                                                                univariate_preconditions_female$Labels_plot),
                                                        type = rep("male",nrow(.))))

pdf("plots/univariate_preconditions_female_male.pdf")
odds_ratio_plot_female_male(univariate_preconditions_female_male,"Pre-existing conditions female and male")
dev.off()
pdf("plots/volcano_preconditions_pvalue_female.pdf")
volcano_plot(univariate_preconditions_female,"p-value","Pre-existing conditions (p-value) female")
dev.off()
pdf("plots/volcano_preconditions_padj_female.pdf")
volcano_plot(univariate_preconditions_female,"padj","Pre-existing conditions (p-adjusted) female")
dev.off()
pdf("plots/volcano_preconditions_pvalue_male.pdf")
volcano_plot(univariate_preconditions_male,"p-value","Pre-existing conditions (p-value) male")
dev.off()
pdf("plots/volcano_preconditions_padj_male.pdf")
volcano_plot(univariate_preconditions_male,"padj","Pre-existing conditions (p-adjusted) male")
dev.off()

# -> Pre-existing conditions (each subsubquestion individually)
subsubquestions_preconditions_tmp <- subsubquestions_preconditions
subsubquestions_preconditions_tmp <- subsubquestions_preconditions_tmp[c(1:14,16,18:20,23:24,26,28:29,32:35,37:43,45:48,51,56:58,60,68:69)]
df_subsubquestions_preconditions_female <- list()
df_subsubquestions_preconditions_male <- list()

aux_function_precondition_subquestion <- function(data,which_rows){
  dat_final_preconditions_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_preconditions_i_temp <- ifelse(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1] == "Ja",1,0)
  dat_final_preconditions_i <- data.frame(subsubquestion_i = dat_final_preconditions_i_temp,status2 = dat_final_preconditions_i[,"status2"])
  return(dat_final_preconditions_i)
}
for (i in 1:length(subsubquestions_preconditions_tmp)) {
  subsubquestion_i <- subsubquestions_preconditions_tmp[i]
  final_ALS_CTR_category_preconditions_i <- final_ALS_CTR_category_preconditions %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_preconditions_i_female <- aux_function_precondition_subquestion(final_ALS_CTR_category_preconditions_i,which_female)
  dat_final_preconditions_i_male <- aux_function_precondition_subquestion(final_ALS_CTR_category_preconditions_i,which_male)
  df_subsubquestions_preconditions_female[[i]] <- univariate_model_new(dat_final_preconditions_i_female)[[1]]
  df_subsubquestions_preconditions_male[[i]] <- univariate_model_new(dat_final_preconditions_i_male)[[1]]
}

univariate_preconditions_subsubquestion_female <- do.call("rbind", df_subsubquestions_preconditions_female) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_preconditions_tmp)
univariate_preconditions_subsubquestion_male <- do.call("rbind", df_subsubquestions_preconditions_male) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_preconditions_tmp)

# -> all results together
univariate_preconditions_subsubquestion_female <- preparate_data_forest(univariate_preconditions_subsubquestion_female)
univariate_preconditions_subsubquestion_male <- preparate_data_forest(univariate_preconditions_subsubquestion_male)

writexl::write_xlsx(univariate_preconditions_subsubquestion_female,"univariate_preconditions_subsubquestion_female.xlsx")
writexl::write_xlsx(univariate_preconditions_subsubquestion_male,"univariate_preconditions_subsubquestion_male.xlsx")

univariate_preconditions_subsubquestion_female <- univariate_preconditions_subsubquestion_female[!duplicated(univariate_preconditions_subsubquestion_female$Labels_plot),]
univariate_preconditions_subsubquestion_male <- univariate_preconditions_subsubquestion_male[!duplicated(univariate_preconditions_subsubquestion_male$Labels_plot),]
univariate_preconditions_subsubquestion_female$Labels_plot %in% univariate_preconditions_subsubquestion_male$Labels_plot

univariate_preconditions_subsubquestion_female_male <- rbindx(univariate_preconditions_subsubquestion_female %>%
                                                 mutate(type = rep("female",nrow(.))),
                                                 univariate_preconditions_subsubquestion_male %>%
                                                   select(-index) %>%
                                                   mutate(index = match(Labels_plot,
                                                                        univariate_preconditions_subsubquestion_female$Labels_plot),
                                                          type = rep("male",nrow(.))))

pdf("plots/univariate_preconditions_subsubquestion_female_male.pdf")
odds_ratio_plot_female_male(univariate_preconditions_subsubquestion_female_male,"Pre-existing conditions female and male")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_pvalue_female.pdf")
volcano_plot(univariate_preconditions_subsubquestion_female,"p-value","Pre-existing conditions subquestion (p-value) female")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_padj_female.pdf")
volcano_plot(univariate_preconditions_subsubquestion_female,"padj","Pre-existing conditions subquestion (p-adjusted) female")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_pvalue_male.pdf")
volcano_plot(univariate_preconditions_subsubquestion_male,"p-value","Pre-existing conditions subquestion (p-value) male")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_padj_male.pdf")
volcano_plot(univariate_preconditions_subsubquestion_male,"padj","Pre-existing conditions subquestion (p-adjusted) male")
dev.off()

## Diet and weight

# -> Diet and weight (each subquestion individually)
dat_final_dietweight_all_female <- dat_final_dietweight_all[which_female,]
dat_final_dietweight_all_male <- dat_final_dietweight_all[which_male,]
univariate_dietweight_all_female <- univariate_model_new(dat_final_dietweight_all_female)[[1]]
univariate_dietweight_all_male <- univariate_model_new(dat_final_dietweight_all_male)[[1]]

univariate_dietweight_all_female <- univariate_dietweight_all_female %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`),
         fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr)) %>%
  arrange(log10_pval)  %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.)) %>%
  left_join(map_questions_plot)

univariate_dietweight_all_male <- univariate_dietweight_all_male %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`),
         fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr)) %>%
  arrange(log10_pval)  %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.)) %>%
  left_join(map_questions_plot)

writexl::write_xlsx(univariate_dietweight_all_female,"univariate_dietweight_female.xlsx")
writexl::write_xlsx(univariate_dietweight_all_male,"univariate_dietweight_male.xlsx")

univariate_dietweight_all_female_male <- rbindx(univariate_dietweight_all_female %>%
                                                                mutate(type = rep("female",nrow(.))),
                                                univariate_dietweight_all_male %>%
                                                  select(-index) %>%
                                                  mutate(index = match(Labels_plot,
                                                                       univariate_dietweight_all_female$Labels_plot),
                                                         type = rep("male",nrow(.))))

pdf("plots/univariate_dietweight_female_male.pdf")
odds_ratio_plot_female_male(univariate_dietweight_all_female_male,"Diet and weight female and male")
dev.off()
pdf("plots/volcano_dietweight_pvalue_female.pdf")
volcano_plot(univariate_dietweight_all_female,"p-value","Diet and weight (p-value) female")
dev.off()
pdf("plots/volcano_dietweight_padj_female.pdf")
volcano_plot(univariate_dietweight_all_female,"padj","Diet and weights (p-adjusted) female")
dev.off()
pdf("plots/volcano_dietweight_pvalue_male.pdf")
volcano_plot(univariate_dietweight_all_male,"p-value","Diet and weight (p-value) male")
dev.off()
pdf("plots/volcano_dietweight_padj_male.pdf")
volcano_plot(univariate_dietweight_all_male,"padj","Diet and weights (p-adjusted) male")
dev.off()

## Lifestyle

# -> Lifestyle (separate the ones with and without subcategories)
# -> first only the ones without subcategories
final_ALS_CTR_category_lifestyle_nosubcategories <- final_ALS_CTR_category_lifestyle %>%
  filter(is.na(`sub-subcategory`))

dat_final_lifestyle_nosubcategories<- dat_final[,c(which(original_names %in% final_ALS_CTR_category_lifestyle_nosubcategories$`original question (ALS)`),ncol(dat_final))]
dat_final_lifestyle_nosubcategories_female <- dat_final_lifestyle_nosubcategories[which_female,]
dat_final_lifestyle_nosubcategories_male <- dat_final_lifestyle_nosubcategories[which_male,]

univariate_lifestyle_nosubcategories_female_temp <- univariate_model_new(dat_final_lifestyle_nosubcategories_female)
univariate_lifestyle_nosubcategories_male_temp <- univariate_model_new(dat_final_lifestyle_nosubcategories_male)
dat_final_lifestyle_nosubcategories_female <- cbind(univariate_lifestyle_nosubcategories_female_temp[[1]][,1:9],
                                                    univariate_lifestyle_nosubcategories_female_temp[[2]][,2:3])
dat_final_lifestyle_nosubcategories_male <- cbind(univariate_lifestyle_nosubcategories_male_temp[[1]][,1:9],
                                                    univariate_lifestyle_nosubcategories_male_temp[[2]][,2:3])

# -> the ones with categories all together
df_subquestions_lifestyle_female <- list()
df_subquestions_lifestyle_male <- list()
aux_function_lifestyle <- function(data,which_rows){
  dat_final_subquestions_lifestyle_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_subquestions_lifestyle_i_temp <- apply(dat_final_subquestions_lifestyle_i[,1:ncol(dat_final_subquestions_lifestyle_i)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
  dat_final_subquestions_lifestyle_i <- data.frame(subquestion_i = apply(dat_final_subquestions_lifestyle_i_temp, 1, function(x) {
    lifestyle_all <- sum(x)
    lifestyle_all <- ifelse(lifestyle_all > 0,1,0)
  }),status2 = dat_final_subquestions_lifestyle_i[,"status2"])
  return(dat_final_subquestions_lifestyle_i)
}
for (i in 1:length(subquestions_lifestyle)) {
  subquestion_i <- subquestions_lifestyle[i]
  final_ALS_CTR_category_subquestions_lifestyle_i <- final_ALS_CTR_category_lifestyle_subcategories %>%
    filter(`sub-category` == subquestion_i)
  dat_final_subquestions_lifestyle_i_female <- aux_function_lifestyle(final_ALS_CTR_category_subquestions_lifestyle_i,which_female)
  dat_final_subquestions_lifestyle_i_male <- aux_function_lifestyle(final_ALS_CTR_category_subquestions_lifestyle_i,which_male)
  df_subquestions_lifestyle_female[[i]] <- univariate_model_new(dat_final_subquestions_lifestyle_i_female)[[1]]
  df_subquestions_lifestyle_male[[i]] <- univariate_model_new(dat_final_subquestions_lifestyle_i_male)[[1]]
}

univariate_lifestyle_subquestion_female <- do.call("rbind", df_subquestions_lifestyle_female) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_lifestyle)
univariate_lifestyle_subquestion_male <- do.call("rbind", df_subquestions_lifestyle_male) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_lifestyle)

# -> the ones with categories separate
subsubquestions_lifestyle <- final_ALS_CTR_category_lifestyle_subcategories$`sub-subcategory` %>% unique()
subsubquestions_lifestyle_tpm <- subsubquestions_lifestyle[c(1:6,8:14)]
df_subsubquestions_lifestyle_female <- list()
df_subsubquestions_lifestyle_male <- list()
aux_function_lifestyle_subquestions <- function(data,which_rows){
  dat_final_subsubquestions_lifestyle_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_subsubquestions_lifestyle_i_temp <- apply(dat_final_subsubquestions_lifestyle_i[,1:ncol(dat_final_subsubquestions_lifestyle_i)-1], 2, function(x) ifelse(x == "Ja",1,0))
  dat_final_subsubquestions_lifestyle_i <- data.frame(subsubquestion_i = apply(dat_final_subsubquestions_lifestyle_i_temp, 1, function(x) {
    lifestyle_all <- sum(x)
    lifestyle_all <- ifelse(lifestyle_all > 0,1,0)
  }),status2 = dat_final_subsubquestions_lifestyle_i[,"status2"])
  return(dat_final_subsubquestions_lifestyle_i)
}
for (i in 1:length(subsubquestions_lifestyle_tpm)) {
  subsubquestion_i <- subsubquestions_lifestyle_tpm[i]
  final_ALS_CTR_category_subsubquestions_lifestyle_i <- final_ALS_CTR_category_lifestyle_subcategories %>%
    filter(`sub-subcategory` == subsubquestion_i)
  final_ALS_CTR_category_subsubquestions_lifestyle_i <- final_ALS_CTR_category_lifestyle_subcategories[42,]
  dat_final_subsubquestions_lifestyle_i_female <- aux_function_lifestyle_subquestions(final_ALS_CTR_category_subsubquestions_lifestyle_i,which_female)
  dat_final_subsubquestions_lifestyle_i_male <- aux_function_lifestyle_subquestions(final_ALS_CTR_category_subsubquestions_lifestyle_i,which_male)
  df_subsubquestions_lifestyle_female[[i]] <- univariate_model_new(dat_final_subsubquestions_lifestyle_i_female)[[1]]
  df_subsubquestions_lifestyle_male[[i]] <- univariate_model_new(dat_final_subsubquestions_lifestyle_i_male)[[1]]
}

univariate_lifestyle_subsubquestion_female <- do.call("rbind", df_subsubquestions_lifestyle_female) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_lifestyle_tpm)
univariate_lifestyle_subsubquestion_male <- do.call("rbind", df_subsubquestions_lifestyle_male) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_lifestyle_tpm)

# -> all results together
univariate_lifestyle_female <- do.call("rbind", list(dat_final_lifestyle_nosubcategories_female,
                                                      univariate_lifestyle_subquestion_female,
                                                      univariate_lifestyle_subsubquestion_female))
dat_final_lifestyle_nosubcategories_male <- dat_final_lifestyle_nosubcategories_male[c(1:17,19:34),]
univariate_lifestyle_male <- do.call("rbind", list(dat_final_lifestyle_nosubcategories_male,
                                                     univariate_lifestyle_subquestion_male,
                                                     univariate_lifestyle_subsubquestion_male))

univariate_lifestyle_female <- preparate_data_forest(univariate_lifestyle_female)
univariate_lifestyle_male <- preparate_data_forest(univariate_lifestyle_male)

writexl::write_xlsx(univariate_lifestyle_female,"univariate_lifestyle_female.xlsx")
writexl::write_xlsx(univariate_lifestyle_male,"univariate_lifestyle_male.xlsx")

univariate_lifestyle_female_male <- rbindx(univariate_lifestyle_female %>%
                                                  mutate(type = rep("female",nrow(.))),
                                           univariate_lifestyle_male %>%
                                             select(-index) %>%
                                             mutate(index = match(Labels_plot,
                                                                  univariate_lifestyle_female$Labels_plot),
                                                    type = rep("male",nrow(.))))

pdf("plots/univariate_lifestyle_female_male.pdf")
odds_ratio_plot_female_male(univariate_lifestyle_female_male,"Lifestyle female and male")
dev.off()
pdf("plots/volcano_lifestyle_pvalue_female.pdf")
volcano_plot(univariate_lifestyle_female,"p-value","Lifestyle (p-value) female")
dev.off()
pdf("plots/volcano_lifestyle_padj_female.pdf")
volcano_plot(univariate_lifestyle_female,"padj","Lifestyle (p-adjusted) female")
dev.off()
pdf("plots/volcano_lifestyle_pvalue_male.pdf")
volcano_plot(univariate_lifestyle_male,"p-value","Lifestyle (p-value) male")
dev.off()
pdf("plots/volcano_lifestyle_padj_male.pdf")
volcano_plot(univariate_lifestyle_male,"padj","Lifestyle (p-adjusted) male")
dev.off()


## ADDITIONAL FUNCTIONS
odds_ratio_plot_female_male <- function(data,title_plot){
  labels_use = data$Labels_plot %>% unique()
  ggplot(data=data, aes(y=index, x=mean, xmin=lower, xmax=upper,color=type)) +
    geom_point() +
    geom_errorbarh(height=.1) +
    scale_y_continuous(breaks=1:length(labels_use), 
                       labels=labels_use) +
    #xlim(-5,5) +
    labs(title=title_plot, x='log(odds-ratio)', y = 'Question') +
    geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
    theme_minimal()
}


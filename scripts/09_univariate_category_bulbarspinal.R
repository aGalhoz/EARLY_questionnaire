### analysis of categories and subcategories stratified by bulbar/spinal

which_bulbar <- dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich"
which_spinal <- dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich"

### Non-motor symptoms
# -> general
dat_final_nonmotor_general_bulbar = dat_final_nonmotor_general[which_bulbar,]
dat_final_nonmotor_general_spinal = dat_final_nonmotor_general[which_spinal,]

univariate_nonmotor_general_bulbar <- univariate_model_new(dat_final_nonmotor_general_bulbar)[[1]] %>%
  mutate(Variables = "general changes")

univariate_nonmotor_general_spinal <- univariate_model_new(dat_final_nonmotor_general_spinal)[[1]] %>%
  mutate(Variables = "general changes")

# -> non-motor related
dat_final_nonmotor_diff_all_bulbar = dat_final_nonmotor_diff_all[which_bulbar,]
dat_final_nonmotor_diff_all_spinal = dat_final_nonmotor_diff_all[which_spinal,]

univariate_nonmotor_diff_all_bulbar <- univariate_model_new(dat_final_nonmotor_diff_all_bulbar)[[1]]
univariate_nonmotor_diff_all_spinal <- univariate_model_new(dat_final_nonmotor_diff_all_spinal)[[1]]

# -> different non-motor (each subquestion individually)
df_subquestions_nonmotor_bulbar <- list()
df_subquestions_nonmotor_spinal <- list()
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
  dat_final_nonmotor_diff_i_bulbar = aux_function_nonmotor(final_ALS_CTR_category_nonmotor_diff_i,which_bulbar)
  dat_final_nonmotor_diff_i_spinal = aux_function_nonmotor(final_ALS_CTR_category_nonmotor_diff_i,which_spinal)
  df_subquestions_nonmotor_bulbar[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_bulbar)[[1]]
  df_subquestions_nonmotor_spinal[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_spinal)[[1]]
}

univariate_nonmotor_diff_subquestion_bulbar <- do.call("rbind", df_subquestions_nonmotor_bulbar) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_nonmotor)
univariate_nonmotor_diff_subquestion_spinal <- do.call("rbind", df_subquestions_nonmotor_spinal) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_nonmotor)

univariate_nonmotor_bulbar <- do.call("rbind", list(univariate_nonmotor_general_bulbar,
                                                    univariate_nonmotor_diff_all_bulbar,
                                                    univariate_nonmotor_diff_subquestion_bulbar))
univariate_nonmotor_spinal <- do.call("rbind", list(univariate_nonmotor_general_spinal,
                                                  univariate_nonmotor_diff_all_spinal,
                                                  univariate_nonmotor_diff_subquestion_spinal))

univariate_nonmotor_bulbar <- preparate_data_forest(univariate_nonmotor_bulbar)
univariate_nonmotor_spinal <- preparate_data_forest(univariate_nonmotor_spinal)

writexl::write_xlsx(univariate_nonmotor_bulbar,"univariate_nonmotor_bulbar.xlsx")
writexl::write_xlsx(univariate_nonmotor_spinal,"univariate_nonmotor_spinal.xlsx")

univariate_nonmotor_bulbar_spinal <- rbindx(univariate_nonmotor_bulbar %>%
                                            mutate(type = rep("bulbar",nrow(.))),
                                          univariate_nonmotor_spinal %>%
                                            select(-index) %>%
                                            mutate(index = match(Labels_plot,
                                                                 univariate_nonmotor_bulbar$Labels_plot),
                                                   type = rep("spinal",nrow(.))))

pdf("plots/univariate_nonmotor_bulbar_spinal.pdf")
odds_ratio_plot_bulbar_spinal(univariate_nonmotor_bulbar_spinal,"Non-motor bulbar and spinal")
dev.off()
pdf("plots/volcano_nonmotor_pvalue_bulbar.pdf")
volcano_plot(univariate_nonmotor_bulbar,"p-value","Non-motor symptoms bulbar (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_pvalue_spinal.pdf")
volcano_plot(univariate_nonmotor_spinal,"p-value","Non-motor symptoms spinal (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_padj_bulbar.pdf")
volcano_plot(univariate_nonmotor_bulbar,"padj","Non-motor symptoms bulbar (p-adjusted)")
dev.off()
pdf("plots/volcano_nonmotor_padj_spinal.pdf")
volcano_plot(univariate_nonmotor_spinal,"padj","Non-motor symptoms spinal (p-adjusted)")
dev.off()

## -> Different Non-motor symptoms (each sub-subcategories)
df_subsubquestions_nonmotor_bulbar <- list()
df_subsubquestions_nonmotor_spinal <- list()
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
  dat_final_nonmotor_diff_i_bulbar <- aux_function_nonmotor_subsubquestion(final_ALS_CTR_category_nonmotor_diff_i,which_bulbar)
  dat_final_nonmotor_diff_i_spinal <- aux_function_nonmotor_subsubquestion(final_ALS_CTR_category_nonmotor_diff_i,which_spinal)
  df_subsubquestions_nonmotor_bulbar[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_bulbar)[[1]]
  df_subsubquestions_nonmotor_spinal[[i]] <- univariate_model_new(dat_final_nonmotor_diff_i_spinal)[[1]]
}

univariate_nonmotor_diff_subsubquestion_bulbar <- do.call("rbind", df_subsubquestions_nonmotor_bulbar) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_nonmotor_temp)
univariate_nonmotor_diff_subsubquestion_spinal <- do.call("rbind", df_subsubquestions_nonmotor_spinal) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_nonmotor_temp)

univariate_nonmotor_subsubquestion_bulbar <- preparate_data_forest(univariate_nonmotor_diff_subsubquestion_bulbar)
univariate_nonmotor_subsubquestion_bulbar[univariate_nonmotor_subsubquestion_bulbar$Variables == "Häufig verstopfte Nase",]$Labels_plot <- "Frequent runny nose"
univariate_nonmotor_subsubquestion_spinal <- preparate_data_forest(univariate_nonmotor_diff_subsubquestion_spinal)
univariate_nonmotor_subsubquestion_spinal[univariate_nonmotor_subsubquestion_spinal$Variables == "Häufig verstopfte Nase",]$Labels_plot <- "Frequent runny nose"

writexl::write_xlsx(univariate_nonmotor_subsubquestion_bulbar,"univariate_nonmotor_subsubquestion_bulbar.xlsx")
writexl::write_xlsx(univariate_nonmotor_subsubquestion_spinal,"univariate_nonmotor_subsubquestion_spinal.xlsx")

univariate_nonmotor_subsubquestion_bulbar_spinal <- rbindx(univariate_nonmotor_subsubquestion_bulbar %>%
                                                           mutate(type = rep("bulbar",nrow(.))),
                                                         univariate_nonmotor_subsubquestion_spinal %>%
                                                           select(-index) %>%
                                                           mutate(index = match(Labels_plot,
                                                                                univariate_nonmotor_subsubquestion_bulbar$Labels_plot),
                                                                  type = rep("spinal",nrow(.))))

pdf("plots/univariate_nonmotor_subsubquestion_bulbar_spinal.pdf",width=10,height=12)
odds_ratio_plot_bulbar_spinal(univariate_nonmotor_subsubquestion_bulbar_spinal,"Non-motor subquestion bulbar and spinal")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_pvalue_bulbar.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_bulbar,"p-value","Non-motor symptoms bulbar (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_pvalue_spinal.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_spinal,"p-value","Non-motor symptoms spinal (p-value)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_padj_bulbar.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_bulbar,"padj","Non-motor symptoms bulbar (p-adjusted)")
dev.off()
pdf("plots/volcano_nonmotor_subsubquestion_padj_spinal.pdf")
volcano_plot(univariate_nonmotor_subsubquestion_spinal,"padj","Non-motor symptoms spinal (p-adjusted)")
dev.off()

## Contacts healthcare system
df_subquestions_healthcare_bulbar <- list()
df_subquestions_healthcare_spinal <- list()
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
  dat_final_healthcare_i_bulbar <- aux_function_healthcare(final_ALS_CTR_category_healthcare_i,which_bulbar)
  dat_final_healthcare_i_spinal <- aux_function_healthcare(final_ALS_CTR_category_healthcare_i,which_spinal)
  df_subquestions_healthcare_bulbar[[i]] <- univariate_model_new(dat_final_healthcare_i_bulbar)[[1]]
  df_subquestions_healthcare_spinal[[i]] <- univariate_model_new(dat_final_healthcare_i_spinal)[[1]]
}

univariate_healthcare_bulbar <- do.call("rbind", df_subquestions_healthcare_bulbar) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_healthcare)
univariate_healthcare_bulbar <- preparate_data_forest(univariate_healthcare_bulbar)
univariate_healthcare_spinal <- do.call("rbind", df_subquestions_healthcare_spinal) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_healthcare)
univariate_healthcare_spinal <- preparate_data_forest(univariate_healthcare_spinal)

writexl::write_xlsx(univariate_healthcare_bulbar,"univariate_healthcare_bulbar.xlsx")
writexl::write_xlsx(univariate_healthcare_spinal,"univariate_healthcare_spinal.xlsx")

univariate_healthcare_bulbar_spinal <- rbindx(univariate_healthcare_bulbar %>%
                                              mutate(type = rep("bulbar",nrow(.))),
                                            univariate_healthcare_spinal %>%
                                              select(-index) %>%
                                              mutate(index = match(Labels_plot,
                                                                   univariate_healthcare_bulbar$Labels_plot),
                                                     type = rep("spinal",nrow(.))))

pdf("plots/univariate_healthcare_bulbar_spinal.pdf")
odds_ratio_plot_bulbar_spinal(univariate_healthcare_bulbar_spinal,"Contacts healthcare bulbar and spinal")
dev.off()
pdf("plots/volcano_healthcare_pvalue_bulbar.pdf")
volcano_plot(univariate_healthcare_bulbar,"p-value","Contacts healthcare bulbar (p-value)")
dev.off()
pdf("plots/volcano_healthcare_pvalue_spinal.pdf")
volcano_plot(univariate_healthcare_spinal,"p-value","Contacts healthcare spinal (p-value)")
dev.off()
pdf("plots/volcano_healthcare_padj_bulbar.pdf")
volcano_plot(univariate_healthcare_bulbar,"padj","Contacts healthcare bulbar (p-adjusted)")
dev.off()
pdf("plots/volcano_healthcare_padj_spinal.pdf")
volcano_plot(univariate_healthcare_spinal,"padj","Contacts healthcare spinal (p-adjusted)")
dev.off()

## Pre-existing conditions
dat_final_preconditions_all_bulbar = dat_final_preconditions_all[which_bulbar,]
dat_final_preconditions_all_spinal = dat_final_preconditions_all[which_spinal,]

univariate_preconditions_all_bulbar <- univariate_model_new(dat_final_preconditions_all_bulbar)[[1]]
univariate_preconditions_all_spinal <- univariate_model_new(dat_final_preconditions_all_spinal)[[1]]

# -> Pre-existing conditions (each subquestion individually)
df_subquestions_preconditions_bulbar <- list()
df_subquestions_preconditions_spinal <- list()
aux_function_precondition <- function(data,which_rows){
  dat_final_preconditions_i <- dat_final[which_rows,c(which(original_names %in% data$`original question (ALS)`),ncol(dat_final))]
  dat_final_preconditions_i_temp <- apply(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
  dat_final_preconditions_i <- data.frame(subquestion_i = apply(dat_final_preconditions_i_temp, 1, function(x) {
    preconditions_all <- sum(x)
  }),status2 = dat_final_preconditions_i[,"status2"])
  return(dat_final_preconditions_i)
}
#for (i in 1:length(subquestions_preconditions)) {
for (i in 1:12) {
  subquestion_i <- subquestions_preconditions[i]
  final_ALS_CTR_category_preconditions_i <- final_ALS_CTR_category_preconditions %>%
    filter(`sub-category` == subquestion_i)
  dat_final_preconditions_i_bulbar <- aux_function_precondition(final_ALS_CTR_category_preconditions_i,which_bulbar)
  dat_final_preconditions_i_spinal <- aux_function_precondition(final_ALS_CTR_category_preconditions_i,which_spinal)
  df_subquestions_preconditions_bulbar[[i]] <- univariate_model_new(dat_final_preconditions_i_bulbar)[[1]]
  df_subquestions_preconditions_spinal[[i]] <- univariate_model_new(dat_final_preconditions_i_spinal)[[1]]
}

univariate_preconditions_subquestion_bulbar <- do.call("rbind", df_subquestions_preconditions_bulbar) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_preconditions[1:12])
univariate_preconditions_subquestion_spinal <- do.call("rbind", df_subquestions_preconditions_spinal) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_preconditions[1:12])

# -> all results together
univariate_preconditions_bulbar <- do.call("rbind", list(univariate_preconditions_all_bulbar,
                                                         univariate_preconditions_subquestion_bulbar))
univariate_preconditions_bulbar <- preparate_data_forest(univariate_preconditions_bulbar)
univariate_preconditions_spinal <- do.call("rbind", list(univariate_preconditions_all_spinal,
                                                       univariate_preconditions_subquestion_spinal))
univariate_preconditions_spinal <- preparate_data_forest(univariate_preconditions_spinal)

writexl::write_xlsx(univariate_preconditions_bulbar,"univariate_preconditions_bulbar.xlsx")
writexl::write_xlsx(univariate_preconditions_spinal,"univariate_preconditions_spinal.xlsx")

univariate_preconditions_bulbar <- univariate_preconditions_bulbar[!duplicated(univariate_preconditions_bulbar$Labels_plot),]
univariate_preconditions_spinal <- univariate_preconditions_spinal[!duplicated(univariate_preconditions_spinal$Labels_plot),]

univariate_preconditions_bulbar_spinal <- rbindx(univariate_preconditions_bulbar %>%
                                                 mutate(type = rep("bulbar",nrow(.))),
                                               univariate_preconditions_spinal %>%
                                                 select(-index) %>%
                                                 mutate(index = match(Labels_plot,
                                                                      univariate_preconditions_bulbar$Labels_plot),
                                                        type = rep("spinal",nrow(.))))

pdf("plots/univariate_preconditions_bulbar_spinal.pdf")
odds_ratio_plot_bulbar_spinal(univariate_preconditions_bulbar_spinal,"Pre-existing conditions bulbar and spinal")
dev.off()
pdf("plots/volcano_preconditions_pvalue_bulbar.pdf")
volcano_plot(univariate_preconditions_bulbar,"p-value","Pre-existing conditions (p-value) bulbar")
dev.off()
pdf("plots/volcano_preconditions_padj_bulbar.pdf")
volcano_plot(univariate_preconditions_bulbar,"padj","Pre-existing conditions (p-adjusted) bulbar")
dev.off()
pdf("plots/volcano_preconditions_pvalue_spinal.pdf")
volcano_plot(univariate_preconditions_spinal,"p-value","Pre-existing conditions (p-value) spinal")
dev.off()
pdf("plots/volcano_preconditions_padj_spinal.pdf")
volcano_plot(univariate_preconditions_spinal,"padj","Pre-existing conditions (p-adjusted) spinal")
dev.off()

# -> Pre-existing conditions (each subsubquestion individually)
subsubquestions_preconditions_tmp <- subsubquestions_preconditions
subsubquestions_preconditions_tmp <- subsubquestions_preconditions_tmp[c(1:14,16,18:20,23:24,26,28:29,32:35,37:43,45:48,51,56:58,60,68:69)]
df_subsubquestions_preconditions_bulbar <- list()
df_subsubquestions_preconditions_spinal <- list()

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
  dat_final_preconditions_i_bulbar <- aux_function_precondition_subquestion(final_ALS_CTR_category_preconditions_i,which_bulbar)
  dat_final_preconditions_i_spinal <- aux_function_precondition_subquestion(final_ALS_CTR_category_preconditions_i,which_spinal)
  df_subsubquestions_preconditions_bulbar[[i]] <- univariate_model_new(dat_final_preconditions_i_bulbar)[[1]]
  df_subsubquestions_preconditions_spinal[[i]] <- univariate_model_new(dat_final_preconditions_i_spinal)[[1]]
}

univariate_preconditions_subsubquestion_bulbar <- do.call("rbind", df_subsubquestions_preconditions_bulbar) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_preconditions_tmp)
univariate_preconditions_subsubquestion_spinal <- do.call("rbind", df_subsubquestions_preconditions_spinal) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_preconditions_tmp)

# -> all results together
univariate_preconditions_subsubquestion_bulbar <- preparate_data_forest(univariate_preconditions_subsubquestion_bulbar)
univariate_preconditions_subsubquestion_spinal <- preparate_data_forest(univariate_preconditions_subsubquestion_spinal)

writexl::write_xlsx(univariate_preconditions_subsubquestion_bulbar,"univariate_preconditions_subsubquestion_bulbar.xlsx")
writexl::write_xlsx(univariate_preconditions_subsubquestion_spinal,"univariate_preconditions_subsubquestion_spinal.xlsx")

univariate_preconditions_subsubquestion_bulbar <- univariate_preconditions_subsubquestion_bulbar[!duplicated(univariate_preconditions_subsubquestion_bulbar$Labels_plot),]
univariate_preconditions_subsubquestion_spinal <- univariate_preconditions_subsubquestion_spinal[!duplicated(univariate_preconditions_subsubquestion_spinal$Labels_plot),]
univariate_preconditions_subsubquestion_bulbar$Labels_plot %in% univariate_preconditions_subsubquestion_spinal$Labels_plot

univariate_preconditions_subsubquestion_bulbar_spinal <- rbindx(univariate_preconditions_subsubquestion_bulbar %>%
                                                                mutate(type = rep("bulbar",nrow(.))),
                                                              univariate_preconditions_subsubquestion_spinal %>%
                                                                select(-index) %>%
                                                                mutate(index = match(Labels_plot,
                                                                                     univariate_preconditions_subsubquestion_bulbar$Labels_plot),
                                                                       type = rep("spinal",nrow(.))))

pdf("plots/univariate_preconditions_subsubquestion_bulbar_spinal.pdf")
odds_ratio_plot_bulbar_spinal(univariate_preconditions_subsubquestion_bulbar_spinal,"Pre-existing conditions bulbar and spinal")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_pvalue_bulbar.pdf")
volcano_plot(univariate_preconditions_subsubquestion_bulbar,"p-value","Pre-existing conditions subquestion (p-value) bulbar")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_padj_bulbar.pdf")
volcano_plot(univariate_preconditions_subsubquestion_bulbar,"padj","Pre-existing conditions subquestion (p-adjusted) bulbar")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_pvalue_spinal.pdf")
volcano_plot(univariate_preconditions_subsubquestion_spinal,"p-value","Pre-existing conditions subquestion (p-value) spinal")
dev.off()
pdf("plots/volcano_preconditions_subsubquestion_padj_spinal.pdf")
volcano_plot(univariate_preconditions_subsubquestion_spinal,"padj","Pre-existing conditions subquestion (p-adjusted) spinal")
dev.off()

## Diet and weight

# -> Diet and weight (each subquestion individually)
dat_final_dietweight_all_bulbar <- dat_final_dietweight_all[which_bulbar,]
dat_final_dietweight_all_spinal <- dat_final_dietweight_all[which_spinal,]
univariate_dietweight_all_bulbar <- univariate_model_new(dat_final_dietweight_all_bulbar)[[1]]
univariate_dietweight_all_spinal <- univariate_model_new(dat_final_dietweight_all_spinal)[[1]]

univariate_dietweight_all_bulbar <- univariate_dietweight_all_bulbar %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`),
         fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr)) %>%
  arrange(log10_pval)  %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.)) %>%
  left_join(map_questions_plot)

univariate_dietweight_all_spinal <- univariate_dietweight_all_spinal %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`),
         fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr)) %>%
  arrange(log10_pval)  %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.)) %>%
  left_join(map_questions_plot)

writexl::write_xlsx(univariate_dietweight_all_bulbar,"univariate_dietweight_bulbar.xlsx")
writexl::write_xlsx(univariate_dietweight_all_spinal,"univariate_dietweight_spinal.xlsx")

univariate_dietweight_all_bulbar_spinal <- rbindx(univariate_dietweight_all_bulbar %>%
                                                  mutate(type = rep("bulbar",nrow(.))),
                                                univariate_dietweight_all_spinal %>%
                                                  select(-index) %>%
                                                  mutate(index = match(Labels_plot,
                                                                       univariate_dietweight_all_bulbar$Labels_plot),
                                                         type = rep("spinal",nrow(.))))

pdf("plots/univariate_dietweight_bulbar_spinal.pdf")
odds_ratio_plot_bulbar_spinal(univariate_dietweight_all_bulbar_spinal,"Diet and weight bulbar and spinal")
dev.off()
pdf("plots/volcano_dietweight_pvalue_bulbar.pdf")
volcano_plot(univariate_dietweight_all_bulbar,"p-value","Diet and weight (p-value) bulbar")
dev.off()
pdf("plots/volcano_dietweight_padj_bulbar.pdf")
volcano_plot(univariate_dietweight_all_bulbar,"padj","Diet and weights (p-adjusted) bulbar")
dev.off()
pdf("plots/volcano_dietweight_pvalue_spinal.pdf")
volcano_plot(univariate_dietweight_all_spinal,"p-value","Diet and weight (p-value) spinal")
dev.off()
pdf("plots/volcano_dietweight_padj_spinal.pdf")
volcano_plot(univariate_dietweight_all_spinal,"padj","Diet and weights (p-adjusted) spinal")
dev.off()

## Lifestyle

# -> Lifestyle (separate the ones with and without subcategories)
# -> first only the ones without subcategories
final_ALS_CTR_category_lifestyle_nosubcategories <- final_ALS_CTR_category_lifestyle %>%
  filter(is.na(`sub-subcategory`))

dat_final_lifestyle_nosubcategories<- dat_final[,c(which(original_names %in% final_ALS_CTR_category_lifestyle_nosubcategories$`original question (ALS)`),ncol(dat_final))]
dat_final_lifestyle_nosubcategories_bulbar <- dat_final_lifestyle_nosubcategories[which_bulbar,]
dat_final_lifestyle_nosubcategories_spinal <- dat_final_lifestyle_nosubcategories[which_spinal,]

univariate_lifestyle_nosubcategories_bulbar_temp <- univariate_model_new(dat_final_lifestyle_nosubcategories_bulbar)
univariate_lifestyle_nosubcategories_spinal_temp <- univariate_model_new(dat_final_lifestyle_nosubcategories_spinal)
dat_final_lifestyle_nosubcategories_bulbar <- cbind(univariate_lifestyle_nosubcategories_bulbar_temp[[1]][,1:9],
                                                    univariate_lifestyle_nosubcategories_bulbar_temp[[2]][,2:3])
dat_final_lifestyle_nosubcategories_spinal <- cbind(univariate_lifestyle_nosubcategories_spinal_temp[[1]][,1:9],
                                                  univariate_lifestyle_nosubcategories_spinal_temp[[2]][,2:3])

# -> the ones with categories all together
df_subquestions_lifestyle_bulbar <- list()
df_subquestions_lifestyle_spinal <- list()
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
  dat_final_subquestions_lifestyle_i_bulbar <- aux_function_lifestyle(final_ALS_CTR_category_subquestions_lifestyle_i,which_bulbar)
  dat_final_subquestions_lifestyle_i_spinal <- aux_function_lifestyle(final_ALS_CTR_category_subquestions_lifestyle_i,which_spinal)
  df_subquestions_lifestyle_bulbar[[i]] <- univariate_model_new(dat_final_subquestions_lifestyle_i_bulbar)[[1]]
  df_subquestions_lifestyle_spinal[[i]] <- univariate_model_new(dat_final_subquestions_lifestyle_i_spinal)[[1]]
}

univariate_lifestyle_subquestion_bulbar <- do.call("rbind", df_subquestions_lifestyle_bulbar) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_lifestyle)
univariate_lifestyle_subquestion_spinal <- do.call("rbind", df_subquestions_lifestyle_spinal) %>% 
  as.data.frame() %>%
  mutate(Variables = subquestions_lifestyle)

# -> the ones with categories separate
subsubquestions_lifestyle <- final_ALS_CTR_category_lifestyle_subcategories$`sub-subcategory` %>% unique()
subsubquestions_lifestyle_tpm <- subsubquestions_lifestyle[c(1:6,8:14)]
df_subsubquestions_lifestyle_bulbar <- list()
df_subsubquestions_lifestyle_spinal <- list()
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
  dat_final_subsubquestions_lifestyle_i_bulbar <- aux_function_lifestyle_subquestions(final_ALS_CTR_category_subsubquestions_lifestyle_i,which_bulbar)
  dat_final_subsubquestions_lifestyle_i_spinal <- aux_function_lifestyle_subquestions(final_ALS_CTR_category_subsubquestions_lifestyle_i,which_spinal)
  df_subsubquestions_lifestyle_bulbar[[i]] <- univariate_model_new(dat_final_subsubquestions_lifestyle_i_bulbar)[[1]]
  df_subsubquestions_lifestyle_spinal[[i]] <- univariate_model_new(dat_final_subsubquestions_lifestyle_i_spinal)[[1]]
}

univariate_lifestyle_subsubquestion_bulbar <- do.call("rbind", df_subsubquestions_lifestyle_bulbar) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_lifestyle_tpm)
univariate_lifestyle_subsubquestion_spinal <- do.call("rbind", df_subsubquestions_lifestyle_spinal) %>% 
  as.data.frame() %>%
  mutate(Variables = subsubquestions_lifestyle_tpm)

# -> all results together
univariate_lifestyle_bulbar <- do.call("rbind", list(dat_final_lifestyle_nosubcategories_bulbar,
                                                     univariate_lifestyle_subquestion_bulbar,
                                                     univariate_lifestyle_subsubquestion_bulbar))
dat_final_lifestyle_nosubcategories_spinal <- dat_final_lifestyle_nosubcategories_spinal[c(1:17,19:34),]
univariate_lifestyle_spinal <- do.call("rbind", list(dat_final_lifestyle_nosubcategories_spinal,
                                                   univariate_lifestyle_subquestion_spinal,
                                                   univariate_lifestyle_subsubquestion_spinal))

univariate_lifestyle_bulbar <- preparate_data_forest(univariate_lifestyle_bulbar)
univariate_lifestyle_spinal <- preparate_data_forest(univariate_lifestyle_spinal)

writexl::write_xlsx(univariate_lifestyle_bulbar,"univariate_lifestyle_bulbar.xlsx")
writexl::write_xlsx(univariate_lifestyle_spinal,"univariate_lifestyle_spinal.xlsx")

univariate_lifestyle_bulbar_spinal <- rbindx(univariate_lifestyle_bulbar %>%
                                             mutate(type = rep("bulbar",nrow(.))),
                                           univariate_lifestyle_spinal %>%
                                             select(-index) %>%
                                             mutate(index = match(Labels_plot,
                                                                  univariate_lifestyle_bulbar$Labels_plot),
                                                    type = rep("spinal",nrow(.))))

pdf("plots/univariate_lifestyle_bulbar_spinal.pdf")
odds_ratio_plot_bulbar_spinal(univariate_lifestyle_bulbar_spinal,"Lifestyle bulbar and spinal")
dev.off()
pdf("plots/volcano_lifestyle_pvalue_bulbar.pdf")
volcano_plot(univariate_lifestyle_bulbar,"p-value","Lifestyle (p-value) bulbar")
dev.off()
pdf("plots/volcano_lifestyle_padj_bulbar.pdf")
volcano_plot(univariate_lifestyle_bulbar,"padj","Lifestyle (p-adjusted) bulbar")
dev.off()
pdf("plots/volcano_lifestyle_pvalue_spinal.pdf")
volcano_plot(univariate_lifestyle_spinal,"p-value","Lifestyle (p-value) spinal")
dev.off()
pdf("plots/volcano_lifestyle_padj_spinal.pdf")
volcano_plot(univariate_lifestyle_spinal,"padj","Lifestyle (p-adjusted) spinal")
dev.off()


## ADDITIONAL FUNCTIONS
odds_ratio_plot_bulbar_spinal <- function(data,title_plot){
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


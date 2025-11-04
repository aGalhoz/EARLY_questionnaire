## UNIVARIATE ANALYSIS

# Univariate analysis

# continuous variables (date and continuous)
dat_fil_con = dat_imp[, colnames(dat_imp) %in% c(colnames(data_patients_control_combined_continuous_temp),
                                                 colnames(data_patients_control_combined_datetime_temp),
                                                 colnames(data_patients_control_combined_datetime_duration_temp))]
dat_fil_con <- apply(dat_fil_con, 2, function(x) as.numeric(as.character(x)))
dat_fil_con = apply(dat_fil_con, 2, function(x) (x -mean(x, na.rm = T))/sd(x, na.rm = T)) %>% as.data.frame()

# categorical variables (binary, categories)
dat_fil_cat = dat_imp[, ! colnames(dat_imp) %in% c(colnames(data_patients_control_combined_continuous_temp),
                                                   colnames(data_patients_control_combined_datetime_temp),
                                                   colnames(data_patients_control_combined_datetime_duration_temp))]
dat_fil_cat = apply(dat_fil_cat, 2, function(x) as.character(x)) %>% as.data.frame()
colnames_dat_fil_cat <- make.names(colnames(dat_fil_cat))
# non_bin = apply(dat_fil_cat, 2, function(x) nlevels(as.factor(x)) > 2)
# dat_fil_cat_nonbin = dat_fil_cat[,non_bin]
# mm = model.matrix()
dat_final = cbind(dat_fil_con, dat_fil_cat)
dat_final$status2 = as.factor(as.numeric(status))
levels(dat_final$status2) <- c(1,0)
dat_final$status2 = factor(dat_final$status2, levels = c(0,1))
original_names <- colnames(dat_final)
new_names <- make.names(colnames(dat_final))
colnames(dat_final) <- new_names

res = list()
ftr = list()
eff_CI = list()
for (i in colnames(dat_final)){
  if (i %in% c("status", "status2")){
    next
  }
  if (nlevels(as.factor(dat_final[,i])) < 2){
    next
  }
  form = as.formula(paste("status2 ~",i))
  if(length(grep("Nein",dat_final[,i]))!=0){
    unique_values <- unique(dat_final[,i])
    if(length(unique_values) < 5){
      unique_values_ordered <- unique_values[str_order(unique_values,decreasing = T)]
      col_i <- factor(dat_final[,i],levels = unique_values_ordered)
      dat_final[,i] <- col_i
    }
  }
  mod = glm(form, dat_final, family = "binomial")
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
    for(l in 1:length(cl)){
      dat_tmp = dat_final
      dat_tmp[,i] = ifelse(dat_tmp[,i] == cl[l],1,0)
      fisher = fisher.test(x = dat_tmp[,i], y = dat_tmp[,"status2"])
      meandiff = mean(dat_final[,i][dat_final[,i] == cl[l]], na.rm = T) - mean(dat_final[,i][dat_final[,i] != cl[l]], na.rm = T)
      eff[l] = log(fisher$estimate)
    }
  } else {
    meandiff = mean(dat_final[,i][dat_final[,"status2"] == 1], na.rm = T) - mean(dat_final[,i][dat_final[,"status2"] == 0], na.rm = T)
    eff = meandiff
    eff = exp(coef(mod))
    eff = log(eff[names(eff) != "(Intercept)"])
  }
  tmp$eff = eff
  tmp$Type = ifelse(i %in% colnames_dat_fil_cat,"Categorical","Continuous")
  res[[i]] = tmp
  ftr[[i]] = rn
  eff_CI[[i]] = eff_CI_tmp
}

df_old <- df
df_old_names <- df_old %>%
  dplyr::select(Variables,Type,Variables_original,Labels,Labels_plot)

writexl::write_xlsx(df_old_names,"df_old_names.xlsx")
df_old_names <- df_old_names[!is.na(df_old_names$Labels_plot),]

df = do.call("rbind", res) %>% as.data.frame() %>% rownames_to_column("Variables")
df$Features = do.call("c", ftr)
df$t_stat = df$Estimate/df$`Std. Error`
df$fdr = p.adjust(df$`Pr(>|z|)`, "fdr")

eff_CI_new = do.call("rbind",eff_CI) %>% as.data.frame() %>% rownames_to_column("Variables") 
colnames(eff_CI_new) <- c("Variables","lower","upper")

writexl::write_xlsx(eff_CI_new,"eff_CI_new.xlsx")
df <- cbind(df,eff_CI_new[,2:3])

df$`2.5 %` <- as.numeric(df$lower)
df$`97.5 %` <- as.numeric(df$upper)
df <- df %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`))

df <- df %>%
  left_join(df_old_names)

df <- df %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr))

df_cont <- df %>%
  filter(Type == "Continuous")

df_cat <- df %>%
  filter(Type == "Categorical")

writexl::write_xlsx(df,"df.xlsx")

# update questionnaire data
final_table_questionnaire_new <- final_ALS_CTR_summary %>% 
  dplyr::rename(Variables_original = `original question (ALS)`) %>%
  left_join(df)

final_table_questionnaire_new <- final_table_questionnaire_new %>%
  distinct()

writexl::write_xlsx(final_table_questionnaire_new,"final_table_questionnaire_22052024.xlsx")

# volcano plot of categorical variables
volcano_plot <- function(data,type_stat,title_plot){
  if(type_stat == "p-value"){
    ggplot(data, aes(x = reorder(Variables, t_stat), y = log10_pval)) + 
      geom_point(col = "grey48",size = 2) +
      geom_point(data = filter(data, `Pr(>|z|)` < 0.15), mapping = aes(x = reorder(Variables, t_stat), y = log10_pval), 
                 col = "lightblue",size = 2) +
      geom_text_repel(data = filter(data, `Pr(>|z|)` < 0.15), 
                      mapping = aes(x = reorder(Variables, t_stat), y = log10_pval,  label = Labels_plot), 
                      min.segment.length = 0, 
                      seed = 42, box.padding = 0.8, 
                      nudge_x = 0.5,
                      max.overlaps = 15,
                      segment.color = "grey52") +
      # theme(axis.text.x = element_blank(),
      #       axis.ticks.x = element_blank(),
      #       panel.background = element_rect(fill = "white")) +
      geom_hline(yintercept=(-log10(0.15)), linetype='dotted', col = 'grey')+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(hjust = 0.5),
            legend.text=element_text(size=14),
            legend.position = c(0.1, 0.9),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.background = element_rect(fill = "white")) +
      labs(title = title_plot,
           x = "T-statistic",
           y = expression("-log"[10]*"(p-value)")) 
  }
  else{
    ggplot(data, aes(x = reorder(Variables, t_stat), y = log10_padj)) + 
      geom_point(col = "grey48") +
      geom_point(data = filter(data, (fdr) < 0.2), mapping = aes(x = reorder(Variables, t_stat), y = log10_padj),  
                 col = "lightblue",size = 2) +
      geom_text_repel(data = filter(data, (fdr) < 0.2), mapping = aes(x = reorder(Variables, t_stat), y = log10_padj, 
                                                                             label = Labels_plot), 
                      min.segment.length = 0, 
                      seed = 42, box.padding = 0.7, 
                      nudge_x = 0.4,
                      max.overlaps = 30,
                      segment.color = "grey52") +
      # theme(axis.text.x = element_blank(),
      #       axis.ticks.x = element_blank(),
      #       panel.background = element_rect(fill = "white")) +
      geom_hline(yintercept=(-log10(0.2)), linetype='dotted', col = 'grey') +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(hjust = 0.5),
            legend.text=element_text(size=14),
            legend.position = c(0.1, 0.9),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.background = element_rect(fill = "white")) +
      labs(title = title_plot,
           x = "T-statistic",
           y = expression("-log"[10]*"(adjusted p-value)"))
  }
}

pdf("plots/volcano categorical p-value.pdf")
volcano_plot(df_cat,"p-value","Categorical variables - p-value")
dev.off()
pdf("plots/volcano categorical padj.pdf")
volcano_plot(df_cat,"adjusted p-value","Categorical variables - adjusted p-value")
dev.off()

# volcano plot of continuous variables
pdf("plots/volcano continuous p-value.pdf")
volcano_plot(df_cont,"p-value","Continuous variables - p-value")
dev.off()
pdf("plots/volcano continuous padj.pdf")
volcano_plot(df_cont,"adjusted p-value","Continuous variables - adjusted p-value")
dev.off()

## Forest plots

# categorical variables
df_cat_pvalue <- df_cat %>%
  filter(`Pr(>|z|)` < 0.15)
df_cat_pvalue <- df_cat_pvalue %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
         index = 1:nrow(df_cat_pvalue))  
df_cat_pvalue$mean <- as.numeric(df_cat_pvalue$mean)
df_cat_pvalue$Labels_plot <- factor(df_cat_pvalue$Labels_plot, levels = df_cat_pvalue$Labels_plot)

df_cat_padj <- df_cat %>%
  filter(abs(t_stat) > 0.015)
df_cat_padj$fdr = p.adjust(df_cat_padj$`Pr(>|z|)`, "fdr")
df_cat_padj <- df_cat_padj %>%
  mutate(log10_padj = -log10(fdr)) %>%
 filter((fdr) < 0.5) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))
df_cat_padj$mean <- as.numeric(df_cat_padj$mean)
df_cat_padj$Labels_plot <- factor(df_cat_padj$Labels_plot, levels = df_cat_padj$Labels_plot)

odds_ratio_plot <- function(data,title_plot){
    ggplot(data, aes(y=index, x=mean, xmin=lower, xmax=upper)) +
      geom_point() +
      geom_errorbarh(height=.1) +
      scale_y_continuous(breaks=1:nrow(data), labels=data$Labels_plot) +
      xlim(-5,5) +
      labs(title=title_plot, x='log(odds-ratio)', y = 'Question') +
      geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
      theme_minimal()
}

pdf("plots/odds-ratio categorical p-value.pdf")
odds_ratio_plot(df_cat_pvalue,"Odds ratio categorical variables (p-value)")
dev.off()
pdf("plots/odds-ratio categorical padj.pdf")
odds_ratio_plot(df_cat_padj,"Odds ratio categorical variables (padj)")
dev.off()

# continuous variables
df_cont_pvalue <- df_cont %>%
  filter(`Pr(>|z|)` < 0.15) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_cont_pvalue$mean <- as.numeric(df_cont_pvalue$mean)
df_cont_pvalue$Labels_plot <- factor(df_cont_pvalue$Labels_plot, levels = df_cont_pvalue$Labels_plot)

df_cont_padj <- df_cont %>%
  filter((fdr) < 0.5) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_cont_padj$mean <- as.numeric(df_cont_padj$mean)
df_cont_padj$Labels_plot <- factor(df_cont_padj$Labels_plot, levels = df_cont_padj$Labels_plot)

pdf("plots/odds-ratio continuous p-value.pdf")
odds_ratio_plot(df_cont_pvalue,"Odds ratio continuous variables (p-value)")
dev.off()
pdf("plots/odds-ratio continuous padj.pdf")
odds_ratio_plot(df_cont_padj,"Odds ratio continuous variables (padj)")
dev.off()

# ggplot(df, aes(x = reorder(Variables, t_stat), y = t_stat, shape = Type)) + 
#   geom_point() +
#   geom_point(data = filter(df, fdr < 0.1), mapping = aes(x = reorder(Variables, t_stat), y = t_stat), col = "red") +
#   geom_text_repel(data = filter(df, fdr < 0.15), mapping = aes(x = reorder(Variables, t_stat), y = t_stat, 
#                                                                label = Labels), max.overlaps = 30) +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.background = element_rect(fill = "white")) +
#   xlab("Variables")
# 
# # volcano plot of all features
# ggplot(df, aes(x = eff, y = -log10(`Pr(>|z|)`), shape = Type)) + 
# geom_point() +
#   geom_point(data = filter(df, fdr < 0.05), mapping = aes(x = eff, y = -log10(`Pr(>|z|)`)), col = "red") +
#   geom_text_repel(data = filter(df, fdr < 0.05), mapping = aes(x = eff, y = -log10(`Pr(>|z|)`), 
#                                                                label = Variables), max.overlaps = 30) +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.background = element_rect(fill = "white")) +
#   xlab("Effect size") +
#   ylab("-log10(Pval)")

# # volcano plot of only significant features
# df_sig = filter(df, fdr < 0.05)
# imp_vars = read.csv("Ana_normal_significant_features.csv", header = T, sep = ";")
# df$Significance = ifelse(df$Variables %in% df_sig$Variables & df$Variables %in% imp_vars$Name, "Both",
#                          ifelse(df$Variables %in% df_sig$Variables,"Univariate",
#                                 ifelse(df$Variables %in% imp_vars$Name, "Multivariate", "Neither")))
# col_sig = c("Neither" = "grey", "Univariate" = "seagreen", "Multivariate" = "cornflowerblue","Both" = "tomato3")
# ggplot(df, aes(x = eff, y = -log10(`Pr(>|z|)`), shape = Type, col = Significance)) +
#   geom_point() +
#   geom_text_repel(data = filter(df, Significance != "Neither" & eff < 0 & `Pr(>|z|)` < 0.05), xlim = c(NA,0),
#                   direction = "both", #nudge_x = c(-1,0),
#                   mapping = aes(x = eff, y = -log10(`Pr(>|z|)`), label = Variables), max.overlaps = 30) +
#   geom_text_repel(data = filter(df, Significance != "Neither" & eff > 0 & `Pr(>|z|)` < 0.05),
#                   mapping = aes(x = eff, y = -log10(`Pr(>|z|)`), label = Variables), max.overlaps = 30) +
#   scale_color_manual(values = col_sig) +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.background = element_rect(fill = "white")) +
#   xlab("Effect size") +
#   ylab("-log10(Pval)")


# # merge results of df with table with common questions between cohorts
# final_ALS_CTR_summary_new <- final_ALS_CTR_summary %>%
#   left_join((df %>% 
#                rename(`original question (ALS)` = Variables_original,
#                       `log(odds_ratio)` = eff,
#                   FDR = fdr,
#                   log10_FDR = log10_padj,
#                   `p-value` = `Pr(>|z|)`,
#                   `log(odds_ratio_lower)` = lower,
#                   `log(odds_ratio_upper)` = upper) %>%
#                dplyr::mutate(zero_tstats = ifelse(t_stat < 10^(-10), "yes","no")) %>%
#                dplyr::select(`original question (ALS)`,Labels,Labels_plot,Features,`log(odds_ratio)`,
#                              `log(odds_ratio_lower)`, `log(odds_ratio_upper)`,
#                              FDR,log10_FDR,`p-value`,log10_pval,)))
# 
# writexl::write_xlsx(final_ALS_CTR_summary_new,"final_table_questionnaire_1711.xlsx")
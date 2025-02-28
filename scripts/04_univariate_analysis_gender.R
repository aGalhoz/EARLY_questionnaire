### univariate analysis stratified by gender

dat_final_female <- dat_final[dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich",]
dat_final_male <- dat_final[dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich",]

# remove columns which only have one factor level 
dat_final_female_aux <- dat_final_female[, sapply(dat_final_female, function(col) length(na.omit(unique(col)))) > 1]
dat_final_male_aux <- dat_final_male[, sapply(dat_final_male, function(col) length(na.omit(unique(col)))) > 1]

univariate_model <- function(data_final){
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
      for(l in 1:length(cl)){
        dat_tmp = data_final
        dat_tmp[,i] = ifelse(dat_tmp[,i] == cl[l],1,0)
        fisher = fisher.test(x = dat_tmp[,i], y = dat_tmp[,"status2"])
        meandiff = mean(data_final[,i][data_final[,i] == cl[l]], na.rm = T) - mean(data_final[,i][data_final[,i] != cl[l]], na.rm = T)
        eff[l] = log(fisher$estimate)
      }
    } else {
      meandiff = mean(data_final[,i][data_final[,"status2"] == 1], na.rm = T) - mean(data_final[,i][data_final[,"status2"] == 0], na.rm = T)
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

results_female <- univariate_model(data_final = dat_final_female_aux)
results_male <- univariate_model(data_final = dat_final_male_aux)

df_female <- results_female[[1]]
CI_female <- results_female[[2]]

df_male <- results_male[[1]]
CI_male <- results_male[[2]]

df_female$`2.5 %` <- CI_female$`2.5 %`
df_female$`97.5 %` <- CI_female$`97.5 %`

df_male$`2.5 %` <- CI_male$`2.5 %`
df_male$`97.5 %` <- CI_male$`97.5 %`

df_female <- df_female %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`))
df_male <- df_male %>%
  mutate(lower = log(`2.5 %`),
         upper = log(`97.5 %`))

df_female <- df_female %>%
  left_join(df_old_names)  %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr))

df_male <- df_male %>%
  left_join(df_old_names)  %>%
  mutate(log10_pval = -log10(`Pr(>|z|)`),
         log10_padj = -log10(fdr))

# division into continuous and categorical 
df_female_cont <- df_female %>%
  filter(Type == "Continuous")
df_female_cat <- df_female %>%
  filter(Type == "Categorical")

df_male_cont <- df_male %>%
  filter(Type == "Continuous")
df_male_cat <- df_male %>%
  filter(Type == "Categorical")

writexl::write_xlsx(df_female,"df_female.xlsx")
writexl::write_xlsx(df_male,"df_male.xlsx")

# create sub data for FDR values
# -> Female
df_female_cont_padj <- df_female_cont %>%
  filter(abs(t_stat) > 0.05)
df_female_cont_padj$fdr = p.adjust(df_female_cont_padj$`Pr(>|z|)`, "fdr")
df_female_cont_padj <- df_female_cont_padj %>%
  mutate(log10_padj = -log10(fdr))

df_female_cat_padj <- df_female_cat %>%
  filter(abs(t_stat) > 0.015)
df_female_cat_padj$fdr = p.adjust(df_female_cat_padj$`Pr(>|z|)`, "fdr")
df_female_cat_padj <- df_female_cat_padj %>%
  mutate(log10_padj = -log10(fdr))

# -> Male 
df_male_cont_padj <- df_male_cont %>%
  filter(abs(t_stat) > 0.015)
df_male_cont_padj$fdr = p.adjust(df_male_cont_padj$`Pr(>|z|)`, "fdr")
df_male_cont_padj <- df_male_cont_padj %>%
  mutate(log10_padj = -log10(fdr))

df_male_cat_padj <- df_male_cat %>%
  filter(abs(t_stat) > 0.015)
df_male_cat_padj$fdr = p.adjust(df_male_cat_padj$`Pr(>|z|)`, "fdr")
df_male_cat_padj <- df_male_cat_padj %>%
  mutate(log10_padj = -log10(fdr))

pdf("plots/volcano categorical p-value Female.pdf")
volcano_plot(df_female_cat,"p-value","Categorical variables - p-value (Female)")
dev.off()
pdf("plots/volcano categorical padj Female.pdf")
volcano_plot(df_female_cat_padj,"adjusted p-value","Categorical variables - adjusted p-value (Female)")
dev.off()
pdf("plots/volcano categorical p-value Male.pdf")
volcano_plot(df_male_cat,"p-value","Categorical variables - p-value (Male)")
dev.off()
pdf("plots/volcano categorical padj Male.pdf")
volcano_plot(df_male_cat_padj,"adjusted p-value","Categorical variables - adjusted p-value (Male)")
dev.off()

# volcano plot of continuous variables
pdf("plots/volcano continuous p-value Female.pdf")
volcano_plot(df_female_cont,"p-value","Continuous variables - p-value (Female)")
dev.off()
pdf("plots/volcano continuous padj Female.pdf")
volcano_plot(df_female_cont_padj,"adjusted p-value","Continuous variables - adjusted p-value (Female)")
dev.off()
pdf("plots/volcano continuous p-value Male.pdf")
volcano_plot(df_male_cont,"p-value","Continuous variables - p-value (Male)")
dev.off()
pdf("plots/volcano continuous padj Male.pdf")
volcano_plot(df_male_cont_padj,"adjusted p-value","Continuous variables - adjusted p-value (Male)")
dev.off()

## Forest plots
# categorical variables
df_female_cat_pvalue <- df_female_cat %>%
  filter(`Pr(>|z|)` < 0.15) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_female_cat_pvalue$mean <- as.numeric(df_female_cat_pvalue$mean)
df_female_cat_pvalue$Labels_plot <- factor(df_female_cat_pvalue$Labels_plot, levels = df_female_cat_pvalue$Labels_plot)

df_female_cat_padj <- df_female_cat %>%
  filter((fdr) < 0.5) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_female_cat_padj$mean <- as.numeric(df_female_cat_padj$mean)
df_female_cat_padj$Labels_plot <- factor(df_female_cat_padj$Labels_plot, levels = df_female_cat_padj$Labels_plot)

pdf("plots/odds-ratio categorical padj Female.pdf")
odds_ratio_plot(df_female_cat_padj,"Odds ratio categorical variables (padj) Female")
dev.off()
pdf("plots/odds-ratio categorical p-value Female.pdf")
odds_ratio_plot(df_female_cat_pvalue,"Odds ratio categorical variables (p-value) Female")
dev.off()

# -> male
df_male_cat_pvalue <- df_male_cat %>%
  filter(`Pr(>|z|)` < 0.15)  %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_male_cat_pvalue$mean <- as.numeric(df_male_cat_pvalue$mean)
df_male_cat_pvalue$Labels_plot <- ifelse(is.na(df_male_cat_pvalue$Labels_plot),df_male_cat_pvalue$Labels,df_male_cat_pvalue$Labels_plot)
df_male_cat_pvalue$Labels_plot <- factor(df_male_cat_pvalue$Labels_plot, levels = df_male_cat_pvalue$Labels_plot)

df_male_cat_padj <- df_male_cat %>%
  filter((fdr) < 0.5) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_male_cat_padj$mean <- as.numeric(df_male_cat_padj$mean)
df_male_cat_padj$Labels_plot <- factor(df_male_cat_padj$Labels_plot, levels = df_male_cat_padj$Labels_plot)

pdf("plots/odds-ratio categorical padj Male.pdf")
odds_ratio_plot(df_male_cat_padj,"Odds ratio categorical variables (padj) Male")
dev.off()
pdf("plots/odds-ratio categorical p-value Male.pdf")
odds_ratio_plot(df_male_cat_pvalue,"Odds ratio categorical variables (p-value) Male")
dev.off()

# female and male together (both significant)
df_female_cat_pvalue_aux <- df_female_cat[df_female_cat$Labels_plot %in% c(df_female_cat_pvalue$Labels_plot,
                                                                           df_male_cat_pvalue$Labels_plot),] %>%
  arrange(log10_pval)
df_male_cat_pvalue_aux <- df_male_cat[df_male_cat$Labels_plot %in% c(df_female_cat_pvalue$Labels_plot,
                                                                     df_male_cat_pvalue$Labels_plot),]
df_male_cat_pvalue_aux <- df_male_cat_pvalue_aux[df_male_cat_pvalue_aux$Labels_plot %in% df_female_cat_pvalue_aux$Labels_plot,]
df_female_cat_pvalue_aux <- df_female_cat_pvalue_aux[df_female_cat_pvalue_aux$Labels_plot %in% df_male_cat_pvalue_aux$Labels_plot,]
df_male_cat_pvalue_aux <- df_male_cat_pvalue_aux[match(df_female_cat_pvalue_aux$Variables,
                                                       df_male_cat_pvalue_aux$Variables),]
df_female_male_cat_pvalue_common <- rbindx(df_female_cat_pvalue_aux %>%
                                      mutate(type = rep("female",nrow(.))) %>%
                                      dplyr::mutate(index = 1:nrow(.),
                                                    mean = eff),
                                      df_male_cat_pvalue_aux %>%
                                      mutate(type = rep("male",nrow(.))) %>%
                                      dplyr::mutate(index = 1:nrow(.),
                                                    mean = eff))
pdf("plots/odds-ratio categorical p-value Female and Male.pdf")
ggplot(data=df_female_male_cat_pvalue_common, aes(y=index, x=mean, xmin=lower, xmax=upper,color = type)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:57, labels=df_female_male_cat_pvalue_common$Labels_plot[1:57]) +
  labs(title='Odds ratio categorical variables (p-value) Female and Male (significant in at least one)', x='log(odds-ratio)', y = 'Question') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()
dev.off()

# -> only in Female 
df_female_cat_pvalue_only <- df_female_cat_pvalue[!df_female_cat_pvalue$Labels_plot %in% df_male_cat_pvalue$Labels_plot,] %>%
  mutate(index = 1:nrow(.))

pdf("plots/odds-ratio categorical p-value Female only.pdf")
odds_ratio_plot(df_female_cat_pvalue_only,"Odds ratio categorical variables (p-value) Female only")
dev.off()

# -> only in Male
df_male_cat_pvalue_only <- df_male_cat_pvalue[!df_male_cat_pvalue$Labels_plot %in% df_female_cat_pvalue$Labels_plot,] %>%
  mutate(index = 1:nrow(.))

pdf("plots/odds-ratio categorical p-value Male only.pdf")
odds_ratio_plot(df_male_cat_pvalue_only,"Odds ratio categorical variables (p-value) Male only")
dev.off()

# continuous variables
df_female_cont_pvalue <- df_female_cont %>%
  filter(`Pr(>|z|)` < 0.15) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_female_cont_pvalue$mean <- as.numeric(df_female_cont_pvalue$mean)
df_female_cont_pvalue$Labels_plot <- factor(df_female_cont_pvalue$Labels_plot, levels = df_female_cont_pvalue$Labels_plot)

df_female_cont_padj <- df_female_cont %>%
  filter((fdr) < 0.5) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_female_cont_padj$mean <- as.numeric(df_female_cont_padj$mean)
df_female_cont_padj$Labels_plot <- factor(df_female_cont_padj$Labels_plot, levels = df_female_cont_padj$Labels_plot)

pdf("plots/odds-ratio continuous p-value Female.pdf")
odds_ratio_plot(df_female_cont_pvalue,"Odds ratio continuous variables (p-value) Female")
dev.off()

pdf("plots/odds-ratio continuous padj Female.pdf")
odds_ratio_plot(df_female_cont_padj,"Odds ratio continuous variables (padj) Female")
dev.off()

# -> male
df_male_cont_pvalue <- df_male_cont %>%
  filter(`Pr(>|z|)` < 0.15)  %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_male_cont_pvalue$mean <- as.numeric(df_male_cont_pvalue$mean)
df_male_cont_pvalue$Labels_plot <- factor(df_male_cont_pvalue$Labels_plot, levels = df_male_cont_pvalue$Labels_plot)

df_male_cont_padj <- df_male_cont %>%
  filter((fdr) < 0.5) %>%
  arrange(log10_pval) %>%
  dplyr::mutate(mean = eff,
                index = 1:nrow(.))  
df_male_cont_padj$mean <- as.numeric(df_male_cont_padj$mean)
df_male_cont_padj$Labels_plot <- factor(df_male_cont_padj$Labels_plot, levels = df_male_cont_padj$Labels_plot)

pdf("plots/odds-ratio continuous p-value Male.pdf")
odds_ratio_plot(df_male_cont_pvalue,"Odds ratio continuous variables (p-value) Male")
dev.off()

pdf("plots/odds-ratio continuous padj Male.pdf")
odds_ratio_plot(df_male_cont_padj,"Odds ratio continuous variables (padj) Male")
dev.off()

# female and male together
df_female_cont_pvalue_aux <- df_female_cont[df_female_cont$Labels %in% c(df_male_cont_pvalue$Labels,
                                                                           df_female_cont_pvalue$Labels),] %>%
  arrange(log10_pval)
df_female_cont_pvalue_aux<- df_female_cont_pvalue_aux[df_female_cont_pvalue_aux$Labels_plot != "gynaecology (10y)",] 
df_male_cont_pvalue_aux <- df_male_cont[df_male_cont$Labels %in% c(df_male_cont_pvalue$Labels,
                                                                     df_female_cont_pvalue$Labels),]
df_male_cont_pvalue_aux <- df_male_cont_pvalue_aux[df_male_cont_pvalue_aux$Labels_plot != "urology (1m)",]
df_male_cont_pvalue_aux <- df_male_cont_pvalue_aux[match(df_female_cont_pvalue_aux$Labels_plot,
                                                         df_male_cont_pvalue_aux$Labels_plot),]
df_female_male_cont_pvalue_common <- rbindx(df_female_cont_pvalue_aux %>%
                                             mutate(type = rep("female",nrow(.))) %>%
                                             dplyr::mutate(index = 1:nrow(.),
                                                           mean = eff),
                                           df_male_cont_pvalue_aux %>%
                                             mutate(type = rep("male",nrow(.))) %>%
                                             dplyr::mutate(index = 1:nrow(.),
                                                           mean = eff))

pdf("plots/odds-ratio continuous p-value Female and Male.pdf")
ggplot(data=df_female_male_cont_pvalue_common, aes(y=index, x=mean, xmin=lower, xmax=upper,color = type)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:23, labels=df_female_male_cont_pvalue_common$Labels_plot %>% unique()) +
  labs(title='Odds ratio continuous variables (p-value) Female and Male (significant in at least one)', x='log(odds-ratio)', y = 'Question') +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()
dev.off()

# -> only in Female 
df_female_cont_pvalue_only <- df_female_cont_pvalue[!df_female_cont_pvalue$Labels_plot %in% df_male_cont_pvalue$Labels_plot,] %>%
  mutate(index = 1:nrow(.))

pdf("plots/odds-ratio continuous p-value Female only.pdf")
odds_ratio_plot(df_female_cont_pvalue_only,"Odds ratio continuous variables (p-value) Female only")
dev.off()

# -> only in Male
df_male_cont_pvalue_only <- df_male_cont_pvalue[!df_male_cont_pvalue$Labels_plot %in% df_female_cont_pvalue$Labels_plot,] %>%
  mutate(index = 1:nrow(.))

pdf("plots/odds-ratio continuous p-value Male only.pdf")
odds_ratio_plot(df_male_cont_pvalue_only,"Odds ratio continuous variables (p-value) Male only")
dev.off()

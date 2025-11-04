## separate univariate analysis for categorical variables

# -> lifestyle
univariate_lifestyle_nosubcategories_new = list()
univariate_lifestyle_nosubcategories_gender_new = list()
univariate_lifestyle_nosubcategories_female_new = list()
univariate_lifestyle_nosubcategories_male_new = list()
for (i in 1:(ncol(dat_final_lifestyle_nosubcategories)-1)) {
  if(is.character(dat_final_lifestyle_nosubcategories[,i])){
    unique_characters = na.omit(dat_final_lifestyle_nosubcategories[,i]) %>% unique()
    for (k in 1:length(unique_characters)) {
      category_tmp = ifelse(dat_final_lifestyle_nosubcategories[,i] == unique_characters[k],1,0)
      category_tmp_female = ifelse(dat_final_lifestyle_nosubcategories[which_female,i] == unique_characters[k],1,0)
      category_tmp_male = ifelse(dat_final_lifestyle_nosubcategories[which_male,i] == unique_characters[k],1,0)
      name_category = paste0(colnames(dat_final_lifestyle_nosubcategories)[i],"_",unique_characters[k])
      lm_tmp = univariate_model_new_categories(data.frame(category_tmp,
                                               status2 = dat_final_lifestyle_nosubcategories$status2))
      lm_tmp_female = univariate_model_new_categories(data.frame(category_tmp_female,
                                                                 status2 = dat_final_lifestyle_nosubcategories[which_female,]$status2))
      lm_tmp_male = univariate_model_new_categories(data.frame(category_tmp_male,
                                                                 status2 = dat_final_lifestyle_nosubcategories[which_male,]$status2))
      lm_tmp_gender = univariate_model_adjustment_new(data.frame(category_tmp,
                                                          gender = as.factor(dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an.),
                                                          status2 = dat_final_lifestyle_nosubcategories$status2),
                                               gender)
      lm_tmp$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
      lm_tmp$Features = unique_characters[k]
      lm_tmp_gender$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
      lm_tmp_gender$Features = unique_characters[k]
      lm_tmp_female$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
      lm_tmp_female$Features = unique_characters[k]
      lm_tmp_male$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
      lm_tmp_male$Features = unique_characters[k]
      univariate_lifestyle_nosubcategories_new = append(univariate_lifestyle_nosubcategories_new,list(lm_tmp))
      univariate_lifestyle_nosubcategories_female_new = append(univariate_lifestyle_nosubcategories_female_new,list(lm_tmp_female))
      univariate_lifestyle_nosubcategories_male_new = append(univariate_lifestyle_nosubcategories_male_new,list(lm_tmp_male))
      univariate_lifestyle_nosubcategories_gender_new = append(univariate_lifestyle_nosubcategories_gender_new,list(lm_tmp_gender))
    }
  }
  else{
    lm_tmp = univariate_model_new(data.frame(dat_final_lifestyle_nosubcategories[,i],
                                             status2 = dat_final_lifestyle_nosubcategories$status2))
    lm_tmp = lm_tmp[[1]]
    lm_tmp_female = univariate_model_new(data.frame(dat_final_lifestyle_nosubcategories[which_female,i],
                                             status2 = dat_final_lifestyle_nosubcategories[which_female,]$status2))
    lm_tmp_female = lm_tmp_female[[1]]
    lm_tmp_male = univariate_model_new(data.frame(dat_final_lifestyle_nosubcategories[which_male,i],
                                                    status2 = dat_final_lifestyle_nosubcategories[which_male,]$status2))
    lm_tmp_male = lm_tmp_male[[1]]
    lm_tmp_gender = univariate_model_adjustment_new(data.frame(dat_final_lifestyle_nosubcategories[,i],
                                                               gender = as.factor(dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an.),
                                                               status2 = dat_final_lifestyle_nosubcategories$status2),
                                                    gender)
    lm_tmp$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
    lm_tmp_female$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
    lm_tmp_male$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
    lm_tmp_gender$Variables = colnames(dat_final_lifestyle_nosubcategories)[i]
    univariate_lifestyle_nosubcategories_new = append(univariate_lifestyle_nosubcategories_new,list(lm_tmp))
    univariate_lifestyle_nosubcategories_female_new = append(univariate_lifestyle_nosubcategories_female_new,list(lm_tmp_female))
    univariate_lifestyle_nosubcategories_male_new = append(univariate_lifestyle_nosubcategories_male_new,list(lm_tmp_male))
    univariate_lifestyle_nosubcategories_gender_new = append(univariate_lifestyle_nosubcategories_gender_new,list(lm_tmp_gender))
  }
}
univariate_lifestyle_nosubcategories_new <- do.call("rbind", univariate_lifestyle_nosubcategories_new) %>% 
  as.data.frame() 
univariate_lifestyle_nosubcategories_female_new <- do.call("rbind", univariate_lifestyle_nosubcategories_female_new) %>% 
  as.data.frame() 
univariate_lifestyle_nosubcategories_male_new <- do.call("rbind", univariate_lifestyle_nosubcategories_male_new) %>% 
  as.data.frame() 
univariate_lifestyle_nosubcategories_gender_new <- do.call("rbind", univariate_lifestyle_nosubcategories_gender_new) %>% 
  as.data.frame() 

univariate_lifestyle_gender_new <- do.call("rbind", list(univariate_lifestyle_nosubcategories_gender_new %>%
                                                           filter(Features != "Nein"),
                                                         univariate_lifestyle_subquestion_gender %>% select(-fdr)))

# -> diet and weight categorical variables
BMI_columns <- dat_final_subquestions_dietweight_continuous[,c(6:9)]
BMI_columns = apply(BMI_columns, 2, function(x) (x -mean(x, na.rm = T))/sd(x, na.rm = T)) %>% as.data.frame()
univariate_dietweight_nosubcategories_new = list()
univariate_dietweight_nosubcategories_gender_new = list()
univariate_dietweight_nosubcategories_female_new = list()
univariate_dietweight_nosubcategories_male_new = list()
dat_final_dietweight_all <- cbind(BMI_columns,dat_final_dietweight_all)
dat_final_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegetarische.Ernährung.umgestellt. <- ifelse(!is.na(dat_final_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegetarische.Ernährung.umgestellt.),
                                                                                                            "Ja","Nein")
dat_final_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegane.Ernährung.umgestellt. <- ifelse(!is.na(dat_final_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegane.Ernährung.umgestellt.),
                                                                                                            "Ja","Nein")
for (i in 1:(ncol(dat_final_dietweight_all)-1)) {
  if(is.character(dat_final_dietweight_all[,i])){
    unique_characters = na.omit(dat_final_dietweight_all[,i]) %>% unique()
    for (k in 1:length(unique_characters)) {
      category_tmp = ifelse(dat_final_dietweight_all[,i] == unique_characters[k],1,0)
      category_tmp_female = ifelse(dat_final_dietweight_all[which_female,i] == unique_characters[k],1,0)
      category_tmp_male = ifelse(dat_final_dietweight_all[which_male,i] == unique_characters[k],1,0)
      name_category = paste0(colnames(dat_final_dietweight_all)[i],"_",unique_characters[k])
      lm_tmp = univariate_model_new_categories(data.frame(category_tmp,
                                                          status2 = dat_final_dietweight_all$status2))
      lm_tmp_female = univariate_model_new_categories(data.frame(category_tmp_female,
                                                          status2 = dat_final_dietweight_all[which_female,]$status2))
      lm_tmp_male = univariate_model_new_categories(data.frame(category_tmp_male,
                                                                 status2 = dat_final_dietweight_all[which_male,]$status2))
      lm_tmp_gender = univariate_model_adjustment_new(data.frame(category_tmp,
                                                                 gender = as.factor(dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an.),
                                                                 status2 = dat_final_dietweight_all$status2),
                                                      gender)
      lm_tmp$Variables = colnames(dat_final_dietweight_all)[i]
      lm_tmp$Features = unique_characters[k]
      lm_tmp_female$Variables = colnames(dat_final_dietweight_all)[i]
      lm_tmp_female$Features = unique_characters[k]
      lm_tmp_male$Variables = colnames(dat_final_dietweight_all)[i]
      lm_tmp_male$Features = unique_characters[k]
      lm_tmp_gender$Variables = colnames(dat_final_dietweight_all)[i]
      lm_tmp_gender$Features = unique_characters[k]
      univariate_dietweight_nosubcategories_new = append(univariate_dietweight_nosubcategories_new,list(lm_tmp))
      univariate_dietweight_nosubcategories_female_new = append(univariate_dietweight_nosubcategories_female_new,list(lm_tmp_female))
      univariate_dietweight_nosubcategories_male_new = append(univariate_dietweight_nosubcategories_male_new,list(lm_tmp_male))
      univariate_dietweight_nosubcategories_gender_new = append(univariate_dietweight_nosubcategories_gender_new,list(lm_tmp_gender))
    }
  }
  else{
    lm_tmp = univariate_model_new(data.frame(dat_final_dietweight_all[,i],
                                             status2 = dat_final_dietweight_all$status2))
    lm_tmp = lm_tmp[[1]]
    lm_tmp_female = univariate_model_new(data.frame(dat_final_dietweight_all[which_female,i],
                                             status2 = dat_final_dietweight_all[which_female,]$status2))
    lm_tmp_female = lm_tmp_female[[1]]
    lm_tmp_male = univariate_model_new(data.frame(dat_final_dietweight_all[which_male,i],
                                             status2 = dat_final_dietweight_all[which_male,]$status2))
    lm_tmp_male = lm_tmp_male[[1]]
    lm_tmp_gender = univariate_model_adjustment_new(data.frame(dat_final_dietweight_all[,i],
                                                               gender = as.factor(dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an.),
                                                               status2 = dat_final_dietweight_all$status2),
                                                    gender)
    lm_tmp$Variables = colnames(dat_final_dietweight_all)[i]
    lm_tmp_female$Variables = colnames(dat_final_dietweight_all)[i]
    lm_tmp_male$Variables = colnames(dat_final_dietweight_all)[i]
    lm_tmp_gender$Variables = colnames(dat_final_dietweight_all)[i]
    univariate_dietweight_nosubcategories_new = append(univariate_dietweight_nosubcategories_new,list(lm_tmp))
    univariate_dietweight_nosubcategories_female_new = append(univariate_dietweight_nosubcategories_female_new,list(lm_tmp_female))
    univariate_dietweight_nosubcategories_male_new = append(univariate_dietweight_nosubcategories_male_new,list(lm_tmp_male))
    univariate_dietweight_nosubcategories_gender_new = append(univariate_dietweight_nosubcategories_gender_new,list(lm_tmp_gender))
  }
}
univariate_dietweight_nosubcategories_new <- do.call("rbind", univariate_dietweight_nosubcategories_new) %>% 
  as.data.frame() 
univariate_dietweight_nosubcategories_female_new <- do.call("rbind", univariate_dietweight_nosubcategories_female_new) %>% 
  as.data.frame() 
univariate_dietweight_nosubcategories_male_new <- do.call("rbind", univariate_dietweight_nosubcategories_male_new) %>% 
  as.data.frame() 
univariate_dietweight_nosubcategories_gender_new <- do.call("rbind", univariate_dietweight_nosubcategories_gender_new) %>% 
  as.data.frame() 

univariate_dietweight_new <- univariate_dietweight_nosubcategories_new %>%
  filter(Features != "Nein")
univariate_dietweight_female_new <- univariate_dietweight_nosubcategories_female_new %>%
  filter(Features != "Nein")
univariate_dietweight_male_new <- univariate_dietweight_nosubcategories_male_new %>%
  filter(Features != "Nein")
univariate_dietweight_gender_new <- univariate_dietweight_nosubcategories_gender_new %>%
  filter(Features != "Nein")
                                                        


univariate_model_new_categories <- function(data_final){
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
  print(res)
  df = do.call("rbind", res) %>% as.data.frame() %>% rownames_to_column("Variables")
  print(df)
  df$Features = do.call("c", ftr)
  df$t_stat = df$Estimate/df$`Std. Error`
  #df$fdr = p.adjust(df$`Pr(>|z|)`, "fdr")
  eff_CI_new = do.call("rbind",eff_CI) %>% as.data.frame() %>% rownames_to_column("Variables") 
  colnames(eff_CI_new) <- c("Variables","2.5 %","97.5 %")
  df <- df %>%
    left_join(eff_CI_new)
  print(df)
  return(df)
}
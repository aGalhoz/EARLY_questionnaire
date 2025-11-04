## Final forest plots

# 1: With main categories and sub-categories

# 1.1: Non-motor symptoms
univariate_nonmotor_general <- do.call("rbind",
                                       list(univariate_nonmotor_gender %>%
                                              select(-c(index,Features,Estimate,`z value`,eff,t_stat)) %>%
                                              mutate(main_category = Variables,
                                                     type = rep("Full sample",12),
                                                     `2.5 %` = exp(lower),
                                                     `97.5 %` = exp(upper),
                                                     exp_mean = exp(mean)),
                                            univariate_nonmotor_female %>%
                                              select(-c(index,Features,Estimate,`z value`,eff,t_stat)) %>%
                                              mutate(main_category = Variables,
                                                     type = rep("Female",12),
                                                     `2.5 %` = exp(lower),
                                                     `97.5 %` = exp(upper),
                                                     exp_mean = exp(mean)),
                                            univariate_nonmotor_male %>%
                                              select(-c(index,Features,Estimate, `z value`,eff,t_stat)) %>%
                                              mutate(main_category = Variables,
                                                     type = rep("Male",12),
                                                     `2.5 %` = exp(lower),
                                                     `97.5 %` = exp(upper),
                                                     exp_mean = exp(mean)),
                                            left_join(univariate_nonmotor_subsubquestion_gender,final_ALS_CTR_category_nonmotor[,7:8] %>% 
                                                        dplyr::rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                              select(-c(index,Features,Estimate,`z value`,eff,t_stat)) %>%
                                              mutate(type = rep("Full sample",78),
                                                     `2.5 %` = exp(lower),
                                                     `97.5 %` = exp(upper),
                                                     exp_mean = exp(mean)),
                                            left_join(univariate_nonmotor_subsubquestion_female,final_ALS_CTR_category_nonmotor[,7:8] %>% 
                                                        dplyr::rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                              select(-c(index,Features,Estimate,`z value`,eff,t_stat)) %>%
                                              mutate(type = rep("Female",77),
                                                     `2.5 %` = exp(lower),
                                                     `97.5 %` = exp(upper),
                                                     exp_mean = exp(mean)),
                                            left_join(univariate_nonmotor_subsubquestion_male,final_ALS_CTR_category_nonmotor[,7:8] %>% 
                                                        dplyr::rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                              select(-c(index,Features,Estimate,`z value`,eff,t_stat)) %>%
                                              mutate(type = rep("Male",77),
                                                     `2.5 %` = exp(lower),
                                                     `97.5 %` = exp(upper),
                                                     exp_mean = exp(mean))))
univariate_nonmotor_general <- univariate_nonmotor_general %>%
  arrange(main_category,Labels_plot,desc(log10_pval))

writexl::write_xlsx(univariate_nonmotor_general,"data code output/univariate_nonmotor_general.xlsx")

# 1.2: Pre-existing commodities
univariate_preconditions_general <- do.call("rbind",
                                            list(univariate_preconditions_gender %>%
                                                   select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                   mutate(main_category = Variables,
                                                          type = rep("Full sample",16),
                                                          `2.5 %` = exp(lower),
                                                          `97.5 %` = exp(upper),
                                                          exp_mean = exp(mean),
                                                          fdr = p.adjust(`Pr(>|z|)`, "fdr")),
                                                 univariate_preconditions_female %>%
                                                   select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                   mutate(main_category = Variables,
                                                          type = rep("Female",14),
                                                          `2.5 %` = exp(lower),
                                                          `97.5 %` = exp(upper),
                                                          exp_mean = exp(mean),
                                                          fdr = p.adjust(`Pr(>|z|)`, "fdr")),
                                                 univariate_preconditions_male %>%
                                                   select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                   mutate(main_category = Variables,
                                                          type = rep("Male",14),
                                                          `2.5 %` = exp(lower),
                                                          `97.5 %` = exp(upper),
                                                          exp_mean = exp(mean),
                                                          fdr = p.adjust(`Pr(>|z|)`, "fdr")),
                                                 left_join(univariate_preconditions_subsubquestion_gender,final_ALS_CTR_category_preconditions[,7:8] %>% 
                                                             rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                                   select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                   mutate(type = rep("Full sample",70),
                                                          `2.5 %` = exp(lower),
                                                          `97.5 %` = exp(upper),
                                                          exp_mean = exp(mean),
                                                          fdr = p.adjust(`Pr(>|z|)`, "fdr")),
                                                 left_join(univariate_preconditions_subsubquestion_female,final_ALS_CTR_category_preconditions[,7:8] %>% 
                                                             rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                                   select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                   mutate(type = rep("Female",45),
                                                          `2.5 %` = exp(lower),
                                                          `97.5 %` = exp(upper),
                                                          exp_mean = exp(mean),
                                                          fdr = p.adjust(`Pr(>|z|)`, "fdr")),
                                                 left_join(univariate_preconditions_subsubquestion_male,final_ALS_CTR_category_preconditions[,7:8] %>% 
                                                             rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                                   select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                   mutate(type = rep("Male",45),
                                                          `2.5 %` = exp(lower),
                                                          `97.5 %` = exp(upper),
                                                          exp_mean = exp(mean),
                                                          fdr = p.adjust(`Pr(>|z|)`, "fdr"))))

univariate_preconditions_general <- univariate_preconditions_general %>%
  arrange(main_category,Labels_plot,desc(log10_pval))

writexl::write_xlsx(univariate_preconditions_general,"data code output/univariate_preconditions_general.xlsx")

# 1.3: Lifestyle
univariate_lifestyle_female_new <- rbind(univariate_lifestyle_nosubcategories_female_new %>%
                                           filter(Features != "Nein"),
                                         univariate_lifestyle_subquestion_female %>% select(-fdr))
univariate_lifestyle_male_new <- rbind(univariate_lifestyle_nosubcategories_male_new %>%
                                         filter(Features != "Nein"),
                                       univariate_lifestyle_subquestion_male %>% select(-fdr))
univariate_lifestyle_general <- do.call("rbind",
                                        list(univariate_lifestyle_gender_new %>%
                                               mutate(main_category = Variables,
                                                      type = rep("Full sample",44),
                                                      lower = log(`2.5 %`),
                                                      upper = log(`97.5 %`),
                                                      fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                               mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                      log10_padj = -log10(fdr),
                                                      mean = eff,
                                                      `2.5 %` = exp(lower),
                                                      `97.5 %` = exp(upper),
                                                      exp_mean = exp(mean)) %>%
                                               select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat)),
                                             univariate_lifestyle_female_new %>%
                                               mutate(main_category = Variables,
                                                      type = rep("Female",44),
                                                                 lower = log(`2.5 %`),
                                                                 upper = log(`97.5 %`),
                                                                 fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                        mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                               log10_padj = -log10(fdr),
                                                               mean = eff,
                                                               `2.5 %` = exp(lower),
                                                               `97.5 %` = exp(upper),
                                                               exp_mean = exp(mean)) %>%
                                                        select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat)),
                                             univariate_lifestyle_male_new %>%
                                               mutate(main_category = Variables,
                                                      type = rep("Male",44),
                                                                 lower = log(`2.5 %`),
                                                                 upper = log(`97.5 %`),
                                                                 fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                        mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                               log10_padj = -log10(fdr),
                                                               mean = eff,
                                                               `2.5 %` = exp(lower),
                                                               `97.5 %` = exp(upper),
                                                               exp_mean = exp(mean)) %>%
                                                        select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat)),
                                             left_join(univariate_lifestyle_subsubquestion_gender,final_ALS_CTR_category_lifestyle[,7:8] %>% 
                                                         rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                               mutate(type = rep("Full sample",54),
                                                                 lower = log(`2.5 %`),
                                                                 upper = log(`97.5 %`),
                                                                 fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                        mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                               log10_padj = -log10(fdr),
                                                               mean = eff,
                                                               `2.5 %` = exp(lower),
                                                               `97.5 %` = exp(upper),
                                                               exp_mean = exp(mean)) %>%
                                                        select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat)),
                                             left_join(univariate_lifestyle_subsubquestion_female,final_ALS_CTR_category_lifestyle[,7:8] %>% 
                                                         rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                               mutate(type = rep("Female",50),
                                                                 lower = log(`2.5 %`),
                                                                 upper = log(`97.5 %`),
                                                                 fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                        mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                               log10_padj = -log10(fdr),
                                                               mean = eff,
                                                               `2.5 %` = exp(lower),
                                                               `97.5 %` = exp(upper),
                                                               exp_mean = exp(mean)) %>%
                                                        select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat)),
                                             left_join(univariate_lifestyle_subsubquestion_male,final_ALS_CTR_category_lifestyle[,7:8] %>% 
                                                         rename(Variables = `sub-subcategory`,main_category = `sub-category`)) %>%
                                               mutate(type = rep("Male",50),
                                                                 lower = log(`2.5 %`),
                                                                 upper = log(`97.5 %`),
                                                                 fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                        mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                               log10_padj = -log10(fdr),
                                                               mean = eff,
                                                               `2.5 %` = exp(lower),
                                                               `97.5 %` = exp(upper),
                                                               exp_mean = exp(mean)) %>%
                                                        select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat))))
                                        
univariate_lifestyle_general <- univariate_lifestyle_general %>%
  arrange(main_category,Variables,desc(log10_pval))
univariate_lifestyle_general <- univariate_lifestyle_general %>%
  distinct()

writexl::write_xlsx(univariate_lifestyle_general,"data code output/univariate_lifestyle_general.xlsx")

## 2: Only main categories

# 2.1: Healthcare
univariate_healthcare_general <- do.call("rbind",
                                         list(univariate_healthcare_gender %>%
                                                select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                mutate(main_category = Variables,
                                                       type = rep("Full sample",24),
                                                       `2.5 %` = exp(lower),
                                                       `97.5 %` = exp(upper),
                                                       exp_mean = exp(mean),
                                                       fdr = p.adjust(`Pr(>|z|)`, "fdr")),
                                              univariate_healthcare_female %>%
                                                select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                mutate(main_category = Variables,
                                                       type = rep("Female",24),
                                                       `2.5 %` = exp(lower),
                                                       `97.5 %` = exp(upper),
                                                       exp_mean = exp(mean),
                                                       fdr = p.adjust(`Pr(>|z|)`, "fdr")),
                                              univariate_healthcare_male %>%
                                                select(-c(index,Features,Estimate,`Std. Error`,`z value`,eff,t_stat)) %>%
                                                mutate(main_category = Variables,
                                                       type = rep("Male",24),
                                                       `2.5 %` = exp(lower),
                                                       `97.5 %` = exp(upper),
                                                       exp_mean = exp(mean),
                                                       fdr = p.adjust(`Pr(>|z|)`, "fdr"))))

univariate_healthcare_general <- univariate_healthcare_general %>%
  arrange(main_category,Labels_plot,desc(log10_pval))

writexl::write_xlsx(univariate_healthcare_general,"data code output/univariate_healthcare_general.xlsx")

# 2.2: Diet and weight 
univariate_dietweight_general <- do.call("rbind",
                                         list(univariate_dietweight_gender_new %>%
                                                mutate(main_category = Variables,
                                                       type = rep("Full sample",14),
                                                       lower = log(`2.5 %`),
                                                       upper = log(`97.5 %`),
                                                       fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                       log10_padj = -log10(fdr),
                                                       mean = eff,
                                                       `2.5 %` = exp(lower),
                                                       `97.5 %` = exp(upper),
                                                       exp_mean = exp(mean)) %>%
                                                select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat)),
                                              univariate_dietweight_female_new %>%
                                                mutate(main_category = Variables,
                                                       type = rep("Female",14),
                                                       lower = log(`2.5 %`),
                                                       upper = log(`97.5 %`),
                                                       fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                       log10_padj = -log10(fdr),
                                                       mean = eff,
                                                       `2.5 %` = exp(lower),
                                                       `97.5 %` = exp(upper),
                                                       exp_mean = exp(mean)) %>%
                                                select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat)),
                                              univariate_dietweight_male_new %>%
                                                mutate(main_category = Variables,
                                                       type = rep("Male",14),
                                                       lower = log(`2.5 %`),
                                                       upper = log(`97.5 %`),
                                                       fdr = p.adjust(`Pr(>|z|)`, "fdr")) %>%
                                                mutate(log10_pval = -log10(`Pr(>|z|)`),
                                                       log10_padj = -log10(fdr),
                                                       mean = eff,
                                                       `2.5 %` = exp(lower),
                                                       `97.5 %` = exp(upper),
                                                       exp_mean = exp(mean)) %>%
                                                select(-c(Estimate,`Std. Error`,`z value`,eff,t_stat))))

univariate_dietweight_general <- univariate_dietweight_general %>%
  arrange(main_category,desc(log10_pval))

writexl::write_xlsx(univariate_dietweight_general,"data code output/univariate_dietweight_general.xlsx")


## All together 
univariate_all_fullsample <- do.call("rbind",
                                     list(univariate_nonmotor_general %>%
                                       filter(type == "Full sample") %>%
                                       select(main_category,Labels_plot,
                                              exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                     univariate_preconditions_general %>%
                                       filter(type == "Full sample") %>%
                                       select(main_category,Labels_plot,
                                              exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                     univariate_lifestyle_general_touse %>%
                                       filter(type == "Full sample") %>%
                                       mutate(exp_mean = exp(mean)) %>%
                                       select(main_category,Labels_plot,
                                              exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                     univariate_healthcare_general %>% 
                                       filter(type == "Full sample") %>%
                                       select(main_category,Labels_plot,
                                              exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                     univariate_dietweight_general %>%
                                       filter(type == "Full sample") %>%
                                       select(main_category,Variables,
                                              exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`) %>%
                                       rename(Labels_plot = Variables))) 

univariate_all_fullsample$fdr = p.adjust(univariate_all_fullsample$`Pr(>|z|)`,"fdr")

univariate_all_female <- do.call("rbind",
                                 list(univariate_nonmotor_general %>%
                                        filter(type == "Female") %>%
                                        select(main_category,Labels_plot,
                                               exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                      univariate_preconditions_general %>%
                                        filter(type == "Female") %>%
                                        select(main_category,Labels_plot,
                                               exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                      univariate_lifestyle_general_touse %>%
                                        filter(type == "Female") %>%
                                        mutate(exp_mean = exp(mean)) %>%
                                        select(main_category,Labels_plot,
                                               exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                      univariate_healthcare_general %>% 
                                        filter(type == "Female") %>%
                                        select(main_category,Labels_plot,
                                               exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                      univariate_dietweight_general %>%
                                        filter(type == "Female") %>%
                                        select(main_category,Variables,
                                               exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`) %>%
                                        rename(Labels_plot = Variables))) 

univariate_all_female$fdr = p.adjust(univariate_all_female$`Pr(>|z|)`,"fdr")
  
univariate_all_male <- do.call("rbind",
                               list(univariate_nonmotor_general %>%
                                      filter(type == "Male") %>%
                                      select(main_category,Labels_plot,
                                             exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                    univariate_preconditions_general %>%
                                      filter(type == "Male") %>%
                                      select(main_category,Labels_plot,
                                             exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                    univariate_lifestyle_general_touse %>%
                                      filter(type == "Male") %>%
                                      mutate(exp_mean = exp(mean)) %>%
                                      select(main_category,Labels_plot,
                                             exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                    univariate_healthcare_general %>% 
                                      filter(type == "Male") %>%
                                      select(main_category,Labels_plot,
                                             exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`),
                                    univariate_dietweight_general %>%
                                      filter(type == "Male") %>%
                                      select(main_category,Variables,
                                             exp_mean,mean,`2.5 %`,`97.5 %`,lower,upper,`Pr(>|z|)`) %>%
                                      rename(Labels_plot = Variables))) 

univariate_all_male$fdr = p.adjust(univariate_all_male$`Pr(>|z|)`,"fdr")

writexl::write_xlsx(univariate_all_fullsample,"univariate_fullsample.xlsx")
writexl::write_xlsx(univariate_all_female,"univariate_female.xlsx")
writexl::write_xlsx(univariate_all_male,"univariate_male.xlsx")


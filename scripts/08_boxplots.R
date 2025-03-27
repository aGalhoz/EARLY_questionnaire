## Boxplots of univariate categories

library(magrittr)

###### -> non-motor Qs

# -> general
dat_final_nonmotor_general_aux <- dat_final_nonmotor_general
dat_final_nonmotor_general_aux[,1] <- sapply(dat_final_nonmotor_general[,1], function(x) ifelse(grepl("Ja",x),1,0))

boxplot_binary(dat_final_nonmotor_general_aux,univariate_nonmotor_general)

# -> subquestions
subquestions_nonmotor <- final_ALS_CTR_category_nonmotor_diff$`sub-category` %>% unique()

dat_final_subquestions_nonmotor <- list()
for (i in 1:length(subquestions_nonmotor)) {
  subquestion_i <- subquestions_nonmotor[i]
  final_ALS_CTR_category_nonmotor_diff_i <- final_ALS_CTR_category_nonmotor_diff %>%
    filter(`sub-category` == subquestion_i)
  dat_final_nonmotor_diff_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_nonmotor_diff_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_nonmotor_diff_i_temp <- apply(dat_final_nonmotor_diff_i[,1:ncol(dat_final_nonmotor_diff_i)-1], 2, function(x) ifelse(!is.na(x),1,0))
  dat_final_subquestions_nonmotor[[i]] <- set_names(data.frame(subquestion_i = apply(dat_final_nonmotor_diff_i_temp, 1, function(x) {
    nonmotor_diff_all <- sum(x)})),subquestion_i)
}
dat_final_subquestions_nonmotor <- do.call("cbind", dat_final_subquestions_nonmotor) %>% 
  as.data.frame() %>%
  mutate(status = dat_final_nonmotor_diff_i[,"status2"]) 

barplot_continuous(dat_final_subquestions_nonmotor)

# -> subsubquestions
subsubquestions_nonmotor <- final_ALS_CTR_category_nonmotor_diff$`sub-subcategory` %>% unique()

dat_final_subsubquestions_nonmotor <- list()
for (i in 1:length(subsubquestions_nonmotor)) {
  subsubquestion_i <- subsubquestions_nonmotor[i]
  final_ALS_CTR_category_nonmotor_diff_i <- final_ALS_CTR_category_nonmotor_diff %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_nonmotor_diff_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_nonmotor_diff_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_nonmotor_diff_i_temp <- ifelse(!is.na(dat_final_nonmotor_diff_i[,1:ncol(dat_final_nonmotor_diff_i)-1]),1,0)
  dat_final_subsubquestions_nonmotor[[i]] <- set_names(data.frame(subsubquestion_i = dat_final_nonmotor_diff_i_temp),subsubquestion_i)
}
dat_final_subsubquestions_nonmotor <- do.call("cbind", dat_final_subsubquestions_nonmotor) %>% 
  as.data.frame() %>%
  mutate(status = dat_final_nonmotor_diff_i[,"status2"])

univariate_nonmotor_diff_subsubquestion <- univariate_nonmotor_diff_subsubquestion[match(names(dat_final_subsubquestions_nonmotor),
                                                                                         univariate_nonmotor_diff_subsubquestion$Variables),]
boxplot_binary(dat_final_subsubquestions_nonmotor,
               univariate_nonmotor_diff_subsubquestion)

## timeplot analysis of relevant Qs
final_ALS_CTR_category_nonmotor_diff_i <- final_ALS_CTR_category_nonmotor_diff %>%
  filter(`sub-subcategory` == "Kalte, blasse oder bläulich verfärbte Extremitäten")
dat_final_nonmotor_diff_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_nonmotor_diff_i$`original question (ALS)`),ncol(dat_final))]

dt_nonmotor_ALS <- as.data.frame(table(c(dat_final_nonmotor_diff_i[dat_final_nonmotor_diff_i$status2==1,1]))) %>% arrange(desc(Freq))
dt_nonmotor_CTR <- as.data.frame(table(c(dat_final_nonmotor_diff_i[dat_final_nonmotor_diff_i$status2==0,1]))) %>% arrange(desc(Freq))
names(dt_nonmotor_ALS) <- c("Levels_en","Freq_ALS")
names(dt_nonmotor_CTR) <- c("Levels_en","Freq_CTR")

# "1 year" = ifelse(dat_final_nonmotor_diff_i[dat_final_nonmotor_diff_i$status2==1,1] == "X1.12Monate",1,0)
# 
# dt_nonmotor_ALS_tmp <- dt_nonmotor_ALS %>%
#   mutate(group = "ALS",
#          Freq = Freq_ALS/514) %>%
#   select(-Freq_ALS)
# dt_nonmotor_CTR_tmp <- dt_nonmotor_CTR %>%
#   mutate(group = "CTR",
#          Freq = Freq_CTR/306) %>%
#   select(-Freq_CTR)
# 
# dt_nonmotor_tmp <- rbind(dt_nonmotor_ALS_tmp,
#                          dt_nonmotor_CTR_tmp)
# dt_nonmotor_tmp %>% 
#   group_by(Levels_en) %>% 
#   summarize(table(group, Freq))
#   summarize(p_value = chisq.test(table(group, Freq))$p.value)


dt_nonmotor_all_tmp <- merge(dt_nonmotor_ALS,
                         dt_nonmotor_CTR)
dt_nonmotor_all_tmp$Variable_en <- rep("Cold extremities",4)
dt_nonmotor_all_tmp$Variable_en <- rep("Trembling of arms or legs",4)
dt_nonmotor_all_tmp$Variable_en <- rep("Excessive saliva",4)
dt_nonmotor_all_tmp$Variable_en <- rep("Frequent falls backwards",3)

dt_nonmotor_all <- rbind(dt_nonmotor_all,dt_nonmotor_all_tmp)

dt_nonmotor_all$Levels_en <- ifelse(dt_nonmotor_all$Levels_en == "X..1Monat",
                                    "1 month",
                                    ifelse(dt_nonmotor_all$Levels_en == "X1.12Monate",
                                           "1 year",
                                           ifelse(dt_nonmotor_all$Levels_en == "X1.5Jahre",
                                                           "5 years",
                                                  ifelse(dt_nonmotor_all$Levels_en == "X5.10Jahre",
                                                                            "10 years",dt_nonmotor_all$Levels_en))))
                              
  
dt_nonmotor_all$Levels_en <- factor(dt_nonmotor_all$Levels_en,
                                    levels = c("1 month","1 year","5 years","10 years"))

barplot_categories(dt_nonmotor_all)

###### -> Contacts healthcare system Qs
subquestions_healthcare <- final_ALS_CTR_category_healthcare$`sub-category` %>% unique()

dat_final_subquestions_healthcare <- list()
for (i in 1:length(subquestions_healthcare)) {
  subquestion_i <- subquestions_healthcare[i]
  final_ALS_CTR_category_healthcare_i <- final_ALS_CTR_category_healthcare %>%
    filter(`sub-category` == subquestion_i)
  dat_final_healthcare_i <- data_patients_control_combined_all[,c(which(colnames(data_patients_control_combined_all) 
                                                                        %in% final_ALS_CTR_category_healthcare_i$`original question (ALS)`),ncol(data_patients_control_combined_all))]
  dat_final_subquestions_healthcare[[i]] <- set_names(data.frame(subquestion_i = apply(dat_final_healthcare_i[,1:ncol(dat_final_healthcare_i)-1], 1, function(x) {
    x <- ifelse(is.na(x),0,1)
    healthcare_all <- sum(x)})),subquestion_i)
}
dat_final_subquestions_healthcare <- do.call("cbind", dat_final_subquestions_healthcare) %>% 
  as.data.frame() %>%
  mutate(status = dat_final_healthcare_i[,"status"])

barplot_continuous(dat_final_subquestions_healthcare)

###### -> Pre-existing conditions Qs

# -> subquestion
# -> Pre-existing conditions (each subquestion individually)
subquestions_preconditions <- final_ALS_CTR_category_preconditions$`sub-category` %>% unique()

dat_final_subquestions_preconditions <- list()
for (i in 1:length(subquestions_preconditions)) {
  subquestion_i <- subquestions_preconditions[i]
  final_ALS_CTR_category_preconditions_i <- final_ALS_CTR_category_preconditions %>%
    filter(`sub-category` == subquestion_i)
  dat_final_preconditions_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_preconditions_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_preconditions_i_temp <- apply(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
  dat_final_subquestions_preconditions[[i]] <- set_names(data.frame(subquestion_i = apply(dat_final_preconditions_i_temp, 1, function(x) {
    preconditions_all <- sum(x)})),subquestion_i)
}
dat_final_subquestions_preconditions <- do.call("cbind", dat_final_subquestions_preconditions) %>% 
  as.data.frame() %>%
  mutate(status = dat_final_preconditions_i[,"status2"])

barplot_continuous(dat_final_subquestions_preconditions)

# -> subsubquestion
subsubquestions_preconditions <- final_ALS_CTR_category_preconditions$`sub-subcategory` %>% unique()
subsubquestions_preconditions <- subsubquestions_preconditions[c(1:25,27:31,33:36,38:41,43:51,53:61,64:65,
                                                                 67:69,73:74,79,81,83,93,99,101,103)]

dat_final_subsubquestions_preconditions <- list()
for (i in 1:length(subsubquestions_preconditions)) {
  subsubquestion_i <- subsubquestions_preconditions[i]
  final_ALS_CTR_category_preconditions_i <- final_ALS_CTR_category_preconditions %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_preconditions_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_preconditions_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_preconditions_i_temp <- ifelse(dat_final_preconditions_i[,1:ncol(dat_final_preconditions_i)-1] == "Ja",1,0)
  dat_final_subsubquestions_preconditions[[i]] <- set_names(data.frame(subsubquestion_i = dat_final_preconditions_i_temp),subsubquestion_i)
}
dat_final_subsubquestions_preconditions <- do.call("cbind", dat_final_subsubquestions_preconditions) %>% 
  as.data.frame() %>%
  mutate(status = dat_final_preconditions_i[,"status2"])

univariate_preconditions_subsubquestion <- univariate_preconditions_subsubquestion[match(names(dat_final_subsubquestions_preconditions),
                                                 univariate_preconditions_subsubquestion$Variables),]
boxplot_binary(dat_final_subsubquestions_preconditions,
               univariate_preconditions_subsubquestion)

###### -> Diet and weight Qs
final_ALS_CTR_category_dietweight <- final_ALS_CTR_category_temp %>%
  filter(category == "diet and weight")

dat_final_subquestions_dietweight <- data_patients_control_combined_all[,c(which(colnames(data_patients_control_combined_all) 
                                                                        %in% final_ALS_CTR_category_dietweight$`original question (ALS)`),ncol(data_patients_control_combined_all))]

dat_final_subquestions_dietweight[,2:3] <- apply(dat_final_subquestions_dietweight[,2:3], 2, function(x) {ifelse(!is.na(x) & x == "Ja",1,0)})
dat_final_subquestions_dietweight_binary <- dat_final_subquestions_dietweight[,c(2:3,8)]
dat_final_subquestions_dietweight_continuous <- apply(dat_final_subquestions_dietweight[,c(1,4:8)],2,
                                                      as.numeric)

boxplot_binary(dat_final_subquestions_dietweight_binary,
               univariate_dietweight_all[c(1,7),])
dat_final_subquestions_dietweight_continuous[322,"Bitte geben Sie Ihre Körpergröße an [cm]."] <- "NA"
dat_final_subquestions_dietweight_continuous[114,"Bitte geben Sie Ihre Körpergröße an [cm]."] <- "NA"
dat_final_subquestions_dietweight_continuous <- as.data.frame(dat_final_subquestions_dietweight_continuous)
dat_final_subquestions_dietweight_continuous[,c(1:5)] <- apply(dat_final_subquestions_dietweight_continuous[,c(1:5)],
                                                               2,as.numeric)
height_m <- conv_unit(as.numeric(dat_final_subquestions_dietweight_continuous$`Bitte geben Sie Ihre Körpergröße an [cm].`),"cm","m")
dat_final_subquestions_dietweight_continuous <- dat_final_subquestions_dietweight_continuous %>%
  dplyr::mutate(BMI_10Y = `Bitte geben Sie Ihr Gewicht an. [10 Jahre vor Erkrankungsbeginn][Gewicht [kg]]`/(height_m)^2,
                BMI_5Y = `Bitte geben Sie Ihr Gewicht an. [5 Jahre vor Erkrankungsbeginn][Gewicht [kg]]`/(height_m)^2,
                BMI_1Y = `Bitte geben Sie Ihr Gewicht an. [Ein Jahr vor Erkrankungbeginn][Gewicht [kg]]`/(height_m)^2,
                BMI_now = `Bitte geben Sie Ihr Gewicht an. [Aktuell][Gewicht [kg]]`/(height_m)^2)
dat_final_subquestions_dietweight_continuous <- dat_final_subquestions_dietweight_continuous[,c(1:5,7:10,6)]
boxplot_continuous(dat_final_subquestions_dietweight_continuous)

# spaghetti plot
data_BMI_timeline <- dat_final_subquestions_dietweight_continuous[,c(6:10)] %>%
  mutate(ID = rep(1:nrow(dat_final_subquestions_dietweight_continuous)),
         status = ifelse(status == 1,"ALS","CTR"))
data_BMI_timeline <- data_BMI_timeline %>% 
  pivot_longer(!c(status,ID), names_to = "BMI", values_to = "BMI_value")
data_BMI_timeline$BMI <- factor(data_BMI_timeline$BMI,
                                levels = c("BMI_now","BMI_1Y","BMI_5Y","BMI_10Y"))
pdf(file = "plots/boxplots/BMI_ALS_CTR.pdf")
ggplot(data_BMI_timeline, aes(BMI, BMI_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI ALS and CTR") +
  theme_minimal() 
dev.off()
pdf(file = "plots/boxplots/BMI_ALS.pdf")
ggplot(data_BMI_timeline %>% filter(status == "ALS"), aes(BMI, BMI_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI ALS") +
  theme_minimal() 
dev.off()
pdf(file = "plots/boxplots/BMI_CTR.pdf")
ggplot(data_BMI_timeline %>% filter(status == "CTR"), aes(BMI, BMI_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI CTR") +
  theme_minimal() 
dev.off()

# spaghetti plots of BMI deltas
data_BMI_delta_timeline <- dat_final_subquestions_dietweight_continuous[,c(6:10)] %>%
  mutate(ID = rep(1:nrow(dat_final_subquestions_dietweight_continuous)),
         status = ifelse(status == 1,"ALS","CTR"),
         BMI_now_1y = BMI_now - BMI_1Y,
         BMI_now_5y = BMI_now - BMI_5Y,
         BMI_now_10y = BMI_now - BMI_10Y) %>%
  dplyr::select(ID,status,BMI_now_10y,BMI_now_1y,BMI_now_5y)
data_BMI_delta_timeline$gender <- data_patients_control_combined_all$`Bitte geben Sie Ihr Geschlecht an.`
data_BMI_delta_timeline <- data_BMI_delta_timeline %>% 
  pivot_longer(!c(status,ID,gender), names_to = "BMI_delta", values_to = "BMI_delta_value")
data_BMI_delta_timeline$BMI_delta <- factor(data_BMI_delta_timeline$BMI_delta,
                                levels = c("BMI_now_1y","BMI_now_5y","BMI_now_10y"))

pdf(file = "plots/boxplots/BMI_delta_ALS_CTR.pdf")
ggplot(data_BMI_delta_timeline, aes(BMI_delta, BMI_delta_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI delta ALS and CTR") +
  theme_minimal() 
dev.off()
pdf(file = "plots/boxplots/BMI_delta_ALS_CTR_female.pdf")
ggplot(data_BMI_delta_timeline %>% filter(gender == "weiblich"), aes(BMI_delta, BMI_delta_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI delta ALS and CTR (Female)") +
  theme_minimal() 
dev.off()
pdf(file = "plots/boxplots/BMI_delta_ALS_CTR_male.pdf")
ggplot(data_BMI_delta_timeline %>% filter(gender == "männlich"), aes(BMI_delta, BMI_delta_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI delta ALS and CTR (Male)") +
  theme_minimal() 
dev.off()
pdf(file = "plots/boxplots/BMI_delta_ALS.pdf")
ggplot(data_BMI_delta_timeline %>% filter(status == "ALS"), aes(BMI_delta, BMI_delta_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI delta ALS") +
  theme_minimal() 
dev.off()
pdf(file = "plots/boxplots/BMI_delta_CTR.pdf")
ggplot(data_BMI_delta_timeline %>% filter(status == "CTR"), aes(BMI_delta, BMI_delta_value, group = ID, color = status)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
  theme(title = element_text(size = 10),
        axis.title.x = element_blank()) +
  ggtitle("BMI delta CTR") +
  theme_minimal() 
dev.off()

dat_final_subquestions_dietweight_continuous$gender <- data_patients_control_combined_all$`Bitte geben Sie Ihr Geschlecht an.`
dat_final_subquestions_dietweight_continuous_female <- dat_final_subquestions_dietweight_continuous %>%
  filter(gender == "weiblich") %>%
  select(-gender)
dat_final_subquestions_dietweight_continuous_male <- dat_final_subquestions_dietweight_continuous %>%
  filter(gender == "männlich") %>%
  select(-gender)
boxplot_continuous_gender(dat_final_subquestions_dietweight_continuous_female,"female")
boxplot_continuous_gender(dat_final_subquestions_dietweight_continuous_male,"male")

library(outliers)
temp <- dat_final_subquestions_dietweight_continuous$`Bitte geben Sie Ihr Gewicht an. [Aktuell][Gewicht [kg]]`
temp[which(temp == 185.0)] <- NA
grubbs.test(temp)
temp[which(temp == 140.0)] <- NA

###### -> Lifestyle Qs
final_ALS_CTR_category_lifestyle <- final_ALS_CTR_category_temp %>%
  filter(category == "lifestyle") %>%
  mutate(`sub-category` = ifelse(is.na(`sub-subcategory`),`question in EN (ALS)`,`sub-category`))

# -> no subcategories
final_ALS_CTR_category_lifestyle_nosubcategories <- final_ALS_CTR_category_lifestyle %>%
  filter(is.na(`sub-subcategory`))

dat_final_nosubcategories_lifestyle <- data_patients_control_combined_all[,c(which(colnames(data_patients_control_combined_all) 
                                                                                %in% final_ALS_CTR_category_lifestyle_nosubcategories$`original question (ALS)`),ncol(data_patients_control_combined_all))]

barplot_continuous(dat_final_nosubcategories_lifestyle[,c(12,18)])
boxplot_binary(dat_final_nosubcategories_lifestyle[,c(4:6,13:16,18)],
               univariate_lifestyle_nosubcategories[c(2:8,13:16),])

#--> freqs for variables with several categories
dat_final_nosubcategories_lifestyle_temp <- dat_final_nosubcategories_lifestyle[,c(1:3,7:11,17,18)]
dat_final_nosubcategories_lifestyle_temp <- apply(dat_final_nosubcategories_lifestyle_temp, 2, 
                                                  function(x) ifelse(is.na(x),"NA",x)) %>%
  as.data.frame()
dat_final_nosubcategories_lifestyle_temp$`Was ist Ihr höchster schulischer Abschluss?` <- ifelse(dat_final_nosubcategories_lifestyle_temp$`Was ist Ihr höchster schulischer Abschluss?` == "Gesamtschule",
                                                                                                 "NA",
                                                                                                 dat_final_nosubcategories_lifestyle_temp$`Was ist Ihr höchster schulischer Abschluss?`)

dat_final_nosubcategories_lifestyle_temp$gender <- data_patients_control_combined_all$`Bitte geben Sie Ihr Geschlecht an.`
dt_categories_ALS <- frequency_status(dat_final_nosubcategories_lifestyle_temp,
                     1,
                     "Freq_ALS")
dt_categories_CTR <- frequency_status(dat_final_nosubcategories_lifestyle_temp,
                                      0,
                                      "Freq_CTR")
dt_categories_ALS_female <- frequency_status(dat_final_nosubcategories_lifestyle_temp  %>% filter(gender == "weiblich"),
                                      1,
                                      "Freq_ALS")
dt_categories_ALS_male <- frequency_status(dat_final_nosubcategories_lifestyle_temp %>% filter(gender == "männlich"),
                                             1,
                                             "Freq_ALS")
dt_categories_CTR_female <- frequency_status(dat_final_nosubcategories_lifestyle_temp  %>% filter(gender == "weiblich"),
                                      0,
                                      "Freq_CTR")
dt_categories_CTR_male <- frequency_status(dat_final_nosubcategories_lifestyle_temp %>% filter(gender == "männlich"),
                                      0,
                                      "Freq_CTR")
dt_categories_ALS_gender <- rbind(cbind(dt_categories_ALS_female,
                                        data.frame(gender = rep("female",45)))[1:43,],
                                  cbind(dt_categories_ALS_male,
                                        data.frame(gender = rep("male",45)))[1:43,])
dt_categories_CTR_gender <- rbind(cbind(dt_categories_CTR_female,
                                        data.frame(gender = rep("female",45)))[1:43,],
                                  cbind(dt_categories_CTR_male,
                                        data.frame(gender = rep("male",45)))[1:43,])

# -> also the category of which sport 
dat_final_nosubcategories_lifestyle_sport <- dat_final_nosubcategories_lifestyle[,c(13:16,18)]
dat_final_nosubcategories_lifestyle_sport$gender <- data_patients_control_combined_all$`Bitte geben Sie Ihr Geschlecht an.`
dat_final_nosubcategories_lifestyle_sport[,c(1:4)] <- apply(dat_final_nosubcategories_lifestyle_sport[,c(1:4)],2,
                                                   function(x) as.numeric(ifelse(x == "Ja",1,ifelse(x=="Nein",0,x))))
dt_sport <- dat_final_nosubcategories_lifestyle_sport %>%
  group_by(status) %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm = TRUE)))
dt_sport_gender <- dat_final_nosubcategories_lifestyle_sport %>%
  group_by(status,gender) %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm = TRUE)))
dt_sport_ALS <- data.frame(Freq_ALS = t(dt_sport %>%
                                          filter(status == 1) %>%
                                          select(-status)))
dt_sport_ALS_gender <- rbind(data.frame(Freq_ALS = t(dt_sport_gender %>%
                                          filter(status == 1 & gender == "weiblich") %>%
                                          ungroup() %>%
                                          select(-c(status,gender)))) %>%
                                          mutate(gender = rep("female",4)),
                             data.frame(Freq_ALS = t(dt_sport_gender %>%
                                                       filter(status == 1 & gender == "männlich") %>%
                                                       ungroup() %>%
                                                       select(-c(status,gender)))) %>%
                               mutate(gender = rep("male",4)))
dt_sport_CTR <- data.frame(Freq_CTR = t(dt_sport %>%
                                          filter(status == 0) %>%
                                          select(-status)))
dt_sport_CTR_gender <- rbind(data.frame(Freq_CTR = t(dt_sport_gender %>%
                                                       filter(status == 0 & gender == "weiblich") %>%
                                                       ungroup() %>%
                                                       select(-c(status,gender)))) %>%
                               mutate(gender = rep("female",4)),
                             data.frame(Freq_CTR = t(dt_sport_gender %>%
                                                       filter(status == 0 & gender == "männlich") %>%
                                                       ungroup() %>%
                                                       select(-c(status,gender)))) %>%
                               mutate(gender = rep("male",4)))

names_rows <- strsplit(gsub('\\]', '', rownames(dt_sport_ALS)), '\\[',) %>% unlist()
names_rows_gender <- strsplit(gsub('\\]', '', rownames(dt_sport_ALS_gender)), '\\[',) %>% unlist()
dt_sport_ALS$Levels <- names_rows[c(2,4,6,8)]
dt_sport_ALS$Variable <- names_rows[c(1,3,5,7)]
dt_sport_ALS <- dt_sport_ALS[,c(3,2,1)]
dt_sport_all <- cbind(dt_sport_ALS,dt_sport_CTR)
dt_sport_ALS_gender$Levels <- names_rows_gender[c(2,4,6,8)]
dt_sport_ALS_gender$Variable <- names_rows_gender[c(1,3,5,7)]
dt_sport_ALS_gender <- dt_sport_ALS_gender[,c(4,3,1,2)]
dt_sport_all_gender <- cbind(dt_sport_ALS_gender,Freq_CTR = dt_sport_CTR_gender$Freq_CTR)

dt_res <- cbind(dt_categories_ALS,dt_categories_CTR[,"Freq_CTR"])
dt_res_gender <- cbind(dt_categories_ALS_gender,Freq_CTR = dt_categories_CTR_gender[,"Freq_CTR"])
names(dt_res) <- c("Variable","Levels","Freq_ALS","Freq_CTR")
dt_res <- rbind(dt_res[-44,],dt_sport_all)
dt_res_gender <- rbind(dt_res_gender,dt_sport_all_gender)

writexl::write_xlsx(dt_res,"data code output/freq_lifestyle_categories.xlsx")

dt_res_tmp <- data.frame(Variable = dt_res$Variable %>% unique(),
           Variable_en = c("Partnership status",
                           "Highest professional degree",
                           "Highest educational degree",
                           "Regular consumption of caffeine",
                           "Regular smoke",
                           "Regular consumption of alcohol",
                           "Regular use of drugs",
                           "Physical activity - professional activity",
                           "Physical activity - sport activity",
                           "Which sport"))
dt_res <- merge(dt_res,dt_res_tmp)
dt_res_gender <- merge(dt_res_gender,dt_res_tmp)

dt_res_tmp <- data.frame(Levels = dt_res$Levels %>% unique(),
                         Levels_en = c("Low","Intensive","Moderate","NA",
                                       "Low (e.g., office job)",
                                        "Intensive (e.g., construction work)","Moderate (e.g. care work)",
                                       "Yes, currently",
                                       "Yes, in the past","No, never", "Yes, in the past",
                                       "Fachschule/Technikerschule","Kein beruflicher Abschluss",
                                       "Lehre/Facharbeiterabschluss","Meisterprüfung","Other",
                                       "Universität/Hochschule","Abitur","Berufsschule",
                                       "Fachabitur","Grundschule","Hauptschule",
                                       "Realschule","Endurance sport","Contact sport",
                                       "Strength sport","Racket sport","Single","Living in partnership"))

dt_res_tmp_gender <- data.frame(Levels = dt_res_gender$Levels %>% unique(),
                         Levels_en = c("Low","Intensive","Moderate","NA",
                                       "Intensive (e.g., construction work)","Moderate (e.g. care work)",
                                       "Low (e.g., office job)",
                                       "Yes, in the past","No, never",
                                       "Yes, currently", "Yes, in the past","Other",
                                       "Universität/Hochschule","Meisterprüfung",
                                       "Lehre/Facharbeiterabschluss",
                                       "Fachschule/Technikerschule","Kein beruflicher Abschluss",
                                       "Hauptschule","Abitur","Berufsschule",
                                       "Fachabitur","Grundschule",
                                       "Realschule","Endurance sport","Contact sport",
                                       "Strength sport","Racket sport","Single","Living in partnership"))
dt_res <- merge(dt_res,dt_res_tmp)
dt_res_gender <- merge(dt_res_gender,dt_res_tmp_gender)
dt_res$Levels_en <- factor(dt_res$Levels_en,
                           levels = c("Low","Moderate","Intensive",
                                      "Low (e.g., office job)",
                                      "Moderate (e.g. care work)",
                                      "Intensive (e.g., construction work)",
                                      "Yes, currently", "Yes, in the past",
                                      "No, never",
                                      "Universität/Hochschule","Meisterprüfung",
                                      "Fachschule/Technikerschule","Abitur",
                                      "Fachabitur",
                                      "Lehre/Facharbeiterabschluss","Gesamtschule",
                                      "Realschule","Berufsschule",
                                      "Hauptschule","Grundschule",
                                      "Other",
                                      "Kein beruflicher Abschluss","NA",
                                      "Endurance sport","Contact sport",
                                      "Strength sport","Racket sport","Living in partnership",
                                      "Single"))
dt_res_gender$Levels_en <- factor(dt_res_gender$Levels_en,
                           levels = c("Low","Moderate","Intensive",
                                      "Low (e.g., office job)",
                                      "Moderate (e.g. care work)",
                                      "Intensive (e.g., construction work)",
                                      "Yes, currently", "Yes, in the past",
                                      "No, never",
                                      "Universität/Hochschule","Meisterprüfung",
                                      "Fachschule/Technikerschule","Abitur",
                                      "Fachabitur",
                                      "Lehre/Facharbeiterabschluss","Gesamtschule",
                                      "Realschule","Berufsschule",
                                      "Hauptschule","Grundschule",
                                      "Other",
                                      "Kein beruflicher Abschluss","NA",
                                      "Endurance sport","Contact sport",
                                      "Strength sport","Racket sport","Living in partnership",
                                      "Single"))
barplot_categories(dt_res)
barplot_categories_gender(dt_res_gender)

# -> with subcategories
final_ALS_CTR_category_lifestyle_subcategories <- final_ALS_CTR_category_lifestyle %>%
  filter(!is.na(`sub-subcategory`))

subquestions_lifestyle <- final_ALS_CTR_category_lifestyle_subcategories$`sub-category` %>% unique()

data_subquestions_lifestyle <- list()
for (i in 1:length(subquestions_lifestyle)) {
  subquestion_i <- subquestions_lifestyle[i]
  final_ALS_CTR_category_subquestions_lifestyle_i <- final_ALS_CTR_category_lifestyle_subcategories %>%
    filter(`sub-category` == subquestion_i)
  dat_final_subquestions_lifestyle_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_subquestions_lifestyle_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_subquestions_lifestyle_i_temp <- apply(dat_final_subquestions_lifestyle_i[,1:ncol(dat_final_subquestions_lifestyle_i)-1], 2, function(x) ifelse(!is.na(x) & x == "Ja",1,0))
  data_subquestions_lifestyle[[i]] <- set_names(data.frame(subquestion_i = apply(dat_final_subquestions_lifestyle_i_temp, 1, function(x) {
    lifestyle_all <- sum(x)
    lifestyle_all <- ifelse(lifestyle_all > 0,1,0)})),subquestion_i)
}
data_subquestions_lifestyle <- do.call("cbind", data_subquestions_lifestyle) %>% 
  as.data.frame() %>%
  mutate(status = dat_final_subquestions_lifestyle_i[,"status2"])

boxplot_binary(data_subquestions_lifestyle,
               univariate_lifestyle_subquestion)

# -> with subsubcategories
subsubquestions_lifestyle <- final_ALS_CTR_category_lifestyle_subcategories$`sub-subcategory` %>% unique()

data_subsubquestions_lifestyle <- list()
for (i in 1:length(subsubquestions_lifestyle)) {
  subsubquestion_i <- subsubquestions_lifestyle[i]
  final_ALS_CTR_category_subsubquestions_lifestyle_i <- final_ALS_CTR_category_lifestyle_subcategories %>%
    filter(`sub-subcategory` == subsubquestion_i)
  dat_final_subsubquestions_lifestyle_i <- dat_final[,c(which(original_names %in% final_ALS_CTR_category_subsubquestions_lifestyle_i$`original question (ALS)`),ncol(dat_final))]
  dat_final_subsubquestions_lifestyle_i_temp <- apply(dat_final_subsubquestions_lifestyle_i[,1:ncol(dat_final_subsubquestions_lifestyle_i)-1], 2, function(x) ifelse(x == "Ja",1,0))
  data_subsubquestions_lifestyle[[i]] <- data.frame(subsubquestion_i = apply(dat_final_subsubquestions_lifestyle_i_temp, 1, function(x) {
    lifestyle_all <- sum(x)
    lifestyle_all <- ifelse(lifestyle_all > 0,1,0)}),subsubquestion_i)
}

data_subsubquestions_lifestyle <- do.call("cbind", data_subsubquestions_lifestyle) %>% 
  as.data.frame() 

data_subsubquestions_lifestyle$"Todesfälle eines sehr nahestenden Menschen" <- data_subsubquestions_lifestyle[,1]
data_subsubquestions_lifestyle$"Wechsel des Arbeitgebers" <- data_subsubquestions_lifestyle[,3]
data_subsubquestions_lifestyle$"Wechsel des Berufes" <- data_subsubquestions_lifestyle[,5]
data_subsubquestions_lifestyle$"Schwere Erkrankung eines sehr nahestenden Menschen (z.B. Partner(in), Angehörige(r))" <- data_subsubquestions_lifestyle[,7]
data_subsubquestions_lifestyle$"Beendigung einer Partnerschaft (Trennung / Scheidung)" <- data_subsubquestions_lifestyle[,9]
data_subsubquestions_lifestyle$"Anderer Umbruch im Leben, der Ihrer Meinung nach Ihr Leben oder Ihre Persönlichkeit wesentlich geprägt haben könnte" <- data_subsubquestions_lifestyle[,11]

data_subsubquestions_lifestyle <- data_subsubquestions_lifestyle[,c(13:18)] 
data_subsubquestions_lifestyle$status <- dat_final_subquestions_lifestyle_i[,"status2"]

boxplot_binary(data_subquestions_lifestyle,
               univariate_lifestyle_subsubquestion[1:6,])

### EXTRA FUNCTIONS

boxplot_binary <- function(data,data_univariate){
  df_sig <-data_univariate %>%
    left_join(map_questions_plot)
  cat_tmpvars = df_sig$Features 
  labels = df_sig$Labels_plot %>% unique() %>% as.character()
  cat_col <- df_sig$Variables
  plot_cat = list()
  for (i in 1:(ncol(data)-1)){
    tmpvar = ifelse((grepl("Ja.",cat_tmpvars[i],fixed = T) | cat_tmpvars[i] == "Ja"),
                    cat_col[i],
                    sub(paste0(".",cat_tmpvars[i]),"",cat_col[i],ignore.case = T))
    tmpvar = sub(paste0(".",cat_tmpvars[i]),"",cat_col[i],ignore.case = T)
    tmpdf = data.frame(Var = data[,colnames(data)[i]], Status = status) %>% na.omit()
    print(tmpdf$Var)
    tmpdf$Var = ifelse((tmpdf$Var == 1 | tmpdf$Var == "Ja"), "One", "Zero")
    #print(cat_tmpvars[i])
    #print(tmpdf)
    #tmpdf$Var = ifelse(tmpdf$Var == cat_tmpvars[i], "One", "Zero")
    #tmpdf$Status = ifelse(tmpdf$Status == "ALS", "One", "Zero")
    conf.m = as.data.frame(table(tmpdf$Var, tmpdf$Status))
    print(conf.m)
    conf.m$Freq = ifelse(conf.m$Var2 == "ALS", (conf.m$Freq/514),
                         ifelse(conf.m$Var2 == "CTR", (conf.m$Freq/306),
                         conf.m$Freq))
    name_variable = ifelse(data_univariate$Features[df_sig$Features == cat_tmpvars[i] & df_sig$Variables == cat_col[i]] != "Nein",
                           df_sig$Labels_plot[df_sig$Features == cat_tmpvars[i] & df_sig$Variables == cat_col[i]],
                           paste0(df_sig$Labels_plot[df_sig$Features == cat_tmpvars[i] & df_sig$Variables == cat_col[i]],
                                  "_",
                                  df_sig$Features[df_sig$Features == cat_tmpvars[i] & df_sig$Variables == cat_col[i]]))
    plot_cat[[i]] = ggplot(data = conf.m,
                           mapping = aes(x = Var1,
                                         y = Var2)) +
      geom_tile(aes(fill = Freq)) +
      geom_text(aes(label = sprintf("%1.2f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "purple",
                          high = "orange",
                          trans = "log") +
      theme(legend.position = "none", title = element_text(size = 10),
            panel.background = element_rect(fill = "white")) +
      ggtitle(paste(df_sig$Labels_plot[df_sig$Features == cat_tmpvars[i] & df_sig$Variables == cat_col[i]],
                    "\nLog odds =",round(as.numeric(df_sig$eff[df_sig$Features == cat_tmpvars[i] & df_sig$Variables == cat_col[i]]), 1))) +
                   # "\nadjusted pval =",signif(df_sig$fdr[df_sig$Features == cat_tmpvars[i] & df_sig$Variables == cat_col[i]], digits=3))) +
      xlab(name_variable) +
      ylab("ALS status")
    name_variable <- ifelse(grepl("Constipation",name_variable),"Constipation",name_variable)
    name_variable <- ifelse(grepl("sweating/trembling",name_variable),"sweating or trembling",name_variable)
    name_variable<- ifelse(grepl("arms/legs",name_variable),"arms or legs",name_variable)
    pdf(file = paste0("plots/boxplots/boxplot_",name_variable,".pdf"))
    print(plot_cat[[i]])
    dev.off()
  }
}


boxplot_continuous <- function(data){
  colnames(data) <-
  c(pull(map_questions_plot[na.omit(match(colnames(data),map_questions_plot$Variables)),"Labels_plot"]),"status")
  for (i in 1:(ncol(data)-1)){
    print(i)
    tmp = data.frame(Status = status, 
                     Value = as.numeric(data[,i]))
    plot_con[[i]] = ggplot(tmp, aes(x = Status, y = Value, fill = Status)) + 
      geom_boxplot(outlier.shape = NA) + 
      geom_jitter(position = position_jitterdodge()) +
      theme_bw() +
      scale_fill_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
      theme(legend.position = "none", title = element_text(size = 10),
            axis.title.x = element_blank()) +
      ggtitle(colnames(data)[i]) +
      stat_compare_means(label.x = 1)
    pdf(file = paste0("plots/boxplots/boxplot_",gsub("/", "", colnames(data)[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
}
boxplot_continuous_gender <- function(data,gender){
  # colnames(data) <-
  #   c(pull(map_questions_plot[na.omit(match(colnames(data),map_questions_plot$Variables)),"Labels_plot"]),"status")
  print(data)
  data$status <- ifelse(data$status==1,"ALS","CTR")
  data$status <- factor(data$status,levels = c("ALS","CTR"))
  print(data)
  for (i in 1:(ncol(data)-1)){
    print(i)
    tmp = data.frame(Status = data$status, 
                     Value = as.numeric(data[,i]))
    plot_con[[i]] = ggplot(tmp, aes(x = Status, y = Value, fill = Status)) + 
      geom_boxplot(outlier.shape = NA) + 
      geom_jitter(position = position_jitterdodge()) +
      theme_bw() +
      scale_fill_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
      theme(legend.position = "none", title = element_text(size = 10),
            axis.title.x = element_blank()) +
      ggtitle(colnames(data)[i]) +
      stat_compare_means(label.x = 1)
    pdf(file = paste0("plots/boxplots/boxplot_",gsub("/", "", colnames(data)[i]),"_",gender,".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
}


barplot_continuous <- function(data){
  colnames(data) <-
    c(pull(map_questions_plot[na.omit(match(colnames(data),map_questions_plot$Variables)),"Labels_plot"]),"status")
  print(colnames(data))
  for (i in 1:(ncol(data)-1)){
    print(i)
    tmp = data.frame(Status = status, 
                     Value = as.numeric(data[,i]))
    tmp <- tmp %>%
      group_by(Value, Status) %>%
      summarise(counts = n())
    tmp$counts <- ifelse(tmp$Status == "ALS",
                         round((tmp$counts)*100/514, digits = 1),
                         ifelse(tmp$Status == "CTR",
                                round((tmp$counts)*100/306,digits = 1),
                                tmp$counts))
    
    tmp$Value <- as.factor(tmp$Value)
    plot_con[[i]] = ggplot(tmp, aes(x = Value, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
        position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#0073C2FF","CTR"="#EFC000FF"))+
      scale_fill_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
      geom_text(aes(label = counts, group = Status), 
        position = position_dodge(0.8),
        vjust = -0.3, size = 3.5) + 
      labs(x = "\n Number of answers", y = "Frequency % \n", title = colnames(data)[i]) +
      theme_minimal() + 
      theme(panel.grid.minor.x=element_blank(), 
            panel.grid.major.x=element_blank(), 
            plot.title = element_text(hjust = 0.5)) 
    # Use position = position_dodge() 
    pdf(file = paste0("plots/boxplots/boxplot_",gsub("/", "", colnames(data)[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
}

barplot_categories <- function(data){
  categories <- data$Variable_en %>% unique()
  for (i in 1:(length(categories))){
    print(i)
    tmp = data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en,Levels_en,Freq_ALS,Freq_CTR)
    tmp = melt(tmp, id.vars = c("Variable_en","Levels_en"))
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    tmp$counts = ifelse(tmp$Status == "ALS",
                        round((tmp$value*100)/514, digits = 1),
                        round((tmp$value*100)/306,digits = 1))
    print(tmp)
    tmp$Levels <- factor(tmp$Levels_en,levels = c("10 years", "5 years","1 year","1 month"))
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+
      scale_fill_manual(values=c("ALS"="#5f91bd","CTR"="#BD8B5F")) +
      geom_text(aes(label = counts, group = Status), 
                position = position_dodge(0.8),
                vjust = -0.3, size = 3.5) + 
      labs(x = " ", y = "Frequency of Patients (%) \n", title = categories[i]) +
      theme_minimal() + 
      theme(panel.grid.minor.x=element_blank(), 
            panel.grid.major.x=element_blank(), 
            plot.title = element_text(hjust = 0.5),
            axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8))
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
    # Use position = position_dodge() 
    pdf(file = paste0("plots/boxplots/boxplot_",gsub("/", "", categories[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
  return(plot_con)
}


barplot_categories_gender <- function(data){
  categories <- data$Variable_en %>% unique()
  for (i in 1:(length(categories))){
    print(i)
    tmp = data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en,Levels_en,Freq_ALS,Freq_CTR,gender)
    print(tmp)
    tmp = melt(tmp, id.vars = c("Variable_en","Levels_en","gender"))
    print(tmp)
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    tmp$counts = ifelse(tmp$Status == "ALS" & tmp$gender == "female",
                        round((tmp$value*100)/200, digits = 1),
                        ifelse(tmp$Status == "ALS" & tmp$gender == "male",
                               round((tmp$value*100)/305, digits = 1),
                               ifelse(tmp$Status == "CTR" & tmp$gender == "female",
                                      round((tmp$value*100)/178,digits = 1),
                                      round((tmp$value*100)/121,digits = 1))))
    tmp$Sex <- tmp$gender
    print(tmp)
    tmp$Levels <- as.factor(tmp$Levels_en)
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#0073C2FF","CTR"="#EFC000FF"))+
      scale_fill_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
      geom_text(aes(label = counts, group = Status), 
                position = position_dodge(0.8),
                vjust = -0.3, size = 3.5) + 
      facet_grid(. ~ Sex) +
      labs(x = "\n Categories", y = "Frequency % \n", title = categories[i]) +
      theme_minimal() + 
      theme(panel.grid.minor.x=element_blank(), 
            panel.grid.major.x=element_blank(), 
            plot.title = element_text(hjust = 0.5),
            axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8))
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
    # Use position = position_dodge() 
    pdf(file = paste0("plots/boxplots/boxplot_gender_",gsub("/", "", categories[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
}

barplot_categories_new <- function(data){
  categories <- data$Variable_en %>% unique()
  for (i in 1:(length(categories))){
    print(i)
    tmp = data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en,Levels_en,Freq_ALS,Freq_CTR)
    tmp = melt(tmp, id.vars = c("Variable_en","Levels_en"))
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    tmp$counts = tmp$value
    print(tmp)
    tmp$Levels <- as.factor(tmp$Levels_en)
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#0073C2FF","CTR"="#EFC000FF"))+
      scale_fill_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) +
      geom_text(aes(label = counts, group = Status), 
                position = position_dodge(0.8),
                vjust = -0.3, size = 3.5) + 
      labs(x = "\n Categories", y = "Frequency % \n", title = categories[i]) +
      theme_minimal() + 
      theme(panel.grid.minor.x=element_blank(), 
            panel.grid.major.x=element_blank(), 
            plot.title = element_text(hjust = 0.5),
            axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8))
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
    # Use position = position_dodge() 
    pdf(file = paste0("plots/boxplots/boxplot_",gsub("/", "", categories[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
}


frequency_status <- function(data,status_bol,name_freq){
  dt <- data %>%
    filter(status == status_bol)
  dt_res = data.frame()
  for (i in 1:ncol(dt)){
    dt_temp = data.frame(t(table(dt[,i])))
    dt_temp$Var1 = names(dt)[i]
    dt_res = rbind(dt_res, dt_temp)
  }
  names(dt_res) = c("Variable","Levels",name_freq)
  return(dt_res)
}


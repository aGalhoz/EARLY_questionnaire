## Boxplots of univariate categories

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
#          Freq = Freq_ALS/513) %>%
#   select(-Freq_ALS)
# dt_nonmotor_CTR_tmp <- dt_nonmotor_CTR %>%
#   mutate(group = "CTR",
#          Freq = Freq_CTR/305) %>%
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
dt_nonmotor_all_tmp$Variable_en <- rep("Cold, pale extremities",4)
dt_nonmotor_all_tmp$Variable_en <- rep("Trembling of arms or legs",4)
dt_nonmotor_all_tmp$Variable_en <- rep("Excessive saliva",4)
dt_nonmotor_all_tmp$Variable_en <- rep("Frequent falls backwards",3)

dt_nonmotor_all <- rbind(dt_nonmotor_all,dt_nonmotor_all_tmp)

dt_nonmotor_all$Levels_en <- ifelse(dt_nonmotor_all$Levels_en == "X..1Monat",
                                    "<1 month",
                                    ifelse(dt_nonmotor_all$Levels_en == "X1.12Monate",
                                           "1-12 months",
                                           ifelse(dt_nonmotor_all$Levels_en == "X1.5Jahre",
                                                           "1-5 years",
                                                  ifelse(dt_nonmotor_all$Levels_en == "X5.10Jahre",
                                                                            "5-10 years",dt_nonmotor_all$Levels_en))))
                              

dt_nonmotor_all$Levels_en <- factor(dt_nonmotor_all$Levels_en,
                                    levels = c("5-10 years","1-5 years","1-12 months","<1 month"))


plots <- barplot_categories(dt_nonmotor_all)

pdf("plots/barplots_specific_prodromal.pdf",width = 18,height = 6)
plot_grid(plots[[1]],plots[[2]],plots[[3]],ncol = 3, nrow=1, width = 10, height = 6, labels = c("A","B","C")) 
dev.off()



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

dat_final[,c(which(original_names %in% "Welche Krankheits des Muskel-Skelett-Systems liegt bzw. lag vor und seit wann? [Bandscheibenvorfall]"),ncol(dat_final))]
View(data_patients_control_combined[,c(150,483)])
slipped_disc_ALS = data_patients_control_combined[,c(150,483)] %>%
  filter(status == 1)
slipped_disc_ALS = data.frame(slipped_disc = slipped_disc_ALS[,1],
                         onset = ALS_open_answers_nonmotor$date)
slipped_disc_ALS = slipped_disc_ALS[!is.na(slipped_disc_ALS$slipped_disc),]
slipped_disc_ALS$year_prioronset = c(14,7,2,6,4,NA,23,0.5,1,57,28,37,1,27,11,12,NA,17,NA,15,11,
                                     20,8,NA,19,NA,22,-3,3,4,18,-1,NA,15,8,20,NA,NA,26,16,16,NA,28,NA)
slipped_disc_ALS$time_prioronset = ifelse(slipped_disc_ALS$year_prioronset < 0,
                                          NA,
                                          ifelse(slipped_disc_ALS$year_prioronset < 1,
                                          "1-12 months",
                                          ifelse(slipped_disc_ALS$year_prioronset < 5,
                                                 "1-5 years",
                                                 ifelse(slipped_disc_ALS$year_prioronset > 5,
                                                        "5-10 years",NA))))
median(na.omit(slipped_disc_ALS$year_prioronset))
library(EnvStats)
IQR(na.omit(slipped_disc_ALS$year_prioronset))
summaryStats(na.omit(slipped_disc_ALS$year_prioronset),quartiles = TRUE)
slipped_disc_CTR = data_patients_control_combined[,c(150,483)] %>%
  filter(status == 0)
slipped_disc_CTR = data.frame(slipped_disc = slipped_disc_CTR[,1],
                              onset = as.numeric(str_extract(data_control_common_final$`Datum Abgeschickt`, "\\d{4}")))
slipped_disc_CTR = slipped_disc_CTR[!is.na(slipped_disc_CTR$slipped_disc),]
slipped_disc_CTR$year_prioronset = c(41,3,3,4,20,14,2,4,10,4,2,15,4,15,1)
slipped_disc_CTR$time_prioronset = ifelse(slipped_disc_CTR$year_prioronset < 1,
                                          "1-12 months",
                                          ifelse(slipped_disc_CTR$year_prioronset < 5,
                                                 "1-5 years",
                                                 ifelse(slipped_disc_CTR$year_prioronset > 5,
                                                        "5-10 years",NA)))
table(slipped_disc_ALS$time_prioronset)
table(slipped_disc_CTR$time_prioronset)
slipped_disc_ALS_CTR = data.frame(Levels_en = c("1-12 months","1-5 years","5-10 years"),
                                  Freq_ALS = c(1,6,25),
                                  Freq_CTR = c(0,9,6),
                                  Variable_en = rep("Slipped disc",3))

slipped_disc_ALS_CTR$Levels_en <- factor(slipped_disc_ALS_CTR$Levels_en,
                                    levels = c("5-10 years","1-5 years","1-12 months"))


plot <- barplot_categories(slipped_disc_ALS_CTR)

pdf("plots/barplot_slipped_disc.pdf",width = 9)
print(plot[1])
dev.off()

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
dat_final_subquestions_dietweight_continuous[321,"Bitte geben Sie Ihre Körpergröße an [cm]."] <- "NA"
dat_final_subquestions_dietweight_continuous[113,"Bitte geben Sie Ihre Körpergröße an [cm]."] <- "NA"
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
dt_res$Levels_en_old <- dt_res$Levels_en
dt_res$Levels_en <- ifelse(dt_res$Levels_en == "Abitur","General qualification for university entrance",
                           ifelse(dt_res$Levels_en == "Fachabitur","Subject-specific university entrance qualification",
                                  ifelse(dt_res$Levels_en == "Gesamtschule","Comprehensive school",
                                         ifelse(dt_res$Levels_en == "Berufsschule","Vocational school",
                                                ifelse(dt_res$Levels_en == "Realschule","Secondary school",
                                                       ifelse(dt_res$Levels_en == "Hauptschule","Lower secondary school",
                                                              ifelse(dt_res$Levels_en == "Grundschule","Primary school",
                                                                     ifelse(dt_res$Levels_en == "Universität/Hochschule","University degree (Bachelor's, Master's, or equivalent degree)",
                                                                            ifelse(dt_res$Levels_en == "Fachschule/Technikerschule","Vocational/tecnhnical school or business academy",
                                                                                   ifelse(dt_res$Levels_en == "Meisterprüfung","Master craftsman examination",
                                                                                          ifelse(dt_res$Levels_en == "Lehre/Facharbeiterabschluss","Apprenticeship/skilled worker qualification",
                                                                                                 ifelse(dt_res$Levels_en == "Kein beruflicher Abschluss","No professional qualification",
                                                                                                        ifelse(dt_res$Levels_en == "Low (e.g., office job)","Low",
                                                                                                               ifelse(dt_res$Levels_en == "Yes, in the past","In the past",
                                                                                                                      ifelse(dt_res$Levels_en == "Yes, currently","Currently",
                                                                                                                             ifelse(dt_res$Levels_en == "No, never","Never",
                                                                                                                                    ifelse(dt_res$Levels_en == "Intensive (e.g., construction work)","Intensive",
                                                                                                                                           ifelse(dt_res$Levels_en == "Moderate (e.g. care work)","Moderate",
                                                                                                                                                  ifelse(dt_res$Levels_en == "Low","Low",
                                                                                                                                                         ifelse(dt_res$Levels_en == "Intensive","Intensive",
                                                                                                                                                                ifelse(dt_res$Levels_en == "Moderate","Moderate",
                                                                                                                                                                       ifelse(dt_res$Levels_en == "Other","Other",
                                                                                                                                                                              ifelse(dt_res$Levels_en == "Endurance sport","Endurance sport",
                                                                                                                                                                                     ifelse(dt_res$Levels_en =="Strength sport","Strength sport",
                                                                                                                                                                                            ifelse(dt_res$Levels_en == "Racket sport","Racket sport",
                                                                                                                                                                                                   ifelse(dt_res$Levels_en == "Contact sport","Contact sport",
                                                                                                                                                                                                          ifelse(dt_res$Levels_en == "Living in partnership","Living in partnership",
                                                                                                                                                                                                                 ifelse(dt_res$Levels_en == "Single","Single",
                                                                                                                                                                                                                        "NA"))))))))))))))))))))))))))))





dt_res_gender$Levels_en_old <- dt_res_gender$Levels_en 
dt_res_gender$Levels_en <- ifelse(dt_res_gender$Levels_en == "Abitur","General qualification \nfor university entrance",
                                  ifelse(dt_res_gender$Levels_en == "Fachabitur","Subject-specific university \nentrance qualification",
                                         ifelse(dt_res_gender$Levels_en == "Gesamtschule","Comprehensive school",
                                                ifelse(dt_res_gender$Levels_en == "Berufsschule","Vocational school",
                                                       ifelse(dt_res_gender$Levels_en == "Realschule","Secondary school",
                                                              ifelse(dt_res_gender$Levels_en == "Hauptschule","Lower secondary school",
                                                                     ifelse(dt_res_gender$Levels_en == "Grundschule","Primary school",
                                                                            ifelse(dt_res_gender$Levels_en == "Universität/Hochschule","University degree (Bachelor's, \nMaster's, or equivalent degree)",
                                                                                   ifelse(dt_res_gender$Levels_en == "Fachschule/Technikerschule","Vocational/tecnhnical school \nor business academy",
                                                                                          ifelse(dt_res_gender$Levels_en == "Meisterprüfung","Master craftsman examination",
                                                                                                 ifelse(dt_res_gender$Levels_en == "Lehre/Facharbeiterabschluss","Apprenticeship/skilled \nworker qualification",
                                                                                                        ifelse(dt_res_gender$Levels_en == "Kein beruflicher Abschluss","No professional qualification",
                                                                                                               ifelse(dt_res_gender$Levels_en == "Low (e.g., office job)","Low",
                                                                                                                      ifelse(dt_res_gender$Levels_en == "Yes, in the past","In the past",
                                                                                                                             ifelse(dt_res_gender$Levels_en == "Yes, currently","Currently",
                                                                                                                                    ifelse(dt_res_gender$Levels_en == "No, never","Never",
                                                                                                                                           ifelse(dt_res_gender$Levels_en == "Intensive (e.g., construction work)","Intensive",
                                                                                                                                                  ifelse(dt_res_gender$Levels_en == "Moderate (e.g. care work)","Moderate",
                                                                                                                                                         ifelse(dt_res_gender$Levels_en == "Low","Low",
                                                                                                                                                                ifelse(dt_res_gender$Levels_en == "Intensive","Intensive",
                                                                                                                                                                       ifelse(dt_res_gender$Levels_en == "Moderate","Moderate",
                                                                                                                                                                              ifelse(dt_res_gender$Levels_en == "Other","Other",
                                                                                                                                                                                     ifelse(dt_res_gender$Levels_en == "Endurance sport","Endurance sport",
                                                                                                                                                                                            ifelse(dt_res_gender$Levels_en =="Strength sport","Strength sport",
                                                                                                                                                                                                   ifelse(dt_res_gender$Levels_en == "Racket sport","Racket sport",
                                                                                                                                                                                                          ifelse(dt_res_gender$Levels_en == "Contact sport","Contact sport",
                                                                                                                                                                                                          ifelse(dt_res_gender$Levels_en == "Living in partnership","Living in partnership",
                                                                                                                                                                                                                 ifelse(dt_res_gender$Levels_en == "Single","Single",
                                                                                                                                                                                                                        "n.a"))))))))))))))))))))))))))))




dt_res$Levels_en <- factor(dt_res$Levels_en,levels = c("Low","Moderate","Intensive",
                                                                     "Currently", "In the past", "Never",
                                                       "General qualification for university entrance",
                                                       "Subject-specific university entrance qualification",
                                                       "Comprehensive school","Vocational school",
                                                       "Secondary school","Lower secondary school","Primary school",
                                                                     "University degree (Bachelor's, Master's, or equivalent degree)",
                                                                     "Vocational/tecnhnical school or business academy",
                                                                     "Master craftsman examination",
                                                                     "Apprenticeship/skilled worker qualification",
                                                                     "No professional qualification", "Other",
                                                                     "NA", "Endurance sport","Contact sport",
                                                                     "Strength sport","Racket sport","Living in partnership",
                                                                     "Single"))
dt_res_gender$Levels_en <- factor(dt_res_gender$Levels_en,levels = c("Low","Moderate","Intensive",
                                                                     "Currently", "In the past", "Never",
                                                                     "General qualification \nfor university entrance",
                                                                     "Subject-specific university \nentrance qualification",
                                                                     "Comprehensive school","Vocational school",
                                                                     "Secondary school","Lower secondary school","Primary school",
                                                                     "University degree (Bachelor's, \nMaster's, or equivalent degree)",
                                                                     "Vocational/tecnhnical school \nor business academy",
                                                                     "Master craftsman examination",
                                                                     "Apprenticeship/skilled \nworker qualification",
                                                                     "No professional qualification", "Other",
                                                                     "n.a", "Endurance sport","Contact sport",
                                                                     "Strength sport","Racket sport","Living in partnership",
                                                                     "Single"))
barplot_categories_new(dt_res %>% filter(Levels_en != "NA"))
plots_categories_gender = barplot_categories_gender(dt_res_gender)

###########################################
# final plots for supplementary figures 
# -> educational and professional degrees
pdf("plots/barplots_educational_professional_degree.pdf",height = 14,width = 9)
plot_grid(plots_categories_gender[[1]],plots_categories_gender[[4]],
          ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()
# -> physical activity in professional and sport activities
pdf("plots/barplots_professional_sport_activities.pdf",height = 12,width = 8)
plot_grid(plots_categories_gender[[6]],plots_categories_gender[[5]],
          ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()
# -> substance consumption
pdf("plots/barplots_substance_use.pdf",height = 12,width = 12)
plot_grid(plots_categories_gender[[9]],plots_categories_gender[[7]],
          plots_categories_gender[[8]],
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()

dt_res_gender_tmp = dt_res_gender
dt_res_gender_tmp$pvalue = apply(dt_res_gender, 1, function(x){
  print(x)
  print(as.numeric(x["Freq_ALS"]))
  ifelse(x["gender"] == "female",prop.test(c(as.numeric(x["Freq_ALS"]),as.numeric(x["Freq_CTR"])),
                                        c(200,178))$p.value,
         prop.test(c(as.numeric(x["Freq_ALS"]),as.numeric(x["Freq_CTR"])),
                   c(305,121))$p.value)
})

temp <- prop.test(c(36,66),c(200,178),cor=F)
nr_variables = dt_res_gender$Variable_en %>% unique()
a = list()
b = list()
for (i in 1:length(nr_variables)) {
  variable_i = nr_variables[i]
  dt_i = dt_res_gender %>%
    filter(Variable_en == variable_i)
  dt_i_female = dt_i %>%
    filter(gender == "female")
  dt_i_female_ALS <- dt_i_female %>%
    select(Freq_ALS)
  dt_i_female_CTR <- dt_i_female %>%
    select(Freq_CTR)
  dt_i_female_all <- data.frame(Freq = c(dt_i_female_ALS$Freq_ALS,dt_i_female_CTR$Freq_CTR))
  dt_i_female_all$type = c(rep("ALS",nrow(dt_i_female_ALS)),
                               (rep("CTR",nrow(dt_i_female_CTR))))
  dt_i_male = dt_i %>%
    filter(gender == "male")
  dt_i_male_ALS = dt_i %>%
    select(Freq_ALS)
  dt_i_male_CTR <- dt_i_male %>%
    select(Freq_CTR)
  dt_i_male_all <- data.frame(Freq = c(dt_i_male_ALS$Freq_ALS,dt_i_male_CTR$Freq_CTR))
  dt_i_male_all$type = c(rep("ALS",nrow(dt_i_male_ALS)),
                        (rep("CTR",nrow(dt_i_male_CTR))))
  a[[i]] = fisher.test(dt_i_female_all[,1],dt_i_female_all[,2])
  b[[i]] = fisher.test(dt_i_male_all[,1],dt_i_male_all[,2])
}

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
    conf.m$Freq = ifelse(conf.m$Var2 == "ALS", (conf.m$Freq/513),
                         ifelse(conf.m$Var2 == "CTR", (conf.m$Freq/305),
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
                         round((tmp$counts)*100/513, digits = 1),
                         ifelse(tmp$Status == "CTR",
                                round((tmp$counts)*100/305,digits = 1),
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
    tmp = reshape2::melt(tmp, id.vars = c("Variable_en","Levels_en"))
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    tmp$counts = ifelse(tmp$Status == "ALS",
                        round((tmp$value*100)/513, digits = 1),
                        round((tmp$value*100)/305,digits = 1))
    print(tmp)
    tmp$Levels =  tmp$Levels_en
    #tmp$Levels <- factor(tmp$Levels_en,levels = c("5-10 years", "1–5 years", "1–12 months", "<1 month"))
    #tmp$Levels <- factor(tmp$Levels_en,levels = c("5-10 years", "1–5 years", "1–12 months"))
    print(tmp)
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(
        name = "Group",   
        breaks = c("ALS", "CTR"),
        labels = c("ALS", "Controls"),
        values = c("ALS" = "#5f91bd", "CTR" = "#BD8B5F")
      ) + 
      scale_fill_manual(  name = "Group",   
                          breaks = c("ALS", "CTR"),
                          labels = c("ALS", "Controls"),
                          values = c("ALS" = "#5f91bd", "CTR" = "#BD8B5F")) +
      geom_text(aes(label = counts, group = Status), 
                position = position_dodge(0.8),
                vjust = -0.3, size = 6) + 
      labs(x = " ", y = "Frequency of Participants (%) \n", title = categories[i]) +
      theme_minimal(base_size = 17) + 
      guides(fill=guide_legend(title="Group"),color = guide_legend(title="Group")) + 
      theme(panel.grid.minor.x=element_blank(), 
            panel.grid.major.x=element_blank(), 
            axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
            plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 17),
            legend.title = element_text(size = 17),
            legend.text = element_text(size = 17))
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
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","Controls")
    tmp$counts = ifelse(tmp$Status == "ALS" & tmp$gender == "female",
                        round((tmp$value*100)/200, digits = 1),
                        ifelse(tmp$Status == "ALS" & tmp$gender == "male",
                               round((tmp$value*100)/305, digits = 1),
                               ifelse(tmp$Status == "Controls" & tmp$gender == "female",
                                      round((tmp$value*100)/178,digits = 1),
                                      round((tmp$value*100)/121,digits = 1))))
    tmp$Sex <- tmp$gender
    print(tmp)
    #tmp$Levels <- factor(tmp$Levels_en, levels = rev(tmp$Levels_en %>% unique()))
    tmp$Levels <- as.factor(tmp$Levels_en)
    print(tmp)
    title_i <- ifelse(categories[i] == "Regular consumption of caffeine","Coffee consumption ever",
                      ifelse(categories[i] == "Regular consumption of alcohol","Alcohol consumption ever",
                             ifelse(categories[i] == "Regular smoke","Cigarette consumption ever",
                                    ifelse(categories[i] == "Physical activity - professional activity","Physical activity in professional activities",
                                           ifelse(categories[i] == "Physical activity - sport activity","Physical activity in sport activities",
                                                  ifelse(categories[i] == "Highest educational degree","Educational degree",
                                                         ifelse(categories[i] == "Highest professional degree","Professional degree",
                                                                categories[i])))))))
    tmp$Sex = ifelse(tmp$Sex == "female","Female","Male")
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#5f91bd","Controls"="#BD8B5F"))+
      scale_fill_manual(values=c("ALS"="#5f91bd","Controls"="#BD8B5F")) +
      # geom_text(aes(label = paste0(counts," %"), group = Status), 
      #           position = position_dodge(0.8),
      #           vjust = -0.5, size = 3) + 
      geom_text(aes(label = counts, group = Status), 
                position = position_dodge(0.8),
                vjust = -0.5, size = 4.5) + 
      facet_wrap(. ~ Sex,ncol = 1) +
                 #scales = "free_x",space = "free_x",
      labs(x = " ", y = "Frequency (%) \n", title = title_i) +
      guides(fill=guide_legend(title="Group"),color = guide_legend(title="Group")) + 
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
        axis.title = element_text(size = 12.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        strip.background  = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.border = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x  = element_blank(),
        axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
        strip.text = element_text(size = 14)) + 
      ylim(0,(max(tmp$counts) + 15)) 
      #coord_flip()
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
    # Use position = position_dodge() 
    pdf(file = paste0("plots/boxplots/boxplot_gender_",gsub("/", "", categories[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
  return(plot_con)
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
    tmp$counts =  ifelse(tmp$Status == "ALS",round((tmp$value*100)/513, digits = 1),
                         ifelse(tmp$Status == "CTR",round((tmp$value*100)/305,digits = 1),
                                                          tmp$value))
    print(tmp)
    tmp$Levels <- as.factor(tmp$Levels_en)
    title_i <- ifelse(categories[i] == "Regular consumption of caffeine","Coffee consumption",
                      ifelse(categories[i] == "Regular consumption of alcohol","Alcohol consumption",
                             ifelse(categories[i] == "Regular smoke","Cigarette smoking",
                                    ifelse(categories[i] == "Physical activity - professional activity","Level of physical activity in professional work",
                                           ifelse(categories[i] == "Physical activity - sport activity","Level of physical activity in sport activities",
                                                  categories[i])))))
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+
      scale_fill_manual(values=c("ALS"="#5f91bd","CTR"="#BD8B5F")) +
      geom_text(aes(label = paste0(counts," %"), group = Status), 
                position = position_dodge(0.8),
                #vjust = -0.5, size = 3)+
               hjust = -0.15, size = 3) + 
      labs(x = "\n Categories", y = "Frequency (%) \n", title = title_i) +
      theme_minimal() + 
      theme( strip.background  = element_blank(),
             panel.grid.major = element_line(colour = "lightgrey"),
             panel.border = element_blank(),
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x  = element_blank(),
             plot.title = element_text(hjust = 0.5)) +
          #  axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) + 
      ylim(0,(max(tmp$counts) + 15)) +
   coord_flip()
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


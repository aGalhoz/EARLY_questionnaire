## Timeline plots of wards visits

## timeplot analysis of relevant Questions
wards_interest <- c("Neurologie", "Logopädie","Endokrinologie")
wards_interest_EN <- c("Neurology", "Speech Therapy", "Endrocrinology")

plot_freq_visit = plot_patients = plot_nrpatients = list()

for (i in 1:length(wards_interest)) {
  ward_interest_i <- wards_interest[i]
  ward_interest_EN_i <- wards_interest_EN[i]
  # get questions for the specific ward
  original_questions_i <- final_ALS_CTR_category_healthcare %>%
    filter(`sub-category` == ward_interest_i) %>%
    pull(`original question (ALS)`)
  # data with questions for specific ward and status info
  dat_healthcare_i <- data_patients_control_combined_all %>%
    select(c(original_questions_i,status))
  dat_healthcare_i[,1:4] <- apply(dat_healthcare_i[,1:4], 2, as.numeric)
  # total amount of visits across time, stratified by cohort (ALS/CTR)
  dat_healthcare_visits_i <- dat_healthcare_i %>%
    dplyr::group_by(status) %>%
    dplyr::summarise(across(everything(), ~ sum(., na.rm = TRUE))) 
  dat_healthcare_visits_i_tmp = apply(dat_healthcare_visits_i[,2:5],1,function(x) {
    round(x * 100 / sum(x),2)})
  time_info <- gsub('(?:.*?\\[[^][]*\\].*?\\[([^][]*)\\].*|.*)', '\\1', colnames(dat_healthcare_visits_i)[2:5], perl=T)
  time <- ifelse(time_info == "5 – 10 Jahre zuvor","10 years",
                 ifelse(time_info == "1 – 5 Jahre zuvor","5 years",
                        ifelse(time_info == "1 – 12 Monate zuvor", "1 year",
                               ifelse(time_info == "&lt; 1 Monat zuvor","1 month",time_info))))
  dat_healthcare_visits_i <- data.frame(CTR = dat_healthcare_visits_i_tmp[,1],
                                        ALS = dat_healthcare_visits_i_tmp[,2],
                                        time = time) 
  dat_healthcare_visits_i <- melt(dat_healthcare_visits_i, id = "time")
  # average amount of visits per patient across time, stratified by cohort (ALS/CTR)
  dat_healthcare_avg_visits_i <- dat_healthcare_i %>%
    dplyr::group_by(status) %>%
    summarise_each(funs(mean(.[. != 0], na.rm = TRUE)))
  dat_healthcare_avg_visits_i <- data.frame(CTR = sapply(t(dat_healthcare_avg_visits_i[,2:5])[,1],function(x) round(x,2)),
                                            ALS = sapply(t(dat_healthcare_avg_visits_i[,2:5])[,2],function(x) round(x,2)),
                                            time = time)
  dat_healthcare_avg_visits_i <- melt(dat_healthcare_avg_visits_i, id = "time")
  # total amount of patients across time, stratified by cohort (ALS/CTR)
  dat_healthcare_binary_i <- data.frame(cbind(apply(dat_healthcare_i[,1:4], 2, 
                                     function(x) ifelse(x>0,1,0)),status = dat_healthcare_i$status))
  dat_healthcare_patients_i_tmp <- dat_healthcare_binary_i %>%
    dplyr::group_by(status) %>%
    dplyr::summarise(across(everything(), ~ sum(., na.rm = TRUE)))
  nr_CTR <- sum(apply(dat_healthcare_binary_i %>% filter(status == 0) %>% select(-status), 1, function(row){ifelse(any(row) == 1,1,0)}),na.rm = T)
  nr_ALS <- sum(apply(dat_healthcare_binary_i %>% filter(status == 1) %>% select(-status), 1, function(row){ifelse(any(row) == 1,1,0)}),na.rm = T)
  # dat_healthcare_patients_i <- data.frame(CTR = round(t(dat_healthcare_patients_i_tmp[,2:5])[,1]/length(which(dat_healthcare_i$status==0))*100,digits = 2),
  #                                         ALS = round(t(dat_healthcare_patients_i_tmp[,2:5])[,2]/length(which(dat_healthcare_i$status==1))*100,digits = 2),
  #                                         time = time)
  dat_healthcare_patients_i <- data.frame(CTR = round(t(dat_healthcare_patients_i_tmp[,2:5])[,1]/nr_CTR*100,digits = 2),
                                          ALS = round(t(dat_healthcare_patients_i_tmp[,2:5])[,2]/nr_ALS*100,digits = 2),
                                          time = time)
  dat_healthcare_patients_i <- melt(dat_healthcare_patients_i, id = "time")
  dat_healthcare_nrpatients_i <- data.frame(CTR = t(dat_healthcare_patients_i_tmp[,2:5])[,1],
                                          ALS = t(dat_healthcare_patients_i_tmp[,2:5])[,2],
                                          time = time)
  dat_healthcare_nrpatients_i <- melt(dat_healthcare_nrpatients_i, id = "time")
  dat_healthcare_visits_i$time = dat_healthcare_avg_visits_i$time = dat_healthcare_patients_i$time = dat_healthcare_nrpatients_i$time <- factor(dat_healthcare_patients_i$time,levels = c("10 years",
                                                                                                                                                       "5 years",
                                                                                                                                                       "1 year",
                                                                                                                                                       "1 month"))
  dat_healthcare_visits_i$time_numeric <- ifelse(dat_healthcare_visits_i$time == "10 years",10,
                                                 ifelse(dat_healthcare_visits_i$time == "5 years",5,
                                                        ifelse(dat_healthcare_visits_i$time == "1 year",1,
                                                               0)))
  dat_healthcare_avg_visits_i$time_numeric <- ifelse(dat_healthcare_avg_visits_i$time == "10 years",10,
                                                 ifelse(dat_healthcare_avg_visits_i$time == "5 years",5,
                                                        ifelse(dat_healthcare_avg_visits_i$time == "1 year",1,
                                                               0)))
  dat_healthcare_patients_i$time_numeric <- ifelse(dat_healthcare_patients_i$time == "10 years",10,
                                                     ifelse(dat_healthcare_patients_i$time == "5 years",5,
                                                            ifelse(dat_healthcare_patients_i$time == "1 year",1,
                                                                   0)))
  plot_freq_visit[[i]] <- ggplot(dat_healthcare_visits_i,aes(x = time, y = value, group = variable,color = variable)) +
    geom_line()+
    geom_point() +
    scale_color_manual(breaks = c("ALS","CTR"),values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
    labs(x = "", y = "Frequency of visits \n(%) \n", title = ward_interest_EN_i, color = "Status") +
    # scale_x_continuous(breaks=c(10,5,1,0),
    #                    labels = c("10 years", "5 years", "1 year", "1 month")) +
    theme_minimal() + 
    theme( plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
  plot_avg_visit[[i]] <- ggplot(dat_healthcare_avg_visits_i,aes(x = time, y = value, group = variable,color = variable)) +
    geom_line()+
    geom_point() +
    scale_color_manual(breaks = c("ALS","CTR"),values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
    labs(x = "", y = "Average visits per patient \n", title = ward_interest_EN_i, color = "Status") +
    # scale_x_continuous(breaks=c(10,5,1,0),
    #                    labels = c("10 years", "5 years", "1 year", "1 month")) +
    theme_minimal() + 
    theme( plot.title = element_text(hjust = 0.5),
           axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
  plot_patients[[i]] <- ggplot(dat_healthcare_patients_i,aes(x = time, y = value, group = variable,color = variable)) +
    geom_line()+
    geom_point() +
    scale_color_manual(breaks = c("ALS","CTR"),values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
    labs(x = "", y = "Frequency of patients \n(%) \n", title = ward_interest_EN_i, color = "Status") +
    # scale_x_continuous(breaks=c(10,5,1,0),
    #                    labels = c("10 years", "5 years", "1 year", "1 month")) +
    theme_minimal() + 
    theme( plot.title = element_text(hjust = 0.5),
           axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
  plot_nrpatients[[i]] <- ggplot(dat_healthcare_nrpatients_i,aes(x = time, y = value, group = variable,color = variable)) +
    geom_line()+
    geom_point() +
    scale_color_manual(breaks = c("ALS","CTR"),values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
    labs(x = "", y = "Number of patients \n", title = ward_interest_EN_i, color = "Status") +
    # scale_x_continuous(breaks=c(10,5,1,0),
    #                    labels = c("10 years", "5 years", "1 year", "1 month")) +
    theme_minimal() + 
    theme( plot.title = element_text(hjust = 0.5),
           axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
}

i = 3
pdf(file=paste0("plots/timeline_",wards_interest_EN[[i]],".pdf"),) 
plot_grid(plot_freq_visit[[i]],plot_patients[[i]],plot_nrpatients[[i]],ncol = 1, nrow=3, width = 6, height = 8, labels = c("A","B","C")) 
dev.off()

pdf(file=paste0("plots/timeline_",wards_interest_EN[1],"_",wards_interest_EN[2],".pdf"),) 
plot_grid(plot_freq_visit[[1]],plot_freq_visit[[2]],
          plot_patients[[1]],plot_patients[[2]],
          plot_nrpatients[[1]], plot_nrpatients[[2]],
          ncol = 2, nrow=3, width = 14, height = 6, labels = c("A","D","B","E","C","F")) 
dev.off()

pdf(file=paste0("plots/timeline_",wards_interest_EN[1],"_",wards_interest_EN[2],"_reduced.pdf"),width = 8,height = 5) 
plot_grid(plot_freq_visit[[1]],plot_freq_visit[[2]],
          plot_nrpatients[[1]], plot_nrpatients[[2]],
          ncol = 2, nrow=2, width = 14, height = 6, labels = c("A","C","B","D")) 
dev.off()

## timeplot analysis of BMI's and weights
data_BMI_timeline <- dat_final_subquestions_dietweight_continuous[,c(6:10)] %>%
  mutate(ID = rep(1:nrow(dat_final_subquestions_dietweight_continuous)),
         status = ifelse(status == 1,"ALS","CTR"))
data_BMI_timeline <- data_BMI_timeline %>% 
  pivot_longer(!c(status,ID), names_to = "BMI", values_to = "BMI_value")
data_BMI_timeline <- data_BMI_timeline %>%
  dplyr::rename(time = BMI,
          BMI = BMI_value)
data_BMI_timeline$time <- ifelse(data_BMI_timeline$time == "BMI_10Y","10 years",
                                 ifelse(data_BMI_timeline$time == "BMI_5Y", "5 years",
                                        ifelse(data_BMI_timeline$time == "BMI_1Y","1 year",
                                        "now")))
data_weight_timeline <- dat_final_subquestions_dietweight_continuous[,c(1,3:5,10)] %>%
  mutate(ID = rep(1:nrow(dat_final_subquestions_dietweight_continuous)),
         status = ifelse(status == 1,"ALS","CTR"))
data_weight_timeline <- data_weight_timeline %>% 
  pivot_longer(!c(status,ID), names_to = "time", values_to = "Weight")
data_weight_timeline$time <- gsub('(?:.*?\\[([^][]*)\\].*|.*)', '\\1', data_weight_timeline$time, perl=T)
data_weight_timeline$time <- ifelse(data_weight_timeline$time == "10 Jahre vor Erkrankungsbeginn","10 years",
                                 ifelse(data_weight_timeline$time == "5 Jahre vor Erkrankungsbeginn", "5 years",
                                        ifelse(data_weight_timeline$time == "Ein Jahr vor Erkrankungbeginn","1 year",
                                               "now")))
data_BMI_timeline_tmp <- summarySE(data_BMI_timeline,measurevar = "BMI",groupvars = c("status","time"),na.rm = T) %>%
  dplyr::rename(mean = BMI)
data_BMI_timeline_tmp <- data_BMI_timeline %>%
  left_join(data_BMI_timeline_tmp)
data_weight_timeline_tmp <- summarySE(data_weight_timeline, measurevar = "Weight", groupvars = c("status","time"),na.rm = T) %>%
  dplyr::rename(mean = Weight)
data_weight_timeline_tmp <- data_weight_timeline %>%
  left_join(data_weight_timeline_tmp)
data_BMI_timeline_tmp$time  <- factor(data_BMI_timeline$time,
                                  levels = c("10 years", "5 years", "1 year", "now")) 
data_weight_timeline_tmp$time <- factor(data_weight_timeline$time,
                                    levels = c("10 years", "5 years", "1 year", "now")) 
plot_BMI <- ggplot(data_BMI_timeline_tmp,aes(x = time, y = mean, group = status, color = status)) +
  geom_line() + 
  geom_point() +
  stat_compare_means(aes(x = time, y = BMI,group = status, 
                         label  = sprintf("p = %2.1e", as.numeric(..p.format..))), 
                     label.y = c(26.2, 26.5, 27,27.1),method = "t.test") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, 
                position=position_dodge(0.05)) +
  scale_color_manual(breaks = c("ALS","CTR"),values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
  labs(x = "", y = "BMI \n", title = "", color = "Status") +
  theme_minimal() + 
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
plot_weight <- ggplot(data_weight_timeline_tmp,aes(x = time, y = mean, group = status, color = status)) +
  geom_line() + 
  geom_point() +
  stat_compare_means(aes(x = time, y = Weight,group = status, label = sprintf("p = %5.3f", as.numeric(..p.format..))), 
                     label.y = c(79.5, 80, 80.5,79.3),method = "t.test") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, 
                position=position_dodge(0.05)) +
  scale_color_manual(breaks = c("ALS","CTR"),values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
  labs(x = "", y = "Weight (kg) \n", title = "", color = "Status") +
  theme_minimal() + 
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
  
pdf(file=paste0("plots/timeline_BMI_weight_ALS_CTR.pdf")) 
plot_grid(plot_BMI,plot_weight,ncol = 1, nrow=2, width = 6, height = 4, labels = c("A","B")) 
dev.off()

# BMI and weight for ALS only
data_BMI_ALS_timeline <- data_BMI_timeline %>%
  filter(status == "ALS") %>%
  select(-c(status))
height <- dat_final_subquestions_dietweight_continuous[dat_final_subquestions_dietweight_continuous$status == 1,]$`Bitte geben Sie Ihre Körpergröße an [cm].` 
data_ALS_weight <- data_ALS_final %>%
  select(`Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]`,
         `Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`) %>%
  mutate(ID = rep(1:nrow(data_ALS_final)))
data_ALS_weight$height <- conv_unit(as.numeric(height),"cm","m")
data_ALS_BMI <- data_ALS_weight %>%
  dplyr::mutate(diagnosis = `Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`/(height)^2,
                onset = `Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]`/(height)^2,) %>%
  select(ID, diagnosis, onset) %>% 
  pivot_longer(!c(ID), names_to = "BMI", values_to = "BMI_value") %>%
  dplyr::rename(time = BMI,
                BMI = BMI_value)
data_BMI_ALS_timeline <- rbind(data_BMI_ALS_timeline,
                               data_ALS_BMI)
data_BMI_ALS_timeline_tmp <- summarySE(data_BMI_ALS_timeline,measurevar = "BMI",groupvars = c("time"),na.rm = T) %>%
  dplyr::rename(mean = BMI)
data_BMI_ALS_timeline_tmp <- data_BMI_ALS_timeline %>%
  left_join(data_BMI_ALS_timeline_tmp)
data_BMI_ALS_timeline_tmp$time  <- factor(data_BMI_ALS_timeline_tmp$time,
                                      levels = c("10 years", "5 years", "1 year", "onset","diagnosis","now")) 
data_weight_ALS_timeline <- data_weight_timeline %>%
  filter(status == "ALS") %>%
  select(-c(status))
data_ALS_weight_tmp <- data_ALS_weight %>% 
  dplyr::rename(diagnosis = `Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`,
         onset = `Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]`) %>%
  select(ID, diagnosis, onset) %>% 
  pivot_longer(!c(ID), names_to = "time", values_to = "Weight") 
data_weight_ALS_timeline <- rbind(data_weight_ALS_timeline,
                                  data_ALS_weight_tmp)
data_weight_ALS_timeline_tmp <- summarySE(data_weight_ALS_timeline,measurevar = "Weight",groupvars = c("time"),na.rm = T) %>%
  dplyr::rename(mean = Weight)
data_weight_ALS_timeline_tmp <- data_weight_ALS_timeline %>%
  left_join(data_weight_ALS_timeline_tmp)
data_weight_ALS_timeline_tmp$time  <- factor(data_weight_ALS_timeline_tmp$time,
                                          levels = c("10 years", "5 years", "1 year", "onset","diagnosis","now")) 
plot_BMI_ALS <- ggplot(data_BMI_ALS_timeline_tmp,aes(x = time, y = mean)) +
  geom_line(aes(group=1),color = "#5f91bd") + 
  geom_point(color = "#5f91bd") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, 
                position=position_dodge(0.05),color = "#5f91bd") +
  labs(x = "", y = "BMI \n", title = "") +
  theme_minimal() + 
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
plot_weight_ALS <- ggplot(data_weight_ALS_timeline_tmp,aes(x = time, y = mean)) +
  geom_line(aes(group=1),color = "#5f91bd") + 
  geom_point(color = "#5f91bd") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, 
                position=position_dodge(0.05),color = "#5f91bd") +
  labs(x = "", y = "Weight () \n", title = "") +
  theme_minimal() + 
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8))

pdf(file=paste0("plots/timeline_BMI_all.pdf")) 
plot_grid(plot_BMI,plot_BMI_ALS,ncol = 1, nrow=2, width = 6, height = 4, labels = c("A","B")) 
dev.off()
pdf(file=paste0("plots/timeline_weight_all.pdf")) 
plot_grid(plot_weight,plot_weight_ALS,ncol = 1, nrow=2, width = 6, height = 4, labels = c("A","B")) 
dev.off()
pdf(file=paste0("plots/timeline_BMI_weight_ALS.pdf")) 
plot_grid(plot_BMI_ALS,plot_weight_ALS,ncol = 1, nrow=2, width = 6, height = 4, labels = c("A","B")) 
dev.off()
pdf(file=paste0("plots/timeline_BMI_weight_all.pdf")) 
plot_grid(plot_BMI,plot_weight,plot_BMI_ALS,plot_weight_ALS,ncol = 2, nrow=2, width = 20, height = 10, labels = c("A","C","B","D")) 
dev.off()


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

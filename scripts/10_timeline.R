## Timeline plots of wards visits

## timeplot analysis of relevant Questions
wards_interest <- c("Neurologie", "Logopädie")
wards_interest_EN <- c("Neurology", "Speech Therapy")

plot_freq_visit = plot_avg_visit = plot_patients = plot_nrpatients = list()

for (i in 1:length(wards_interest)) {
  ward_interest_i <- wards_interest[i]
  ward_interest_EN_i <- wards_interest_EN[i]
  # get questions for the specific ward
  original_questions_i <- final_healthcare %>%
    filter(`sub-category` == ward_interest_i) %>%
    pull(`original question (ALS)`)
  # data with questions for specific ward and status info
  dat_healthcare_i <- data_combined %>%
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
  dat_healthcare_visits_i <- reshape2::melt(dat_healthcare_visits_i, id = "time")
  # average amount of visits per patient across time, stratified by cohort (ALS/CTR)
  dat_healthcare_avg_visits_i <- dat_healthcare_i %>%
    dplyr::group_by(status) %>%
    summarise_each(funs(mean(.[. != 0], na.rm = TRUE)))
  dat_healthcare_avg_visits_i <- data.frame(CTR = sapply(t(dat_healthcare_avg_visits_i[,2:5])[,1],function(x) round(x,2)),
                                            ALS = sapply(t(dat_healthcare_avg_visits_i[,2:5])[,2],function(x) round(x,2)),
                                            time = time)
  dat_healthcare_avg_visits_i <- reshape2::melt(dat_healthcare_avg_visits_i, id = "time")
  # total amount of patients across time, stratified by cohort (ALS/CTR)
  dat_healthcare_binary_i <- data.frame(cbind(apply(dat_healthcare_i[,1:4], 2, 
                                                    function(x) ifelse(x>0,1,0)),status = dat_healthcare_i$status))
  dat_healthcare_patients_i_tmp <- dat_healthcare_binary_i %>%
    dplyr::group_by(status) %>%
    dplyr::summarise(across(everything(), ~ sum(., na.rm = TRUE)))
  nr_CTR <- sum(apply(dat_healthcare_binary_i %>% filter(status == 0) %>% select(-status), 1, function(row){ifelse(any(row) == 1,1,0)}),na.rm = T)
  nr_ALS <- sum(apply(dat_healthcare_binary_i %>% filter(status == 1) %>% select(-status), 1, function(row){ifelse(any(row) == 1,1,0)}),na.rm = T)
  print(nr_CTR)
  print(nr_ALS)
  # dat_healthcare_patients_i <- data.frame(CTR = round(t(dat_healthcare_patients_i_tmp[,2:5])[,1]/length(which(dat_healthcare_i$status==0))*100,digits = 2),
  #                                         ALS = round(t(dat_healthcare_patients_i_tmp[,2:5])[,2]/length(which(dat_healthcare_i$status==1))*100,digits = 2),
  #                                         time = time)
  # dat_healthcare_patients_i <- data.frame(CTR = round(t(dat_healthcare_patients_i_tmp[,2:5])[,1]/nr_CTR*100,digits = 2),
  #                                         ALS = round(t(dat_healthcare_patients_i_tmp[,2:5])[,2]/nr_ALS*100,digits = 2),
  #                                         time = time)
  dat_healthcare_patients_i <- data.frame(CTR = round(t(dat_healthcare_patients_i_tmp[,2:5])[,1]/285*100,digits = 2),
                                          ALS = round(t(dat_healthcare_patients_i_tmp[,2:5])[,2]/475*100,digits = 2),
                                          time = time)
  dat_healthcare_patients_i <- reshape2::melt(dat_healthcare_patients_i, id = "time")
  dat_healthcare_nrpatients_i <- data.frame(CTR = t(dat_healthcare_patients_i_tmp[,2:5])[,1],
                                            ALS = t(dat_healthcare_patients_i_tmp[,2:5])[,2],
                                            time = time)
  dat_healthcare_nrpatients_i <- reshape2::melt(dat_healthcare_nrpatients_i, id = "time")
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
  plot_patients[[i]] <- ggplot(dat_healthcare_patients_i,
                               aes(x = time, y = value, group = variable,color = variable)) +
    geom_line()+
    geom_point() +
    scale_color_manual(name = "Group", breaks = c("ALS","CTR"),
                       labels = c("ALS" = "ALS", "CTR" = "Controls"),
                       values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
    scale_x_discrete(
      labels = c(
        "10 years" = "5–10 years",
        "5 years"  = "1–5 years",
        "1 year"   = "1–12 months",
        "1 month"  = "<1 month"
      )
    ) +
    labs(x = "", y = "Number of participants \n(%) \n", title = ward_interest_EN_i, color = "Status") +
    # scale_x_continuous(breaks=c(10,5,1,0),
    #                    labels = c("10 years", "5 years", "1 year", "1 month")) +
    theme_minimal(base_size = 14) + 
    theme( axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
           plot.title = element_text(face = "bold", size = 15.5, hjust = 0.5),
           axis.title = element_text(size = 14.7),
           axis.text = element_text(size = 14.2),
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 14),
           legend.position = "bottom",   
           legend.box = "horizontal")   
  plot_nrpatients[[i]] <- ggplot(dat_healthcare_nrpatients_i,aes(x = time, y = value, group = variable,color = variable)) +
    geom_line()+
    geom_point() +
    scale_color_manual(breaks = c("ALS","CTR"),values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+ 
    labs(x = "", y = "Number of participants \n", title = ward_interest_EN_i, color = "Status") +
    # scale_x_continuous(breaks=c(10,5,1,0),
    #                    labels = c("10 years", "5 years", "1 year", "1 month")) +
    theme_minimal() + 
    theme( plot.title = element_text(hjust = 0.5),
           axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) 
}

pdf("plots/timeline_freq_healthcare.pdf",width = 6,height = 9) 
plot_grid(plot_patients[[1]], plot_patients[[2]],
          ncol = 1, nrow=2, labels = c("B","C")) 
dev.off()

# Analysis of ALS weight
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

data_ALS_weight <- ALS_specific %>%
  select(`Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]`,
         `Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`) %>%
  mutate(ID = rep(1:nrow(ALS_specific)))

data_weight_timeline <- data_combined %>%
  dplyr::rename(now = `Bitte geben Sie Ihr Gewicht an. [Aktuell][Gewicht [kg]]`,
                one_y = `Bitte geben Sie Ihr Gewicht an. [Ein Jahr vor Erkrankungbeginn][Gewicht [kg]]`,
                five_y = `Bitte geben Sie Ihr Gewicht an. [5 Jahre vor Erkrankungsbeginn][Gewicht [kg]]`,
                ten_y = `Bitte geben Sie Ihr Gewicht an. [10 Jahre vor Erkrankungsbeginn][Gewicht [kg]]`) %>%
  mutate(ID = rep(1:nrow(data_combined)),
         now = as.numeric(now),
         one_y = as.numeric(one_y),
         five_y = as.numeric(five_y),
         ten_y = as.numeric(ten_y)) %>%
  select(ID, now, one_y,five_y,ten_y,status)

data_weight_timeline <- data_weight_timeline %>% 
  pivot_longer(!c(status,ID), names_to = "time", values_to = "Weight")

gender_data = tibble(ID = rep(1:760),
                     gender = data_combined$`Bitte geben Sie Ihr Geschlecht an.`)

df <- data_weight_timeline %>%
  left_join(gender_data)

data_weight_timeline_tmp <- summarySE(df, measurevar = "Weight", groupvars = c("status","time","gender"),na.rm = T) %>%
  dplyr::rename(mean = Weight) %>%
  filter(!is.na(gender))
data_weight_timeline_tmp <- df %>%
  left_join(data_weight_timeline_tmp)

data_weight_ALS_timeline <- data_weight_timeline %>%
  filter(status == "1") %>%
  select(-c(status))

data_ALS_weight_tmp <- data_ALS_weight %>% 
  dplyr::rename(diagnosis = `Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`,
                onset = `Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]`) %>%
  select(ID, diagnosis, onset) %>% 
  pivot_longer(!c(ID), names_to = "time", values_to = "Weight") 

data_ALS_weight_tmp <- data_ALS_weight_tmp %>%
  left_join(df %>% filter(status == 1) %>% select(ID,gender)) %>% distinct()
data_ALS_weight_tmp <- data_ALS_weight_tmp %>% distinct()
data_weight_ALS_timeline <- rbind(df %>% filter(status == 1) %>% select(-status),
                                  data_ALS_weight_tmp)
data_weight_ALS_timeline_tmp <- summarySE(data_weight_ALS_timeline,measurevar = "Weight",groupvars = c("time","gender"),na.rm = T) %>%
  dplyr::rename(mean = Weight)
data_weight_ALS_timeline_tmp <- data_weight_ALS_timeline %>%
  left_join(data_weight_ALS_timeline_tmp)
data_weight_ALS_timeline_tmp$time  <- factor(data_weight_ALS_timeline_tmp$time,
                                             levels = c("ten_y", "five_y", "one_y", "onset","diagnosis","now")) 
data_weight_ALS_timeline_tmp %>% group_by(time,gender) %>% dplyr::summarise(mean = mean(na.omit(Weight)))

plot_weight_ALS <- ggplot(data_weight_ALS_timeline_tmp %>% filter(!is.na(gender)),aes(x = time, y = mean)) +
  geom_line(aes(linetype=gender,group = gender),color = "#5f91bd") + 
  geom_point(color = "#5f91bd") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, 
                position=position_dodge(0.5),color = "#5f91bd") +
  labs(x = "", y = "Weight (kg) \n", title = "") +
  theme_minimal() + 
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8))

## plot of all of them together 
data_weight_ALS_CTR_timeline <- rbind(data_weight_ALS_timeline_tmp %>% mutate(status = rep("ALS")),
                                      data_weight_timeline_tmp %>% filter(status == 0) %>% 
                                        select(ID,time,Weight,gender,N,mean,sd,se,ci,status)) %>%
  mutate(status = ifelse(status == 0, "CTR",status))

data_weight_ALS_CTR_timeline_tmp2 <- data_weight_ALS_CTR_timeline %>%
  filter(!is.na(Weight) & !is.na(gender))

data_weight_ALS_CTR_timeline$time <- factor(
  data_weight_ALS_CTR_timeline$time,
  levels = c("ten_y", "five_y", "one_y", "onset","diagnosis","now")
)

model <- lm(Weight ~ gender, data = data_weight_ALS_CTR_timeline)
data_weight_ALS_CTR_timeline_tmp2$weight_adj <- residuals(model)
ttest_results <- data_weight_ALS_CTR_timeline_tmp2 %>%
  dplyr::group_by(time) %>%
  filter(status %in% c("ALS", "CTR") & !is.na(weight_adj)) %>%
  filter(!time %in% c("onset","diagnosis")) %>%
  #filter(time == "now") %>%
  dplyr::summarise(
    p_value = tryCatch(
      t.test(weight_adj ~ status)$p.value),
    .groups = "drop"
  )

# LMM model with time as categorical and adjusting for gender
model_lmm <- lmer(
  Weight ~ status * time + gender + (1 | ID),
  data = data_weight_ALS_CTR_timeline
)
em <- emmeans(model_lmm, ~ status | time)
em_df <- as.data.frame(em)

# get differences between ALS vs CTR
contrast_df <- contrast(em, method = "pairwise", adjust = "none") 

# Add formatted p-values and significance stars
contrast_df <- summary(contrast_df,infer = T) %>%
  as.data.frame() %>%
  mutate(
    p_value_sci = formatC(p.value, format = "e", digits = 2),  # scientific notation
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    label = paste0("p=", p_value_sci, " \n", stars)
  )

contrast_df$time <- as.factor(contrast_df$time)

placeholders <- data.frame(
  status = "CTR",
  time = c("onset", "diagnosis"),
  emmean = NA,
  SE = NA,
  df = NA,
  lower.CL = NA,
  upper.CL = NA
)
# Combine with your existing data
em_df_fixed <- bind_rows(em_df, placeholders)
em_df_fixed <- em_df %>%
  group_by(status) %>%
  tidyr::fill(emmean, .direction = "downup") 
em_df_fixed$emmean[em_df_fixed$status == "CTR" & em_df_fixed$time == "onset"] <- 78.1  # valye from 1 y
em_df_fixed$emmean[em_df_fixed$status == "CTR" & em_df_fixed$time == "diagnosis"] <- 78.1

em_df_fixed_ALS_CTR = em_df_fixed
contrast_ALS_CTR = contrast_df

# Plot (LMM adjusted for weight)
plot_weight_ALS_CTR <- ggplot(em_df_fixed, aes(x = time, y = emmean, color = status, group = status)) +
  geom_line(linewidth = 1,size = 1) + 
  geom_point(data = filter(em_df_fixed, !is.na(SE))) +
  geom_errorbar(data = filter(em_df_fixed, !is.na(emmean)),
                aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, 
                position=position_dodge(0.05)) +
  geom_text(data = contrast_df,
            aes(x = time,
                y = max(em_df_fixed$emmean, na.rm = TRUE) + 0.1, 
                label = label),
            color = "black",
            size = 4,
            vjust = 0,
            inherit.aes = F) +
  scale_color_manual(
    name = "Group",   
    breaks = c("ALS", "CTR"),
    labels = c("ALS", "Controls"),
    values = c("ALS" = "#5f91bd", "CTR" = "#BD8B5F")
  ) + 
  scale_x_discrete(
    labels = c(
      "ten_y" = "5–10 years",
      "five_y"  = "1–5 years",
      "one_y"   = "1–12 months",
      "onset"    = "Onset",
      "diagnosis" = "Diagnosis",
      "now"      = "Questionnaire \ncompletion"
    )
  ) +
  labs(x = "", y = "Weight (kg) \n", title = "") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
    axis.title = element_text(size = 12.5),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.background  = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey"),
    panel.border = element_blank(),
    axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
    strip.text = element_text(size = 14)) 

# without adjustment of gender (but stratified by gender)
data_weight_ALS_CTR_timeline_tmp <- summarySE(data_weight_ALS_CTR_timeline,measurevar = "Weight",
                                              groupvars = c("time","status"),na.rm = T) %>%
  dplyr::rename(mean = Weight)

# LMM model with time as categorical
model_lmm <- lmer(
  Weight ~ status * time * gender + (1 | ID),
  data = data_weight_ALS_CTR_timeline
)
em <- emmeans(model_lmm, ~ status * gender | time)
em_df <- as.data.frame(em)

# get differences between ALS vs CTR
contrast_df <- contrast(em, method = "pairwise", adjust = "none") 

# Add formatted p-values and significance stars
contrast_df <- summary(contrast_df,infer = T) %>%
  as.data.frame() %>%
  mutate(
    p_value_sci = formatC(p.value, format = "e", digits = 2),  # scientific notation
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    label = paste0("p=", p_value_sci, " \n", stars)
  )

contrast_df$time <- as.factor(contrast_df$time)

em_df_fixed <- em_df %>%
  group_by(status,gender) %>%
  tidyr::fill(emmean, .direction = "downup") 

plot_weight_ALS <- ggplot(em_df_fixed %>% filter(!is.na(gender) & status == "ALS"),
                          aes(x = time, y = emmean)) +
  geom_line(aes(linetype=gender,group = gender),color = "#5f91bd",size = 1) + 
  geom_point(color = "#5f91bd") +
  scale_x_discrete(
    labels = c(
      "ten_y" = "5–10 years",
      "five_y"  = "1–5 years",
      "one_y"   = "1–12 months",
      "onset"    = "Onset",
      "diagnosis" = "Diagnosis",
      "now"      = "Questionnaire \ncompletion"
    )) +
  scale_linetype_manual(
    name = "Sex",     # legend title
    breaks = c("männlich", "weiblich"),  
    labels = c("Male", "Female"),
    values = c("männlich" = "solid", "weiblich" = "dashed")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), width=.2, 
                position=position_dodge(0.5),color = "#5f91bd") +
  labs(x = "", y = "Weight (kg) \n", title = "") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
    axis.title = element_text(size = 12.5),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.background  = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey"),
    panel.border = element_blank(),
    axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
    strip.text = element_text(size = 14))


plot_weight_ALS_CTR_sex <- ggplot(em_df_fixed %>% filter(!is.na(gender)),
                                  aes(x = time, y = emmean, color = status, 
                                      linetype = gender,group = interaction(status, gender))) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), width = 0.2,
                position = position_dodge(0.05)) +
  geom_text(data = contrast_df,
            aes(x = time,
                y = max(em_df_fixed$emmean, na.rm = TRUE) + 1, 
                label = label),
            color = "black",
            size = 4,
            vjust = 0,
            inherit.aes = F) +
  scale_color_manual(
    name = "Group",   
    breaks = c("ALS", "CTR"),
    labels = c("ALS", "Controls"),
    values = c("ALS" = "#5f91bd", "CTR" = "#BD8B5F")
  ) +
  scale_linetype_manual(
    name = "Sex",     # legend title
    breaks = c("männlich", "weiblich"),  # adjust if your dataset uses German labels
    labels = c("Male", "Female"),
    values = c("männlich" = "solid", "weiblich" = "dashed")
  ) +
  scale_x_discrete(
    labels = c(
      "ten_y" = "5–10 years",
      "five_y"  = "1–5 years",
      "one_y"   = "1–12 months",
      "onset"    = "Onset",
      "diagnosis" = "Diagnosis",
      "now"      = "Questionnaire \ncompletion"
    )
  ) +
  labs(
    x = "",
    y = "Weight (kg)\n",
    title = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
    axis.title = element_text(size = 12.5),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.background  = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey"),
    panel.border = element_blank(),
    axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
    strip.text = element_text(size = 14))

pdf(file = "plots/timeline_weight_CTR_ALS.pdf",width = 10, height = 6)
plot_weight_ALS_CTR
dev.off()

# first version of the weight plot
pdf(file="plots/timeline_weight_all_v1.pdf",width =10, height = 9) 
plot_grid(plot_weight_ALS_CTR,plot_weight_ALS,ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()

# second version of the weight plot
pdf(file="plots/timeline_weight_all_v2.pdf",width = 9.5, height = 9) 
plot_grid(plot_weight_ALS_CTR,plot_weight_ALS_CTR_sex,ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()

contrast_ALS_CTR_sex = contrast_df
em_df_fixed_ALS_CTR_sex = em_df_fixed

em_df_fixed_ALS_CTR 
contrast_ALS_CTR 

# for ALS only
model_lmm_ALS <- lmer(
  Weight ~ time * gender + (1 | ID),
  data = data_weight_ALS_CTR_timeline %>% filter(status == "ALS")
)
summary(model_lmm_ALS)
Anova(model_lmm_ALS, type = 3)

model_lmm_ALS_until_onset <- lmer(
  Weight ~ time * gender + (1 | ID),
  data = data_weight_ALS_CTR_timeline %>% filter(status == "ALS", time %in% c("one_y","five_y","ten_y"))
)
summary(model_lmm_ALS_until_onset)
Anova(model_lmm_ALS_until_onset, type = 3)

data_weight_ALS_CTR_timeline$status <- factor(data_weight_ALS_CTR_timeline$status, levels = c("CTR","ALS"))
data_weight_ALS_CTR_timeline$time <- factor(data_weight_ALS_CTR_timeline$time, levels = c("ten_y", "five_y", "one_y", "onset","diagnosis","now"))

model_lmm_until_onset <- lmer(
  Weight ~ status * time * gender + (1 | ID),
  data = data_weight_ALS_CTR_timeline %>% filter(time %in% c("one_y","five_y","ten_y"))
)
summary(model_lmm_until_onset)
Anova(model_lmm_until_onset, type = 3)

model_lmm_until_onset_male <- lmer(
  Weight ~ status * time  + (1 | ID),
  data = data_weight_ALS_CTR_timeline %>% filter(time %in% c("one_y","five_y","ten_y") & gender == "männlich")
)
summary(model_lmm_until_onset_male)
Anova(model_lmm_until_onset_male, type = 3)

model_lmm_until_onset_female <- lmer(
  Weight ~ status * time  + (1 | ID),
  data = data_weight_ALS_CTR_timeline %>% filter(time %in% c("one_y","five_y","ten_y") & gender == "weiblich")
)
summary(model_lmm_until_onset_female)
Anova(model_lmm_until_onset_female, type = 3)




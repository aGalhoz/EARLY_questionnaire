## Open answers from non-motor symptoms

EARLY_AllAnswers <- read_excel("data input/EARLY_AllAnswers.xlsx")
general_symptoms_PL <- read_excel("data input/general_symptoms_PL.xlsx")
specific_symptoms_PL <- read_excel("data input/specific_symptoms_PL.xlsx")

EARLY_AllAnswers$symptom_general_PL <- ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$neuromuscular,
                                              "Neuromuscular",
                                              ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`Sleep/fatigue`,
                                              "Sleep/fatigue",
                                              ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`skin/soft tissue`,
                                                     "Skin/soft tissue",
                                                     ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`gastro-metabolic`,
                                                            "Gastro-metabolic",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$sensory,
                                                                                      "Sensory",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$psychological,
                                                                                                       "Psychological",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`Immuno-oncological`,
                                                                                                                              "Immuno-oncological",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$vegetative,
                                                                                                                                                          "Vegetative",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`Body measures`,
                                                                                                                                                                              "Body measures",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`Injury/operation`,
                                                                                                                                                                                                     "Injury/operation",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$cardiovascular,
                                                                                                                                                                                                                               "Cardiovascular",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$Other,
                                                                                                                                                                                                                                                       "Other",EARLY_AllAnswers$`Symptom/Syndrome_General_1`))))))))))))
EARLY_AllAnswers$symptom_general_PL <- ifelse(is.na(EARLY_AllAnswers$`Symptom/Syndrome_General_1`),NA,EARLY_AllAnswers$symptom_general_PL)
EARLY_AllAnswers$symptom_specific_PL <- ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$`neuro-motor`,
                                              "Neuro-motor",
                                              ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$sensory,
                                                     "Sensory",
                                                     ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$psychological,
                                                            "Psychological",
                                                            ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$oncological,
                                                                   "Oncological",
                                                                   ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$gastrointestinal,
                                                                                             "Gastrointestinal",
                                                                          ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$urological,
                                                                                                              "Urological",
                                                                                 ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$immunological,
                                                                                                                                     "Immunological",
                                                                                        ifelse(EARLY_AllAnswers$Symptom_Specific_1 %in% specific_symptoms_PL$other,
                                                                                               "Other",EARLY_AllAnswers$Symptom_Specific_1))))))))

EARLY_AllAnswers$symptom_specific_PL <- ifelse(is.na(EARLY_AllAnswers$Symptom_Specific_1),NA,EARLY_AllAnswers$symptom_specific_PL)

EARLY_AllAnswers_symptom_general_freq <- as.data.frame(table(c(EARLY_AllAnswers$symptom_general_PL))) %>% arrange(desc(Freq))
EARLY_AllAnswers_symptom_specific_freq <- as.data.frame(table(c(EARLY_AllAnswers$symptom_specific_PL))) %>% arrange(desc(Freq))
EARLY_AllAnswers_symptom_general_unique <- EARLY_AllAnswers$symptom_general_PL %>% unique() %>% na.omit()
EARLY_AllAnswers_symptom_specific_unique <- EARLY_AllAnswers$symptom_specific_PL %>% unique() %>% na.omit()

writexl::write_xlsx(EARLY_AllAnswers_symptom_general_freq,"data code output/freq_general_symptoms_PL.xlsx")
writexl::write_xlsx(EARLY_AllAnswers_symptom_specific_freq,"data code output/freq_specific_symptoms_PL.xlsx")

EARLY_AllAnswers_symptom_general_freq$Var1 <- factor(EARLY_AllAnswers_symptom_general_freq$Var1,
                                                     levels = EARLY_AllAnswers_symptom_general_freq$Var1)
## Pie charts
pdf("plots/pie_generalsymptoms.pdf")
ggplot(EARLY_AllAnswers_symptom_general_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Paired")+
  theme_minimal() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y", start = 0) +
  ggtitle("general symptoms") +
  #geom_label_repel(aes(label = Freq), size=4, show.legend = F, nudge_x = 1) +
  theme_void()
dev.off()

neuromuscular_freq <- as.data.frame(table(c(EARLY_AllAnswers[EARLY_AllAnswers$symptom_general_PL == "Neuromuscular",]$`Symptom/Syndrome_General_1`))) %>% arrange(desc(Freq))
neuromuscular_freq$Var1 <- factor(neuromuscular_freq$Var1,levels = neuromuscular_freq$Var1)
pdf("plots/pie_neuromuscular.pdf")
ggplot(neuromuscular_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Paired")+
  theme_void() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  ggtitle("neuromuscular - general symptoms")
dev.off()

sensory_freq <- as.data.frame(table(c(EARLY_AllAnswers[EARLY_AllAnswers$symptom_general_PL == "Sensory",]$`Symptom/Syndrome_General_1`))) %>% arrange(desc(Freq))
sensory_freq$Var1 <- factor(sensory_freq$Var1,levels = sensory_freq$Var1)
pdf("plots/pie_sensory.pdf")
ggplot(sensory_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Paired")+
  theme_void() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  ggtitle("sensory - general symptoms")
dev.off()


EARLY_AllAnswers_symptom_specific_freq$Var1 <- factor(EARLY_AllAnswers_symptom_specific_freq$Var1,
                                                      levels = EARLY_AllAnswers_symptom_specific_freq$Var1)

pdf("plots/pie_specificsymptoms.pdf")
ggplot(EARLY_AllAnswers_symptom_specific_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Paired")+
  theme_minimal() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  ggtitle("specific symptoms")
dev.off()

neuromotor_freq <- as.data.frame(table(c(EARLY_AllAnswers[EARLY_AllAnswers$symptom_specific_PL == "Neuro-motor",]$Symptom_Specific_1))) %>% arrange(desc(Freq))
neuromotor_freq$Var1 <- factor(neuromotor_freq$Var1,levels = neuromotor_freq$Var1)
pdf("plots/pie_neuromotor.pdf")
ggplot(neuromotor_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = c("red", "coral", "darkred", 
                               "chocolate1", "cornsilk4", "cyan1",
                               "darkgoldenrod2", "darkorchid2", "deeppink2",
                               "darkslategray4", "chartreuse4", "brown1",
                               "blueviolet", "aquamarine", "blue3", 
                               "azure4", "darkorange3", "brown3", 
                               "burlywood4")) +
  theme_void() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  ggtitle("neuromotor - specific symptoms")
dev.off()

sensory_freq2 <- as.data.frame(table(c(EARLY_AllAnswers[EARLY_AllAnswers$symptom_specific_PL == "Sensory",]$Symptom_Specific_1))) %>% arrange(desc(Freq))
sensory_freq2$Var1 <- factor(sensory_freq2$Var1,levels = sensory_freq2$Var1)
pdf("plots/pie_sensory2.pdf")
ggplot(sensory_freq2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = c("red", "coral", "darkred", 
                               "chocolate1", "cornsilk4", "cyan1",
                               "darkgoldenrod2", "darkorchid2", "deeppink2",
                               "darkslategray4", "chartreuse4", "brown1",
                               "blueviolet", "aquamarine", "blue3", 
                               "azure4", "darkorange3", "brown3", 
                               "burlywood4")) +
  theme_void() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  ggtitle("sensory - specific symptoms")
dev.off()


# open answers ALS
ALS_open_answers_nonmotor <- data_patients_common_final[,grep("Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen. [][Welche Veränderung?]...",
                                                              colnames(data_patients_common_final))]
ALS_open_answers_nonmotor <- ALS_open_answers_nonmotor[,c(1,3,5,7,9)]

# frequency of general symptom in ALS 
ALS_open_answers_nonmotor_symptom_general <- apply(ALS_open_answers_nonmotor, 2, 
                                                   function(x) ifelse(x %in% EARLY_AllAnswers$Answer,EARLY_AllAnswers$`Symptom/Syndrome_General_1`,x))
ALS_open_answers_nonmotor_symptom_general_freq <- apply(ALS_open_answers_nonmotor_symptom_general, 2, 
                                                        function(x) ifelse(!is.na(x),1,0))
ALS_open_answers_nonmotor_symptom_general_freq <- as.data.frame(table(apply(ALS_open_answers_nonmotor_symptom_general_freq, 1, 
                                                        sum))) %>% arrange(Freq)

# frequency of specific symptom in ALS 
ALS_open_answers_nonmotor_symptom_specific <- apply(ALS_open_answers_nonmotor, 2, 
                                                   function(x) ifelse(x %in% EARLY_AllAnswers$Answer,EARLY_AllAnswers$Symptom_Specific_1,x))
ALS_open_answers_nonmotor_symptom_specific_freq <- apply(ALS_open_answers_nonmotor_symptom_specific, 2, 
                                                        function(x) ifelse(!is.na(x),1,0))
ALS_open_answers_nonmotor_symptom_specific_freq <- as.data.frame(table(apply(ALS_open_answers_nonmotor_symptom_specific_freq, 1, 
                                                                            sum))) %>% arrange(Freq)
  
# open answers CTR
CTR_open_answers_nonmotor <- data_control_common_final[,grep("Bitte beschreiben Sie alle Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre einfallen.  [][Welche Veränderung?]",
                                                             colnames(data_control_common_final))]
CTR_open_answers_nonmotor <- CTR_open_answers_nonmotor[,c(1,3,5,7,9)]

# frequency of general symptom in CTR 
CTR_open_answers_nonmotor_symptom_general <- apply(CTR_open_answers_nonmotor, 2, 
                                                   function(x) ifelse(x %in% EARLY_AllAnswers$Answer,EARLY_AllAnswers$`Symptom/Syndrome_General_1`,x))
CTR_open_answers_nonmotor_symptom_general_freq <- apply(CTR_open_answers_nonmotor_symptom_general, 2, 
                                                        function(x) ifelse(!is.na(x),1,0))
CTR_open_answers_nonmotor_symptom_general_freq <- as.data.frame(table(apply(CTR_open_answers_nonmotor_symptom_general_freq, 1, 
                                                                            sum))) %>% arrange(Freq)

# frequency of specific symptom in CTR 
CTR_open_answers_nonmotor_symptom_specific <- apply(CTR_open_answers_nonmotor, 2, 
                                                    function(x) ifelse(x %in% EARLY_AllAnswers$Answer,EARLY_AllAnswers$Symptom_Specific_1,x))
CTR_open_answers_nonmotor_symptom_specific_freq <- apply(CTR_open_answers_nonmotor_symptom_specific, 2, 
                                                         function(x) ifelse(!is.na(x),1,0))
CTR_open_answers_nonmotor_symptom_specific_freq <- as.data.frame(table(apply(CTR_open_answers_nonmotor_symptom_specific_freq, 1, 
                                                                             sum))) %>% arrange(Freq)

###### DEPRECATED
EARLY_AllAnswers_symptom_general_freq <- as.data.frame(table(c(EARLY_AllAnswers$`Symptom/Syndrome_General_1`,
                                                               EARLY_AllAnswers$`Symptom/Syndrome_General_2`))) %>% arrange(desc(Freq))
EARLY_AllAnswers_symptom_specific_freq <- as.data.frame(table(c(EARLY_AllAnswers$Symptom_Specific_1,
                                                                EARLY_AllAnswers$Symptom_Specific_2))) %>% arrange(desc(Freq))
EARLY_AllAnswers_symptom_general_unique <- c(EARLY_AllAnswers$`Symptom/Syndrome_General_1`,
                                             EARLY_AllAnswers$`Symptom/Syndrome_General_2`) %>% unique() %>% na.omit()
EARLY_AllAnswers_symptom_specific_unique <- c(EARLY_AllAnswers$Symptom_Specific_1,
                                              EARLY_AllAnswers$Symptom_Specific_2) %>% unique() %>% na.omit()

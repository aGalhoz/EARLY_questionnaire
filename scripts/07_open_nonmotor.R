## Open answers from non-motor symptoms

EARLY_AllAnswers <- read_excel("data input/EARLY_AllAnswers.xlsx")
general_symptoms_PL <- read_excel("data input/general_symptoms_PL_IC.xlsx")
specific_symptoms_PL <- read_excel("data input/specific_symptoms_PL.xlsx")

EARLY_AllAnswers$symptom_general_PL <- ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`neuro-motor`,
                                              "Neuro-motor",
                                              ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`sleep/fatigue`,
                                              "Sleep/fatigue",
                                              ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`skin/soft tissue`,
                                                     "Skin/soft tissue",
                                                     ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`gastro-metabolic`,
                                                            "Gastro-metabolic",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`Sensory and vestibular`,
                                                                                      "Sensory and vestibular", ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$urological,
                                                                                                                       "Urological",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$psychological,
                                                                                                       "Psychological",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`immuno-onco`,
                                                                                                                              "Immuno-onco",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$vegetative,
                                                                                                                                                          "Vegetative",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`body measures`,
                                                                                                                                                                              "Body measures",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$`injury/operation`,
                                                                                                                                                                                                     "Injury/operation",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$cardiovascular,
                                                                                                                                                                                                                               "Cardiovascular",ifelse(EARLY_AllAnswers$`Symptom/Syndrome_General_1` %in% general_symptoms_PL$other,
                                                                                                                                                                                                                                                       "Other",EARLY_AllAnswers$`Symptom/Syndrome_General_1`)))))))))))))
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
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
pdf("plots/pie_generalsymptoms.pdf")
ggplot(EARLY_AllAnswers_symptom_general_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Set2")+
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y", start = 0) +
  ggtitle("general symptoms") +
  #geom_label_repel(aes(label = Freq), size=4, show.legend = F, nudge_x = 1) +
  theme_void()
dev.off()

neuromuscular_freq <- as.data.frame(table(c(EARLY_AllAnswers[EARLY_AllAnswers$symptom_general_PL == "Neuro-motor",]$`Symptom/Syndrome_General_1`))) %>% arrange(desc(Freq))
neuromuscular_freq$Var1 <- factor(neuromuscular_freq$Var1,levels = neuromuscular_freq$Var1)
nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
pdf("plots/pie_neuromuscular.pdf")
ggplot(neuromuscular_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values = mycolors) +
  theme_void() +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  ggtitle("neuro-motor - general symptoms")
dev.off()

sensory_freq <- as.data.frame(table(c(EARLY_AllAnswers[EARLY_AllAnswers$symptom_general_PL == "Sensory and vestibular",]$`Symptom/Syndrome_General_1`))) %>% arrange(desc(Freq))
sensory_freq$Var1 <- factor(sensory_freq$Var1,levels = sensory_freq$Var1)
pdf("plots/pie_sensory.pdf")
ggplot(sensory_freq, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values = mycolors) +
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
ALS_open_answers_nonmotor_answers <- ALS_open_answers_nonmotor[,c(1,3,5,7,9)]

# frequency of general symptom in ALS 
ALS_open_answers_nonmotor_symptom_general <- apply(ALS_open_answers_nonmotor_answers, 2, 
                                                   function(x) ifelse(x %in% EARLY_AllAnswers$Answer,EARLY_AllAnswers$`Symptom/Syndrome_General_1`,x))
ALS_open_answers_nonmotor_symptom_general_freq <- apply(ALS_open_answers_nonmotor_symptom_general, 2, 
                                                        function(x) ifelse(!is.na(x),1,0))
ALS_open_answers_nonmotor_symptom_general_freq <- as.data.frame(table(apply(ALS_open_answers_nonmotor_symptom_general_freq, 1, 
                                                        sum))) %>% arrange(Freq)

# frequency of specific symptom in ALS 
ALS_open_answers_nonmotor_symptom_specific <- apply(ALS_open_answers_nonmotor_answers, 2, 
                                                   function(x) ifelse(x %in% EARLY_AllAnswers$Answer,EARLY_AllAnswers$Symptom_Specific_1,x))
ALS_open_answers_nonmotor_symptom_specific_freq <- apply(ALS_open_answers_nonmotor_symptom_specific, 2, 
                                                        function(x) ifelse(!is.na(x),1,0))
ALS_open_answers_nonmotor_symptom_specific_freq <- as.data.frame(table(apply(ALS_open_answers_nonmotor_symptom_specific_freq, 1, 
                                                                            sum))) %>% arrange(Freq)

# -> check on time information of general symptoms
get_oldest_date <- function(x, cutoff = as.Date("1980-01-01")) {
  # keep only values that look like dd/mm/yyyy
  valid_strs <- x[grepl("^\\d{2}/\\d{2}/\\d{4}$", x)]
  if (length(valid_strs) == 0) return(as.Date(NA))
  
  # convert to Date
  dates <- as.Date(valid_strs, format = "%d/%m/%Y")
  
  # treat dates before cutoff as invalid
  dates[!is.na(dates) & dates < cutoff] <- as.Date(NA)
  
  # return oldest valid date (or NA as Date)
  if (all(is.na(dates))) return(as.Date(NA))
  min(dates, na.rm = TRUE)
}
ALS_open_answers_nonmotor$date <- format(as.Date(
  sapply(seq_len(nrow(data_first_symptom)), function(i) get_oldest_date(data_first_symptom[i, ])),
  origin = "1970-01-01"
),"%Y")
extract_year <- function(date_string) {
  year <- str_extract(date_string, "\\d{4}")  # Extracts the 4-digit year
  return(as.numeric(year))  # Convert to numeric
}
ALS_open_answers_nonmotor <- ALS_open_answers_nonmotor %>%
  mutate(date = as.numeric(str_extract(date, "\\d{4}")))  # Extracts the 4-digit year
seit_wann_cols <- grep("\\[Seit wann\\?\\]", names(ALS_open_answers_nonmotor), value = TRUE)
for (col in seit_wann_cols) {
  ALS_open_answers_nonmotor[[paste0("diff_", col)]] <-  ALS_open_answers_nonmotor$date - extract_year(ALS_open_answers_nonmotor[[col]])
}
which_symptoms_cols <- grep("\\[Welche Veränderung\\?\\]", names(ALS_open_answers_nonmotor), value = TRUE)

ALS_open_answers_nonmotor_long = ALS_open_answers_nonmotor[c(1,3,5,7,9)] %>%
  pivot_longer(cols = all_of(which_symptoms_cols), 
               names_to = "symptom", 
               values_to = "which_symptom") %>%
  mutate(change_year = as.numeric(str_extract(change_year, "\\d{4}"))) %>%
  mutate(year_difference = change_year - birth_year)  # Compute difference
neuromuscular_questions <- EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Neuro-motor")
sensory_questions <- EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Sensory and vestibular")
sleep_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Sleep/fatigue")
psychological_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Psychological")
bodymeasures_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Body measures")
cardiovascular_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Cardiovascular")
immune_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Immuno-onco")
injuries_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Injury/operation")
skin_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Skin/soft tissue")
gastro_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Gastro-metabolic")
others_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Other")
urological_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Urological")
vegetative_questions = EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Vegetative")



ALS_open_answers_nonmotor_neuromuscular = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% neuromuscular_questions$Answer |
                                                                      ALS_open_answers_nonmotor[,3] %in% neuromuscular_questions$Answer |
                                                                      ALS_open_answers_nonmotor[,5] %in% neuromuscular_questions$Answer | 
                                                                      ALS_open_answers_nonmotor[,7] %in% neuromuscular_questions$Answer | 
                                                                      ALS_open_answers_nonmotor[,9] %in% neuromuscular_questions$Answer,]
ALS_open_answers_nonmotor_sensory = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% sensory_questions$Answer |
                                                                      ALS_open_answers_nonmotor[,2] %in% sensory_questions$Answer |
                                                                      ALS_open_answers_nonmotor[,3] %in% sensory_questions$Answer | 
                                                                      ALS_open_answers_nonmotor[,4] %in% sensory_questions$Answer | 
                                                                      ALS_open_answers_nonmotor[,5] %in% sensory_questions$Answer,]
writexl::write_xlsx(ALS_open_answers_nonmotor_neuromuscular,"ALS_open_answers_nonmotor_neuromuscular.xlsx")
writexl::write_xlsx(ALS_open_answers_nonmotor_sensory,"ALS_open_answers_nonmotor_sensory.xlsx")

ALS_open_answers_nonmotor_sleep = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% sleep_questions$Answer |
                                                                ALS_open_answers_nonmotor[,2] %in% sleep_questions$Answer |
                                                                ALS_open_answers_nonmotor[,3] %in% sleep_questions$Answer | 
                                                                ALS_open_answers_nonmotor[,4] %in% sleep_questions$Answer | 
                                                                ALS_open_answers_nonmotor[,5] %in% sleep_questions$Answer,]

ALS_open_answers_nonmotor_psychological = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% psychological_questions$Answer |
                                                              ALS_open_answers_nonmotor[,2] %in% psychological_questions$Answer |
                                                              ALS_open_answers_nonmotor[,3] %in% psychological_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,4] %in% psychological_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,5] %in% psychological_questions$Answer,]
ALS_open_answers_nonmotor_bodymeasures = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% bodymeasures_questions$Answer |
                                                              ALS_open_answers_nonmotor[,2] %in% bodymeasures_questions$Answer |
                                                              ALS_open_answers_nonmotor[,3] %in% bodymeasures_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,4] %in% bodymeasures_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,5] %in% bodymeasures_questions$Answer,]

ALS_open_answers_nonmotor_cardiovascular = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% cardiovascular_questions$Answer |
                                                              ALS_open_answers_nonmotor[,2] %in% cardiovascular_questions$Answer |
                                                              ALS_open_answers_nonmotor[,3] %in% cardiovascular_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,4] %in% cardiovascular_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,5] %in% cardiovascular_questions$Answer,]

ALS_open_answers_nonmotor_immune = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% immune_questions$Answer |
                                                              ALS_open_answers_nonmotor[,2] %in% immune_questions$Answer |
                                                              ALS_open_answers_nonmotor[,3] %in% immune_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,4] %in% immune_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,5] %in% immune_questions$Answer,]

ALS_open_answers_nonmotor_injuries = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% injuries_questions$Answer |
                                                              ALS_open_answers_nonmotor[,2] %in% injuries_questions$Answer |
                                                              ALS_open_answers_nonmotor[,3] %in% injuries_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,4] %in% injuries_questions$Answer | 
                                                              ALS_open_answers_nonmotor[,5] %in% injuries_questions$Answer,]

ALS_open_answers_nonmotor_skin = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% skin_questions$Answer |
                                                                       ALS_open_answers_nonmotor[,2] %in% skin_questions$Answer |
                                                                       ALS_open_answers_nonmotor[,3] %in% skin_questions$Answer | 
                                                                       ALS_open_answers_nonmotor[,4] %in% skin_questions$Answer | 
                                                                       ALS_open_answers_nonmotor[,5] %in% skin_questions$Answer,]

ALS_open_answers_nonmotor_gastro = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% gastro_questions$Answer |
                                                               ALS_open_answers_nonmotor[,2] %in% gastro_questions$Answer |
                                                               ALS_open_answers_nonmotor[,3] %in% gastro_questions$Answer | 
                                                               ALS_open_answers_nonmotor[,4] %in% gastro_questions$Answer | 
                                                               ALS_open_answers_nonmotor[,5] %in% gastro_questions$Answer,]

ALS_open_answers_nonmotor_others = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% others_questions$Answer |
                                                                 ALS_open_answers_nonmotor[,2] %in% others_questions$Answer |
                                                                 ALS_open_answers_nonmotor[,3] %in% others_questions$Answer | 
                                                                 ALS_open_answers_nonmotor[,4] %in% others_questions$Answer | 
                                                                 ALS_open_answers_nonmotor[,5] %in% others_questions$Answer,]

ALS_open_answers_nonmotor_urological = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% urological_questions$Answer |
                                                               ALS_open_answers_nonmotor[,2] %in% urological_questions$Answer |
                                                               ALS_open_answers_nonmotor[,3] %in% urological_questions$Answer | 
                                                               ALS_open_answers_nonmotor[,4] %in% urological_questions$Answer | 
                                                               ALS_open_answers_nonmotor[,5] %in% urological_questions$Answer,]

ALS_open_answers_nonmotor_vegetative = ALS_open_answers_nonmotor[ALS_open_answers_nonmotor[,1] %in% vegetative_questions$Answer |
                                                               ALS_open_answers_nonmotor[,2] %in% vegetative_questions$Answer |
                                                               ALS_open_answers_nonmotor[,3] %in% vegetative_questions$Answer | 
                                                               ALS_open_answers_nonmotor[,4] %in% vegetative_questions$Answer | 
                                                               ALS_open_answers_nonmotor[,5] %in% vegetative_questions$Answer,]

# median neuro-motor
diff_neuromuscular = c(ALS_open_answers_nonmotor_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                       ALS_open_answers_nonmotor_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                       ALS_open_answers_nonmotor_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                       ALS_open_answers_nonmotor_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                       ALS_open_answers_nonmotor_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
library(EnvStats)
median(na.omit(diff_neuromuscular))
IQR(na.omit(diff_neuromuscular))
summaryStats(na.omit(diff_neuromuscular), quartiles = TRUE) 
mean(na.omit(diff_neuromuscular))

# median sensory
diff_sensory = c(ALS_open_answers_nonmotor_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                 ALS_open_answers_nonmotor_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                 ALS_open_answers_nonmotor_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                 ALS_open_answers_nonmotor_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                 ALS_open_answers_nonmotor_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
median(na.omit(diff_sensory))
iqr(na.omit(diff_sensory))
summaryStats(na.omit(diff_sensory), quartiles = TRUE) 
mean(na.omit(diff_sensory))

answers_neuromuscular <- which(ALS_open_answers_nonmotor[,1] %in% neuromuscular_questions$Answer |
        ALS_open_answers_nonmotor[,2] %in% neuromuscular_questions$Answer |
        ALS_open_answers_nonmotor[,3] %in% neuromuscular_questions$Answer | 
        ALS_open_answers_nonmotor[,4] %in% neuromuscular_questions$Answer | 
        ALS_open_answers_nonmotor[,5] %in% neuromuscular_questions$Answer)

answers_sensory <- which(ALS_open_answers_nonmotor[,1] %in% sensory_questions$Answer |
                           ALS_open_answers_nonmotor[,2] %in% sensory_questions$Answer |
                           ALS_open_answers_nonmotor[,3] %in% sensory_questions$Answer | 
                           ALS_open_answers_nonmotor[,4] %in% sensory_questions$Answer | 
                           ALS_open_answers_nonmotor[,5] %in% sensory_questions$Answer)

answers_neuromuscular[answers_neuromuscular %in% answers_sensory] # only 18 patients reported both sensory and neuromuscular

# distribution of all categories 
# -> sleep
diff_sleep = c(ALS_open_answers_nonmotor_sleep$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
               ALS_open_answers_nonmotor_sleep$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
               ALS_open_answers_nonmotor_sleep$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
               ALS_open_answers_nonmotor_sleep$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
               ALS_open_answers_nonmotor_sleep$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> psychological
diff_psychological = c(ALS_open_answers_nonmotor_psychological$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                       ALS_open_answers_nonmotor_psychological$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                       ALS_open_answers_nonmotor_psychological$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                       ALS_open_answers_nonmotor_psychological$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                       ALS_open_answers_nonmotor_psychological$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> body measures
diff_bodymeasures = c(ALS_open_answers_nonmotor_bodymeasures$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                      ALS_open_answers_nonmotor_bodymeasures$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                      ALS_open_answers_nonmotor_bodymeasures$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                      ALS_open_answers_nonmotor_bodymeasures$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                      ALS_open_answers_nonmotor_bodymeasures$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> cardiovascular
diff_cardiovascular = c(ALS_open_answers_nonmotor_cardiovascular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                        ALS_open_answers_nonmotor_cardiovascular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                        ALS_open_answers_nonmotor_cardiovascular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                        ALS_open_answers_nonmotor_cardiovascular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                        ALS_open_answers_nonmotor_cardiovascular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> immune
diff_immune = c(ALS_open_answers_nonmotor_immune$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                ALS_open_answers_nonmotor_immune$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                ALS_open_answers_nonmotor_immune$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                ALS_open_answers_nonmotor_immune$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                ALS_open_answers_nonmotor_immune$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> injuries
diff_injuries = c(ALS_open_answers_nonmotor_injuries$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                  ALS_open_answers_nonmotor_injuries$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                  ALS_open_answers_nonmotor_injuries$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                  ALS_open_answers_nonmotor_injuries$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                  ALS_open_answers_nonmotor_injuries$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> skin
diff_skin = c(ALS_open_answers_nonmotor_skin$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
              ALS_open_answers_nonmotor_skin$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
              ALS_open_answers_nonmotor_skin$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
              ALS_open_answers_nonmotor_skin$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
              ALS_open_answers_nonmotor_skin$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> gastro
diff_gastro = c(ALS_open_answers_nonmotor_gastro$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                ALS_open_answers_nonmotor_gastro$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                ALS_open_answers_nonmotor_gastro$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                ALS_open_answers_nonmotor_gastro$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                ALS_open_answers_nonmotor_gastro$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)
# -> others

# -> urological

# -> vegetative

#### figure of distribution of when open answers were done for non-motor and sensory 
# -> data
neuro = as.numeric(na.omit(diff_neuromuscular))
sensory = as.numeric(na.omit(diff_sensory))

df = tibble(
  years = c(neuro, sensory),
  symptom = rep(c("Neuro/motor", "Sensory/vestibular"),
                times = c(length(neuro), length(sensory)))
) %>%
  mutate(
    period = case_when(
      years > 0  ~ "Pre-onset",
      years == 0 ~ "Onset",
      years < 0  ~ "Post-onset"
    ),
    period = factor(period, levels = c("Pre-onset", "Onset", "Post-onset"))
  )

# -> plot
palette <- c("Neuro/motor" = "#66C2A5", "Sensory/vestibular" = "#B1A783")
plot_neuro_sensory <- ggplot(df, aes(x = years, fill = symptom)) +
  # annotate("rect", xmin = min(df$years), xmax = 0, ymin = 0, ymax = Inf,
  #          fill = "grey90", alpha = 0.6) +
  # annotate("rect", xmin = 0, xmax = max(df$years), ymin = 0, ymax = Inf,
  #          fill = "white", alpha = 0.5) +
  geom_density(alpha = 0.45, adjust = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = palette) +
  scale_x_reverse(
    name = "Years",
    breaks = c(40, 20, 10, 0, -10, -20, -40),
    labels = c("40", "20", "10", "Onset", "10", "20", "40")
  ) +
  # Text annotations for clarity
  annotate("text", x = 27, y = 0.02, label = "Pre-onset", size = 4.35, color = "gray30", fontface = "italic") +
  annotate("text", x = -29, y = 0.02, label = "Post-onset", size = 4.35, color = "gray30", fontface = "italic") +
  # Labels
  labs(
    title = "Time distribution of symptoms",
    y = "Density",
    fill = "Symptom"
  ) +
  # Theme adjustments for manuscript
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
    axis.title = element_text(size = 12.5),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

pdf("plots/distribution_neuro_motor.pdf",width = 8,height = 6)
plot_neuro_sensory
dev.off()


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

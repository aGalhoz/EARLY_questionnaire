EARLY_AllAnswers <- read_excel("data input/EARLY_AllAnswers.xlsx")
general_symptoms_PL <- read_excel("data input/general_symptoms_PL_IC.xlsx")
specific_symptoms_PL <- read_excel("data input/specific_symptoms_PL.xlsx")

# --- Convert wide reference tables to long lookup tables ---
general_lookup <- general_symptoms_PL %>%
  pivot_longer(cols = everything(),
               names_to = "category_general",
               values_to = "symptom") %>%
  filter(!is.na(symptom))

specific_lookup <- specific_symptoms_PL %>%
  pivot_longer(cols = everything(),
               names_to = "category_specific",
               values_to = "symptom") %>%
  filter(!is.na(symptom))

EARLY_AllAnswers <- EARLY_AllAnswers %>%
  left_join(general_lookup,
            by = c("Symptom/Syndrome_General_1" = "symptom")) %>%
  left_join(specific_lookup,
            by = c("Symptom_Specific_1" = "symptom")) %>%
  dplyr::rename(symptom_general_PL = category_general,
         symptom_specific_PL = category_specific)

EARLY_AllAnswers <- EARLY_AllAnswers %>%
  mutate(symptom_general_PL = case_when(
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`neuro-motor` ~ "Neuro-motor",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`sleep/fatigue` ~ "Sleep/fatigue",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`skin/soft tissue` ~ "Skin/soft tissue",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`gastro-metabolic` ~ "Gastro-metabolic",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`Sensory and vestibular` ~ "Sensory and vestibular",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`urological` ~ "Urological",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`psychological` ~ "Psychological",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`immuno-onco` ~ "Immuno-onco",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`vegetative` ~ "Vegetative",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`body measures` ~ "Body measures",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`injury/operation` ~ "Injury/operation",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`cardiovascular` ~ "Cardiovascular",
    `Symptom/Syndrome_General_1` %in% general_symptoms_PL$`other` ~ "Other",
    TRUE ~ `Symptom/Syndrome_General_1`  # keep original if not in any category
  ))

# Replace NAs if original symptom is NA
EARLY_AllAnswers$symptom_general_PL <- ifelse(
  is.na(EARLY_AllAnswers$`Symptom/Syndrome_General_1`),
  NA,
  EARLY_AllAnswers$symptom_general_PL
)

# Specific symptom mapping
EARLY_AllAnswers <- EARLY_AllAnswers %>%
  mutate(symptom_specific_PL = case_when(
    Symptom_Specific_1 %in% specific_symptoms_PL$`neuro-motor` ~ "Neuro-motor",
    Symptom_Specific_1 %in% specific_symptoms_PL$sensory ~ "Sensory",
    Symptom_Specific_1 %in% specific_symptoms_PL$psychological ~ "Psychological",
    Symptom_Specific_1 %in% specific_symptoms_PL$oncological ~ "Oncological",
    Symptom_Specific_1 %in% specific_symptoms_PL$gastrointestinal ~ "Gastrointestinal",
    Symptom_Specific_1 %in% specific_symptoms_PL$urological ~ "Urological",
    Symptom_Specific_1 %in% specific_symptoms_PL$immunological ~ "Immunological",
    Symptom_Specific_1 %in% specific_symptoms_PL$other ~ "Other",
    TRUE ~ Symptom_Specific_1  # keep original if not in any category
  ))

# Replace NAs if original symptom is NA
EARLY_AllAnswers$symptom_specific_PL <- ifelse(
  is.na(EARLY_AllAnswers$Symptom_Specific_1),
  NA,
  EARLY_AllAnswers$symptom_specific_PL
)

# open answers ALS
# --- Function to extract numeric year from date strings ---
extract_year <- function(date_str) {
  y <- str_extract(date_str, "\\d{4}")
  as.numeric(y)
}

# --- Function to compute time differences ---
compute_diff <- function(df, date_col, seit_wann_cols) {
  df %>%
    mutate(ref_year = extract_year({{date_col}})) %>%
    rowwise() %>%
    mutate(across(all_of(seit_wann_cols), 
                  ~ ref_year - extract_year(.), 
                  .names = "diff_{.col}")) %>%
    ungroup()
}

# Compute summary stats
summary_stats <- function(vec) {
  vec <- na.omit(vec)
  list(
    median = median(vec),
    mean = mean(vec),
    IQR = IQR(vec),
    summary = EnvStats::summaryStats(vec, quartiles = TRUE)
  )
}

# Select columns for open answers
ALS_open_answers <- ALS_common[,grep("Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen. [][Welche Veränderung?]...",
                                              colnames(ALS_common))]
ALS_open_answers_answers <- ALS_open_answers[, c(1,3,5,7,9)]
ALS_open_answers_answers$sex <- ALS_common$`Bitte geben Sie Ihr Geschlecht an.`

# Compute general and specific symptom frequencies
ALS_open_answers_symptom_general <- apply(ALS_open_answers_answers, 2, 
                                                   function(x) ifelse(x %in% EARLY_AllAnswers$Answer,
                                                                      EARLY_AllAnswers$symptom_general_PL, x))
ALS_open_answers_symptom_general_freq <- get_freq_table(apply(ALS_open_answers_symptom_general, 1, function(x) sum(!is.na(x))))

ALS_open_answers_symptom_specific <- apply(ALS_open_answers_answers, 2, 
                                                    function(x) ifelse(x %in% EARLY_AllAnswers$Answer,
                                                                       EARLY_AllAnswers$Symptom_Specific_1, x))
ALS_open_answers_symptom_specific_freq <- get_freq_table(apply(ALS_open_answers_symptom_specific, 1, function(x) sum(!is.na(x))))

ALS_open_answers_symptom_general <- as.data.frame(ALS_open_answers_symptom_general)
ALS_open_answers_symptom_general$sex = ALS_common$`Bitte geben Sie Ihr Geschlecht an.`
ALS_open_answers_symptom_general$bulbar_spinal = spinal_bulbar$spinal_or_bulbar

# -> check on time information of general symptoms
# Identify "Seit wann?" columns
seit_wann_cols <- grep("\\[Seit wann\\?\\]", names(ALS_open_answers), value = TRUE)

# Compute difference from reference year
ALS_open_answers <- compute_diff(ALS_open_answers, date_col = date, seit_wann_cols)


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

ALS_open_answers$date <- format(as.Date(
  sapply(seq_len(nrow(data_first_symptom)), function(i) get_oldest_date(data_first_symptom[i, ])),
  origin = "1970-01-01"
),"%Y")

extract_year <- function(date_string) {
  year <- str_extract(date_string, "\\d{4}")  # Extracts the 4-digit year
  return(as.numeric(year))  # Convert to numeric
}
ALS_open_answers <- ALS_open_answers %>%
  mutate(date = as.numeric(str_extract(date, "\\d{4}")))  # Extracts the 4-digit year
seit_wann_cols <- grep("\\[Seit wann\\?\\]", names(ALS_open_answers), value = TRUE)
for (col in seit_wann_cols) {
  ALS_open_answers[[paste0("diff_", col)]] <-  ALS_open_answers$date - extract_year(ALS_open_answers[[col]])
}

ALS_open_answers_tmp = ALS_open_answers

for (i in 1:5) {
  for (j in 1:nrow(ALS_open_answers_tmp)) {
    ALS_open_answers_tmp[j,2*(i-1)+1] = ifelse(ALS_open_answers_tmp[j,11+i]<0,
                                               NA,
                                               ALS_open_answers_tmp[j,2*(i-1)+1])
  }
}
 
neuromuscular_questions <- EARLY_AllAnswers %>%
  filter(symptom_general_PL == "neuro-motor") %>%
  pull(Answer)

sensory_questions <- EARLY_AllAnswers %>%
  filter(symptom_general_PL == "Sensory and vestibular") %>%
  pull(Answer)

# Select relevant columns in ALS_open_answers_nonmotor
relevant_cols <- c(1,3,5,7,9)  # columns with open answers

# Subset for neuromuscular
ALS_open_answers_neuromuscular <- ALS_open_answers_tmp
ALS_open_answers_neuromuscular[, relevant_cols] <- lapply(ALS_open_answers_tmp[, relevant_cols], 
                                                   function(x) ifelse(x %in% neuromuscular_questions, x, NA))
for(i in seq_along(relevant_cols)) {
  main_col <- relevant_cols[i]
  related_col <- main_col + 1  
  related_col2 = c(12,13,14,15,16)[i]
  ALS_open_answers_neuromuscular[[related_col]] <- ifelse(
    is.na(ALS_open_answers_neuromuscular[[main_col]]),
    NA,
    ALS_open_answers_neuromuscular[[related_col]]
  )
  ALS_open_answers_neuromuscular[[related_col2]] <- ifelse(
    is.na(ALS_open_answers_neuromuscular[[main_col]]),
    NA,
    ALS_open_answers_neuromuscular[[related_col2]]
  )
}

# Keep only rows with at least one neuromuuscular answer
ALS_open_answers_neuromuscular <- ALS_open_answers_neuromuscular %>%
  filter(rowSums(!is.na(across(all_of(relevant_cols)))) > 0)

# Subset for sensory
sensory_cols <- c(1,3,5,7,9)  # columns with sensory answers
ALS_open_answers_sensory <- ALS_open_answers_tmp
ALS_open_answers_sensory[, sensory_cols] <- lapply(ALS_open_answers_tmp[, sensory_cols], 
                                                  function(x) ifelse(x %in% sensory_questions, x, NA))
for(i in seq_along(sensory_cols)) {
  main_col <- sensory_cols[i]
  related_col <- main_col + 1  
  related_col2 = c(12,13,14,15,16)[i]
  ALS_open_answers_sensory[[related_col]] <- ifelse(
    is.na(ALS_open_answers_sensory[[main_col]]),
    NA,
    ALS_open_answers_sensory[[related_col]]
  )
  ALS_open_answers_sensory[[related_col2]] <- ifelse(
    is.na(ALS_open_answers_sensory[[main_col]]),
    NA,
    ALS_open_answers_sensory[[related_col2]]
  )
}

# Keep only rows with at least one sensory answer
ALS_open_answers_sensory <- ALS_open_answers_sensory %>%
  filter(rowSums(!is.na(across(all_of(sensory_cols)))) > 0)

writexl::write_xlsx(ALS_open_answers_neuromuscular,"ALS_open_answers_neuromuscular.xlsx")
writexl::write_xlsx(ALS_open_answers_sensory,"ALS_open_answers_sensory.xlsx")

# median neuro-motor
diff_neuromuscular = c(ALS_open_answers_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                       ALS_open_answers_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                       ALS_open_answers_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                       ALS_open_answers_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                       ALS_open_answers_neuromuscular$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)

# median sensory
diff_sensory = c(ALS_open_answers_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...58`,
                 ALS_open_answers_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...60`,
                 ALS_open_answers_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...62`,
                 ALS_open_answers_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...64`,
                 ALS_open_answers_sensory$`diff_Bitte beschreiben Sie all diese Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre vor Beginn der Motoneuronerkrankung einfallen.  [][Seit wann?]...66`)

compute_summary <- function(x, symptom_name) {
  x_clean <- na.omit(x)
  data.frame(
    Symptom   = symptom_name,
    N         = length(x_clean),
    Mean      = round(mean(x_clean), 4),
    SD        = round(sd(x_clean), 4),
    Median    = median(x_clean),
    Min       = min(x_clean),
    Max       = max(x_clean),
    `1st Qu.` = quantile(x_clean, 0.25),
    `3rd Qu.` = quantile(x_clean, 0.75)
  )
}

# Compute summaries (positive diff means year_onset > year_answer, answer was pre-onset)
# -> changing values to become more intuitive: negative means before onset, positve after onset
neuromuscular_df <- compute_summary(diff_neuromuscular, "neuro-motor")
sensory_df       <- compute_summary(diff_sensory, "Sensory/vestibular")

combined_summary <- bind_rows(neuromuscular_df, sensory_df)
combined_summary

#### figure of distribution of when open answers were done for non-motor and sensory 
# -> data
neuro = (-1)*as.numeric(na.omit(diff_neuromuscular))
sensory = (-1)*as.numeric(na.omit(diff_sensory))

df = tibble(
  years = c(neuro, sensory),
  symptom = rep(c("Neuro/motor", "Sensory/vestibular"),
                times = c(length(neuro), length(sensory)))
) %>%
  mutate(
    period = case_when(
      years == 0 ~ "Onset",
      years < 0  ~ "Pre-onset"
    ),
    period = factor(period, levels = c("Pre-onset", "Onset", "Post-onset"))
  ) %>%
  mutate(median_year = ifelse(symptom == "Neuro/motor",-1,-2))

# -> plot
palette <- c("Neuro/motor" = "#66C2A5", "Sensory/vestibular" = "#B1A783")
plot_neuro_sensory <- ggplot(df, aes(x = years, fill = symptom)) +
  # annotate("rect", xmin = min(df$years), xmax = 0, ymin = 0, ymax = Inf,
  #          fill = "grey90", alpha = 0.6) +
  # annotate("rect", xmin = 0, xmax = max(df$years), ymin = 0, ymax = Inf,
  #          fill = "white", alpha = 0.5) +
  geom_density(alpha = 0.45, adjust = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # geom_vline(data = df, aes(xintercept = median_year, color = symptom),
  #            linetype = "dotted", size = 1) +  # dotted vertical lines for medians
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +  
  scale_x_continuous(name = "Years",
                     breaks = c(-30, -20, -10, 0,1),
                     labels = c("30", "20", "10", "Onset","")) +
  # Text annotations for clarity
  annotate("text", x = -25, y = 0.03, label = "Pre-onset", size = 4.35, color = "gray30", fontface = "italic") +
 # annotate("text", x = 27, y = 0.03, label = "Post-onset", size = 4.35, color = "gray30", fontface = "italic") +
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

# --- Frequency tables function ---
get_freq_table <- function(symptom_column) {
  as.data.frame(table(symptom_column)) %>% arrange(desc(Freq))
}

# how many IDs answered
ALS_open_answers_tmp %>%
  filter(rowSums(!is.na(across(all_of(relevant_cols)))) > 0)

ALS_open_answers_tmp %>%
  filter(rowSums(!is.na(across(all_of(relevant_cols)))) >= 2)

new_answers = c(ALS_open_answers_tmp[,1] %>% pull(),
                ALS_open_answers_tmp[,3] %>% pull(),
                ALS_open_answers_tmp[,5] %>% pull(),
                ALS_open_answers_tmp[,7] %>% pull(),
                ALS_open_answers_tmp[,9] %>% pull())
EARLY_AllAnswers_new = EARLY_AllAnswers
EARLY_AllAnswers_new = EARLY_AllAnswers_new %>%
  filter(Answer %in% na.omit(new_answers))
freq_general <- get_freq_table(EARLY_AllAnswers_new$symptom_general_PL)
freq_specific <- get_freq_table(EARLY_AllAnswers_new$symptom_specific_PL)

# --- Save frequency tables ---
write_xlsx(freq_general, "data code output/freq_general_symptoms_PL.xlsx")
write_xlsx(freq_specific, "data code output/freq_specific_symptoms_PL.xlsx")

## Visualisation of open answers
plot_pie <- function(freq_df, title, colors = NULL, file = NULL) {
  if(is.null(colors)) {
    nb.cols <- nrow(freq_df)
    colors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  }
  p <- ggplot(freq_df, aes(x="", y=Freq, fill=Var1)) +
    geom_bar(width=1, stat="identity") +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = Freq), position = position_stack(vjust=0.5)) +
    coord_polar(theta = "y") +
    ggtitle(title) +
    theme_void()
  
  if(!is.null(file)) pdf(file, width=6, height=6); print(p); if(!is.null(file)) dev.off()
  return(p)
}

freq_general$Var1 <- factor(freq_general$symptom_column,levels = freq_general$symptom_column)
p = plot_pie(freq_general, "General Symptoms", file = "plots/pie_generalsymptoms.pdf")
pdf("plots/pie_generalsymptoms.pdf")
p
dev.off()

neuromuscular_freq <- as.data.frame(table(c(EARLY_AllAnswers_new[EARLY_AllAnswers_new$symptom_general_PL == "neuro-motor",]$`Symptom/Syndrome_General_1`))) %>% 
  arrange(desc(Freq))
neuromuscular_freq$Var1 <- factor(neuromuscular_freq$Var1,levels = neuromuscular_freq$Var1)
p = plot_pie(neuromuscular_freq, "Neuro-motor", file = "plots/pie_neuromuscular.pdf")
pdf("plots/pie_neuromuscular.pdf")
p
dev.off()

sensory_freq <- as.data.frame(table(c(EARLY_AllAnswers_new[EARLY_AllAnswers_new$symptom_general_PL == "Sensory and vestibular",]$`Symptom/Syndrome_General_1`))) %>% 
  arrange(desc(Freq))
sensory_freq$Var1 <- factor(sensory_freq$Var1,levels = sensory_freq$Var1)
p = plot_pie(sensory_freq, "Sensory and vestibular", file = "plots/pie_sensory.pdf")
pdf("plots/pie_sensory.pdf")
p
dev.off()


# open answers CTR
CTR_open_answers_nonmotor <- CTR_common[,grep("Bitte beschreiben Sie alle Veränderungen, die Ihnen in Bezug auf die letzten 10 Jahre einfallen.  [][Welche Veränderung?]",
                                                             colnames(CTR_common))]
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


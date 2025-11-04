# demographics & stats

Early_ALS_ALSFRS_R_dictionary_score <- read_excel("data input/Early_ALS ALSFRS R dictionary score.xlsx") 
unique_ALS_FRS_R <- read_excel("data input/unique_ALS-FRS-R.xlsx")
final_ALS_stats <- read_excel("data input/final_ALS_CTR_questionnaire_summary_IC.xlsx", sheet = "ALS")
other_ALStype_IC <- read_excel("data input/other_ALStype_IC_PL.xlsx",sheet = "Sonstiges")


# ALS specific questions
# -> ALS risk score
ALS_FRS_R_questions <- final_ALS_stats %>%
  filter(`ALS-FRS-R`==1)

data_ALS_FRS_R <- data_ALS_final[,c(which(colnames(data_ALS_final) %in% ALS_FRS_R_questions$`original question (ALS)`))] %>%
  mutate(ID = 1:nrow(.))

data_ALS_FRS_R_long <- melt(setDT(data_ALS_FRS_R), id.vars = "ID", variable.name = "Subquestion")
patient_ALS_FRS_R <- data_ALS_FRS_R_long %>%
  left_join(unique_ALS_FRS_R) %>%
  dplyr::group_by(ID) %>%
  distinct() %>%
  dplyr::summarise(ALSFRSR = sum(`Points`)) 

temp <- data_ALS_FRS_R_long %>%
  left_join(unique_ALS_FRS_R)
View(temp[is.na(temp$`Point to remove`),])
which(is.na(patient_ALS_FRS_R$ALSFRSR)) %>% length() # 34 did not answer the questions (are the patients which did not finalise the Q)
View(data_ALS_final[which(is.na(patient_ALS_FRS_R$ALSFRSR)),])
View(data_patients_common_final[which(is.na(patient_ALS_FRS_R$ALSFRSR)),])
writexl::write_xlsx(cbind(data_patients_common_final[which(is.na(patient_ALS_FRS_R$ALSFRSR)),],
                          data_ALS_final[which(is.na(patient_ALS_FRS_R$ALSFRSR)),]),
                    "patients_ALSFRSR_NA.xlsx")

ALS_FRS_R_stats <- summary(patient_ALS_FRS_R$ALSFRSR)
skim(patient_ALS_FRS_R$ALSFRSR)

# -> ALS progression (ALSFRSR - 48/DATE_QUESTIONNAIRE - DATE_FIRSTSYMPTOM)
first_symptom <- final_ALS_stats %>%
  filter(first_symptom==1)

data_first_symptom <- data_ALS_final[,c(which(colnames(data_ALS_final) %in% first_symptom$`original question (ALS)`))]
data_first_symptom <- apply(data_first_symptom, 2, function(x) format(as.Date(paste("01",x,sep="/"),"%d/%m/%Y"),"%d/%m/%Y"))
date_questionnaire <- format(as.Date(data_patients_common_final$`Datum letzte Aktivität`,format = "%Y-%m-%d"),"%d/%m/%Y")
diff_date <- apply(data_first_symptom, 2, function(x)  {
  (difftime(strptime(date_questionnaire, format = "%d/%m/%Y"),strptime(x, format = "%d/%m/%Y"))/365)*12})
diff_date_oldest <- apply(diff_date,1,function(x) {
  # remove ages that don't make sense like 1920, 1921, etc.
 # x <- na.omit(x[x<1200])
  ceiling(max(na.omit(x)))
})

View(data_first_symptom[which(diff_date_oldest== "-Inf"),]) ## 35 several with NA 
View(data_patients_common_final[which(diff_date_oldest== "-Inf"),])

ALS_progression <- (patient_ALS_FRS_R$ALSFRSR - 48)/diff_date_oldest # 34 with NA date from ALS-FRS-R

ALS_progression_stats <- skim(ALS_progression[diff_date_oldest!=(-Inf)]) # 479 (23 with Date NA from ALS-FRS-R)
n_slow_progressor <- skim(ALS_progression[ALS_progression < ALS_progression_stats$numeric.p50 & diff_date_oldest!=(-Inf)]) # 228
n_fast_progressor <- skim(ALS_progression[ALS_progression >= ALS_progression_stats$numeric.p50 & diff_date_oldest!=(-Inf)]) # 223 

# spinal & bulbar 
spinal <- final_ALS_stats %>%
  filter(`site of onset` == "spinal")
bulbar <- final_ALS_stats %>%
  filter(`site of onset` == "bulbar")

# -> spinal
data_first_symptom_spinal <- data_ALS_final[,c(which(colnames(data_ALS_final) %in% spinal$`original question (ALS)`))]
data_first_symptom_spinal <- apply(data_first_symptom_spinal, 2, function(x) format(as.Date(paste("01",x,sep="/"),"%d/%m/%Y"),"%d/%m/%Y"))
diff_date_spinal <- apply(data_first_symptom_spinal, 2, function(x) (as.numeric(difftime(strptime(date_questionnaire, format = "%d/%m/%Y"),
                                                                                                     strptime(x, format = "%d/%m/%Y"),units = "days")/365))*12)
diff_date_spinal_oldest <- apply(diff_date_spinal,1,function(x) {
  # remove ages that don't make sense like 1920, 1921, etc.
  x <- na.omit(x[x<800])
  ceiling(max(na.omit(x)))
})

# -> bulbar
data_first_symptom_bulbar <- data_ALS_final[,c(which(colnames(data_ALS_final) %in% bulbar$`original question (ALS)`))]
data_first_symptom_bulbar <- apply(data_first_symptom_bulbar, 2, function(x) format(as.Date(paste("01",x,sep="/"),"%d/%m/%Y"),"%d/%m/%Y"))
diff_date_bulbar <- apply(data_first_symptom_bulbar, 2, function(x) (as.numeric(difftime(strptime(date_questionnaire, format = "%d/%m/%Y"),
                                                                                         strptime(x, format = "%d/%m/%Y"),units = "days")/365))*12)
diff_date_bulbar_oldest <- apply(diff_date_bulbar,1,function(x) {
  # remove ages that don't make sense like 1920, 1921, etc.
  x <- na.omit(x[x<1200])
  ceiling(max(na.omit(x)))
})

spinal_bulbar <- data.frame(diff_spinal = diff_date_spinal_oldest,
                                             diff_bulbar = diff_date_bulbar_oldest)
spinal_bulbar <- spinal_bulbar %>%
  mutate(spinal_or_bulbar = ifelse(diff_spinal == (-Inf) & diff_bulbar == (-Inf),NA,
                                   ifelse(diff_spinal> diff_bulbar,"spinal","bulbar")))
spinal_bulbar_stats <- spinal_bulbar %>% 
  dplyr::group_by(spinal_or_bulbar) %>%
  dplyr::summarise(n_rows = n(),
                   freq = n()/nrow(spinal_bulbar)*100)

# ALS progression stratified by spinal and bulbar ALS
ALS_progression_spinal <- (patient_ALS_FRS_R[which(spinal_bulbar$spinal_or_bulbar == "spinal"),]$ALSFRSR - 48)/diff_date_spinal_oldest[which(spinal_bulbar$spinal_or_bulbar == "spinal")]
ALS_progression_spinal_stats <- skim(ALS_progression_spinal) # 387
slow_progressor_spinal_stats <- skim(ALS_progression_spinal[ALS_progression_spinal < ALS_progression_spinal_stats$numeric.p50]) # 193
fast_progressor_spinal_stats <- skim(ALS_progression_spinal[ALS_progression_spinal > ALS_progression_spinal_stats$numeric.p50]) # 193

ALS_progression_bulbar <- (patient_ALS_FRS_R[which(spinal_bulbar$spinal_or_bulbar == "bulbar"),]$ALSFRSR - 48)/diff_date_bulbar_oldest[which(spinal_bulbar$spinal_or_bulbar == "bulbar")]
ALS_progression_bulbar_stats <- skim(ALS_progression_bulbar) # 80
slow_progressor_bulbar_stats <- skim(ALS_progression_bulbar[ALS_progression_bulbar < ALS_progression_bulbar_stats$numeric.p50]) # 39
fast_progressor_bulbar_stats <- skim(ALS_progression_bulbar[ALS_progression_bulbar > ALS_progression_bulbar_stats$numeric.p50]) # 39

# check amount of patients under Riluzol or Edaravon treatment
skim(data_ALS_final$`Besteht aktuell eine Therapie mit Riluzol?`=="Ja")
skim(data_ALS_final$`Besteht aktuell eine Therapie mit Edaravone?`=="Ja")

# age at onset (age - years since onset)
years_since_onset <- ifelse(is.na(spinal_bulbar$spinal_or_bulbar),NA,
                            ifelse(spinal_bulbar$spinal_or_bulbar == "spinal",
                                   floor(spinal_bulbar$diff_spinal/12),
                                   floor(spinal_bulbar$diff_bulbar/12)))
spinal_bulbar$years_since_onset = years_since_onset
data_patients_common_final$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <- data_patients_new$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`
birth <- format(as.Date(paste("01",
                              data_patients_common_final$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`,
                              sep="/"),"%d/%m/%Y"),"%d/%m/%Y")
birth <- ifelse(birth == "01/10/0049","01/10/1949",birth)
age <- floor(as.numeric(difftime(strptime(date_questionnaire, format = "%d/%m/%Y"),
                                 strptime(birth, format = "%d/%m/%Y"))/365))

age_onset <- age - years_since_onset
age_onset_stats <- skim(age_onset) # 431 missing -> only 228 missing changed

# -> very young patient
young_patient_data = cbind(data_patients_common_final[303,],
                      data_ALS_final[303,])
writexl::write_xlsx(young_patient_data,"data_young_onset.xlsx")


# ALS subtype
data_ALS_subtype <- data_ALS_final[,"Welcher ALS-Subtyp besteht bei Ihnen?"] %>% as_tibble()
data_ALS_subtype_sonstiges <- data_ALS_final[,"Welcher ALS-Subtyp besteht bei Ihnen? [Sonstiges]"]  %>% as_tibble()
data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?` <- ifelse(data_ALS_subtype == "Sonstiges",
                                                                   data_ALS_subtype_sonstiges$`Welcher ALS-Subtyp besteht bei Ihnen? [Sonstiges]`,
                                                                   data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?`)
data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?` <- ifelse(data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?` %in% other_ALStype_IC$other_ALStype,
                                                                   other_ALStype_IC$Answer,
                                                                   data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?`)
data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?` <- ifelse(data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?` == "Flail arm",
                                                                   "Flail-arm-Syndrom",
                                                                   ifelse(data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?` == "Flail leg",
                                                                          "Flail-leg-Syndrom",
                                                                          ifelse(data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?`=="NA",
                                                                                                     "Unknown",data_ALS_subtype$`Welcher ALS-Subtyp besteht bei Ihnen?`)))
skim(data_ALS_subtype)
data_ALS_subtype_stats <- data_ALS_subtype %>% 
  mutate(ALS_type = `Welcher ALS-Subtyp besteht bei Ihnen?`) %>%
  dplyr::group_by(ALS_type) %>%
  dplyr::summarise(n_rows = n(),
                   freq = n()/nrow(data_ALS_subtype)*100)
 
# Familial ALS
data_ALS_familial <- data_ALS_final$`Gibt es weitere Familienmitglieder mit einer ALS-Diagnose?` %>% as.tibble()
skim(data_ALS_familial)
data_ALS_familial_stats <- data_ALS_familial %>% 
  dplyr::group_by(value) %>%
  dplyr::summarise(n_rows = n(),
                   freq = n()/nrow(data_ALS_familial)*100)

# Mutation in which gene
data_ALS_gene <- data_ALS_final$`In welchem Gen liegt die bei Ihnen nachgewiesene Mutation?` %>% as.tibble()
data_ALS_gene_sonstiges <- data_ALS_final$`In welchem Gen liegt die bei Ihnen nachgewiesene Mutation? [Sonstiges]` %>% as.tibble()
data_ALS_gene$value <- ifelse(data_ALS_gene$value == "Sonstiges",
                              data_ALS_gene_sonstiges$value,
                              data_ALS_gene$value)
data_ALS_gene_stats <- data_ALS_gene %>% 
  dplyr::group_by(value) %>%
  dplyr::summarise(n_rows = n(),
                   freq = n()/nrow(data_ALS_gene)*100)

# ALS & CTR questions (age & sex)
# -> age 
date_questionnaire_CTR <- format(as.Date(data_control_common_final$`Datum letzte Aktivität`,format = "%Y-%m-%d"),"%d/%m/%Y")
data_control_common_final$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <- data_control_new$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`
birth_CTR <- format(as.Date(paste("01",
                                  data_control_common_final$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.`,
                                  sep="/"),"%d/%m/%Y"),"%d/%m/%Y")
age_CTR <- floor(as.numeric(difftime(strptime(date_questionnaire_CTR, format = "%d/%m/%Y"),
                                     strptime(birth_CTR, format = "%d/%m/%Y"))/365))
age_CTR_stats <- skim(age_CTR)

# separation between Female and Male
# -> CTR
IDs_CTR_female <- data_control_common_final$`Bitte geben Sie Ihr Geschlecht an.` == "weiblich"
IDs_CTR_male <- data_control_common_final$`Bitte geben Sie Ihr Geschlecht an.` == "männlich"

age_CTR_female = age_CTR[IDs_CTR_female]
age_CTR_male = age_CTR[IDs_CTR_male]
age_CTR_female_stats = skim(age_CTR_female)
age_CTR_male_stats = skim(age_CTR_male)
t.test(na.omit(age_CTR_female),na.omit(age_CTR_male))

# -> ALS
IDs_female = data_patients_common_final$`Bitte geben Sie Ihr Geschlecht an.` == "weiblich"
IDs_male = data_patients_common_final$`Bitte geben Sie Ihr Geschlecht an.` == "männlich"

age_female = age[IDs_female]
age_male = age[IDs_male]
age_female_stats = skim(age_female)
age_male_stats = skim(age_male)
t.test(na.omit(age_female),na.omit(age_male))


age_CTR_ALS <- rbind(data.frame(type = "CTR",age = age_CTR),
                     data.frame(type = "ALS",age = age))
age_CTR_ALS_stats <- t.test(age ~ type, data = age_CTR_ALS)
pdf("plots/age_ALS_vs_CTR.pdf")
ggplot(age_CTR_ALS, aes(x=age, fill = type)) + 
  geom_histogram(alpha = .5, bins = 30, position = "identity") + 
  theme_classic() +
  scale_fill_manual(values=c("ALS"="#0073C2FF","CTR"="#EFC000FF")) 
dev.off()
# variables are not normal, so using Mann-Whitney test
als_ages <- na.omit(age)
control_ages <- na.omit(age_CTR)
wilcox.test(als_ages,control_ages) # using this one
t.test(als_ages,control_ages,var.equal = FALSE)

# -> Sex
sex_CTR <- data_control_common_final$`Bitte geben Sie Ihr Geschlecht an.`
sex_ALS <- data_patients_common_final$`Bitte geben Sie Ihr Geschlecht an.`
sex_CTR_stats <- skim(as.factor(sex_CTR))
sex_ALS_stats <- skim(as.factor(sex_ALS))

# statistics of gender
cont = as.table(rbind(c(200,178),c(305,121)))
dimnames(cont) = list(gender = c("F","M"),
                      group = c("ALS","CTR"))
chisq.test(cont)
fisher.test(cont)

# weight change
dat_weights <- dat[,c(1,3,4,5)]
dat_weights <- apply(dat_weights,2,as.numeric)
dat_weights <- data.frame(dat_weights) %>% mutate(status = status)
dat_weights_tmp <- dat_weights %>% group_by(status) %>% summarise_each(funs(mean(.[!is.na(.)]))) 
mean(data_ALS_final$`Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`,na.rm=T)
mean(data_ALS_final$`Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]`,na.rm=T)
lm(c(77.8,78.4,78.9,73.5) ~ c(1,2,3,4))

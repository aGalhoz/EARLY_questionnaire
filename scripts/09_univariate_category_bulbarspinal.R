### analysis of categories and subcategories stratified by bulbar/spinal

dat_final_spinalbulbar = dat_final %>%
  mutate(status_aux = c(spinal_bulbar$spinal_or_bulbar,
                        rep("CTR",305)))

# check if there is a signal with weight loss
weight_onset_diagnosis = data_ALS_weight$`Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]` - data_ALS_weight$`Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`
weight_diagnosis_now = data_ALS_weight$`Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]` - 
  dat_final_subquestions_dietweight_continuous[1:513,]$`Bitte geben Sie Ihr Gewicht an. [Aktuell][Gewicht [kg]]`

wilcox.test(weight_onset_diagnosis ~ status_aux, data = subset(dat_final_spinalbulbar, status2 == 1)) # not significant
wilcox.test(weight_diagnosis_now ~ status_aux, data = subset(dat_final_spinalbulbar, status2 == 1)) # not significant
wilcox.test(Bitte.geben.Sie.Ihr.Gewicht.an...Aktuell..Gewicht..kg.. ~ status_aux,
            data = subset(dat_final_spinalbulbar, status2 == 1)) # not significant

# check if there is any significance in the substance consumption 
dat_final_spinalbulbar$caffeine_use <- dat_final$Konsumieren.Sie.aktuell.regelmäßig.koffeinhaltige.Getränke.oder.haben.Sie.jemals.in.Ihrem.Leben.regelmäßig.koffeinhaltige.Getränke.konsumiert.
dat_final_spinalbulbar$alcohol_use <- dat_final$Konsumieren.Sie.aktuell.regelmäßig.alkoholische.Getränke.oder.haben.Sie.jemals.in.Ihrem.Leben.regelmäßig.alkoholische.Getränke.konsumiert.
dat_final_spinalbulbar$cigarette_use <- dat_final$Rauchen.Sie.aktuell.oder.haben.Sie.jemals.in.Ihrem.Leben.regelmäßig.Zigaretten..oder.Zigarren.etc...geraucht.

dat_final_spinalbulbar$caffeine_use = factor(dat_final_spinalbulbar$caffeine_use,
                                             levels = c("Nein..noch.nie", "Ja..in.der.Vergangenheit", "Ja..aktuell"),
                                             labels = c("Never", "Past", "Current"))
dat_final_spinalbulbar$alcohol_use = factor(dat_final_spinalbulbar$alcohol_use,
                                             levels = c("Nein..noch.nie", "Ja..in.der.Vergangenheit", "Ja..aktuell"),
                                             labels = c("Never", "Past", "Current"))
dat_final_spinalbulbar$cigarette_use = factor(dat_final_spinalbulbar$cigarette_use,
                                             levels = c("Nein..noch.nie", "Ja..in.der.Vergangenheit", "Ja..aktuell"),
                                             labels = c("Never", "Past", "Current"))
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux, dat_final_spinalbulbar[1:513,]$caffeine_use)) # not significant
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux, dat_final_spinalbulbar[1:513,]$alcohol_use)) # not signicantt
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux, dat_final_spinalbulbar[1:513,]$cigarette_use)) # nott significant

# check if there is any significance in the slipped disc report
dat_final_spinalbulbar$slipped_disc = dat_final[,220]
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux, dat_final_spinalbulbar[1:513,]$slipped_disc))

# check if there is any significance in prodromal conditions (trembling of arms or legs; excessive saliva; cold,pale extremities)
dat_final_spinalbulbar$trembling_arms_legs = ifelse(is.na(dat_final$Verschiedenes...Zittern.der.Arme.oder.Beine..Skala.1.),
                                                    "no","yes")
dat_final_spinalbulbar$excessive_saliva = ifelse(is.na(dat_final$Verschiedenes...Übermaß.an.Speichel.im.Mund..Skala.1.),
                                                    "no","yes")
dat_final_spinalbulbar$cold_pale_extremities = ifelse(is.na(dat_final$Haut...Gefühl...Kalte..blasse.oder.bläulich.verfärbte.Extremitäten...Skala.1.),
                                                 "no","yes")
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux, dat_final_spinalbulbar[1:513,]$trembling_arms_legs)) # not significant
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux, dat_final_spinalbulbar[1:513,]$excessive_saliva)) #  significant
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux, dat_final_spinalbulbar[1:513,]$cold_pale_extremities)) # not significant

# check if there is any significance in healthcare visits
neurology_visit = apply(data_patients_common_final[,grep("Neurologie",names(data_patients_common_final))],1,
                                                      function(z) ifelse(any(!is.na(z)),"yes","no"))
speech_therapy_visit = apply(data_patients_common_final[,grep("Logopädie",names(data_patients_common_final))],1,
                             function(z) ifelse(any(!is.na(z)),"yes","no"))
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux,neurology_visit)) # not significant
fisher.test(table(dat_final_spinalbulbar[1:513,]$status_aux,speech_therapy_visit)) #  significant



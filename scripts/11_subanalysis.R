### analysis of categories and subcategories stratified by bulbar/spinal

dat_final_spinalbulbar = dat_final %>%
  mutate(status_aux = c(spinal_bulbar$spinal_or_bulbar,
                        rep("CTR",285))) 

dat_final_spinalbulbar$status_aux <- factor(dat_final_spinalbulbar$status_aux,
                    levels = c("CTR", "spinal","bulbar"))

run_multinom_for_item <- function(data, item, group_var = "status_aux") {
  dat <- data %>%
    select(all_of(c(group_var, item))) %>%
    na.omit()
  
  if (nrow(dat) == 0) {
    return(tibble(
      item     = item,
      contrast = character(0),
      OR       = numeric(0),
      CI_low   = numeric(0),
      CI_high  = numeric(0),
      p_value  = numeric(0)
    ))
  }
  
  # Fit multinomial model
  form <- as.formula(paste(group_var, "~", item))
  fit  <- multinom(form, data = dat, trace = FALSE)
  summ <- summary(fit)
  
  coefs <- summ$coefficients   # rows: ALS_bulbar, ALS_spinal; cols: intercept + item
  ses   <- summ$standard.errors
  
  # Identify the column for the item (works for numeric or 0/1-coded factor)
  col_item <- grep(paste0("^", item), colnames(coefs))
  if (length(col_item) != 1) {
    warning("Item '", item, "' has ", length(col_item),
            " coefficient columns in multinom; recode to binary.")
    return(tibble())
  }
  
  # ALS_bulbar vs control
  beta_bulbar <- coefs["bulbar", col_item]
  se_bulbar   <- ses["bulbar",   col_item]
  OR_bulbar   <- exp(beta_bulbar)
  CI_bulbar   <- exp(beta_bulbar + c(-1.96, 1.96) * se_bulbar)
  z_bulbar    <- beta_bulbar / se_bulbar
  p_bulbar    <- 2 * pnorm(abs(z_bulbar), lower.tail = FALSE)
  
  # ALS_spinal vs control
  beta_spinal <- coefs["spinal", col_item]
  se_spinal   <- ses["spinal",   col_item]
  OR_spinal   <- exp(beta_spinal)
  CI_spinal   <- exp(beta_spinal + c(-1.96, 1.96) * se_spinal)
  z_spinal    <- beta_spinal / se_spinal
  p_spinal    <- 2 * pnorm(abs(z_spinal), lower.tail = FALSE)
  
  # For ALS_bulbar vs ALS_spinal, relevel reference to ALS_bulbar
  dat2 <- dat %>%
    mutate(group3_bulbar_ref = relevel(get(group_var), ref = "spinal"))
  
  form2 <- as.formula(paste("group3_bulbar_ref ~", item))
  fit2  <- multinom(form2, data = dat2, trace = FALSE)
  summ2 <- summary(fit2)
  coefs2 <- summ2$coefficients
  ses2   <- summ2$standard.errors
  
  col_item2 <- grep(paste0("^", item), colnames(coefs2))
  if (length(col_item2) != 1) {
    warning("Item '", item, "' has ", length(col_item2),
            " coefficient columns in multinom (bulbar ref); recode to binary.")
    return(tibble())
  }
  
  beta_bvsp <- coefs2["bulbar", col_item2]
  se_bvsp   <- ses2["bulbar",   col_item2]
  OR_bvsp   <- exp(beta_bvsp)
  CI_bvsp   <- exp(beta_bvsp + c(-1.96, 1.96) * se_bvsp)
  z_bvsp    <- beta_bvsp / se_bvsp
  p_bvsp    <- 2 * pnorm(abs(z_bvsp), lower.tail = FALSE)
  
  tibble(
    item     = item,
    contrast = c("bulbar vs CTR",
                 "spinal vs CTR",
                 "spinal vs bulbar"),
    OR       = c(OR_bulbar, OR_spinal, OR_bvsp),
    CI_low   = c(CI_bulbar[1], CI_spinal[1], CI_bvsp[1]),
    CI_high  = c(CI_bulbar[2], CI_spinal[2], CI_bvsp[2]),
    p_value  = c(p_bulbar, p_spinal, p_bvsp)
  )
}

# prodromal conditions (trembling of arms or legs; excessive saliva; cold,pale extremities)
dat_final_spinalbulbar$trembling_arms_legs = ifelse(is.na(dat_final$Verschiedenes...Zittern.der.Arme.oder.Beine..Skala.1.),
                                                    "No","Yes")
dat_final_spinalbulbar$excessive_saliva = ifelse(is.na(dat_final$Verschiedenes...Übermaß.an.Speichel.im.Mund..Skala.1.),
                                                 "No","Yes")
dat_final_spinalbulbar$cold_pale_extremities = ifelse(is.na(dat_final$Haut...Gefühl...Kalte..blasse.oder.bläulich.verfärbte.Extremitäten...Skala.1.),
                                                      "No","Yes")

prodromal_condition_spinal_bulbar = map_dfr(c("trembling_arms_legs",
                                              "excessive_saliva","cold_pale_extremities"),
                                            ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# healthcare visits
dat_final_spinalbulbar$neurology_visit = apply(data_combined[,grep("Neurologie",names(data_combined))],1,
                        function(z) ifelse(any(!is.na(z)),"Yes","No"))
dat_final_spinalbulbar$speech_therapy_visit = apply(data_combined[,grep("Logopädie",names(data_combined))],1,
                             function(z) ifelse(any(!is.na(z)),"Yes","No"))

healthcare_visit_spinal_bulbar = map_dfr(c("neurology_visit","speech_therapy_visit"),
                                            ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# check if there is any significance in the slipped disc report
dat_final_spinalbulbar$slipped_disc = ifelse(dat_final_tmp$Welche.Krankheits.des.Muskel.Skelett.Systems.liegt.bzw..lag.vor.und.seit.wann...Bandscheibenvorfall. == "Ja","Yes","No")
dat_final_spinalbulbar$musculosketel_disease = apply(dat_final_tmp[,grep("Muskel.Skelett",names(dat_final_tmp))],1,
        function(z) ifelse(any(z %in% "Ja"),"Yes","No"))

preexisting_conditions_spinal_bulbar = map_dfr(c("slipped_disc","musculosketel_disease"),
                                         ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# lifestyle - single
dat_final_spinalbulbar$single = ifelse(dat_final$Wie.ist.aktuell.Ihr.Partnerschaftsstatus. == "Alleinstehend","Yes","No")

single_spinal_bulbar = map_dfr(c("single"),~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# lifestyle - have children
dat_final_spinalbulbar$children = ifelse(dat_final$Haben.Sie.eigene.Kinder. == "Ja","Yes","No")

children_spinal_bulbar = map_dfr(c("children"),~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# lifestyle - educational attainment
dat_final_spinalbulbar$lower_secondary_school = ifelse(dat_final$Was.ist.Ihr.höchster.schulischer.Abschluss. == "Hauptschule","Yes","No")
dat_final_spinalbulbar$vocational_school = ifelse(dat_final$Was.ist.Ihr.höchster.schulischer.Abschluss. == "Berufsschule","Yes","No")
dat_final_spinalbulbar$other_school = ifelse(dat_final$Was.ist.Ihr.höchster.schulischer.Abschluss. == "Sonstiges","Yes","No")

educational_spinal_bulbar = map_dfr(c("lower_secondary_school","vocational_school",
                                      "other_school"), ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# lifestyle - professional attainment
dat_final_spinalbulbar$volcational_degree = ifelse(dat_final$Was.ist.Ihr.höchster.beruflicher.Abschluss. == "Fachschule...Technikerschule...Handelsakademie","Yes","No")

professional_spinal_bulbar = map_dfr(c("volcational_degree"), ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# lifestyle - physical activity in professional activites
dat_final_spinalbulbar$intensive_job = ifelse(dat_final$Bitte.beschreiben.Sie.das.Maß.an.körperlicher.Aktivität.im.Rahmen.Ihrer.beruflichen.Tätigkeit. == "Intensiv..z.B..Bauarbeiten.",
                                 "Yes","No")

intensive_job_spinal_bulbar = map_dfr(c("intensive_job"), ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# lifestyle - physical activity in recreational activites
dat_final_spinalbulbar$intensive_sport = ifelse(dat_final$Bitte.beschreiben.Sie.das.Maß.an.körperlicher.Aktivität.im.Rahmen.der.in.Ihrem.Leben.durchgeführten.sportlichen.Aktivitäten.. == "Intensiv",
                                              "Yes","No")

intensive_sport_spinal_bulbar = map_dfr(c("intensive_sport"), ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# lifestyle substance past consumption 
dat_final_spinalbulbar$coffee = ifelse(dat_final$Konsumieren.Sie.aktuell.regelmäßig.koffeinhaltige.Getränke.oder.haben.Sie.jemals.in.Ihrem.Leben.regelmäßig.koffeinhaltige.Getränke.konsumiert. == "Ja..in.der.Vergangenheit",
                                       "Yes","No")
dat_final_spinalbulbar$alcohol = ifelse(dat_final$Konsumieren.Sie.aktuell.regelmäßig.alkoholische.Getränke.oder.haben.Sie.jemals.in.Ihrem.Leben.regelmäßig.alkoholische.Getränke.konsumiert. == "Ja..in.der.Vergangenheit",
                                       "Yes","No")
dat_final_spinalbulbar$smoke = ifelse(dat_final$Rauchen.Sie.aktuell.oder.haben.Sie.jemals.in.Ihrem.Leben.regelmäßig.Zigaretten..oder.Zigarren.etc...geraucht. == "Ja..in.Vergangenheit",
                                       "Yes","No")

substance_consumption_spinal_bulbar = map_dfr(c("coffee","alcohol",
                                      "smoke"), ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

# check if there is a signal with weight loss
dat_final_spinalbulbar$sex = data_combined$`Bitte geben Sie Ihr Geschlecht an.`
weight_1yonset_now = data_weight_timeline%>% filter(time == "one_y" & status == 1)  %>% pull(Weight) - data_weight_timeline %>% filter(time == "now" & status == 1) %>% pull(Weight)
weight_onset_diagnosis = data_ALS_weight$`Bitte geben Sie Ihr Gewicht an. [Bei Erkrankungsbeginn (erstes Symptom der Motoneuronerkrankung)][Gewicht [kg]]` - data_ALS_weight$`Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]`
weight_diagnosis_now = data_ALS_weight$`Bitte geben Sie Ihr Gewicht an. [Bei Erstdiagnose][Gewicht [kg]]` - 
  data_weight_timeline %>% filter(time == "now" & status == 1) %>% pull(Weight)

dat_final_spinalbulbar$weight_loss = data_weight_timeline %>% filter(time == "ten_y") %>% pull(Weight) - data_weight_timeline %>% filter(time == "now") %>% pull(Weight)
dat_final_spinalbulbar$weight_now = data_weight_timeline %>% filter(time == "now") %>% pull(Weight)
dat_final_spinalbulbar$weight_1y = data_weight_timeline %>% filter(time == "one_y") %>% pull(Weight)
dat_final_spinalbulbar$weight_5y = data_weight_timeline %>% filter(time == "five_y") %>% pull(Weight)
dat_final_spinalbulbar$weight_10y = data_weight_timeline %>% filter(time == "ten_y") %>% pull(Weight)
weight_now_spinal_bulbar = map_dfr(c("weight_now","weight_1y","weight_5y","weight_10y"),
                                   ~ run_multinom_for_item(dat_final_spinalbulbar, .x))

wilcox.test(data_weight_timeline %>% filter(time == "now" & status == 1) %>% pull(Weight) ~ status_aux, data = subset(dat_final_spinalbulbar, status2 == 1))
wilcox.test(weight_loss ~ sex, data = subset(dat_final_spinalbulbar, status2 == 1))
wilcox.test(weight_loss ~ status2, data = dat_final_spinalbulbar)
model <- glm(status2 ~ weight_loss+sex, data = dat_final_spinalbulbar, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
dat_final_spinalbulbar %>%
  filter(status2 == 1) %>%
  group_by(sex) %>%
  dplyr::summarise(
    mean_loss = mean(weight_loss, na.rm = TRUE),
    median_loss = median(weight_loss, na.rm = TRUE),
    sd_loss = sd(weight_loss, na.rm = TRUE),
    n = n()
  )

wilcox.test(weight_onset_diagnosis ~ status_aux, data = subset(dat_final_spinalbulbar, status2 == 1)) # not significant
wilcox.test(weight_diagnosis_now ~ status_aux, data = subset(dat_final_spinalbulbar, status2 == 1)) # not significant

# all together & save
spinal_bulbar_analyses = do.call("rbind",list(prodromal_condition_spinal_bulbar,
                                              healthcare_visit_spinal_bulbar,
                                              preexisting_conditions_spinal_bulbar,
                                              single_spinal_bulbar,
                                              children_spinal_bulbar,
                                              educational_spinal_bulbar,
                                              professional_spinal_bulbar,
                                              intensive_job_spinal_bulbar,
                                              intensive_sport_spinal_bulbar,
                                              substance_consumption_spinal_bulbar,
                                              weight_now_spinal_bulbar))
write_xlsx(spinal_bulbar_analyses,"data code output/spinal_bulbar_analyses.xlsx")


### check if there is a significant effect of intensive physical activity in slipped disc and males
dat_final_spinalbulbar_tmp = dat_final_spinalbulbar %>%
  mutate(slipped_disc = ifelse(slipped_disc  == "Yes",1,0))

# only with spinal and bulbar
fit = glm(slipped_disc ~ status_aux + intensive_job,
    data = dat_final_spinalbulbar_tmp %>% filter(status_aux != "CTR"),
    family = binomial)

summary(fit)
broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)

# with spinal, bulbar and ctr
fit_multi <- multinom(slipped_disc ~ sex + status_aux + intensive_job,
                      data = dat_final_spinalbulbar_tmp %>% filter(status_aux != "CTR"), trace = FALSE)
summary(fit_multi)
broom::tidy(fit_multi, exponentiate = TRUE, conf.int = TRUE)

# check interaction
fit_inter <- glm(slipped_disc ~ sex + status_aux * intensive_job,
                 data = dat_final_spinalbulbar_tmp,
                 family = binomial)
summary(fit_inter)
broom::tidy(fit_inter, exponentiate = TRUE, conf.int = TRUE)


##############################################################################
################## additional subanlyses on ALS categories ##################

# (ii) stratified by age at onset/disease duration/progression

# ════════════════════════════════════════════════════════════════
# 1: general function to run univariate analyses for each subgroup

run_multinom_generalised <- function(data, item, group_var, ref_level = "CTR") {
  
  dat <- data %>%
    dplyr::select(all_of(c(group_var, item, "sex"))) %>%
    na.omit() %>%
    droplevels() %>%
    mutate(across(all_of(group_var),
                  ~ factor(.x) %>% relevel(ref = ref_level)))
  
  if (nrow(dat) == 0 || length(unique(dat[[group_var]])) < 2) return(tibble())
  
  all_levels <- levels(dat[[group_var]])
  als_levels <- setdiff(all_levels, ref_level)
  if (length(als_levels) == 0) return(tibble())
  
  form <- as.formula(paste(group_var, "~", item, "+ sex"))
  fit  <- tryCatch(multinom(form, data = dat, trace = FALSE), error = function(e) NULL)
  if (is.null(fit)) return(tibble())
  
  summ  <- summary(fit)
  coefs <- summ$coefficients
  ses   <- summ$standard.errors
  
  if (is.null(dim(coefs))) {
    coefs <- matrix(coefs, nrow = 1, dimnames = list(als_levels, names(coefs)))
    ses   <- matrix(ses,   nrow = 1, dimnames = list(als_levels, names(ses)))
  }
  
  col_item <- grep(paste0("^", item), colnames(coefs))
  if (length(col_item) != 1) {
    warning("Item '", item, "': ", length(col_item), " matching columns — skipping.")
    return(tibble())
  }
  
  # vs CTR
  results_vs_ctr <- map_dfr(als_levels, function(lvl) {
    beta <- coefs[lvl, col_item]
    se   <- ses[lvl, col_item]
    tibble(item     = item,
           contrast = paste(lvl, "vs", ref_level),
           OR       = exp(beta),
           CI_low   = exp(beta - 1.96 * se),
           CI_high  = exp(beta + 1.96 * se),
           p_value  = 2 * pnorm(abs(beta / se), lower.tail = FALSE))
  })
  
  if (length(als_levels) < 2) return(results_vs_ctr)
  
  # pairwise
  results_pairwise <- map_dfr(combn(als_levels, 2, simplify = FALSE), function(pair) {
    lvl_a <- pair[1]
    lvl_b <- pair[2]
    
    # Step 1: filter rows to only relevant levels
    dat_pair <- dat %>%
      filter(as.character(!!sym(group_var)) %in% c(lvl_a, lvl_b, ref_level))
    
    # Step 2: drop unused levels and relevel
    dat_pair[[group_var]] <- factor(
      as.character(dat_pair[[group_var]]),
      levels = c(ref_level, lvl_a, lvl_b)
    )
    dat_pair[[group_var]] <- relevel(dat_pair[[group_var]], ref = lvl_b)
    
    # Verify
    cat("Pair:", lvl_a, "vs", lvl_b, 
        "| Levels:", paste(levels(dat_pair[[group_var]]), collapse=", "),
        "| N:", nrow(dat_pair), "\n")
    
    fit2 <- tryCatch(
      multinom(as.formula(paste(group_var, "~", item, "+ sex")),
               data = dat_pair, trace = FALSE),
      error = function(e) NULL
    )
    if (is.null(fit2)) return(tibble())
    
    s2  <- summary(fit2)
    c2  <- s2$coefficients
    se2 <- s2$standard.errors
    
    cat("Rownames c2:", rownames(c2), "\n")
    
    if (is.null(dim(c2))) {
      c2  <- matrix(c2,  nrow = 1, dimnames = list(lvl_a, names(c2)))
      se2 <- matrix(se2, nrow = 1, dimnames = list(lvl_a, names(se2)))
    }
    
    cp <- grep(paste0("^", item), colnames(c2))
    if (length(cp) != 1) return(tibble())
    if (!lvl_a %in% rownames(c2)) {
      cat("WARNING: lvl_a", lvl_a, "not in rownames\n")
      return(tibble())
    }
    
    beta <- c2[lvl_a, cp]
    se   <- se2[lvl_a, cp]
    tibble(item     = item,
           contrast = paste(lvl_a, "vs", lvl_b),
           OR       = exp(beta),
           CI_low   = exp(beta - 1.96 * se),
           CI_high  = exp(beta + 1.96 * se),
           p_value  = 2 * pnorm(abs(beta / se), lower.tail = FALSE))
  })
  
  bind_rows(results_vs_ctr, results_pairwise)
}

# ═══════════════════
# 2: data preparation

prepare_subgroup_data <- function(dat_final,
                                  patient_ALS_progression,
                                  ALS_progression_tmp_grouped,
                                  data_combined, data_weight_timeline,
                                  data_ALS_weight) {
  
  # ── progression group ──────────────────────────────────────────
  ALS_progression_vector <- patient_ALS_progression %>%
    left_join(ALS_progression_tmp_grouped %>%
                dplyr::rename(ID = ALS_progression.ID,
                              ALS_progression = ALS_progression.ALS_progression)) %>%
    pull(group)
  
  progression_group <- c(
    ifelse(ALS_progression_vector == "slow", "slow",
           ifelse(ALS_progression_vector == "intermediate", "moderate",
                  ifelse(ALS_progression_vector == "fast", "fast", NA))),
    rep("CTR", sum(dat_final$status == 0))
  )
  
  # ── disease duration group (median split on ALS only) ──────────
  als_duration    <- diff_date_oldest
  als_duration    <- ifelse(is.infinite(als_duration), NA, als_duration)  # replace -Inf with NA
  median_duration <- median(als_duration, na.rm = TRUE)
  
  duration_group <- c(
    case_when(
      is.na(als_duration)              ~ NA_character_,
      als_duration <= median_duration  ~ "short",
      als_duration >  median_duration  ~ "long"
    ),
    rep("CTR", sum(dat_final$status == 0))
  )
  
  # ── age at onset group (median split on ALS only) ──────────────
  als_onset    <- age_onset                                     
  median_onset <- median(als_onset, na.rm = TRUE)
  
  onset_group <- c(
    case_when(
      is.na(als_onset)             ~ NA_character_,
      als_onset <= median_onset    ~ "early",
      als_onset >  median_onset    ~ "late"
    ),
    rep("CTR", sum(dat_final$status == 0))
  )
  
  # ── base dataset with ALL derived item columns ─────────────────
  df <- dat_final %>%
    mutate(
      progression_group = factor(progression_group,
                                 levels = c("CTR","slow","moderate","fast")),
      duration_group    = factor(duration_group,
                                 levels = c("CTR","short","long")),
      onset_group       = factor(onset_group,
                                 levels = c("CTR","early","late")),
      
      # ── prodromal conditions ──
      trembling_arms_legs  = ifelse(is.na(Verschiedenes...Zittern.der.Arme.oder.Beine..Skala.1.), "No", "Yes"),
      excessive_saliva     = ifelse(is.na(Verschiedenes...Übermaß.an.Speichel.im.Mund..Skala.1.), "No", "Yes"),
      cold_pale_extremities = ifelse(is.na(Haut...Gefühl...Kalte..blasse.oder.bläulich.verfärbte.Extremitäten...Skala.1.), "No", "Yes"),
      
      # ── healthcare visits ──
      neurology_visit     = apply(data_combined[, grep("Neurologie",  names(data_combined))], 1,
                                  function(z) ifelse(any(!is.na(z)), "Yes", "No")),
      speech_therapy_visit = apply(data_combined[, grep("Logopädie",  names(data_combined))], 1,
                                   function(z) ifelse(any(!is.na(z)), "Yes", "No")),
      
      # ── musculoskeletal ──
      slipped_disc        = ifelse(dat_final_tmp$Welche.Krankheits.des.Muskel.Skelett.Systems.liegt.bzw..lag.vor.und.seit.wann...Bandscheibenvorfall. == "Ja", "Yes", "No"),
      musculosketel_disease = apply(dat_final_tmp[, grep("Muskel.Skelett", names(dat_final_tmp))], 1,
                                    function(z) ifelse(any(z %in% "Ja"), "Yes", "No")),
      
      # ── lifestyle ──
      single              = ifelse(Wie.ist.aktuell.Ihr.Partnerschaftsstatus.. == "Alleinstehend", "Yes", "No"),
      children            = ifelse(Haben.Sie.eigene.Kinder. == "Ja", "Yes", "No"),
      lower_secondary_school = ifelse(Was.ist.Ihr.höchster.schulischer.Abschluss. == "Hauptschule", "Yes", "No"),
      vocational_school   = ifelse(Was.ist.Ihr.höchster.schulischer.Abschluss. == "Berufsschule", "Yes", "No"),
      other_school        = ifelse(Was.ist.Ihr.höchster.schulischer.Abschluss. == "Sonstiges", "Yes", "No"),
      volcational_degree  = ifelse(Was.ist.Ihr.höchster.beruflicher.Abschluss. == "Fachschule...Technikerschule...Handelsakademie", "Yes", "No"),
      intensive_job       = ifelse(Bitte.beschreiben.Sie.das.Maß.an.körperlicher.Aktivität.im.Rahmen.Ihrer.beruflichen.Tätigkeit.. == "Intensiv..z.B..Bauarbeiten.", "Yes", "No"),
      intensive_sport     = ifelse(Bitte.beschreiben.Sie.das.Maß.an.körperlicher.Aktivität.im.Rahmen.der.in.Ihrem.Leben.durchgeführten.sportlichen.Aktivitäten.. == "Intensiv", "Yes", "No"),
      
      # ── substance use (past) ──
      coffee              = ifelse(data_combined$`Konsumieren Sie aktuell regelmäßig koffeinhaltige Getränke oder haben Sie jemals in Ihrem Leben regelmäßig koffeinhaltige Getränke konsumiert?` == "Ja, in der Vergangenheit", "Yes", "No"),
      alcohol             = ifelse(data_combined$`Konsumieren Sie aktuell regelmäßig alkoholische Getränke oder haben Sie jemals in Ihrem Leben regelmäßig alkoholische Getränke konsumiert?` == "Ja, in der Vergangenheit", "Yes", "No"),
      smoke               = ifelse(data_combined$`Rauchen Sie aktuell oder haben Sie jemals in Ihrem Leben regelmäßig Zigaretten (oder Zigarren etc.) geraucht? ` == "Ja, in Vergangenheit", "Yes", "No"),
      
      # ── weight ──
      weight_now  = dat_final$Bitte.geben.Sie.Ihr.Gewicht.an...Aktuell..Gewicht..kg..,
      weight_1y   = dat_final$Bitte.geben.Sie.Ihr.Gewicht.an...Ein.Jahr.vor.Erkrankungbeginn..Gewicht..kg..,
      weight_5y   = dat_final$Bitte.geben.Sie.Ihr.Gewicht.an...5.Jahre.vor.Erkrankungsbeginn..Gewicht..kg..,
      weight_10y  = dat_final$Bitte.geben.Sie.Ihr.Gewicht.an...10.Jahre.vor.Erkrankungsbeginn..Gewicht..kg..,
      
      # ── substance quantity ──
      smoke_consumption   = data_combined$`Rauchen Sie aktuell oder haben Sie jemals in Ihrem Leben regelmäßig Zigaretten (oder Zigarren etc.) geraucht? `,
      alcohol_consumption = data_combined$`Konsumieren Sie aktuell regelmäßig alkoholische Getränke oder haben Sie jemals in Ihrem Leben regelmäßig alkoholische Getränke konsumiert?`,
      coffee_consumption  = data_combined$`Konsumieren Sie aktuell regelmäßig koffeinhaltige Getränke oder haben Sie jemals in Ihrem Leben regelmäßig koffeinhaltige Getränke konsumiert?`,
      
      cig_num = as.numeric(data_combined$`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`),
      quantity_smoke = case_when(
        cig_num < 7  ~ "Few (1-6 p/ day)",
        cig_num < 15 ~ "Moderate (7-14 p/ day)",
        cig_num >= 15 ~ "High (>14 p/ day)",
        TRUE ~ NA_character_
      ),
      alcohol_quantity = case_when(
        data_combined$`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert?  ` %in%
          c("Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche",
            "Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche") ~ "Low (1-2 drinks per day)",
        data_combined$`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert?  ` %in%
          c("Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche",
            "Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche") ~ "High (> 1-2 drinks per day)",
        TRUE ~ NA_character_
      ),
      coffee_quantity = data_combined$`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`,
      
      # ── substance quantity binary flags ──
      smoke_current_few      = as.integer(smoke_consumption == "Ja, aktuell"           & quantity_smoke == "Few (1-6 p/ day)"),
      smoke_current_moderate = as.integer(smoke_consumption == "Ja, aktuell"           & quantity_smoke == "Moderate (7-14 p/ day)"),
      smoke_current_high     = as.integer(smoke_consumption == "Ja, aktuell"           & quantity_smoke == "High (>14 p/ day)"),
      smoke_past_few         = as.integer(smoke_consumption == "Ja, in Vergangenheit"  & quantity_smoke == "Few (1-6 p/ day)"),
      smoke_past_moderate    = as.integer(smoke_consumption == "Ja, in Vergangenheit"  & quantity_smoke == "Moderate (7-14 p/ day)"),
      smoke_past_high        = as.integer(smoke_consumption == "Ja, in Vergangenheit"  & quantity_smoke == "High (>14 p/ day)"),
      
      alcohol_current_few    = as.integer(alcohol_consumption == "Ja, aktuell"              & alcohol_quantity == "Low (1-2 drinks per day)"),
      alcohol_current_high   = as.integer(alcohol_consumption == "Ja, aktuell"              & alcohol_quantity == "High (> 1-2 drinks per day)"),
      alcohol_past_few       = as.integer(alcohol_consumption == "Ja, in der Vergangenheit" & alcohol_quantity == "Low (1-2 drinks per day)"),
      alcohol_past_high      = as.integer(alcohol_consumption == "Ja, in der Vergangenheit" & alcohol_quantity == "High (> 1-2 drinks per day)"),
      
      coffee_current_few     = as.integer(coffee_consumption == "Ja, aktuell"              & coffee_quantity == "Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)"),
      coffee_current_moderate = as.integer(coffee_consumption == "Ja, aktuell"             & coffee_quantity == "Moderat (z.B. 3 – 5 Tassen Kaffee pro Tag)"),
      coffee_current_high    = as.integer(coffee_consumption == "Ja, aktuell"              & coffee_quantity == "Viel (> 5 Tassen Kaffee pro Tag)"),
      coffee_past_few        = as.integer(coffee_consumption == "Ja, in der Vergangenheit" & coffee_quantity == "Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)"),
      coffee_past_moderate   = as.integer(coffee_consumption == "Ja, in der Vergangenheit" & coffee_quantity == "Moderat (z.B. 3 – 5 Tassen Kaffee pro Tag)"),
      coffee_past_high       = as.integer(coffee_consumption == "Ja, in der Vergangenheit" & coffee_quantity == "Viel (> 5 Tassen Kaffee pro Tag)")
    )
  
  df
}

# ══════════════════════════════════════════════════════════════════
# prepara values of questions of interest for the multinom function

define_item_groups <- function() {
  list(
    prodromal          = list(items = c("trembling_arms_legs","excessive_saliva","cold_pale_extremities"), filter = NULL),
    healthcare         = list(items = c("neurology_visit","speech_therapy_visit"),                         filter = NULL),
    musculoskeletal    = list(items = c("slipped_disc","musculosketel_disease"),                           filter = NULL),
    single             = list(items = "single",                                                            filter = NULL),
    children           = list(items = "children",                                                          filter = NULL),
    education          = list(items = c("lower_secondary_school","vocational_school","other_school"),       filter = NULL),
    professional       = list(items = "volcational_degree",                                                filter = NULL),
    intensive_job      = list(items = "intensive_job",                                                     filter = NULL),
    intensive_sport    = list(items = "intensive_sport",                                                   filter = NULL),
    substance_past     = list(items = c("coffee","alcohol","smoke"),                                       filter = NULL),
    weight             = list(items = c("weight_now","weight_1y","weight_5y","weight_10y"),                filter = NULL),
    smoke_current_qty  = list(items = c("smoke_current_few","smoke_current_moderate","smoke_current_high"),
                              filter = function(x) x$smoke_consumption == "Ja, aktuell"),
    smoke_past_qty     = list(items = c("smoke_past_few","smoke_past_moderate","smoke_past_high"),
                              filter = function(x) x$smoke_consumption == "Ja, in Vergangenheit"),
    alcohol_current_qty = list(items = c("alcohol_current_few","alcohol_current_high"),
                               filter = function(x) x$alcohol_consumption == "Ja, aktuell"),
    alcohol_past_qty   = list(items = c("alcohol_past_few","alcohol_past_high"),
                              filter = function(x) x$alcohol_consumption == "Ja, in der Vergangenheit"),
    coffee_current_qty = list(items = c("coffee_current_few","coffee_current_moderate","coffee_current_high"),
                              filter = function(x) x$coffee_consumption == "Ja, aktuell"),
    coffee_past_qty    = list(items = c("coffee_past_few","coffee_past_moderate","coffee_past_high"),
                              filter = function(x) x$coffee_consumption == "Ja, in der Vergangenheit")
  )
}

# ═════════════════════════
# run all subgroup analyses

run_subgroup_analysis <- function(df, group_var, ref_level = "CTR") {
  item_groups <- define_item_groups()
  
  map_dfr(item_groups, function(grp) {
    data_to_use <- if (!is.null(grp$filter)) {
      df[grp$filter(df) & !is.na(grp$filter(df)), ]
    } else {
      df
    }
    run_all_items(data_to_use, grp$items, group_var, ref_level)
  })
}

run_all_items <- function(data, items, group_var, ref_level = "CTR") {
  map_dfr(items, ~ run_multinom_generalised(data, .x, group_var, ref_level))
}

# ══════════════════════════════════════════════════════════════════
# STEP 5: execute
# ══════════════════════════════════════════════════════════════════
dat_final$sex = dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an.

df_subgroups <- prepare_subgroup_data(
  dat_final,
  patient_ALS_progression, ALS_progression_tmp_grouped,
  data_combined, data_weight_timeline, data_ALS_weight
)

progression_analyses <- run_subgroup_analysis(df_subgroups, "progression_group")
duration_analyses    <- run_subgroup_analysis(df_subgroups, "duration_group")
onset_analyses       <- run_subgroup_analysis(df_subgroups, "onset_group")

skim(ALS_progression_tmp_grouped[ALS_progression_tmp_grouped$group == "fast",]$ALS_progression.ALS_progression)
skim(ALS_progression_tmp_grouped[ALS_progression_tmp_grouped$group == "slow",]$ALS_progression.ALS_progression)
skim(ALS_progression_tmp_grouped[ALS_progression_tmp_grouped$group == "intermediate",]$ALS_progression.ALS_progression)

als_duration    <- diff_date_oldest
als_duration    <- ifelse(is.infinite(als_duration), NA, als_duration)  # replace -Inf with NA
median_duration <- median(als_duration, na.rm = TRUE)
als_duration_grouped <- case_when(is.na(als_duration)              ~ NA_character_,
                                  als_duration <= median_duration  ~ "short",
                                  als_duration >  median_duration  ~ "long")

skim(als_duration[als_duration_grouped == "short"])
skim(als_duration[als_duration_grouped == "long"])

als_onset    <- age_onset                                     
median_onset <- median(als_onset, na.rm = TRUE)
als_onset_grouped <-  case_when(is.na(als_onset)             ~ NA_character_,
                                als_onset <= median_onset    ~ "early",
                                als_onset >  median_onset    ~ "late")

skim(als_onset[als_onset_grouped == "early"])
skim(als_onset[als_onset_grouped == "late"])

write_xlsx(
  list(progression = progression_analyses,
       duration    = duration_analyses,
       onset       = onset_analyses,
       site_onset  = spinal_bulbar_analyses),
  "data code output/subgroup_analyses.xlsx"
)

#### Make heatmap visualisation
questions_interest = c("Trembling of arms or legs",
                       "Excessive saliva",
                       "Cold, pale extremities",
                       "Neurology",
                       "Speech therapy",
                       "Musculoskeletal disorders",
                       "Slipped disc",
                       "Single",
                       "Have children",
                       "Lower secondary school",
                       "Vocational school",
                       "Educational degree (other)",
                       "Vocational/tecnhnical school or business academy",
                       "Occupational physical activity (intensive)",
                       "Recreational sports activity (high intensity)",
                       "Caffeine consumption (in the past)",
                       "Alcohol consumption (in the past)",
                       "Cigarette consumption (in the past)",
                       "Weight (now)")
                       # "Weight (1-12months)",
                       # "Weight (1-5y)",
                       # "Weight (5-10y)")
n_ctr          <- sum(dat_final$status == 0)
n_als_total    <- sum(dat_final$status == 1)
n_female       <- length(which(dat_final[1:475,]$Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich"))
n_male         <- length(which(dat_final[1:475,]$Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich"))
n_slow         <- sum(df_subgroups$progression_group == "slow",         na.rm = TRUE)
n_intermediate <- sum(df_subgroups$progression_group == "moderate", na.rm = TRUE)
n_fast         <- sum(df_subgroups$progression_group == "fast",         na.rm = TRUE)
n_bulbar       <- sum(spinal_bulbar$spinal_or_bulbar == "bulbar",              na.rm = TRUE)  
n_spinal       <- sum(spinal_bulbar$spinal_or_bulbar  == "spinal",              na.rm = TRUE)
n_short        <- sum(df_subgroups$duration_group == "short",           na.rm = TRUE)
n_long         <- sum(df_subgroups$duration_group == "long",            na.rm = TRUE)
n_early        <- sum(df_subgroups$onset_group == "early",              na.rm = TRUE)
n_late         <- sum(df_subgroups$onset_group == "late",               na.rm = TRUE)

lbl_original     <- paste0("Total cohort\n(n=", n_als_total, ")")
lbl_female       <- paste0("Female\n(n=", n_female, ")")
lbl_male         <- paste0("Male\n(n=", n_male, ")")
lbl_slow         <- paste0("Slow\n(n=", n_slow, ")")
lbl_moderate     <- paste0("Moderate\n(n=", n_intermediate, ")")
lbl_fast         <- paste0("Fast\n(n=", n_fast, ")")
lbl_bulbar       <- paste0("Bulbar\n(n=", n_bulbar, ")")
lbl_spinal       <- paste0("Spinal\n(n=", n_spinal, ")")
lbl_short        <- paste0("Short\n(n=", n_short, ")")
lbl_long         <- paste0("Long\n(n=", n_long, ")")
lbl_early        <- paste0("Early\n(n=", n_early, ")")
lbl_late         <- paste0("Late\n(n=", n_late, ")")
lbl_sp1          <- "\u2006"
lbl_sp2          <- "\u2006\u2006"
lbl_sp3          <- "\u2006\u2006\u2006"
lbl_sp4          <- "\u2006\u2006\u2006\u2006"
lbl_sp5          <- "\u2006\u2006\u2006\u2006\u2006"

col_order <- c(
  lbl_original,
  lbl_sp1,
  lbl_female,lbl_male,
  lbl_sp2,
  lbl_bulbar, lbl_spinal,
  lbl_sp3,
  lbl_slow, lbl_moderate, lbl_fast,
  lbl_sp4,
  lbl_short, lbl_long,
  lbl_sp5,
  lbl_early, lbl_late
)

original_ref = do.call("rbind",list(univariate_nonmotor_general_extra_new %>% 
                                      mutate(question_type = "non-motor"),
                                    univar_preconditions_general_extra_new  %>% 
                                      mutate(question_type = "pre-conditions"),
                                    univar_lifestyle_general_extra_new %>%
                                      mutate(question_type = "lifestyle"),
                                    univar_healthcare_general_extra_new %>%
                                      mutate(question_type = "healthcare"),
                                    univar_dietweight_general_extra_new %>%
                                      mutate(question_type = "dietweight"))) %>%
  filter(type == "Full sample") %>%
  dplyr::select(`Specific category`, `Main category`, question_type,
                `log(odds-ratio)`, `P-value`) %>%
  dplyr::rename(
    item          = `Specific category`,
    main_category = `Main category`,
    log_OR        = `log(odds-ratio)`,
    p_value       = `P-value`
  ) %>%
  mutate(
    col_label = lbl_original,   # use the label variable — not a new string
    item = ifelse(main_category == "Educational degree" & item == "Other",
                  "Educational degree (other)", item)
  ) %>%
  mutate(item = ifelse(item == "Herniated disc","Slipped disc",item)) %>%
  filter(item %in% questions_interest)

# sex answers
female_ref = univariate_all_female %>%
  dplyr::select(`Specific category`, `Main category`, question_type,
                `log(odds-ratio)`, `P-value`) %>%
  dplyr::rename(
    item          = `Specific category`,
    main_category = `Main category`,
    log_OR        = `log(odds-ratio)`,
    p_value       = `P-value`
  ) %>%
  mutate(
    col_label = lbl_female,   # use the label variable — not a new string
    item = ifelse(main_category == "Educational degree" & item == "Other",
                  "Educational degree (other)", item)
  ) %>%
  filter(item %in% questions_interest)

male_ref = univariate_all_male %>%
  dplyr::select(`Specific category`, `Main category`, question_type,
                `log(odds-ratio)`, `P-value`) %>%
  dplyr::rename(
    item          = `Specific category`,
    main_category = `Main category`,
    log_OR        = `log(odds-ratio)`,
    p_value       = `P-value`
  ) %>%
  mutate(
    col_label = lbl_male,   # use the label variable — not a new string
    item = ifelse(main_category == "Educational degree" & item == "Other",
                  "Educational degree (other)", item)
  ) %>%
  filter(item %in% questions_interest)

# extract vs ctr
extract_vsctr <- function(df_subgroup, contrast_pattern, new_label) {
  df_subgroup %>%
    filter(str_detect(contrast, paste0("^", contrast_pattern, " vs CTR$"))) %>%
    dplyr::select(item, log_OR, p_value) %>%
    mutate(col_label = new_label) %>%
    filter(item %in% questions_interest)
}

# -> prepare data of subgroup anallyses 
prepare_subgroup_result <- function(df, subanalysis_name) {
  df %>%
    mutate(
      log_OR      = log(OR),
      subanalysis = subanalysis_name,
      item = case_when(
        item == "trembling_arms_legs"    ~ "Trembling of arms or legs",
        item == "excessive_saliva"       ~ "Excessive saliva",
        item == "cold_pale_extremities"  ~ "Cold, pale extremities",
        item == "neurology_visit"        ~ "Neurology",
        item == "speech_therapy_visit"   ~ "Speech therapy",
        item == "slipped_disc"           ~ "Slipped disc",
        item == "musculosketel_disease"  ~ "Musculoskeletal disorders",
        item == "single"                 ~ "Single",
        item == "children"               ~ "Have children",
        item == "lower_secondary_school" ~ "Lower secondary school",
        item == "vocational_school"      ~ "Vocational school",
        item == "other_school"           ~ "Educational degree (other)",
        item == "volcational_degree"     ~ "Vocational/tecnhnical school or business academy",
        item == "intensive_job"          ~ "Occupational physical activity (intensive)",
        item == "intensive_sport"        ~ "Recreational sports activity (high intensity)",
        item == "coffee"                 ~ "Caffeine consumption (in the past)",
        item == "alcohol"                ~ "Alcohol consumption (in the past)",
        item == "smoke"                  ~ "Cigarette consumption (in the past)",
        item == "weight_now"             ~ "Weight (now)",
        item == "weight_1y"              ~ "Weight (1-12months)",
        item == "weight_5y"              ~ "Weight (1-5y)",
        item == "weight_10y"             ~ "Weight (5-10y)",
        item == "smoke_current_few"      ~ "Smoking current (1-6/day)",
        item == "smoke_current_moderate" ~ "Smoking current (7-14/day)",
        item == "smoke_current_high"     ~ "Smoking current (>14/day)",
        item == "smoke_past_few"         ~ "Smoking past (1-6/day)",
        item == "smoke_past_moderate"    ~ "Smoking past (7-14/day)",
        item == "smoke_past_high"        ~ "Smoking past (>14/day)",
        item == "alcohol_current_few"    ~ "Alcohol current (low)",
        item == "alcohol_current_high"   ~ "Alcohol current (high)",
        item == "alcohol_past_few"       ~ "Alcohol past (low)",
        item == "alcohol_past_high"      ~ "Alcohol past (high)",
        item == "coffee_current_few"     ~ "Caffeine current (low)",
        item == "coffee_current_moderate"~ "Caffeine current (moderate)",
        item == "coffee_current_high"    ~ "Caffeine current (high)",
        item == "coffee_past_few"        ~ "Caffeine past (low)",
        item == "coffee_past_moderate"   ~ "Caffeine past (moderate)",
        item == "coffee_past_high"       ~ "Caffeine past (high)",
        TRUE                             ~ item  
      )
    ) %>%
    dplyr::select(item, contrast, log_OR, p_value, subanalysis)
}

prog_long  <- prepare_subgroup_result(progression_analyses, "progression")
dur_long   <- prepare_subgroup_result(duration_analyses,    "duration")
onset_long <- prepare_subgroup_result(onset_analyses,       "onset")
site_long  <- prepare_subgroup_result(spinal_bulbar_analyses, "site_onset")

# auxiliar function for heatmap
all_cols <- bind_rows(
  original_ref %>% dplyr::select(item, log_OR, p_value, col_label),
  
  female_ref %>% dplyr::select(item, log_OR, p_value, col_label),
  male_ref %>% dplyr::select(item, log_OR, p_value, col_label),
  
  extract_vsctr(site_long,  "bulbar vs CTR" %>% str_remove(" vs CTR"), lbl_bulbar),
  extract_vsctr(site_long,  "spinal",       lbl_spinal),
  
  extract_vsctr(prog_long,  "slow",         lbl_slow),
  extract_vsctr(prog_long,  "moderate",     lbl_moderate),   
  extract_vsctr(prog_long,  "fast",         lbl_fast),
  
  extract_vsctr(dur_long,   "short",        lbl_short),
  extract_vsctr(dur_long,   "long",         lbl_long),
  
  extract_vsctr(onset_long, "early",        lbl_early),
  extract_vsctr(onset_long, "late",         lbl_late)
) %>%
  mutate(item =ifelse(item == "Slipped disc","Herniated disc",item))

questions_interest = ifelse(questions_interest == "Slipped disc","Herniated disc",questions_interest)

spacer_df <- tibble(
  item      = rep(questions_interest, 5),
  log_OR    = NA_real_,
  p_value   = NA_real_,
  col_label = rep(c(lbl_sp1, lbl_sp2, lbl_sp3, lbl_sp4,lbl_sp5),
                  each = length(questions_interest))
)

# get significance
sig_label <- function(p) {
  case_when(
    is.na(p)   ~ "",
    p < 0.001  ~ "***",
    p < 0.01   ~ "**",
    p < 0.05   ~ "*",
    TRUE       ~ as.character(round(p, 2))
  )
}

df_heatmap <- bind_rows(all_cols, spacer_df) %>%
  mutate(
    item        = factor(item, levels = rev(questions_interest)),
    col_label   = factor(col_label, levels = col_order),
    log_OR_plot = pmax(pmin(log_OR, 3), -3),
    significant = !is.na(p_value) & p_value < 0.05,
    is_spacer   = col_label %in% c(lbl_sp1, lbl_sp2, lbl_sp3, lbl_sp4,lbl_sp5),
    sig_text    = sig_label(p_value)
  ) %>%
  # drop any rows where col_label became NA (unmatched) — safety net
  filter(!is.na(col_label))

df_heatmap <- df_heatmap %>%
  left_join(
    original_ref %>% dplyr::select(item, main_category) %>% distinct(),
    by = "item"
  ) %>%
  mutate(
    main_category = ifelse(is.na(main_category), "", main_category)
  )

df_heatmap <- df_heatmap %>%
  mutate(item = factor(item, levels = rev(questions_interest)))

df_data    <- df_heatmap %>% filter(!is_spacer)
df_spacers <- df_heatmap %>% filter(is_spacer)

# ═════════════════════════
# group header annotations


# x positions for group bracket annotations (based on factor level indices)
col_levels <- col_order
get_x_range <- function(labels) {
  idx <- which(col_levels %in% labels)
  c(min(idx) - 0.5, max(idx) + 0.5)
}

grp_annotations <- tibble(
  label = c("Total Cohort", "Sex","Site of onset","Progression", 
            "Disease duration", "Onset time"),
  xmin  = c(get_x_range(lbl_original)[1],
            get_x_range(c(lbl_female, lbl_male))[1],
            get_x_range(c(lbl_bulbar, lbl_spinal))[1],
            get_x_range(c(lbl_slow, lbl_moderate, lbl_fast))[1],
            get_x_range(c(lbl_short, lbl_long))[1],
            get_x_range(c(lbl_early, lbl_late))[1]),
  xmax  = c(get_x_range(lbl_original)[2],
            get_x_range(c(lbl_female, lbl_male))[2],
            get_x_range(c(lbl_bulbar, lbl_spinal))[2],
            get_x_range(c(lbl_slow, lbl_moderate, lbl_fast))[2],
            get_x_range(c(lbl_short, lbl_long))[2],
            get_x_range(c(lbl_early, lbl_late))[2]),
  xmid  = (xmin + xmax) / 2
)

# ════════
# Heatmap

# ── plot ──
p_combined <- ggplot(df_data, aes(x = col_label, y = item)) +
  
  geom_tile(
    aes(fill = case_when(is_spacer ~ NA_real_, TRUE ~ log_OR_plot)),
    color = "white", linewidth = 0.25
  ) +
  
  geom_text(
    data = df_heatmap %>% filter(!is_spacer & sig_text != ""),
    aes(label = sig_text, color = significant),
    size = 3.0, fontface = "plain", vjust = 0.5
  ) +
  
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey25"),
                     guide  = "none") +
  
  scale_fill_gradientn(
    colors   = c("#2166AC","#6BAED6","grey95","#FC8D59","#B2182B"),
    values   = scales::rescale(c(-3, -1, 0, 1, 3)),
    limits   = c(-3, 3),
    oob      = scales::squish,
    na.value = "white",
    name     = "log(OR)",
    guide    = guide_colorbar(barwidth = 0.5, barheight = 5,
                              title.position = "top",
                              title.hjust    = 0.5)
  ) +
  
  annotate("segment",
           x     = grp_annotations$xmin,
           xend  = grp_annotations$xmax,
           y     = length(questions_interest) + 0.6,
           yend  = length(questions_interest) + 0.6,
           color = "grey40", linewidth = 0.4) +
  
  annotate("text",
           x      = grp_annotations$xmid,
           y      = length(questions_interest) + 1.1,
           label  = grp_annotations$label,
           size   = 3.3, color = "grey20", fontface = "bold") +
  
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  
  coord_cartesian(clip = "off") +
  
  labs(x = NULL, y = NULL) +
  
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x        = element_text(size = 8.5, color = "grey20",
                                      angle = 35, hjust = 1, vjust = 1),
    axis.text.y        = element_text(size = 8.5, color = "grey20"),
    axis.ticks         = element_blank(),
    panel.grid         = element_blank(),
    panel.spacing.y    = unit(0.1, "lines"),
    legend.position    = "right",
    legend.title       = element_text(size = 9),
    legend.text        = element_text(size = 8.5),
    plot.margin        = margin(30, 8, 8, 8)
  )

ggsave("plots/heatmap_combined.pdf", p_combined,
       width = 14, height = 0.3 * length(questions_interest) + 3,
       limitsize = FALSE)


### smaller version for the ppt
questions_interest_tmp = questions_interest[questions_interest %in% c("Cold, pale extremities","Excessive saliva",
                                                                      "Trembling of arms or legs","Herniated disc",
                                                                      "Occupational physical activity (intensive)",
                                                                      "Speech therapy","Neurology","Weight (now)",
                                                                      "Cigarette consumption (in the past)",
                                                                      "Alcohol consumption (in the past)","Caffeine consumption (in the past)")]

df_heatmap_tmp = bind_rows(all_cols, spacer_df) %>%
  mutate(
    item        = factor(item, levels = rev(questions_interest_tmp)),
    col_label   = factor(col_label, levels = col_order),
    log_OR_plot = pmax(pmin(log_OR, 3), -3),
    significant = !is.na(p_value) & p_value < 0.05,
    is_spacer   = col_label %in% c(lbl_sp1, lbl_sp2, lbl_sp3, lbl_sp4),
    sig_text    = sig_label(p_value)
  ) %>%
  # drop any rows where col_label became NA (unmatched) — safety net
  filter(!is.na(col_label))  %>%
  left_join(
    original_ref %>% dplyr::select(item, main_category) %>% distinct(),
    by = "item"
  ) %>%
  mutate(
    main_category = ifelse(is.na(main_category), "", main_category)
  ) %>%
  mutate(item = factor(item, levels = rev(questions_interest_tmp))) %>%
  dplyr::filter(item %in% c("Cold, pale extremities","Excessive saliva",
                            "Trembling of arms or legs","Herniated disc",
                            "Occupational physical activity (intensive)",
                            "Speech therapy","Neurology","Weight (now)",
                            "Cigarette consumption (in the past)",
                            "Alcohol consumption (in the past)","Caffeine consumption (in the past)"))

df_data_tmp = df_heatmap_tmp %>% filter(!is_spacer)

p_combined_tmp <- ggplot(df_data_tmp, aes(x = col_label, y = item)) +
  
  geom_tile(
    aes(fill = case_when(is_spacer ~ NA_real_, TRUE ~ log_OR_plot)),
    color = "white", linewidth = 0.25
  ) +
  
  geom_text(
    data = df_heatmap_tmp %>% filter(!is_spacer & sig_text != ""),
    aes(label = sig_text, color = significant),
    size = 3, fontface = "plain", vjust = 0.5
  ) +
  
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey25"),
                     guide  = "none") +
  
  scale_fill_gradientn(
    colors   = c("#2166AC","#6BAED6","grey95","#FC8D59","#B2182B"),
    values   = scales::rescale(c(-3, -1, 0, 1, 3)),
    limits   = c(-3, 3),
    oob      = scales::squish,
    na.value = "white",
    name     = "log(OR)",
    guide    = guide_colorbar(barwidth = 0.5, barheight = 5,
                              title.position = "top",
                              title.hjust    = 0.5)
  ) +
  
  annotate("segment",
           x     = grp_annotations$xmin,
           xend  = grp_annotations$xmax,
           y     = length(questions_interest_tmp) + 0.6,
           yend  = length(questions_interest_tmp) + 0.6,
           color = "grey40", linewidth = 0.4) +
  
  annotate("text",
           x      = grp_annotations$xmid,
           y      = length(questions_interest_tmp) + 1.1,
           label  = grp_annotations$label,
           size   = 3.5, color = "grey20", fontface = "bold") +
  
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = TRUE) +
  
  coord_cartesian(clip = "off") +
  
  labs(x = NULL, y = NULL) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(size = 11, color = "grey20",
                                      angle = 35, hjust = 1, vjust = 1),
    axis.text.y        = element_text(size = 11, color = "grey20"),
    axis.ticks         = element_blank(),
    panel.grid         = element_blank(),
    panel.spacing.y    = unit(0.1, "lines"),
    legend.position    = "right",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 10),
    plot.margin        = margin(30, 8, 8, 8)
  )

ggsave("plots/heatmap_combined_ppt.pdf", p_combined_tmp,
       width = 14, height = 0.3 * length(questions_interest) + 3,
       limitsize = FALSE)


### EXTRA: make analysis of incomplete survey submissions
controls_incomplete = data_control_new[is.na(data_control_new$`Datum Abgeschickt`), ] # 20 IDs
ALS_incomplete = data_patients_new[is.na(data_patients_new$`Datum Abgeschickt`), ] # 38 IDs

# compute ALSFRS-R
ALSFRS_R_Q_incomplete = ALS_incomplete[,colnames(ALS_incomplete) %in% colnames(data_ALS_FRS_R)] # only 5 answers of ALSFRS-R

# disease duration (26 out of the 38 available)
first_symptom_incomplete = ALS_incomplete[,c(which(colnames(ALS_incomplete) %in% first_symptom$`original question (ALS)`))]
first_symptom_incomplete = apply(first_symptom_incomplete, 2, function(x) format(as.Date(paste("01",x,sep="/"),"%d/%m/%Y"),"%d/%m/%Y"))
date_questionnaire_incomplete <- format(as.Date(ALS_incomplete$`Datum letzte Aktivität`,format = "%Y-%m-%d"),"%d/%m/%Y")
diff_date_incomplete <- apply(first_symptom_incomplete, 2, function(x)  {
  (difftime(strptime(date_questionnaire_incomplete, format = "%d/%m/%Y"),strptime(x, format = "%d/%m/%Y"))/365)*12})
diff_date_oldest_incomplete <- apply(diff_date_incomplete,1,function(x) {
  # remove ages that don't make sense like 1920, 1921, etc.
  x <- x[x < 400]
  ceiling(max((na.omit(x))))
})
skim(diff_date_oldest_incomplete[diff_date_oldest_incomplete!=(-Inf)])

disease_duration_incomplete = ifelse(is.infinite(diff_date_oldest_incomplete), NA, diff_date_oldest_incomplete)
median_duration_incomplete = median(disease_duration_incomplete, na.rm = TRUE)

duration_group_incomplete <- case_when(
    is.na(disease_duration_incomplete)              ~ NA_character_,
    disease_duration_incomplete <= median_duration_incomplete  ~ "short",
    disease_duration_incomplete >  median_duration_incomplete  ~ "long"
  )

# ALS phenotype (24 out of the 38)
ALS_phenotype_incomplete = case_when(
  ALS_incomplete$`Welcher ALS-Subtyp besteht bei Ihnen?` == "Klassische Amyotrophe Lateralskerose (ALS)" ~ "Classical ALS",
  ALS_incomplete$`Welcher ALS-Subtyp besteht bei Ihnen?` == "Progressive Bulbärparalyse" ~ "PBP",
  ALS_incomplete$`Welcher ALS-Subtyp besteht bei Ihnen?` %in% c("Flail-arm-Syndrom", "Flail-leg-Syndrom") ~ "Flail limb",
  ALS_incomplete$`Welcher ALS-Subtyp besteht bei Ihnen?` == "Progressive Muskelatrophie (PMA)" ~ "PMA", 
  ALS_incomplete$`Welcher ALS-Subtyp besteht bei Ihnen?` == "Amyotrophe Lateralsklerose mit Frontotemporaler Demenz (ALS-FTD)" ~ "ALS-FTD", 
  ALS_incomplete$`Welcher ALS-Subtyp besteht bei Ihnen?` == "Primäre Lateralsklerose (PLS)" ~ "PLS",
    TRUE ~ "Unknown"
  )

# site of onset (26 out of 38)
data_first_symptom_spinal_incomplete <- ALS_incomplete[,c(which(colnames(ALS_incomplete) %in% spinal$`original question (ALS)`))]
data_first_symptom_spinal_incomplete <- apply(data_first_symptom_spinal_incomplete, 2, 
                                              function(x) format(as.Date(paste("01",x,sep="/"),"%d/%m/%Y"),"%d/%m/%Y"))
diff_date_spinal_incomplete <- apply(data_first_symptom_spinal_incomplete, 2, 
                                     function(x) (as.numeric(difftime(strptime(date_questionnaire_incomplete, format = "%d/%m/%Y"),
                                                                                         strptime(x, format = "%d/%m/%Y"),units = "days")/365))*12)
diff_date_spinal_oldest_incomplete <- apply(diff_date_spinal_incomplete,1,function(x) {
  # remove ages that don't make sense like 1920, 1921, etc.
  x <- na.omit(x[x<800])
  ceiling(max(na.omit(x)))
})

# -> bulbar
data_first_symptom_bulbar_incomplete <- ALS_incomplete[,c(which(colnames(ALS_incomplete) %in% bulbar$`original question (ALS)`))]
data_first_symptom_bulbar_incomplete <- apply(data_first_symptom_bulbar_incomplete, 2, function(x) format(as.Date(paste("01",x,sep="/"),"%d/%m/%Y"),"%d/%m/%Y"))
diff_date_bulbar_incomplete <- apply(data_first_symptom_bulbar_incomplete, 2, 
                                     function(x) (as.numeric(difftime(strptime(date_questionnaire_incomplete, format = "%d/%m/%Y"),
                                                                                         strptime(x, format = "%d/%m/%Y"),units = "days")/365))*12)
diff_date_bulbar_oldest_incomplete <- apply(diff_date_bulbar_incomplete,1,function(x) {
  # remove ages that don't make sense like 1920, 1921, etc.
  x <- na.omit(x[x<1200])
  ceiling(max(na.omit(x)))
})

spinal_bulbar_incomplete <- data.frame(diff_spinal = diff_date_spinal_oldest_incomplete,
                            diff_bulbar = diff_date_bulbar_oldest_incomplete)
spinal_bulbar_incomplete <- spinal_bulbar_incomplete %>%
  mutate(spinal_or_bulbar = ifelse(diff_spinal == (-Inf) & diff_bulbar == (-Inf),NA,
                                   ifelse(diff_spinal> diff_bulbar,"spinal","bulbar")))
spinal_bulbar_stats_incomplete <- spinal_bulbar_incomplete %>% 
  dplyr::group_by(spinal_or_bulbar) %>%
  dplyr::summarise(n_rows = n(),
                   freq = n()/nrow(spinal_bulbar_incomplete)*100)

# make incomplet data with site of onset, als phenotype and disease duration
dat_excluded_ALS = data.frame(
  site_of_onset = spinal_bulbar_incomplete$spinal_or_bulbar,
  disease_duration = diff_date_oldest_incomplete,
  phenotype_grouped = ALS_phenotype_incomplete,
  group = rep("Excluded"))

dat_included_ALS = sparsity_df_ALS %>% 
  select(site_of_onset,phenotype_grouped) %>%
  mutate(group = "Included",
         disease_duration = diff_date_oldest) %>%
  select(site_of_onset,disease_duration,phenotype_grouped,group)

comparison_df <- bind_rows(dat_included_ALS, dat_excluded_ALS)

# Disease duration
comparison_df <- comparison_df %>%
  mutate(disease_duration = ifelse(is.infinite(disease_duration), 
                                   NA, disease_duration))
wilcox.test(disease_duration ~ group, data = comparison_df)
comparison_df %>%
  group_by(group) %>%
  dplyr::summarise(
    n = n(),
    dd_median = round(median(disease_duration, na.rm=TRUE),1),
    dd_Q1 = round(quantile(disease_duration, 0.25, na.rm=TRUE),1),
    dd_Q3 = round(quantile(disease_duration, 0.75, na.rm=TRUE),1)
  )

# Site of onset
fisher.test(table(comparison_df$group, 
                  comparison_df$site_of_onset))

# Phenotype
fisher.test(table(comparison_df$group, 
                  comparison_df$phenotype_grouped))
table(comparison_df$group, comparison_df$phenotype_grouped)

library(openxlsx)

# 1. Summary statistics per group
summary_stats <- comparison_df %>%
  group_by(group) %>%
  dplyr::summarise(
    N = n(),
    # Disease duration
    dd_n = sum(!is.na(disease_duration)),
    dd_median = round(median(disease_duration, na.rm=TRUE), 1),
    dd_Q1 = round(quantile(disease_duration, 0.25, na.rm=TRUE), 1),
    dd_Q3 = round(quantile(disease_duration, 0.75, na.rm=TRUE), 1),
    # Site of onset
    site_n = sum(!is.na(site_of_onset)),
    n_bulbar = sum(site_of_onset == "bulbar", na.rm=TRUE),
    pct_bulbar = round(mean(site_of_onset == "bulbar", na.rm=TRUE)*100, 1),
    n_spinal = sum(site_of_onset == "spinal", na.rm=TRUE),
    pct_spinal = round(mean(site_of_onset == "spinal", na.rm=TRUE)*100, 1),
    # Phenotype
    pheno_n = sum(!is.na(phenotype_grouped) & phenotype_grouped != "Unknown"),
    n_classical = sum(phenotype_grouped == "Classical ALS", na.rm=TRUE),
    pct_classical = round(mean(phenotype_grouped == "Classical ALS", na.rm=TRUE)*100, 1),
    n_PBP_ftd = sum(phenotype_grouped == "PBP", na.rm=TRUE),
    pct_PBP_ftd = round(mean(phenotype_grouped == "PBP", na.rm=TRUE)*100, 1),
    n_FTD_ftd = sum(phenotype_grouped == "ALS-FTD", na.rm=TRUE),
    pct_FTD_ftd = round(mean(phenotype_grouped == "ALS-FTD", na.rm=TRUE)*100, 1),
    n_pls = sum(phenotype_grouped == "PLS", na.rm=TRUE),
    pct_pls_ftd = round(mean(phenotype_grouped == "PLS", na.rm=TRUE)*100, 1),
    n_PMA = sum(phenotype_grouped == "PMA", na.rm=TRUE),
    pct_PMA_ftd = round(mean(phenotype_grouped == "PMA", na.rm=TRUE)*100, 1),
    n_spinal_var = sum(phenotype_grouped == "Flail limb", na.rm=TRUE),
    pct_spinal_var = round(mean(phenotype_grouped == "Flail limb", na.rm=TRUE)*100, 1)
  )

# 2. Build clean formatted table
clean_table <- data.frame(
  Variable = c(
    "N (total)",
    "Disease duration, months — median (IQR)",
    "Site of onset, n available",
    "  Bulbar, n (%)",
    "  Spinal, n (%)",
    "ALS phenotype, n available",
    "  Classical ALS, n (%)",
    "  PBP, n (%)",
    "  ALS-FTD, n (%)",
    "  PLS, n (%)",
    "  PMA, n (%)",
    "  Flail limb, n (%)",
    "ALSFRS-R"
  ),
  Excluded = c(
    "38",
    paste0(summary_stats$dd_median[1], " (", summary_stats$dd_Q1[1], "–", summary_stats$dd_Q3[1], ")"),
    as.character(summary_stats$site_n[1]),
    paste0(summary_stats$n_bulbar[1], " (", summary_stats$pct_bulbar[1], ")"),
    paste0(summary_stats$n_spinal[1], " (", summary_stats$pct_spinal[1], ")"),
    as.character(summary_stats$pheno_n[1]),
    paste0(summary_stats$n_classical[1], " (", summary_stats$pct_classical[1], ")"),
    paste0(summary_stats$n_PBP_ftd[1], " (", summary_stats$pct_PBP_ftd[1], ")"),
    paste0(summary_stats$n_FTD_ftd[1], " (", summary_stats$pct_FTD_ftd[1], ")"),
    paste0(summary_stats$n_pls[1], " (", summary_stats$pct_pls_ftd[1], ")"),
    paste0(summary_stats$n_PMA[1], " (", summary_stats$pct_PMA_ftd[1], ")"),
    paste0(summary_stats$n_spinal_var[1], " (", summary_stats$pct_spinal_var[1], ")"),
    "Available for 5/38 only; not formally compared"
  ),
  Included = c(
    "475",
    paste0(summary_stats$dd_median[2], " (", summary_stats$dd_Q1[2], "–", summary_stats$dd_Q3[2], ")"),
    as.character(summary_stats$site_n[2]),
    paste0(summary_stats$n_bulbar[2], " (", summary_stats$pct_bulbar[2], ")"),
    paste0(summary_stats$n_spinal[2], " (", summary_stats$pct_spinal[2], ")"),
    as.character(summary_stats$pheno_n[2]),
    paste0(summary_stats$n_classical[1], " (", summary_stats$pct_classical[1], ")"),
    paste0(summary_stats$n_PBP_ftd[1], " (", summary_stats$pct_PBP_ftd[1], ")"),
    paste0(summary_stats$n_FTD_ftd[1], " (", summary_stats$pct_FTD_ftd[1], ")"),
    paste0(summary_stats$n_pls[1], " (", summary_stats$pct_pls_ftd[1], ")"),
    paste0(summary_stats$n_PMA[1], " (", summary_stats$pct_PMA_ftd[1], ")"),
    paste0(summary_stats$n_spinal_var[1], " (", summary_stats$pct_spinal_var[1], ")"),
    "—"
  ),
  `P-value` = c(
    "—",
    "0·249",
    "—",
    "0·104",
    "0·104",
    "—",
    "0·031",
    "0·031",
    "0·031",
    "0·031",
    "0·031",
    "0·031",
    "—"
  ),
  check.names = FALSE
)

# 3. Save to Excel
wb <- createWorkbook()
addWorksheet(wb, "Excluded vs Included ALS")
writeData(wb, "Excluded vs Included ALS", clean_table)

# Formatting
headerStyle <- createStyle(
  textDecoration = "bold",
  fgFill = "#D5E8F0",
  border = "Bottom"
)
addStyle(wb, "Excluded vs Included ALS", headerStyle,
         rows = 1, cols = 1:4, gridExpand = TRUE)
setColWidths(wb, "Excluded vs Included ALS",
             cols = 1:4,
             widths = c(40, 20, 20, 15))

saveWorkbook(wb, "data code output/Supplementary_Table_Excluded_vs_Included.xlsx", 
             overwrite = TRUE)

cat("Saved successfully!\n")

####### --- deprecated
data_combined
ID_sparsity_raw <- apply(dat_na, 1, function(x) round(sum(x) * 100 / length(x), 3))
ID_sparsity_imputed <- apply(dat_na_imp, 1, function(x) round(sum(x) * 100 / length(x), 3))

sparsity_df = data.frame(ID = 1:nrow(dat_na),
                         sparsity_raw = ID_sparsity_raw,
                         sparsity_imputed = ID_sparsity_imputed,
                         status = status,
                         sex = dat_final_spinalbulbar_tmp$sex,
                         site_of_onset = dat_final_spinalbulbar_tmp$status_aux,
                         progression_rate = df_subgroups$progression_group,
                         ALSFRS_R = c(patient_ALS_FRS_R$ALSFRSR,rep("CTR",285)),
                         disease_duration = df_subgroups$duration_group,
                         phenotype = c(ALS_specific$`Welcher ALS-Subtyp besteht bei Ihnen?`,
                                       rep("CTR",285)))


wilcox.test(sparsity_raw ~ status, data = sparsity_df)
wilcox.test(sparsity_imputed ~ status, data = sparsity_df)

status_summary <- sparsity_df %>%
  filter(!is.na(status)) %>%
  group_by(Subgroup = status) %>%
  dplyr::summarise(
    N = n(),
    Median_sparsity = round(median(sparsity_raw), 2),
    Q1 = round(quantile(sparsity_raw, 0.25), 2),
    Q3 = round(quantile(sparsity_raw, 0.75), 2)
  ) %>%
  dplyr::mutate(
    `Median (IQR)` = paste0(Median_sparsity, " (", Q1, "–", Q3, ")"),
    Category = "Status",
    `KW p-value` = 0.06966,
    `Statistic` = ""
  ) %>%
  select(Category, Subgroup, N, `Median (IQR)`, `KW p-value`, Statistic)

sparsity_df %>%
  filter(!is.na(status)) %>%
  group_by(Subgroup = status) %>%
  dplyr::summarise(
    N = n(),
    Median_sparsity = round(median(sparsity_imputed), 2),
    Q1 = round(quantile(sparsity_imputed, 0.25), 2),
    Q3 = round(quantile(sparsity_imputed, 0.75), 2)
  ) %>%
  dplyr::mutate(
    `Median (IQR)` = paste0(Median_sparsity, " (", Q1, "–", Q3, ")"),
    Category = "Status",
    `KW p-value` = 0.06966,
    `Statistic` = ""
  ) %>%
  select(Category, Subgroup, N, `Median (IQR)`, `KW p-value`, Statistic)

sparsity_df_ALS = sparsity_df %>%
  filter(status == "ALS") %>%
  mutate(ALSFRS_R = as.numeric(ALSFRS_R))

sparsity_df_ALS <- sparsity_df_ALS %>%
  mutate(phenotype_grouped = case_when(
    phenotype == "Klassische Amyotrophe Lateralskerose (ALS)" ~ "Classical ALS",
    phenotype  == "Progressive Bulbärparalyse" ~ "PBP",
    phenotype  %in% c("Flail-arm-Syndrom", "Flail-leg-Syndrom") ~ "Flail limb",
    phenotype   == "Progressive Muskelatrophie (PMA)" ~ "PMA", 
    phenotype   == "Amyotrophe Lateralsklerose mit Frontotemporaler Demenz (ALS-FTD)" ~ "ALS-FTD", 
    phenotype == "Primäre Lateralsklerose (PLS)" ~ "PLS",
    TRUE ~ "Unknown"
  ))

# 3. Statistical comparisons using Kruskal-Wallis

# By site of onset
kruskal.test(sparsity_imputed ~ site_of_onset, data = sparsity_df_ALS)

# By progression rate tertiles (if continuous, bin first)
sparsity_df_ALS <- sparsity_df_ALS %>%
  mutate(progression_tertile = ntile(progression_rate, 3) %>%
           factor(labels = c("Slow", "Moderate", "Fast")))
kruskal.test(sparsity_imputed ~ progression_tertile, data = sparsity_df_ALS)

# By phenotype group
kruskal.test(sparsity_imputed ~ phenotype_grouped, data = sparsity_df_ALS)

# By ALSFRS-R as continuous (Spearman correlation)
cor.test(sparsity_df_ALS$sparsity_imputed, as.integer(sparsity_df_ALS$ALSFRS_R), 
         method = "spearman")

# save results 
install.packages("openxlsx")

library(openxlsx) # or openxlsx

# 1. Calculate median and IQR per subgroup
# Site of onset
site_summary <- sparsity_df_ALS %>%
  filter(!is.na(site_of_onset)) %>%
  group_by(Subgroup = site_of_onset) %>%
  dplyr::summarise(
    N = n(),
    Median_sparsity = round(median(sparsity_raw), 2),
    Q1 = round(quantile(sparsity_raw, 0.25), 2),
    Q3 = round(quantile(sparsity_raw, 0.75), 2)
  ) %>%
  dplyr::mutate(
    `Median (IQR)` = paste0(Median_sparsity, " (", Q1, "–", Q3, ")"),
    Category = "Site of onset",
    `KW p-value` = 0.6316,
    `Statistic` = ""
  ) %>%
  select(Category, Subgroup, N, `Median (IQR)`, `KW p-value`, Statistic)

# Progression rate
progression_summary <- sparsity_df_ALS %>%
  filter(!is.na(progression_tertile)) %>%
  dplyr::group_by(Subgroup = progression_tertile) %>%
  dplyr::summarise(
    N = n(),
    Median_sparsity = round(median(sparsity_raw), 2),
    Q1 = round(quantile(sparsity_raw, 0.25), 2),
    Q3 = round(quantile(sparsity_raw, 0.75), 2)
  ) %>%
  dplyr::mutate(
    `Median (IQR)` = paste0(Median_sparsity, " (", Q1, "–", Q3, ")"),
    Category = "Progression rate",
    `KW p-value` = 0.4417,
    Statistic = ""
  ) %>%
  select(Category, Subgroup, N, `Median (IQR)`, `KW p-value`, Statistic)

# Phenotype
phenotype_summary <- sparsity_df_ALS %>%
  filter(!is.na(phenotype_grouped), phenotype_grouped != "Unknown") %>%
  dplyr::group_by(Subgroup = phenotype_grouped) %>%
  dplyr::summarise(
    N = n(),
    Median_sparsity = round(median(sparsity_raw), 2),
    Q1 = round(quantile(sparsity_raw, 0.25), 2),
    Q3 = round(quantile(sparsity_raw, 0.75), 2)
  ) %>%
  dplyr::mutate(
    `Median (IQR)` = paste0(Median_sparsity, " (", Q1, "–", Q3, ")"),
    Category = "ALS phenotype",
    `KW p-value` = 0.1399,
    Statistic = ""
  ) %>%
  select(Category, Subgroup, N, `Median (IQR)`, `KW p-value`, Statistic)

# ALSFRS-R Spearman correlation (single row)
alsfrs_summary <- data.frame(
  Category = "ALSFRS-R (continuous)",
  Subgroup = "All ALS",
  N = sum(!is.na(sparsity_df_ALS$ALSFRS_R)),
  `Median (IQR)` = "—",
  `KW p-value` = NA,
  Statistic = "ρ = -0.031, p = 0.496",
  check.names = FALSE
)

# 2. Combine all into one table
full_table <- bind_rows(
  site_summary,
  progression_summary,
  phenotype_summary,
  alsfrs_summary
)

# 3. Save to Excel
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Sparsity by subgroup")
writeData(wb, "Sparsity by subgroup", full_table)

# Optional formatting
headerStyle <- createStyle(textDecoration = "bold", 
                           fgFill = "#D5E8F0",
                           border = "Bottom")
addStyle(wb, "Sparsity by subgroup", headerStyle, 
         rows = 1, cols = 1:6, gridExpand = TRUE)
setColWidths(wb, "Sparsity by subgroup", 
             cols = 1:6, 
             widths = c(25, 20, 8, 20, 15, 25))

saveWorkbook(wb, "data code output/Supplementary_Table_Sparsity.xlsx", overwrite = TRUE)

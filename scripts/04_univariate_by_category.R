##############################################################################
## HELPER FUNCTIONS

# -----------------------------
# Helper: safe message wrapper
safe_msg <- function(...) message(paste0(...))

# -----------------------------
# 1) univariate_model_new: GLM-based, returns list(df_combined, effCI_combined)
univariate_model_new <- function(data_final, status_var = "status") {
  res_list <- list()
  effCI_list <- list()
  
  vars <- setdiff(colnames(data_final), c(status_var, "status2"))
  
  for (v in vars) {
    x <- data_final[[v]]
    # Skip all NA or <2 unique
    if (all(is.na(x))) {
      safe_msg("Skipping '", v, "': all NA")
      next
    }
    if (length(unique(na.omit(x))) < 2) {
      safe_msg("Skipping '", v, "': < 2 unique non-NA values")
      next
    }
    
    #fmla <- as.formula(paste(status_var, "~", v,"+",0))
    fmla <- as.formula(paste(status_var, "~", v))
    
    fit <- tryCatch(
      glm(fmla, data = data_final, family = binomial),
      error = function(e) {
        safe_msg("Skipping '", v, "' due to glm error: ", e$message)
        NULL
      },
      warning = function(w) {
        # continue on warnings
        invokeRestart("muffleWarning")
      }
    )
    if (is.null(fit)) next
    
    coef_tab <- tryCatch(summary(fit)$coefficients, error = function(e) { safe_msg("coef extraction fail for ", v); NULL })
    if (is.null(coef_tab)) next
    
    # remove intercept row
    if ("(Intercept)" %in% rownames(coef_tab)) {
      coef_no_int <- coef_tab[rownames(coef_tab) != "(Intercept)", , drop = FALSE]
    } else {
      coef_no_int <- coef_tab
    }
    if (nrow(coef_no_int) == 0) {
      safe_msg("Skipping '", v, "': no non-intercept coefficients")
      next
    }
    
    rn <- rownames(coef_no_int)
    
    # try confint.default, fallback to Wald
    ci_mat <- tryCatch(confint.default(fit), error = function(e) NULL)
    if (!is.null(ci_mat)) {
      if ("(Intercept)" %in% rownames(ci_mat)) {
        ci_no_int <- ci_mat[rownames(ci_mat) != "(Intercept)", , drop = FALSE]
      } else {
        ci_no_int <- ci_mat
      }
      eff_CI_tmp <- exp(ci_no_int)
      rownames(eff_CI_tmp) <- rn
    } else {
      est_all <- coef_tab[, "Estimate"]
      se_all  <- coef_tab[, "Std. Error"]
      est_sub <- est_all[rownames(coef_no_int)]
      se_sub  <- se_all[rownames(coef_no_int)]
      lower <- exp(est_sub - 1.96 * se_sub)
      upper <- exp(est_sub + 1.96 * se_sub)
      eff_CI_tmp <- cbind(lower, upper)
      rownames(eff_CI_tmp) <- rn
    }
    
    tmp_df <- as.data.frame(coef_no_int, stringsAsFactors = FALSE)
    tmp_df$Variables <- rn
    tmp_df$Features <- rn
    tmp_df$t_stat <- tmp_df$Estimate / tmp_df$`Std. Error`
    rownames(tmp_df) <- NULL
    
    res_list[[v]] <- tmp_df
    effCI_list[[v]] <- as.data.frame(eff_CI_tmp, stringsAsFactors = FALSE)
  }
  
  if (length(res_list) == 0) {
    warning("No predictors successfully modeled in univariate_model_new.")
    return(list(data.frame(), data.frame()))
  }
  
  df_combined <- do.call(rbind, res_list)
  # Build effCI combined: include Variables column
  effCI_combined <- do.call(rbind, lapply(names(effCI_list), function(nm) {
    dfci <- effCI_list[[nm]]
    vars_here <- rownames(dfci)
    out <- as.data.frame(dfci, stringsAsFactors = FALSE)
    out$Variables <- vars_here
    rownames(out) <- NULL
    out
  }))
  # name CI columns consistently
  if (ncol(effCI_combined) >= 2) colnames(effCI_combined)[1:2] <- c("2.5 %", "97.5 %") else {
    colnames(effCI_combined)[1] <- "2.5 %" ; effCI_combined[["97.5 %"]] <- NA
  }
  return(list(df_combined, effCI_combined))
}

# -----------------------------
# 2) univariate_model_adjustment_new: same structure but formula includes adjust_var (e.g. sex)
univariate_model_adjustment_new <- function(df, adjust_var, status_var = "status") {
  results <- list()
  effCI <- list()
  preds <- setdiff(names(df), c(status_var, adjust_var, "status2"))
  
  for (var in preds) {
    x <- df[[var]]
    if (all(is.na(x))) { safe_msg("Skipping '", var, "': all NA"); next }
    if (length(unique(na.omit(x))) < 2) { safe_msg("Skipping '", var, "': < 2 unique non-NA"); next }
    
    #f <- as.formula(paste(status_var, "~", var, "+", adjust_var,"+",0))
    f <- as.formula(paste(status_var, "~", var, "+", adjust_var))
    fit <- tryCatch(glm(f, data = df, family = binomial), error = function(e) { safe_msg("GLM error for ", var, ": ", e$message); NULL })
    if (is.null(fit)) next
    
    coef_tab <- summary(fit)$coefficients
    # keep predictor-related rows only (not intercept or adjust_var)
    rows_keep <- setdiff(rownames(coef_tab), c("(Intercept)", adjust_var))
    coef_sub <- coef_tab[rows_keep, , drop = FALSE]
    if (nrow(coef_sub) == 0) next
    
    rn <- rownames(coef_sub)
    ci_all <- tryCatch(confint.default(fit), error = function(e) NULL)
    if (!is.null(ci_all)) {
      ci_sub <- ci_all[rownames(ci_all) %in% rn, , drop = FALSE]
      ci_exp <- exp(ci_sub)
    } else {
      est <- coef_sub[, "Estimate"]
      se  <- coef_sub[, "Std. Error"]
      ci_exp <- cbind(exp(est - 1.96 * se), exp(est + 1.96 * se))
      rownames(ci_exp) <- rn
    }
    
    df_out <- as.data.frame(coef_sub, stringsAsFactors = FALSE)
    df_out$Variables <- rn
    df_out$Features <- rn
    df_out$t_stat <- df_out$Estimate / df_out$`Std. Error`
    
    results[[var]] <- df_out
    effCI[[var]] <- as.data.frame(ci_exp, stringsAsFactors = FALSE)
  }
  
  if (length(results) == 0) {
    warning("No predictors successfully modeled in univariate_model_adjustment_new.")
    return(list(data.frame(), data.frame()))
  }
  df_res <- do.call(rbind, results)
  effCI_all <- do.call(rbind, lapply(names(effCI), function(nm) {
    dfci <- effCI[[nm]] ; dfci$Variables <- rownames(dfci); rownames(dfci) <- NULL; dfci
  }))
  if (ncol(effCI_all) >= 2) colnames(effCI_all)[1:2] <- c("2.5 %", "97.5 %") else { colnames(effCI_all)[1] <- "2.5 %"; effCI_all[["97.5 %"]] <- NA }
  return(list(df_res, effCI_all))
}

# -----------------------------
# 3) run_univariate wrapper: uses the above two functions and ensures 'eff' column exists
run_univariate <- function(dat, adjust_var_name = NULL) {
  
 # Relevel categorical variables to have "Nein" as reference
 vars <- setdiff(colnames(dat), c("status", "status2", adjust_var_name))
 for (v in vars) {
 x <- dat[[v]]
 #Only act on categorical variables with more than 1 unique value
 if ((is.factor(x) || is.character(x)) && length(unique(na.omit(x))) > 1) {
  x <- factor(x)
 if ("Nein" %in% levels(x)) x <- relevel(x, ref = "Nein")
 dat[[v]] <- x
 }}

  standard_cols <- c(
    "Variables", "eff", "Estimate", "Std.Error", "z value",
    "Pr(>|z|)", "2.5 %", "97.5 %"
  )
  
  out <- tryCatch({
    varname = colnames(dat)[1]
    print(varname)
    if (!is.null(adjust_var_name)) {
      tmp <- univariate_model_adjustment_new(cbind(dat, dat_final_age_sex[adjust_var_name, drop = FALSE]), 
                                             adjust_var_name)
      res <- tmp[[1]]
      ci  <- tmp[[2]]
    } else {
      tmp <- univariate_model_new(dat)
      res <- tmp[[1]]
      ci  <- tmp[[2]]
    }
    if (is.null(res) || nrow(res) == 0) {
      empty <- as.data.frame(t(rep(NA, length(standard_cols))))
      colnames(empty) <- standard_cols
      empty$Variables <- colnames(dat)[1]
      return(empty)
    }
    if (!"eff" %in% colnames(res)) res$eff <- res$Estimate
    # attach CI columns into res for downstream functions (merge by Variables)
    if (!is.null(ci) && nrow(ci) > 0) {
      # ensure numeric CI columns
      ci[["2.5 %"]] <- as.numeric(ci[["2.5 %"]])
      ci[["97.5 %"]] <- as.numeric(ci[["97.5 %"]])
      res <- merge(res, ci, by = "Variables", all.x = TRUE, sort = FALSE)
    } else {
      res[["2.5 %"]] <- NA ; res[["97.5 %"]] <- NA
    }
    for (nm in standard_cols) {
      if (!nm %in% colnames(res)) res[[nm]] <- NA
    }
    res <- res[, standard_cols, drop = FALSE]
    
    res
  }, error = function(e) {
    empty <- as.data.frame(t(rep(NA, length(standard_cols))))
    colnames(empty) <- standard_cols
    empty$Variables <- colnames(dat)[1]
    empty
  })
  out
}

# -----------------------------
# 4) preparate_data_forest: calculate logs, fdr, join map_questions_plot (by Variables)
preparate_data_forest <- function(data_univariate) {
  if (is.null(data_univariate) || nrow(data_univariate) == 0) return(data_univariate)
  # ensure CI numeric
  data_univariate$`2.5 %` <- as.numeric(data_univariate$`2.5 %`)
  data_univariate$`97.5 %` <- as.numeric(data_univariate$`97.5 %`)
  data_univariate <- data_univariate %>%
    dplyr::mutate(lower = ifelse(!is.na(`2.5 %`) & `2.5 %` > 0, log(`2.5 %`), NA_real_),
           upper = ifelse(!is.na(`97.5 %`) & `97.5 %` > 0, log(`97.5 %`), NA_real_),
           fdr = p.adjust(`Pr(>|z|)`, method = "fdr"),
           log10_pval = -log10(`Pr(>|z|)`),
           log10_padj = -log10(fdr),
           mean = eff,
           exp_mean = exp(mean),
           index = row_number()) %>%
    dplyr::arrange(desc(log10_pval))
  # join map_questions_plot by Variables (user-specified)
  if ("Variables" %in% colnames(map_questions_plot)) {
    data_univariate <- merge(data_univariate, map_questions_plot, by = "Variables", all.x = TRUE, sort = FALSE)
  } else {
    warning("map_questions_plot does not contain 'Variables' column; skipping join.")
  }
  data_univariate
}

# -----------------------------
# 5) run_subcategory: iterate sub-categories, optionally sum rows
run_subcategory <- function(final_cat_df, dat_final, sub_col = "sub-category", 
                            sum_rows = TRUE, adjust_var_name = NULL,
                            na_check = TRUE,na_yes_check = FALSE, sum_higher = FALSE) {
  subcats <- unique(final_cat_df[[sub_col]])
  df_list <- list()
  df_list_gender <- list()
  skipped_vars <- c()
  
  for (i in seq_along(subcats)) {
    sc <- subcats[i]
    temp_df <- final_cat_df[final_cat_df[[sub_col]] == sc, , drop = FALSE]
    # columns in dat_final matching these original question DE names (make.names)
    selected_names <- make.names(temp_df$`original question (ALS)`)
    keep_idx <- which(colnames(dat_final) %in% selected_names)
    if (length(keep_idx) == 0) {
      skipped_vars <- c(skipped_vars, paste0(sc, " (no matching cols)"))
      next
    }
    dat_sub <- dat_final[, c(keep_idx, ncol(dat_final)), drop = FALSE]
    names(dat_sub)
    
    if(na_check & ncol(dat_sub) > 2){
      dat_sub[,1:(ncol(dat_sub)-1)] = apply(dat_sub[,1:(ncol(dat_sub)-1)],2,
                                             function(x) {ifelse(!is.na(x),1,0)})
    }
    
    if(na_check & ncol(dat_sub) == 2){
      dat_sub[,1] <- as.numeric(!is.na(dat_sub[,1]))
    }
    
    if(na_yes_check & ncol(dat_sub) > 2){
      dat_sub[,1:(ncol(dat_sub)-1)] = apply(dat_sub[,1:(ncol(dat_sub)-1)],2,
                                            function(x) {ifelse(!is.na(x) & x == "Ja",1,0)})
    }
    
    if(na_yes_check & ncol(dat_sub) == 2){
      dat_sub[,1] <- as.numeric((!is.na(dat_sub[,1]) & dat_sub[,1] == "Ja"))
    }
    
    dat_sub$status <- factor(dat_sub$status, levels = c(0,1))
    
    if(sum_higher){
      counts <- apply(dat_sub[, 1:(ncol(dat_sub)-2), drop = FALSE], 1, function(x) 
      {sum(x)})
      print(counts)
      counts = ifelse(counts  > 0, 1, 0)
      print(counts)
      dat_sub2 <- data.frame(subcat = counts, status = dat_sub[, ncol(dat_sub)], status2 = dat_sub$status2)
      colnames(dat_sub2)[1] <- "subcat"
      res <- run_univariate(dat_sub2, adjust_var_name = NULL)
      res$Variables = sc
      if (!is.null(res)) { df_list[[length(df_list)+1]] <- res } else skipped_vars <- c(skipped_vars, sc)
      # gender adjusted
      resg <- run_univariate(dat_sub2, adjust_var_name)
      resg$Variables = c(sc,adjust_var_name)
      if (!is.null(resg)) df_list_gender[[length(df_list_gender)+1]] <- resg
    }
    
    if (sum_rows) {
      # sum non-NA answers per row across the subcategory columns
      counts <- apply(dat_sub[, 1:(ncol(dat_sub)-2), drop = FALSE], 1, function(x) 
        {sum(x)})
      dat_sub2 <- data.frame(subcat = counts, status = dat_sub[, ncol(dat_sub)], status2 = dat_sub$status2)
      colnames(dat_sub2)[1] <- "subcat"
      res <- run_univariate(dat_sub2, adjust_var_name = NULL)
      res$Variables = sc
      if (!is.null(res)) { df_list[[length(df_list)+1]] <- res } else skipped_vars <- c(skipped_vars, sc)
      # gender adjusted
      resg <- run_univariate(dat_sub2, adjust_var_name)
      resg$Variables = c(sc,adjust_var_name)
      if (!is.null(resg)) df_list_gender[[length(df_list_gender)+1]] <- resg
    } else {
      # run univariate on each question separately (not aggregated)
      # here we run the univariate_model_new across the block and rbind results
      tmp_res <- run_univariate(dat_sub, adjust_var_name = NULL)
      if (!is.null(tmp_res)) df_list[[length(df_list)+1]] <- tmp_res else skipped_vars <- c(skipped_vars, sc)
      # gender adjusted
      tmp_resg <- run_univariate(dat_sub, adjust_var_name)
      if (!is.null(tmp_resg)) df_list_gender[[length(df_list_gender)+1]] <- tmp_resg
    }
  } # end loop
  
  if (length(df_list) == 0) univar <- data.frame() else univar <- do.call(rbind, df_list)
  if (length(df_list_gender) == 0) univar_gender <- data.frame() else univar_gender <- do.call(rbind, df_list_gender)
  if (length(skipped_vars) > 0) message("Skipped due to errors/no-match: ", paste(unique(skipped_vars), collapse = "; "))
  
  #univar = univar %>% mutate(Variables = subcats)
  list(univar = univar, univar_gender = univar_gender)
}

# -----------------------------
# 6) save_univar_results: export tables + plots 
save_univar_results <- function(univar, univar_gender, plot_title, prefix) {
  if (is.null(univar) || nrow(univar) == 0) {
    message("No univar results for ", prefix, " — skipping save/plot.")
    return(NULL)
  }
  univar_p <- preparate_data_forest(univar)
  univar_gender_p <- preparate_data_forest(univar_gender)
  
  writexl::write_xlsx(univar_p, paste0("data code output/",prefix, ".xlsx"))
  writexl::write_xlsx(univar_gender_p, paste0("data code output/",prefix, "_gender.xlsx"))
  
  # if (!is.null(univar_p) && nrow(univar_p) > 0) {
  #   # Odds ratio plot
  #   try({
  #     pdf(paste0("plots/", prefix, ".pdf"))
  #     odds_ratio_plot(univar_p, plot_title)
  #     dev.off()
  #   }, silent = TRUE)
  #   
  #   # Volcano plots
  #   try({
  #     pdf(paste0("plots/volcano_", prefix, "_pvalue.pdf"))
  #     volcano_plot(univar_p, "p-value", paste0(plot_title, " (p-value)"))
  #     dev.off()
  #   }, silent = TRUE)
  #   
  #   try({
  #     pdf(paste0("plots/volcano_", prefix, "_padj.pdf"))
  #     volcano_plot(univar_p, "padj", paste0(plot_title, " (p-adjusted)"))
  #     dev.off()
  #   }, silent = TRUE)
  # } else {
  #   message("No data for plots for ", prefix, " — skipping.")
  # }
  # 
  # # Gender-adjusted plots
  # if (!is.null(univar_gender_p) && nrow(univar_gender_p) > 0) {
  #   try({
  #     pdf(paste0("plots/", prefix, "_gender.pdf"))
  #     odds_ratio_plot(univar_gender_p, paste0(plot_title, ", sex adjusted"))
  #     dev.off()
  #   }, silent = TRUE)
  #   
  #   try({
  #     pdf(paste0("plots/volcano_", prefix, "_gender_pvalue.pdf"))
  #     volcano_plot(univar_gender_p, "p-value", paste0(plot_title, ", sex adjusted (p-value)"))
  #     dev.off()
  #   }, silent = TRUE)
  #   
  #   try({
  #     pdf(paste0("plots/volcano_", prefix, "_gender_padj.pdf"))
  #     volcano_plot(univar_gender_p, "padj", paste0(plot_title, ", sex adjusted (p-adjusted)"))
  #     dev.off()
  #   }, silent = TRUE)
  # } else {
  #   message("No gender-adjusted data for plots for ", prefix, " — skipping.")
  # }
  
  message("Univariate results saved for ", prefix)
}


save_univar_results_gender <- function(univar_female, univar_male, plot_title, prefix) {
  if (is.null(univar_female) || nrow(univar_female) == 0) {
    message("No univar results for ", prefix, " — skipping save/plot.")
    return(NULL)
  }
  univar_female_p <- preparate_data_forest(univar_female)
  univar_male_p <- preparate_data_forest(univar_male)
  
  writexl::write_xlsx(univar_female_p, paste0("data code output/",prefix, "_female.xlsx"))
  writexl::write_xlsx(univar_male_p, paste0("data code output/",prefix, "_male.xlsx"))
  
  message("Univariate results saved for ", prefix)
}

# -----------------------------
# 7) Helper to attach status column to subset before calling run_univariate
attach_status <- function(df_block) {
  if (!("status" %in% colnames(df_block))) df_block$status <- dat_final$status
  if (!("status2" %in% colnames(df_block))) df_block$status2 <- dat_final$status2
  # ensure column order keeps predictors first and status last:
  status_cols <- intersect(c("status","status2"), colnames(df_block))
  pred_cols <- setdiff(colnames(df_block), status_cols)
  df_block[, c(pred_cols, status_cols), drop = FALSE]
}

##############################################################################
## Univariate analyses

# -----------------------------
# 1. load data: which questions to focus on 
final_ALS_CTR_category <- read_excel("data input/final_ALS_CTR_questionnaire_summary_IC.xlsx", sheet = "common") 
map_questions_plot <- read_excel("data input/map_questions_plot.xlsx") 

# -----------------------------
# 2. Filter only questions with category
final_ALS_CTR_category_temp <- final_ALS_CTR_category[!is.na(final_ALS_CTR_category$category), 
                                                      c("original question (ALS)","original question (CTR)",
                                                        "question in EN (ALS)","question in EN (CTR)",
                                                        "col_classification","category","sub-category","sub-subcategory")]
# age/sex rows
final_ALS_CTR_category_temp_age_sex <- final_ALS_CTR_category[
  final_ALS_CTR_category$`question in EN (ALS)` %in% c("birth","sex"), , drop = FALSE
]

# -----------------------------
# 3. Data preprocessing: split continuous vs categorical
# (Assumes dat_imp exists and question_annotation vector exists.)
dat_fil_con <- dat_imp[, question_annotation == "Continuous", drop = FALSE]
# numeric + scale
if (ncol(dat_fil_con) > 0) {
  dat_fil_con <- as.data.frame(lapply(dat_fil_con, function(x) as.numeric(as.character(x))))
  dat_fil_con <- as.data.frame(lapply(dat_fil_con, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))
}

dat_fil_cat <- dat_imp[, question_annotation != "Continuous", drop = FALSE]
if (ncol(dat_fil_cat) > 0) dat_fil_cat <- as.data.frame(lapply(dat_fil_cat, as.character), stringsAsFactors = FALSE)

# keep original names before make.names
# combine and canonicalize column names
dat_final <- cbind(as.data.frame(dat_fil_con), as.data.frame(dat_fil_cat))
# canonical status columns:
status_numeric <- ifelse(as.character(status) == "ALS", 1L, 0L)
dat_final$status <- status_numeric
dat_final$status2 <- factor(status_numeric, levels = c(0L,1L))
# standardize column names
colnames(dat_final) <- make.names(colnames(dat_final))
original_names <- colnames(dat_final)

# dat_final_age_sex: extract age/sex from the metadata using make.names
sex_col_candidates <- intersect(original_names, make.names(final_ALS_CTR_category_temp_age_sex$`original question (ALS)`))
if (length(sex_col_candidates) == 0) {
  warning("Sex column not found in dat_final. Check metadata names. Found candidates: ",
          paste(head(original_names, 20), collapse = ", "))
} else {
  dat_final_age_sex <- dat_final[, sex_col_candidates, drop = FALSE]
  # ensure sex column is factor named consistently
  if ("Bitte.geben.Sie.Ihr.Geschlecht.an." %in% colnames(dat_final_age_sex)) {
    dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an. <- as.factor(dat_final_age_sex$Bitte.geben.Sie.Ihr.Geschlecht.an.)
  } else {
    # If the column name is different, make sure it's a factor
    dat_final_age_sex[,1] <- as.factor(dat_final_age_sex[,1])
  }
}

# -----------------------------
##############################################################################
## 4. Run analyses by category: Non-motor, Healthcare, Pre-conditions, Diet, Lifestyle

# pick adjust_var_name: if dat_final_age_sex has a known sex column use it, otherwise NULL
adjust_var_name <- NULL
if (exists("dat_final_age_sex") && ncol(dat_final_age_sex) >= 1) {
  # prefer the canonical German label if present, else first column
  if ("Bitte.geben.Sie.Ihr.Geschlecht.an." %in% colnames(dat_final_age_sex)) {
    adjust_var_name <- "Bitte.geben.Sie.Ihr.Geschlecht.an."
  } else {
    adjust_var_name <- colnames(dat_final_age_sex)[1]
  }
  # ensure adjust_var_name is make.names (same style as dat_final columns)
  adjust_var_name <- make.names(adjust_var_name)
  # ensure dat_final_age_sex colnames also match
  colnames(dat_final_age_sex) <- make.names(colnames(dat_final_age_sex))
}

# Gender specific 
which_female <- dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich"
which_male <- dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich"

# ---- Non-motor symptoms ----
final_nonmotor <- final_ALS_CTR_category_temp[final_ALS_CTR_category_temp$category == "non-motor symptoms", , drop = FALSE]
final_nonmotor_general <- final_nonmotor[final_nonmotor$`sub-category` == "general", , drop = FALSE]
sel_cols_general <- intersect(make.names(final_nonmotor_general$`original question (ALS)`), colnames(dat_final))
dat_nonmotor_general <- dat_final[, c(sel_cols_general, "status"), drop = FALSE]
dat_nonmotor_general <- attach_status(dat_nonmotor_general)
dat_nonmotor_general$Haben.Sie.an.sich.selbst..Ihrem.Körper.oder.Ihrem.Verhalten.in.den.letzten.10.Jahren.vor.Erkrankungsbeginn..erstes.Symptom.der.Motoneuronerkrankung..Veränderungen.festgestellt..die.Sie..bislang..nicht.mit.der.Motoneuronerkrankung.oder.mit.einer.anderen.bei.Ihnen.bekannten.Erkrankung.in.Verbindung.brachten. = dplyr::case_when(
  grepl("Ja..zurückblickend", dat_nonmotor_general$Haben.Sie.an.sich.selbst..Ihrem.Körper.oder.Ihrem.Verhalten.in.den.letzten.10.Jahren.vor.Erkrankungsbeginn..erstes.Symptom.der.Motoneuronerkrankung..Veränderungen.festgestellt..die.Sie..bislang..nicht.mit.der.Motoneuronerkrankung.oder.mit.einer.anderen.bei.Ihnen.bekannten.Erkrankung.in.Verbindung.brachten.) ~ "YES",
  grepl("^Nein", dat_nonmotor_general$Haben.Sie.an.sich.selbst..Ihrem.Körper.oder.Ihrem.Verhalten.in.den.letzten.10.Jahren.vor.Erkrankungsbeginn..erstes.Symptom.der.Motoneuronerkrankung..Veränderungen.festgestellt..die.Sie..bislang..nicht.mit.der.Motoneuronerkrankung.oder.mit.einer.anderen.bei.Ihnen.bekannten.Erkrankung.in.Verbindung.brachten.) ~ "NO",
  TRUE ~ NA_character_
)

univar_general <- list(
  univar = run_univariate(dat_nonmotor_general, adjust_var_name = NULL),
  univar_gender = run_univariate(dat_nonmotor_general, 
                                 adjust_var_name = adjust_var_name)
)

univar_general_female = run_univariate(dat_nonmotor_general[which_female,], adjust_var_name = NULL)
univar_general_male = run_univariate(dat_nonmotor_general[which_male,], adjust_var_name = NULL)

# nonmotor other subcategories
final_nonmotor_diff <- final_nonmotor[final_nonmotor$`sub-category` != "general", , drop = FALSE]
univar_diff <- run_subcategory(final_nonmotor_diff, 
                               dat_final, sub_col = "sub-category", 
                               sum_rows = TRUE, adjust_var_name = adjust_var_name,
                               na_check = TRUE)
univar_diff_female <- run_subcategory(final_nonmotor_diff, 
                               dat_final[which_female,], sub_col = "sub-category", 
                               sum_rows = TRUE, adjust_var_name = NULL,
                               na_check = TRUE)[[1]]
univar_diff_male <- run_subcategory(final_nonmotor_diff, 
                                      dat_final[which_male,], sub_col = "sub-category", 
                                      sum_rows = TRUE, adjust_var_name = NULL,
                                      na_check = TRUE)[[1]]

# nonmotor each sub-subcategory individually
univar_diff_subcat = run_subcategory(final_nonmotor_diff,
                                     dat_final, sub_col = "sub-subcategory",
                                     sum_rows = FALSE,
                                     adjust_var_name = adjust_var_name,
                                     na_check = TRUE)
univar_diff_subcat_female = run_subcategory(final_nonmotor_diff,
                                     dat_final[which_female,], sub_col = "sub-subcategory",
                                     sum_rows = FALSE,
                                     adjust_var_name = NULL,
                                     na_check = TRUE)[[1]]
univar_diff_subcat_male = run_subcategory(final_nonmotor_diff,
                                            dat_final[which_male,], sub_col = "sub-subcategory",
                                            sum_rows = FALSE,
                                            adjust_var_name = NULL,
                                            na_check = TRUE)[[1]]

# combine and save
univar_nonmotor <- do.call(rbind, Filter(Negate(is.null), list(univar_general$univar, 
                                                               univar_diff$univar,
                                                               univar_diff_subcat$univar)))
univar_nonmotor_gender <- do.call(rbind, Filter(Negate(is.null), list(univar_general$univar_gender, 
                                                                      univar_diff$univar_gender,
                                                                      univar_diff_subcat$univar_gender)))
save_univar_results(univar_nonmotor, univar_nonmotor_gender, "Non-motor symptoms", "univariate_nonmotor")

univar_nonmotor_female = do.call(rbind, Filter(Negate(is.null), list(univar_general_female, 
                                                                     univar_diff_female,
                                                                     univar_diff_subcat_female)))
univar_nonmotor_male = do.call(rbind, Filter(Negate(is.null), list(univar_general_male, 
                                                                     univar_diff_male,
                                                                     univar_diff_subcat_male)))
save_univar_results_gender(univar_nonmotor_female,univar_nonmotor_male,"Non-motor symptoms", "univariate_nonmotor")

# ---- Contacts healthcare system ----
final_healthcare <- final_ALS_CTR_category_temp[final_ALS_CTR_category_temp$category == "contacts healthcare system", , drop = FALSE]
univar_healthcare <- run_subcategory(final_healthcare, dat_final, sub_col = "sub-category", 
                                     sum_rows = TRUE, adjust_var_name = adjust_var_name,
                                     na_check = FALSE)
save_univar_results(univar_healthcare$univar, univar_healthcare$univar_gender, "Contacts healthcare system", "univariate_healthcare")

univar_healthcare_female = run_subcategory(final_healthcare, dat_final[which_female,], sub_col = "sub-category", 
                                           sum_rows = TRUE, adjust_var_name = NULL,
                                           na_check = FALSE)[[1]]

univar_healthcare_male = run_subcategory(final_healthcare, dat_final[which_male,], sub_col = "sub-category", 
                                           sum_rows = TRUE, adjust_var_name = NULL,
                                           na_check = FALSE)[[1]]
save_univar_results_gender(univar_healthcare_female,univar_healthcare_male,"Contacts healthcare system", "univariate_healthcare")

# ---- Pre-existing conditions ----
final_preconditions <- final_ALS_CTR_category_temp[final_ALS_CTR_category_temp$category == "pre-existing conditions", , drop = FALSE]
univar_preconditions <- run_subcategory(final_preconditions, dat_final, 
                                        sub_col = "sub-category", sum_rows = TRUE, 
                                        adjust_var_name = adjust_var_name,
                                        na_check = FALSE, na_yes_check = TRUE)
univar_preconditions_female <- run_subcategory(final_preconditions, dat_final[which_female,], 
                                        sub_col = "sub-category", sum_rows = TRUE, 
                                        adjust_var_name = NULL,
                                        na_check = FALSE, na_yes_check = TRUE)[[1]]
univar_preconditions_male <- run_subcategory(final_preconditions, dat_final[which_male,], 
                                               sub_col = "sub-category", sum_rows = TRUE, 
                                               adjust_var_name = NULL,
                                               na_check = FALSE, na_yes_check = TRUE)[[1]]

univar_preconditions_subcat = run_subcategory(final_preconditions,dat_final,
                                              sub_col = "sub-subcategory",sum_rows = FALSE,
                                              adjust_var_name = adjust_var_name,
                                              na_check = FALSE, na_yes_check = TRUE)
univar_preconditions_subcat_female = run_subcategory(final_preconditions,dat_final[which_female,],
                                              sub_col = "sub-subcategory",sum_rows = FALSE,
                                              adjust_var_name = NULL,
                                              na_check = FALSE, na_yes_check = TRUE)[[1]]
univar_preconditions_subcat_male = run_subcategory(final_preconditions,dat_final[which_male,],
                                              sub_col = "sub-subcategory",sum_rows = FALSE,
                                              adjust_var_name = NULL,
                                              na_check = FALSE, na_yes_check = TRUE)[[1]]

# combine and save
univar_preconditions_all <- do.call(rbind, Filter(Negate(is.null), list(univar_preconditions$univar, 
                                                                        univar_preconditions_subcat$univar)))
univar_preconditions_all_gender <- do.call(rbind, Filter(Negate(is.null), 
                                                         list(univar_preconditions$univar_gender, 
                                                              univar_preconditions_subcat$univar_genderunivar_diff_subcat$univar_gender)))
save_univar_results(univar_preconditions_all, univar_preconditions_all_gender, "pre-existing conditions", "univariate_preexisting")


univar_preconditions_all_female = do.call(rbind, Filter(Negate(is.null), list(univar_preconditions_female, 
                                                                              univar_preconditions_subcat_female)))
univar_preconditions_all_male = do.call(rbind, Filter(Negate(is.null), list(univar_preconditions_male, 
                                                                              univar_preconditions_subcat_male)))
save_univar_results_gender(univar_preconditions_all_female,univar_preconditions_all_male,"pre-existing conditions", "univariate_preexisting")

# ---- Diet and weight ----
final_dietweight <- final_ALS_CTR_category_temp[final_ALS_CTR_category_temp$category == "diet and weight", , drop = FALSE]
sel_cols_diet <- intersect(make.names(final_dietweight$`original question (ALS)`), colnames(dat_final))
dat_dietweight_all <- dat_final[, c(sel_cols_diet, "status"), drop = FALSE]
dat_dietweight_all <- attach_status(dat_dietweight_all)
dat_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegane.Ernährung.umgestellt. = ifelse(!is.na(dat_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegetarische.Ernährung.umgestellt.),
                                                                                               "Ja","Nein")
dat_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegetarische.Ernährung.umgestellt. = ifelse(!is.na(dat_dietweight_all$Wann.haben.Sie.die.Ernährung.auf.eine.vegetarische.Ernährung.umgestellt.),
                                                                                               "Ja","Nein")
univar_dietweight <- list(
  univar = run_univariate(dat_dietweight_all, adjust_var_name = NULL),
  univar_gender = run_univariate(dat_dietweight_all, adjust_var_name = adjust_var_name)
)
save_univar_results(univar_dietweight$univar, univar_dietweight$univar_gender, "Diet and weight", "univariate_dietweight")

univar_dietweight_female = run_univariate(dat_dietweight_all[which_female,], adjust_var_name = NULL)
univar_dietweight_male = run_univariate(dat_dietweight_all[which_male,], adjust_var_name = NULL)
save_univar_results_gender(univar_dietweight_female,univar_dietweight_male,"Diet and weight", "univariate_dietweight")

# ---- Lifestyle ----
final_lifestyle <- final_ALS_CTR_category_temp[final_ALS_CTR_category_temp$category == "lifestyle", , drop = FALSE] %>%
  mutate(`sub-category` = ifelse(is.na(`sub-subcategory`),`question in EN (ALS)`,`sub-category`))
# only categorical
dat_final_lifestyle_categorical = dat_final
final_lifestyle_categorical = final_lifestyle %>% filter(is.na(`sub-subcategory`) & col_classification == "categorical")
dat_final_lifestyle_categorical[,colnames(dat_final) %in%
        make.names(final_lifestyle_categorical$`original question (ALS)`)] = lapply(dat_final_lifestyle_categorical[,colnames(dat_final) %in%
                                        make.names(final_lifestyle_categorical$`original question (ALS)`)],factor)
                               
univar_lifestyle_cat <- run_subcategory(final_lifestyle_categorical, 
                                    dat_final_lifestyle_categorical, sub_col = "sub-category", sum_rows = FALSE, 
                                      adjust_var_name = adjust_var_name,na_check = FALSE,
                                      na_yes_check = FALSE)
univar_lifestyle_cat_female = run_subcategory(final_lifestyle_categorical, 
                                              dat_final_lifestyle_categorical[which_female,], 
                                              sub_col = "sub-category", sum_rows = FALSE, 
                                              adjust_var_name = NULL,na_check = FALSE,
                                              na_yes_check = FALSE)
univar_lifestyle_cat_male = run_subcategory(final_lifestyle_categorical, 
                                              dat_final_lifestyle_categorical[which_male,], 
                                              sub_col = "sub-category", sum_rows = FALSE, 
                                              adjust_var_name = NULL,na_check = FALSE,
                                              na_yes_check = FALSE)

# binary or continuous
dat_final_lifestyle_categorical = dat_final
final_lifestyle_categorical = final_lifestyle %>% filter(is.na(`sub-subcategory`) & col_classification == "binary")
dat_final_lifestyle_categorical[,colnames(dat_final) %in%
                                  make.names(final_lifestyle_categorical$`original question (ALS)`)] = lapply(dat_final_lifestyle_categorical[,colnames(dat_final) %in%
                                                                                                                                                make.names(final_lifestyle_categorical$`original question (ALS)`)],factor)
univar_lifestyle <- run_subcategory(final_lifestyle_categorical, 
                                    dat_final_lifestyle_categorical, sub_col = "sub-category", 
                                    sum_rows = FALSE, 
                                    adjust_var_name = adjust_var_name,na_check = FALSE,
                                    na_yes_check = TRUE)
univar_lifestyle_female = run_subcategory(final_lifestyle_categorical, 
                                          dat_final_lifestyle_categorical[which_female,], 
                                          sub_col = "sub-category", sum_rows = FALSE, 
                                          adjust_var_name = NULL,na_check = FALSE,
                                          na_yes_check = FALSE)[[1]]
univar_lifestyle_male = run_subcategory(final_lifestyle_categorical, 
                                          dat_final_lifestyle_categorical[which_male,], 
                                          sub_col = "sub-category", sum_rows = FALSE, 
                                          adjust_var_name = NULL,na_check = FALSE,
                                          na_yes_check = FALSE)[[1]]

univar_lifestyle_subcat = run_subcategory(final_lifestyle%>% filter(!is.na(`sub-subcategory`)),
                                          dat_final,sub_col = "sub-subcategory", sum_rows = FALSE, 
                                          adjust_var_name = adjust_var_name,na_check = FALSE,
                                          na_yes_check = TRUE,sum_higher = TRUE)
univar_lifestyle_subcat_female = run_subcategory(final_lifestyle%>% filter(!is.na(`sub-subcategory`)),
                                          dat_final[which_female,],sub_col = "sub-subcategory", sum_rows = FALSE, 
                                          adjust_var_name = NULL,na_check = FALSE,
                                          na_yes_check = TRUE,sum_higher = TRUE)[[1]]
univar_lifestyle_subcat_male = run_subcategory(final_lifestyle%>% filter(!is.na(`sub-subcategory`)),
                                                 dat_final[which_male,],sub_col = "sub-subcategory", sum_rows = FALSE, 
                                                 adjust_var_name = NULL,na_check = FALSE,
                                                 na_yes_check = TRUE,sum_higher = TRUE)[[1]]

univar_lifestyle_kinder = list(
  run_univariate(dat_final[, c(make.names("Wie viele Kinder haben Sie?"), "status"), drop = FALSE],adjust_var_name = NULL),
  run_univariate(dat_final[, c(make.names("Wie viele Kinder haben Sie?"), "status"), drop = FALSE],adjust_var_name = adjust_var_name)
  )
univar_lifestyle_kinder_female = run_univariate(dat_final[which_female, c(make.names("Wie viele Kinder haben Sie?"), "status"), drop = FALSE],
                                                adjust_var_name = NULL)
univar_lifestyle_kinder_male = run_univariate(dat_final[which_male, c(make.names("Wie viele Kinder haben Sie?"), "status"), drop = FALSE],
                                                adjust_var_name = NULL)

# combine and save
univar_lifestyle_all <- do.call(rbind, Filter(Negate(is.null), list(univar_lifestyle$univar, 
                                                                    univar_lifestyle_cat$univar,
                                                                    univar_lifestyle_subcat$univar,
                                                                    univar_lifestyle_kinder$univar)))
univar_lifestyle_all_gender <- do.call(rbind, Filter(Negate(is.null), 
                                                         list(univar_lifestyle$univar_gender, 
                                                              univar_lifestyle_cat$univar_gender,
                                                              univar_lifestyle_subcat$univar_gender,
                                                              univar_lifestyle_kinder$univar_gender)))
save_univar_results(univar_lifestyle_all, univar_lifestyle_all_gender, "Lifestyle", "univariate_lifestyle")

univar_lifestyle_all_female = do.call(rbind, Filter(Negate(is.null), list(univar_lifestyle_female, 
                                                                          univar_lifestyle_cat_female$univar,
                                                                          univar_lifestyle_subcat_female,
                                                                          univar_lifestyle_kinder_female)))
univar_lifestyle_all_male = do.call(rbind, Filter(Negate(is.null), list(univar_lifestyle_male, 
                                                                          univar_lifestyle_cat_male$univar,
                                                                          univar_lifestyle_subcat_male,
                                                                          univar_lifestyle_kinder_male)))
save_univar_results_gender(univar_lifestyle_all_female,univar_lifestyle_all_male,"Lifestyle", "univariate_lifestyle")


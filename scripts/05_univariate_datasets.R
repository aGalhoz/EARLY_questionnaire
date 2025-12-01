## Make final datasets with main categories and sub-categories

## function to clean datasets
split_category_vec <- function(x) {
  has_dots <- grepl("\\.\\.\\.", x)
  
  main <- ifelse(
    has_dots,
    sub("\\.\\.\\..*$", "", x),
    x
  )
  
  specific_raw <- ifelse(
    has_dots,
    sub("^.*?\\.\\.\\.", "", x),
    NA
  )
  
  specific_raw <- ifelse(
    !is.na(specific_raw),
    sub("\\.Skala.*$", "", specific_raw),
    NA
  )
  
  specific <- ifelse(
    !is.na(specific_raw),
    gsub("\\.", " ", specific_raw),
    NA
  )
  
  list(
    Main_category = main,
    Specific_category = trimws(specific)
  )
}

# function to run univariate together the full sample, female and male results
process_univar <- function(data, group_name) {
  
  df <- preparate_data_forest(data) %>%
    dplyr::select(c(Variables, exp_mean, mean, `2.5 %`, `97.5 %`, lower, upper, `Pr(>|z|)`)) %>%
    dplyr::rename(
      "odds-ratio (OR)" = exp_mean,
      "log(odds-ratio)" = mean,
      "2.5% (OR)" = `2.5 %`,
      "97.5% (OR)" = `97.5 %`,
      "log(2.5% [OR])" = lower,
      "log(97.5% [OR])" = upper,
      "P-value" = `Pr(>|z|)`
    ) %>%
    filter(!Variables %in% c(
      "Bitte.geben.Sie.Ihr.Geschlecht.an.weiblich",
      "Bitte.geben.Sie.Ihr.Geschlecht.an."
    )) %>%
    mutate(
      out = map(Variables, split_category_vec),
      "Main category" = map_chr(out, "Main_category"),
      "Specific category" = map_chr(out, "Specific_category"),
      "Specific category" = ifelse(is.na(`Specific category`), `Main category`, `Specific category`),
      "P-adjusted value" = p.adjust(`P-value`, method = "fdr"),
      type = group_name
    ) %>%
    select(-out)
  
  return(df)
}

# 1.1: Non-motor symptoms
datasets <- list(
  "Full sample" = univar_nonmotor_gender,
  "Female" = univar_nonmotor_female,
  "Male" = univar_nonmotor_male
)

univariate_nonmotor_general <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univariate_nonmotor_general <- univariate_nonmotor_general %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univariate_nonmotor_general, "data code output/univariate_nonmotor_general.xlsx")

# 1.2: Pre-existing conditions
datasets <- list(
  "Full sample" = univar_preconditions_all_gender,
  "Female" = univar_preconditions_all_female,
  "Male" = univar_preconditions_all_male
)

univar_preconditions_general <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_preconditions_general <- univar_preconditions_general %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_preconditions_general, "data code output/univar_preconditions_general.xlsx")

# 1.3: Lifestyle
datasets <- list(
  "Full sample" = univar_lifestyle_all_gender,
  "Female" = univar_lifestyle_all_female,
  "Male" = univar_lifestyle_all_male
)

univar_lifestyle_general <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_lifestyle_general <- univar_lifestyle_general %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_lifestyle_general, "data code output/univar_lifestyle_general.xlsx")

# 1.4: healthcare
datasets <- list(
  "Full sample" = univar_healthcare$univar_gender,
  "Female" = univar_healthcare_female,
  "Male" = univar_healthcare_male
)

univar_healthcare_general <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_healthcare_general <- univar_healthcare_general %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_healthcare_general, "data code output/univar_healthcare_general.xlsx")

# 1.5: diet and weight
datasets <- list(
  "Full sample" = univar_dietweight$univar_gender,
  "Female" = univar_dietweight_female,
  "Male" = univar_dietweight_male
)

univar_dietweight_general <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_dietweight_general <- univar_dietweight_general %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_dietweight_general, "data code output/univar_dietweight_general.xlsx")

# Stratify by full sample, female and male 
univariate_all_together = do.call("rbind",list(univariate_nonmotor_general,
                                                 univar_preconditions_general,
                                                 univar_lifestyle_general,
                                                 univar_healthcare_general,
                                                 univar_dietweight_general))

# -> Full sample
univariate_all_fullsample = univariate_all_together %>% 
  filter(type == "Full sample") %>%
  mutate("P-adjusted value" = p.adjust(`P-value`, method = "fdr"))

# -> Female
univariate_all_female = univariate_all_together %>% 
  filter(type == "Female") %>%
  mutate("P-adjusted value" = p.adjust(`P-value`, method = "fdr"))

# -> Male
univariate_all_male = univariate_all_together %>% 
  filter(type == "Male") %>%
  mutate("P-adjusted value" = p.adjust(`P-value`, method = "fdr"))

writexl::write_xlsx(univariate_all_fullsample,"data code output/univariate_fullsample.xlsx")
writexl::write_xlsx(univariate_all_female,"data code output/univariate_female.xlsx")
writexl::write_xlsx(univariate_all_male,"data code output/univariate_male.xlsx")



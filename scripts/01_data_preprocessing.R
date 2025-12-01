#######################################################################
###                       AUXILIAR FUNCTIONS                        ###
#######################################################################

############################################################
# 1. Replace "N/A" and "NA" strings with real NA
clean_na_strings <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col[col %in% c("N/A", "NA")] <- NA
    }
    col
  })
  df
}

############################################################
# 2. Convert German yes/no to numeric 1/0  
convert_ja_nein <- function(df) {
  df[] <- lapply(df, function(col) {
    if (!is.character(col)) return(col)
    if (!all(na.omit(unique(col)) %in% c("Ja", "Nein"))) return(col)
    ifelse(col == "Ja", 1L, ifelse(col == "Nein", 0L, NA))
  })
  df
}

############################################################
# 3. Count answered questions (1 = answered, 0 = missing)
answered_matrix <- function(df) {
  as.data.frame(lapply(df, function(col) {
    ifelse(is.na(col) | col %in% c("NA", "N/A"), 0L, 1L)
  }))
}

answered_summary <- function(df, prefix = "") {
  mat <- answered_matrix(df)
  data.frame(
    col = names(mat),
    frac = sapply(mat, function(x) paste0(sum(x), "/", length(x))),
    total = sapply(mat, sum),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

############################################################
# 4. Count positive answers (Ja = 1)
positive_matrix <- function(df) {
  df_bin <- convert_ja_nein(df)
  as.data.frame(lapply(df_bin, function(col) {
    if (!is.numeric(col)) return(rep(0, length(col)))
    ifelse(col == 1, 1L, 0L)
  }))
}

positive_summary <- function(df, prefix = "") {
  mat <- positive_matrix(df)
  data.frame(
    col = names(mat),
    frac = sapply(mat, function(x) paste0(sum(x,na.rm = T), "/", length(x))),
    total = sapply(mat, function(x) sum(x,na.rm = T)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

############################################################
# 5. Safe join — warns if keys mismatch
safe_left_join <- function(x, y, by) {
  missing_keys <- setdiff(x[[by]], y[[by]])
  if (length(missing_keys) > 0) {
    message("⚠️ Warning: ", length(missing_keys), " IDs missing when joining by ", by)
  }
  merge(x, y, by = by, all.x = TRUE)
}

############################################################
# 6. Apply DOB correction robustly
apply_dob <- function(df_old, df_new, dob_col_original, dob_col_new) {
  fixed <- ifelse(
    is.na(df_old[[dob_col_original]]),
    df_new[[dob_col_new]],
    df_old[[dob_col_original]]
  )
  fixed
}

############################################################
# 7. Split common vs cohort-only columns 
split_common_specific <- function(map_table_ALS, map_table_CTR, df_ALS, df_CTR) {
  
  # match_cols <- function(map_DE, df_names) {
  #   sapply(map_DE, function(col) {
  #     exact <- df_names[df_names == col]
  #     if(length(exact) > 0) return(exact)
  #     partial <- df_names[grepl(col, df_names, ignore.case = TRUE, fixed = TRUE)]
  #     if(length(partial) > 0) return(partial[1])
  #     NA
  #   })
  # }
  # 
  # 
  # print(match(common_EN, map_table_ALS$col_EN))
  # print(match(common_EN, map_table_CTR$col_EN))
  # 
  # ALS_common_DE <- match_cols(map_table_ALS$col_DE[match(common_EN, map_table_ALS$col_EN)], names(df_ALS))
  # CTR_common_DE <- match_cols(map_table_CTR$col_DE[match(common_EN, map_table_CTR$col_EN)], names(df_CTR))
  # 
  # # Remove NAs before subsetting
  # ALS_common_DE <- ALS_common_DE[!is.na(ALS_common_DE)]
  # CTR_common_DE <- CTR_common_DE[!is.na(CTR_common_DE)]
  # 
  # print(ALS_common_DE)
  # print(CTR_common_DE)
  
  ALS_common <- df_ALS[, which(map_table_ALS$col_EN %in% map_col_ALS_CTR$`question in EN (ALS)`), 
                       drop = FALSE]
  CTR_common <- df_CTR[, which(map_table_CTR$col_EN %in% c(map_col_ALS_CTR$`question in EN (CTR)`,"caffeinestop")), 
                       drop = FALSE]
  
  ALS_specific <- df_ALS[, setdiff(names(df_ALS), names(ALS_common)), drop = FALSE]
  CTR_specific <- df_CTR[, setdiff(names(df_CTR), names(CTR_common)), drop = FALSE]
  
  list(
    common_EN = map_table_ALS$col_EN[which(map_table_ALS$col_EN %in% map_col_ALS_CTR$`question in EN (ALS)`)],
    ALS_common = ALS_common,
    CTR_common = CTR_common,
    ALS_specific = ALS_specific,
    CTR_specific = CTR_specific
  )
}

############################################################
# 8. Build final summary table (ALS + CTR merged)
build_final_summary <- function(
    common_EN,
    frac_ALS_common, frac_CTR_common,
    pos_ALS_common, pos_CTR_common
) {
  data.frame(
    Name_EN = common_EN,
    ALS_frac = frac_ALS_common$frac,
    ALS_total = frac_ALS_common$total,
    CTR_frac = frac_CTR_common$frac,
    CTR_total = frac_CTR_common$total,
    ALS_positive_frac = pos_ALS_common$frac,
    ALS_positive_total = pos_ALS_common$total,
    CTR_positive_frac = pos_CTR_common$frac,
    CTR_positive_total = pos_CTR_common$total,
    stringsAsFactors = FALSE
  )
}


#######################################################################
###                     FULL ANALYSIS SCRIPT                        ###
#######################################################################

#######################################################################
# 1. LOAD ALL RAW DATA 

data_patients_new_2024 <- readr::read_csv(
  "original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Patienten _results-survey529321.csv"
)

data_control_new_2024 <- readr::read_csv(
  "original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Angehörige_results-survey333685.csv"
)

data_patients_old_en <- readr::read_csv("data input/ALS_Patient_Original.csv")
data_control_old_en  <- readr::read_csv("data input/ALS_Control_Original.csv",
                                        locale = readr::locale(encoding = "latin1"))


#######################################################################
# 2. BUILD COLUMN MAPS (DE ↔ EN)

original_col_ALS <- names(data_patients_new_2024)
original_col_CTR <- names(data_control_new_2024)

en_col_ALS <- sapply(strsplit(names(data_patients_old_en), ".", fixed = TRUE), `[`, 1)
en_col_CTR <- sapply(strsplit(names(data_control_old_en), ".", fixed = TRUE), `[`, 1)

map_col_ALS <- data.frame(col_DE = original_col_ALS, col_EN = en_col_ALS, stringsAsFactors = FALSE)
map_col_CTR <- data.frame(col_DE = original_col_CTR, col_EN = en_col_CTR, stringsAsFactors = FALSE)

map_col_ALS_CTR <- read_excel("data code output/final_table_questionnaire_new_deprecated.xlsx")
map_col_ALS_CTR$`original question (CTR)` <- ifelse(is.na(map_col_ALS_CTR$`original question (CTR)`),
                                                    map_col_ALS_CTR$`original question (ALS)`,
                                                    map_col_ALS_CTR$`original question (CTR)`)
map_col_ALS_CTR$`question in EN (CTR)` <- ifelse(is.na(map_col_ALS_CTR$`question in EN (CTR)`),
                                                 map_col_ALS_CTR$`question in EN (ALS)`,
                                                 map_col_ALS_CTR$`question in EN (CTR)`)

#######################################################################
# 3. Data with most updated DATES 

data_patients_new_2024 <- readr::read_delim(
  "original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Patienten _results-survey529321 copy.csv",
  delim = ";"
)

data_control_new_2024 <- readr::read_delim(
  "original data/EARLY_ALS_originaldata_250324/20240304_EARLY-ALS für Angehörige_results-survey333685 copy.csv",
  delim = ";"
)

#######################################################################
# 4. DOB from TEAR study (increase the amount of dates in the original data)

TEAR_study <- readr::read_csv("data input/20240517_TEAR_ALS_results-survey557312.csv")

# Extract DOB fields (patients + Angehörige)
TEAR_study_tmp <- TEAR_study[, c(
  "Zugangscode",
  "Geburtsmonat und -jahr des Patienten",
  "Geburtsmonat und -jahr des Angehörigen"
)]

Tokens_EARLY_TEAR <- readxl::read_excel("data input/Tokens_EARLY_TEAR.xlsx")

### --- DOB merges ---

tmp_pat <- safe_left_join(
  data_patients_new_2024,
  Tokens_EARLY_TEAR[, c("Patienten_Token_EARLY", "Patienten_Token_TEAR")] %>%
    dplyr::rename(Zugangscode = Patienten_Token_EARLY),
  by = "Zugangscode"
)

tmp_pat <- safe_left_join(
  tmp_pat,
  setNames(TEAR_study_tmp, c("Patienten_Token_TEAR", "DOB_pat_TEAR", "DOB_carer_ignore")),
  by = "Patienten_Token_TEAR"
)

data_patients_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <-
  apply_dob(
    df_old = data_patients_new_2024,
    df_new = tmp_pat,
    dob_col_original = "Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.",
    dob_col_new = "DOB_pat_TEAR"
  )

tmp_ctr <- safe_left_join(
  data_control_new_2024,
  Tokens_EARLY_TEAR[, c("Angehörige_Token_EARLY", "Angehörige_Token_TEAR", "Patienten_ID")] %>%
    dplyr::rename(Zugangscode = Angehörige_Token_EARLY),
  by = "Zugangscode"
)

tmp_ctr <- safe_left_join(
  tmp_ctr,
  setNames(TEAR_study_tmp, c("Angehörige_Token_TEAR", "DOB_ignore", "DOB_ctr_TEAR")),
  by = "Angehörige_Token_TEAR"
)

data_control_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <-
  apply_dob(
    df_old = data_control_new_2024,
    df_new = tmp_ctr,
    dob_col_original = "Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.",
    dob_col_new = "DOB_ctr_TEAR"
  )

#######################################################################
# 5. ADD ADDITIONAL DOBs (Rostock + Köln)

Referenzliste_Rostock <- readxl::read_excel("data input/Referenzliste_Rostock.xlsx")
DOB_dates_new <- readxl::read_excel("data input/EARLY_DOB_all_Rostock.xlsx")

# Match Rostock center DOBs
rost_match <- match(DOB_dates_new$Patienten_Probanden_ID,Referenzliste_Rostock$`Patienten/Probanden ID`)
RO_ref <- Referenzliste_Rostock[rost_match %>% unique() %>% na.omit(), c("Patienten/Probanden ID", "DOB Patient", "DOB Angehöriger")]

DOB_dates_new$DOB_Pat_clean[match(Referenzliste_Rostock$`Patienten/Probanden ID`,
                                  DOB_dates_new$Patienten_Probanden_ID) %>% na.omit()] <- format(as.Date(RO_ref$`DOB Patient`, "%Y-%m-%d"), "%d.%m.%Y")
DOB_dates_new$DOB_Angehoerige_clean[match(Referenzliste_Rostock$`Patienten/Probanden ID`,
                                          DOB_dates_new$Patienten_Probanden_ID) %>% na.omit()] <- format(as.Date(RO_ref$`DOB Angehöriger`, "%Y-%m-%d"), "%d.%m.%Y")

# Köln merges
DOB_dates_Koln <- readxl::read_excel(
  "data input/Referenzliste EARLY & TEAR-ALS_Cologne.xlsx",
  col_types = c("numeric", "date", "date")
)

DOB_dates_new <- rbind(
  DOB_dates_new,
  data.frame(
    Center = "Köln",
    Patienten_Probanden_ID = DOB_dates_Koln$`Patienten/Probanden ID`,
    DOB_Pat_clean = format(DOB_dates_Koln$`DOB Patient`, "%d.%m.%Y"),
    DOB_Angehoerige_clean = format(DOB_dates_Koln$`DOB Angehöriger`, "%d.%m.%Y")
  )
)

#######################################################################
# 6. Apply DOB lists for patients + controls 

### Controls
ctr_tmp <- safe_left_join(
  data_control_new_2024,
  Tokens_EARLY_TEAR[, c("Angehörige_Token_EARLY", "Patienten_ID")] %>%
    dplyr::rename(Zugangscode = Angehörige_Token_EARLY) ,
  by = "Zugangscode"
)

ctr_tmp <- safe_left_join(
  ctr_tmp,
  DOB_dates_new[, c("Patienten_Probanden_ID", "DOB_Angehoerige_clean")] %>%
    dplyr::rename(Patienten_ID = Patienten_Probanden_ID),
  by = "Patienten_ID"
)

ctr_tmp$DOB_Angehoerige_clean <- format(as.Date(ctr_tmp$DOB_Angehoerige_clean, "%d.%m.%Y"), "%m/%Y")

data_control_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <-
  apply_dob(data_control_new_2024, ctr_tmp,
            "Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.",
            "DOB_Angehoerige_clean")

### Patients
pat_tmp <- safe_left_join(
  data_patients_new_2024,
  Tokens_EARLY_TEAR[, c("Patienten_Token_EARLY", "Patienten_ID")] %>%
    dplyr::rename(Zugangscode = Patienten_Token_EARLY),
  by = "Zugangscode"
)

pat_tmp <- safe_left_join(
  pat_tmp,
  DOB_dates_new[, c("Patienten_Probanden_ID", "DOB_Pat_clean")] %>%
    dplyr::rename(Patienten_ID = Patienten_Probanden_ID),
  by = "Patienten_ID"
)

# manual overwrites preserved
pat_tmp$DOB_Pat_clean[pat_tmp$`Antwort ID` == 610] <- "01.01.1973"
pat_tmp$DOB_Pat_clean[pat_tmp$`Antwort ID` == 590] <- "01.01.1974"
pat_tmp$DOB_Pat_clean[pat_tmp$`Antwort ID` == 540] <- "01.01.1956"

pat_tmp$DOB_Pat_clean <- format(as.Date(pat_tmp$DOB_Pat_clean, "%d.%m.%Y"), "%m/%Y")

data_patients_new_2024$`Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.` <-
  apply_dob(data_patients_new_2024, pat_tmp,
            "Bitte geben Sie Ihren Geburtsmonat und das Geburtsjahr an.",
            "DOB_Pat_clean")

#######################################################################
# 7. Get patients with ALS & controls related to those patients
data_patients_new <- data_patients_new_2024 %>%
  filter(`Welche Diagnose wurde bei Ihnen gestellt? ` == "Amyotrophe Lateralsklerose (ALS)")
data_patients_new <- data_patients_new[c(1:6,8:514),]  # 513 ALS patients

data_control_new <- data_control_new_2024[data_control_new_2024$Zugangscode %in% data_patients_new$Zugangscode,] 
data_control_new <- data_control_new[c(1:5,7:306),] # 305 CTR patients, there is another repetitive control but has different answers

#######################################################################
# 7. Identify ALS / CTR datasets and keep only completed entries

df_pat <- data_patients_new[!is.na(data_patients_new$`Datum Abgeschickt`), ] # 38 IDs
df_ctr <- data_control_new[!is.na(data_control_new$`Datum Abgeschickt`), ] # 20 IDs

#######################################################################
# 8. Split common vs cohort-specific columns

split_cols <- split_common_specific(
  map_col_ALS, map_col_CTR,
  df_pat, df_ctr
)

ALS_common <- split_cols$ALS_common
CTR_common <- split_cols$CTR_common
ALS_specific <- split_cols$ALS_specific
CTR_specific <- split_cols$CTR_specific

#######################################################################
# 9. Compute answer summaries 

### Amount answered
sum_ALS_common  <- answered_summary(ALS_common)
sum_CTR_common  <- answered_summary(CTR_common)
sum_ALS_spec    <- answered_summary(ALS_specific)
sum_CTR_spec    <- answered_summary(CTR_specific)

### Amount positive
pos_ALS_common  <- positive_summary(ALS_common)
pos_CTR_common  <- positive_summary(CTR_common)
pos_ALS_spec    <- positive_summary(ALS_specific)
pos_CTR_spec    <- positive_summary(CTR_specific)

#######################################################################
# 10. Build final summary table (ALS vs CTR)

final_summary <- build_final_summary(
  common_EN = split_cols$common_EN,
  frac_ALS_common = sum_ALS_common,
  frac_CTR_common = sum_CTR_common,
  pos_ALS_common = pos_ALS_common,
  pos_CTR_common = pos_CTR_common
)

final_summary$Name_ALS_DE = colnames(split_cols$ALS_common)
final_summary$Name_CTR_DE = colnames(split_cols$CTR_common)

final_summary = final_summary[,c(1,10,11,2:9)]

#######################################################################
# 11. Export final results 

writexl::write_xlsx(
  list(
    ALS_common = ALS_common,
    ALS_specific = ALS_specific,
    CTR_common = CTR_common,
    CTR_specific = CTR_specific
  ),
  "data code output/EARLY_data.xlsx"
)

writexl::write_xlsx(final_summary,"data code output/Questionnaire_summary.xlsx")

info_ML <- read_excel("data code output/final_ALS_CTR_questionnaire_new_deprecated.xlsx")

info_ML <- info_ML %>%
  select(`question in EN (ALS)`,col_classification,use_ML,Note) %>%
  dplyr::rename(Name_EN = `question in EN (ALS)`)

final_summary <- final_summary %>%
  left_join(info_ML)

final_summary[is.na(final_summary$use_ML),12:14] <- data.frame(col_classification = c("binary",
                                                                                      rep("open-answer",26),
                                                                                      rep("binary",2)),
                                                               use_ML = rep(0),
                                                               Note = rep("Empty answers"))

writexl::write_xlsx(final_summary,"data code output/Questionnaire_summary.xlsx")

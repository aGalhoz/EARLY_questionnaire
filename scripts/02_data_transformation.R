#######################################################################
###                       AUXILIAR FUNCTIONS                        ###
#######################################################################

############################################################
# 1.Binary transformation 
transform_binary <- function(x) {
  x <- ifelse(x %in% c("Ja", 1), "Ja",
              ifelse(x %in% c("Nein", 0), "Nein", NA))
  factor(x, levels = c("Nein", "Ja"))
}

############################################################
# 2. Categorical transformation 
transform_categorical <- function(col_data_category){
  replacements <- c(
    "Vor 5-10Jahren|5-10Jahre|5 – 10 Jahre vor Erkrankungsbeginn" = "5-10Jahre",
    "Vor 1-5Jahren|1-5Jahre|1 – 5 Jahre vor Erkrankungsbeginn" = "1-5Jahre",
    "Vor 1-12Monaten|1-12Monate|1 – 12 Monate vor Erkrankungsbeginn" = "1-12Monate",
    "Bis zu 2 Standardgetränke.*" = "Bis zu 2 Standardgetränke",
    "Mehr als 2 Standardgetränke.*" = "Mehr als 2 Standardgetränke"
  )
  
  col_data_category <- str_replace_all(col_data_category, replacements)
  
  col_data_category[!is.na(col_data_category)] <- make.names(col_data_category[!is.na(col_data_category)])
  return(col_data_category)
}


############################################################
# 3. Date transformation to number of years 
transform_date <- function(x, start_year) {
  x_clean <- ifelse(str_detect(x, "bis"), str_split_fixed(x, " bis ", 2)[,1], x)
  x_clean <- case_when(
    str_length(x_clean) == 4 ~ as.numeric(x_clean),
    str_length(x_clean) == 7 ~ as.numeric(format(as.Date(paste0("01/", x_clean), "%d/%m/%Y"), "%Y")),
    TRUE ~ NA_real_
  )
  as.numeric(start_year) - x_clean
}

############################################################
# 3. Date duration transformation to number of years 
transform_date_duration <- function(x, start_year) {
  x_split <- str_split_fixed(x, " bis ", 2)
  start <- ifelse(str_length(x_split[,1]) == 4, as.numeric(x_split[,1]), NA_real_)
  end   <- ifelse(str_length(x_split[,2]) == 4, as.numeric(x_split[,2]), NA_real_)
  col_time <- as.numeric(start_year) - start
  col_duration <- end - start
  list(time = col_time, duration = col_duration)
}

#######################################################################
###                     FULL ANALYSIS SCRIPT                        ###
#######################################################################

#######################################################################
# 1. Combine patients and controls into one dataset

# Subset and standardize column names
data_patients_temp <- ALS_common
data_control_temp <- CTR_common

data_patients_temp$status <- 1
data_control_temp$status <- 0

# Make column names consistent
colnames(data_control_temp) <- colnames(data_patients_temp)

data_combined <- rbind(data_patients_temp, data_control_temp)

# Identify start year for date-time calculations
start_year <- format(as.Date(data_combined$`Datum gestartet`, "%Y-%m-%d"), "%Y")

# Filter only ML-relevant questions
final_ALS_CTR_interest <- final_summary %>%
  filter(use_ML == 1)

data_combined = data_combined[,c(match(final_ALS_CTR_interest$Name_ALS_DE,colnames(data_combined)),559)] # 492 (491 questions, 1 status)

data_combined[data_combined == "N/A"] = NA
#######################################################################
# 2. Apply transformations based on question type 

# Continuous variables
questions_continuous <- final_ALS_CTR_interest %>%
  filter(col_classification == "continuous") %>%
  pull(Name_ALS_DE)

data_continuous <- data_combined[, questions_continuous, drop=FALSE] %>%
  lapply(as.numeric)

# Binary variables
questions_binary <- final_ALS_CTR_interest %>%
  filter(col_classification == "binary") %>%
  pull(Name_ALS_DE)

data_binary <- data_combined[, questions_binary, drop=FALSE] %>%
  lapply(transform_binary)

# Categorical variables
questions_categorical <- final_ALS_CTR_interest %>%
  filter(col_classification == "categorical") %>%
  pull(Name_ALS_DE)

data_categorical <- data_combined[, questions_categorical, drop=FALSE] %>%
  lapply(transform_categorical) %>%
  lapply(as.factor)

# Date-time variables
questions_datetime <- final_ALS_CTR_interest %>%
  filter(col_classification == "date-time") %>%
  pull(Name_ALS_DE)

data_datetime <- data_combined[, questions_datetime, drop=FALSE] %>%
  lapply(transform_date, start_year = start_year)

# Date-time-duration variables
questions_datetime_duration <- final_ALS_CTR_interest %>%
  filter(col_classification == "date-time-duration") %>%
  pull(Name_ALS_DE)

transformed_duration <- lapply(data_combined[, questions_datetime_duration, drop=FALSE],
                               transform_date_duration, start_year = start_year)

data_datetime_duration_time <- lapply(transformed_duration, function(l) l$time)
data_datetime_duration_duration <- lapply(transformed_duration, function(l) l$duration)

#######################################################################
### 4. Impute missing values ###

# Continuous: 0 (only impute the visits of doctors)
data_continuous[1:(length(data_continuous)-7)] <- lapply(data_continuous[1:(length(data_continuous)-7)], function(x) ifelse(is.na(x), 0, x))

# Binary: "Nein" (impute binary answers)
data_binary <- lapply(data_binary, function(x) { x[is.na(x)] <- "Nein"; x })

# Date-time: 0
# data_datetime <- lapply(data_datetime, function(x) ifelse(is.na(x), 0, x))
# data_datetime_duration_time <- lapply(data_datetime_duration_time, function(x) ifelse(is.na(x), 0, x))
# data_datetime_duration_duration <- lapply(data_datetime_duration_duration, function(x) ifelse(is.na(x), 0, x))

#######################################################################
### 5. Final dataset ready for ML ###

data_patients_control_combined_final <- cbind(
  as.data.frame(data_continuous),
  as.data.frame(data_binary),
  as.data.frame(data_categorical),
  as.data.frame(data_datetime),
  #as.data.frame(data_datetime_duration_time),
  as.data.frame(data_datetime_duration_duration),
  status = data_combined$status
)

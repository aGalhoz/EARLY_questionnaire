# classification of questions into open question, binary, date, etc.

library(stringr)

##### start transformation of the data depending on their classification
# -> filter to only questions we are using (484)
final_ALS_CTR_interest <- final_ALS_CTR_summary %>%
  filter(use_ML == 1)

data_patients_common_final_temp <- data_patients_common_final[,match(final_ALS_CTR_interest$`original question (ALS)`,
                                                                colnames(data_patients_common_final))]
data_control_common_final_temp <- data_control_common_final[,match(final_ALS_CTR_interest$`original question (CTR)`,
                                                                colnames(data_control_common_final))]
data_patients_common_final_temp$status <- 1
data_control_common_final_temp$status <- 0
colnames(data_control_common_final_temp) <- colnames(data_patients_common_final_temp)
data_patients_control_combined <- rbind(data_patients_common_final_temp,
                                        data_control_common_final_temp)

data_patients_common_final_all <- data_patients_common_final[,match(final_ALS_CTR_summary$`original question (ALS)`,
                                                                    colnames(data_patients_common_final))]
data_control_common_final_all <- data_control_common_final[,match(final_ALS_CTR_summary$`original question (CTR)`,
                                                                   colnames(data_control_common_final))]

data_patients_common_final_all$status <- 1
data_control_common_final_all$status <- 0
colnames(data_control_common_final_all) <- colnames(data_patients_common_final_all)
data_patients_control_combined_all <- rbind(data_patients_common_final_all,
                                        data_control_common_final_all)

# -> check which ones are continuous only (103 for now)
questions_continuous <- final_ALS_CTR_interest %>%
  filter(col_classification == "continuous")
data_patients_control_combined_continuous <- data_patients_control_combined[,colnames(data_patients_control_combined) %in% questions_continuous$`original question (ALS)`]
data_patients_control_combined_continuous_temp <- apply(data_patients_control_combined_continuous, 2, as.numeric)

# -> check which ones are binary only (191 for now)
questions_binary <- final_ALS_CTR_interest %>%
  filter(col_classification == "binary")
data_patients_control_combined_binary <- data_patients_control_combined[,colnames(data_patients_control_combined) %in% questions_binary$`original question (ALS)`]
data_patients_control_combined_binary_temp <- apply(data_patients_control_combined_binary,2,transform_binary)
data_patients_control_combined_binary_temp <- apply(data_patients_control_combined_binary_temp,2,as.factor)

transform_binary <- function(col_data_binary){
  unique_values <- na.omit(unique(col_data_binary))
  return_col <- col_data_binary
  if(unique_values[1] %in% c("Ja","Nein")){
    #return_col <- factor(col_data_binary,levels = c("0","1"),labels = c("Nein","Ja"))
    return_col <- ifelse(col_data_binary == "Ja","Ja","Nein")
  }
  if(unique_values[1] %in% c(" 1")){
    #return_col <- factor(col_data_binary,levels = c("1"),labels = c(" 1"))
    return_col <- ifelse(col_data_binary == " 1","Ja","Nein")
  }
  return(return_col)
}

# -> check which ones are categorical only (104)
questions_categorical <- final_ALS_CTR_interest %>%
  filter(col_classification == "categorical")
data_patients_control_combined_categorical <- data_patients_control_combined[,colnames(data_patients_control_combined) %in% questions_categorical$`original question (ALS)`]
data_patients_control_combined_categorical_temp <- apply(data_patients_control_combined_categorical, 2, transform_categorical)
data_patients_control_combined_categorical_temp <- apply(data_patients_control_combined_categorical_temp, 2, as.factor)

transform_categorical <- function(col_data_category){
  unique_values <- na.omit(unique(col_data_category))
  if(unique_values[1] %in% c("Vor 5-10Jahren","Vor 1-5Jahren","Vor 1-12Monaten","Vor < 1Monat",
                             "5-10Jahre","< 1Monat","1-5Jahre","1-12Monate","Vor",
                             "1 – 5 Jahre vor Erkrankungsbeginn","5 – 10 Jahre vor Erkrankungsbeginn",
                             "1 – 12 Monate vor Erkrankungsbeginn")){
    col_data_category <- ifelse((col_data_category == "Vor 5-10Jahren" | col_data_category == "5-10Jahre" | col_data_category == "5 – 10 Jahre vor Erkrankungsbeginn"),
                                "5-10Jahre",
                                ifelse((col_data_category == "Vor 1-5Jahren" | col_data_category == "1-5Jahre" | col_data_category == "1 – 5 Jahre vor Erkrankungsbeginn"),
                                       "1-5Jahre",
                                       ifelse((col_data_category == "Vor 1-12Monaten" | col_data_category == "1-12Monate" | col_data_category == "1 – 12 Monate vor Erkrankungsbeginn"),
                                              "1-12Monate",
                                              "< 1Monat")))
  }
  if(unique_values[1] %in% c("Nein, die Symptome der Motoneuronerkrankung bzw. der anderen bei mir bekannten Erkrankung(en) waren die ersten / einzigen Veränderungen, die ich an mir festgestellt habe",
                             "Ja, zurückblickend habe ich Veränderungen an mir festgestellt, diese aber nicht mit der Motoneuronerkrankung bzw. der anderen bei mir bekannten Erkrankung(en) in Verbindung gebracht",
                             "Nein, ich habe keine Veränderungen festgestellt bzw. die Symptome der bei mir bekannten Erkrankung(en) waren die ersten / einzigen Veränderungen, die ich an mir festgestellt habe",
                             "Ja, zurückblickend habe ich Veränderungen an mir festgestellt, diese aber nicht mit der bei mir bekannten Erkrankung(en) in Verbindung gebracht")){
    col_data_category <- ifelse((col_data_category == "Nein, die Symptome der Motoneuronerkrankung bzw. der anderen bei mir bekannten Erkrankung(en) waren die ersten / einzigen Veränderungen, die ich an mir festgestellt habe" |
                                   col_data_category == "Nein, ich habe keine Veränderungen festgestellt bzw. die Symptome der bei mir bekannten Erkrankung(en) waren die ersten / einzigen Veränderungen, die ich an mir festgestellt habe"),
                                "Nein, ich habe keine Veränderungen festgestellt bzw. die Symptome der bei mir bekannten Erkrankung(en) waren die ersten / einzigen Veränderungen, die ich an mir festgestellt habe",
                                "Ja, zurückblickend habe ich Veränderungen an mir festgestellt, diese aber nicht mit der bei mir bekannten Erkrankung(en) in Verbindung gebracht")
  }
  if(unique_values[1] %in% c("Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche",
                             "Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche",
                             "Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche",
                             "Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche")){
    col_data_category <- ifelse((col_data_category == "Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche" |
                                   col_data_category == "Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche"),
                                "Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche",
                                "Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag(Frauen) an max. 5 Tagen in der Woche")
  }
  col_data_category[!is.na(col_data_category)] <- make.names(col_data_category[!is.na(col_data_category)])
  return(col_data_category)
}

# -> check which ones are date-time only (31)
questions_datetime <- final_ALS_CTR_interest %>%
  filter(col_classification == "date-time")
data_patients_control_combined_datetime <- data_patients_control_combined[,colnames(data_patients_control_combined) %in% questions_datetime$`original question (ALS)`]
data_patients_control_combined_datetime_temp <- apply(data_patients_control_combined_datetime, 2, transform_date)

library(stringr)
transform_date <- function(col_data_datetime){
  unique_values <- na.omit(unique(col_data_datetime))
  start_year <- format(as.Date(data_patients_control_combined_all$`Datum gestartet`,format = "%Y-%m-%d"),"%Y")
  print(grepl("bis",unique_values))
  if(any(grepl("bis",unique_values))){
    bis_idx <- grep("bis",col_data_datetime)
    datetime <- col_data_datetime
    datetime[bis_idx] <- unlist(lapply(strsplit(datetime[bis_idx]," bis "),function(x) x[1]))
  } else{
    datetime <- col_data_datetime
  }
  datetime <- ifelse(str_length(datetime) == 4, 
                     format(as.Date(as.character(datetime),"%Y"),"%Y"),
                     ifelse(str_length(datetime) == 7,
                            format(as.Date(paste("01",datetime,sep="/"),"%d/%m/%Y"),"%Y"),
                            datetime)) # convert only to year 
  col_data_datetime <- as.numeric(start_year) - as.numeric(datetime) # get amount of years between start date and date written 
  return(col_data_datetime)
}

# -> check which ones are date-time with duration (62)
questions_datetime_duration <- final_ALS_CTR_interest %>%
  filter(col_classification == "date-time-duration")
data_patients_control_combined_datetime_duration <- data_patients_control_combined[,colnames(data_patients_control_combined) %in% questions_datetime_duration$`original question (ALS)`]
data_patients_control_combined_datetime_duration_temp <- apply(data_patients_control_combined_datetime_duration, 2, transform_date_duration)

transform_date_duration <- function(col_data_datetime){
  unique_values <- na.omit(unique(col_data_datetime))
  start_year <- format(as.Date(data_patients_control_combined_all$`Datum gestartet`,format = "%Y-%m-%d"),"%Y")
  if(any(grepl("bis",unique_values))){
    bis_idx <- grep("bis",col_data_datetime)
    datetime <- col_data_datetime
    datetime_final <- col_data_datetime
    datetime_final[bis_idx] <- unlist(lapply(strsplit(datetime[bis_idx]," bis "),function(x) x[2]))
    datetime[bis_idx] <- unlist(lapply(strsplit(datetime[bis_idx]," bis "),function(x) x[1]))
  } else{
    datetime <- col_data_datetime
    datetime_final <- col_data_datetime
  }
  datetime <- ifelse(str_length(datetime) == 4, 
                     format(as.Date(as.character(datetime),"%Y"),"%Y"),
                     ifelse(str_length(datetime) == 7,
                            format(as.Date(paste("01",datetime,sep="/"),"%d/%m/%Y"),"%Y"),
                            datetime)) # convert only to year 
  datetime_final <- ifelse(str_length(datetime_final) == 4, 
                           format(as.Date(as.character(datetime_final),"%Y"),"%Y"),
                           ifelse(str_length(datetime_final) == 7,
                                  format(as.Date(paste("01",datetime_final,sep="/"),"%d/%m/%Y"),"%Y"),
                                  ifelse(str_length(datetime_final) == 5, #heute 
                                         start_year,
                                         datetime_final))) # convert only to year 
  col_data_datetime <- as.numeric(start_year) - as.numeric(datetime) # get amount of years between start date and date written 
  col_data_duration <- as.numeric(datetime_final) - as.numeric(datetime)
  return(col_data_datetime)
}

# -> combine all continuous, categorical, binary and date-time variables
data_patients_control_combined_final <- cbind(data_patients_control_combined_continuous_temp,
                                              data_patients_control_combined_binary_temp,
                                              data_patients_control_combined_categorical_temp,
                                              data_patients_control_combined_datetime_temp,
                                              data_patients_control_combined_datetime_duration_temp)

### IMPUTATIONS
# -> continuous variables
idx_continuous_impute <- 8:ncol(data_patients_control_combined_continuous_temp)
data_patients_control_combined_continuous_imp_temp <- data_patients_control_combined_continuous_temp
data_patients_control_combined_continuous_imp_temp[,idx_continuous_impute] <- apply(data_patients_control_combined_continuous_imp_temp[,idx_continuous_impute], 2, 
                                                                                    function(col){
                                                                                      ifelse(is.na(col),0,col)
                                                                                    })
# -> binary variables
data_patients_control_combined_binary_imp_temp <- data_patients_control_combined_binary_temp
data_patients_control_combined_binary_imp_temp <- apply(data_patients_control_combined_binary_imp_temp, 2, 
                                                        function(col){
                                                          ifelse(is.na(col),"Nein",col)
                                                        })

# -> date-time variables
data_patients_control_combined_datetime_imp_temp <- data_patients_control_combined_datetime_temp
data_patients_control_combined_datetime_imp_temp <- apply(data_patients_control_combined_datetime_imp_temp, 2, 
                                                        function(col){
                                                          ifelse(is.na(col),0,col)
                                                        })
data_patients_control_combined_datetime_duration_imp_temp <- data_patients_control_combined_datetime_duration_temp
data_patients_control_combined_datetime_duration_imp_temp <- apply(data_patients_control_combined_datetime_duration_imp_temp, 2, 
                                                          function(col){
                                                            ifelse(is.na(col),0,col)
                                                          })

data_patients_control_combined_final_imp <- cbind(data_patients_control_combined_continuous_imp_temp,
                                                  data_patients_control_combined_binary_imp_temp,
                                                  data_patients_control_combined_categorical_temp,
                                                  data_patients_control_combined_datetime_temp,
                                                  data_patients_control_combined_datetime_duration_temp)

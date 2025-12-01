## 1. Prepare question annotation based on the new transformed dataset
question_annotation <- c(
  rep("Continuous", length(data_continuous)),
  rep("Binary", length(data_binary)),
  rep("Categorical", length(data_categorical)),
  rep("Continuous", length(data_datetime)),
  rep("Continuous", length(data_datetime_duration_time))
)

status <- factor(data_patients_control_combined_final$status)
status <- revalue(status, c("1" = "ALS", "0" = "Controls"))
status <- factor(status, levels = c("ALS", "Controls"))

## 2. No imputation: create NA matrix
dat <- data_combined[,c(1:(ncol(data_combined)-1))]
dat_na <- apply(dat, 2, function(x) ifelse(is.na(x), 1, 0))

# Annotations
column_an <- HeatmapAnnotation(Group = status,
                               col = list(Group = c("ALS" = "#5f91bd", "Controls" = "#BD8B5F")))
col_fun <- colorRamp2(c(0, 1), c("#8d99ae", "#e6e6e6"))

Var<- c("Binary" = "#81B29A", 
                 "Categorical" = "#E07A5F", 
                 "Continuous" = "#F2CC8F")
#Var <- setNames(sample(manualcolors, length(unique(question_annotation))), unique(question_annotation))

row_an <- rowAnnotation(
  Question = question_annotation,
  "%" = anno_barplot(apply(dat_na, 2, function(x) round(sum(x) * 100 / length(x), 3))),
  col = list(Question = Var)
)

# Heatmap of missingness (no imputation)
pdf("plots/heatmap_noimputation.pdf")
Heatmap(
  t(dat_na),
  top_annotation = column_an,
  left_annotation = row_an,
  show_column_names = FALSE,
  show_row_names = FALSE,
  show_heatmap_legend = FALSE,
  col = col_fun
)
dev.off()

## 3. Manual imputation: NA handling
# For continuous: already imputed (0 for visits/metrics)
# For binary: NA -> "Nein"
dat_imp <- data_patients_control_combined_final[,1:(ncol(data_patients_control_combined_final)-1)]

dat_na_imp <- apply(dat_imp, 2, function(x) ifelse(is.na(x), 1, 0))

row_an_imp <- rowAnnotation(
  Question = question_annotation,
  "%" = anno_barplot(apply(dat_na_imp, 2, function(x) round(sum(x) * 100 / length(x), 3))),
  col = list(Question = Var)
)

# Heatmap of missingness after imputation
pdf("plots/heatmap_manualimputation.pdf")
Heatmap(
  t(dat_na_imp),
  top_annotation = column_an,
  left_annotation = row_an_imp,
  show_column_names = FALSE,
  show_row_names = FALSE,
  show_heatmap_legend = FALSE,
  col = col_fun
)
dev.off()

# # Filtering for missingness
# # Filtering criteria: Features that have <= 70% NAs in both classes
# keep_cols = apply(dat_na_imp[,c(1:ncol(dat_na_imp))], 2, function(x) sum(x[status == "ALS"]) <= length(x[status == "ALS"])*70/100 &
#                     sum(x[status == "CTR"]) <= length(x[status == "CTR"])*70/100)
# # sparsity_annot <- apply(dat_na_imp,2,function(x) {round((sum(x)*100)/length(x),digits = 3)})
# # sparsity_annot_filter <- sparsity_annot[sparsity_annot < 30]  # 306 variables (use all -> 491)
# dat_fil = dat_imp[,which(keep_cols)]
# dat_na_imp = apply(dat_fil, 2, function(x) ifelse(is.na(x), 1, 0))
# question_annotation <- question_annotation[keep_cols]
# #Var <- setNames(sample(manualcolors,length(unique(question_annotation))), unique(question_annotation))
# row_an_imp <- rowAnnotation(
#   Question = question_annotation,
#   "%" = anno_barplot(apply(dat_na_imp, 2, function(x) round(sum(x) * 100 / length(x), 3))),
#   col = list(Question = Var)
# )
# Heatmap(t(dat_na_imp), top_annotation = column_an,
#         left_annotation = row_an_imp,
#         show_column_names = F, show_row_names = F,
#         show_heatmap_legend = F, col = col_fun)

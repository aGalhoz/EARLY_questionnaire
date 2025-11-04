## Imputation analysis 
library(ComplexHeatmap)
library(circlize)
library(plyr)

# question annotation based if categorical, continuous, binary or date-time
question_annotation <- c(rep("Continuous",ncol(data_patients_control_combined_continuous_temp)),
                         rep("Binary",ncol(data_patients_control_combined_binary_temp)),
                         rep("Categorical",ncol(data_patients_control_combined_categorical_temp)),
                         rep("Continuous",ncol(data_patients_control_combined_datetime_temp)),
                         rep("Continuous",ncol(data_patients_control_combined_datetime_duration_temp)))

status = factor(data_patients_control_combined_all$status)
status = revalue(status,c("1"="ALS","0"="Controls"))
status = factor(status, levels = c("ALS","Controls"))

# no imputation (818 IDs and 482 Questions)
dat = data_patients_control_combined_final
dat_na = apply(dat, 2, function(x) ifelse(is.na(x), 1, 0))
column_an = HeatmapAnnotation(Group = status, col = list(Group = c("ALS"="#5f91bd","Controls"="#BD8B5F")))
col_fun = colorRamp2(c(0, 1), c("#8d99ae", "#e6e6e6"))
manualcolors<-brewer.pal(n = 5, name = "Paired")
manualcolors<- c('forestgreen', 'orange',  'darkblue', 'seagreen1',"mediumvioletred")
manualcolors<- c( "#E07A5F",  "#3D405B", "#81B29A","#F2CC8F","#F4F1DE")
Var <- setNames(sample(manualcolors,length(unique(question_annotation))), unique(question_annotation))
row_an = rowAnnotation(Question = question_annotation,
                       "%" = anno_barplot(apply(dat_na,2,function(x) {round((sum(x)*100)/length(x),digits = 3)})),
                       col = list(Question = Var))
pdf("plots/heatmap_noimputation.pdf")
Heatmap(t(dat_na), top_annotation = column_an, 
        left_annotation = row_an,
        show_column_names = F, show_row_names = F,
        show_heatmap_legend = F, col = col_fun)
dev.off()

# manual imputation
dat_imp = data_patients_control_combined_final_imp
dat_na = apply(dat_imp, 2, function(x) ifelse(is.na(x), 1, 0))
column_an = HeatmapAnnotation(Group = status, col = list(Group = c("ALS"="#5f91bd","Controls"="#BD8B5F")))
col_fun = colorRamp2(c(0, 1), c("#8d99ae", "#e6e6e6"))
manualcolors<-brewer.pal(n = 5, name = "Paired")
manualcolors<- c('forestgreen', 'orange',  'darkblue', 'seagreen1',"mediumvioletred")
manualcolors<- c( "#E07A5F",  "#3D405B", "#81B29A","#F2CC8F","#F4F1DE")
Var <- setNames(sample(manualcolors,length(unique(question_annotation))), unique(question_annotation))
row_an = rowAnnotation(Question = question_annotation,
                       "%" = anno_barplot(apply(dat_na,2,function(x) {round((sum(x)*100)/length(x),digits = 3)})),
                       col = list(Question = Var))
pdf("plots/heatmap_manualimputation.pdf")
Heatmap(t(dat_na), top_annotation = column_an, 
        left_annotation = row_an,
        show_column_names = F, show_row_names = F,
        show_heatmap_legend = F, col = col_fun)
dev.off()

# Filtering for missingness
# Filtering criteria: Features that have <= 70% NAs in both classes
# keep_cols = apply(dat_na, 2, function(x) sum(x[status == "ALS"]) <= length(x[status == "ALS"])*70/100 &
#                     sum(x[status == "CTR"]) <= length(x[status == "CTR"])*70/100)
# sparsity_annot <- apply(dat_na,2,function(x) {round((sum(x)*100)/length(x),digits = 3)})
# sparsity_annot_filter <- sparsity_annot[sparsity_annot < 100]  # 304 variables (use all -> 485)
# dat_fil = dat_imp[,colnames(dat_imp) %in% names(sparsity_annot_filter)]
# dat_na = apply(dat_fil, 2, function(x) ifelse(is.na(x), 1, 0))
# column_an = HeatmapAnnotation(status = status, col = list(status = c("ALS"="#0073C2FF","CTR"="#EFC000FF")))
# col_fun = colorRamp2(c(0, 1), c("tomato3", "grey"))
# #question_annotation <- question_annotation[sparsity_annot < 25]
# Var <- setNames(sample(manualcolors,length(unique(question_annotation))), unique(question_annotation))
# row_an = rowAnnotation(question = question_annotation,
#                        sparsity = anno_barplot(apply(dat_na,2,function(x) {round((sum(x)*100)/length(x),digits = 3)})),
#                        col = list(question = Var))
# Heatmap(t(dat_na), top_annotation = column_an, 
#         left_annotation = row_an,
#         show_column_names = F, show_row_names = F,
#         show_heatmap_legend = F, col = col_fun)

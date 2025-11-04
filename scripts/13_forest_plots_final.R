# final forest plots

## Non-motor questions
# -> non-motor symptoms (all)
univariate_nonmotor_general_touse <- read_excel("data code output/univariate_nonmotor_general_touse.xlsx")
data_use = univariate_nonmotor_general_touse %>%
  filter(main_category != "Non-motor symptoms (all)")
p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_nonmotor_complete.pdf",height = 18,width = 8)
p
dev.off()

# -> non-motor (main) 
data_use <- univariate_nonmotor_general_touse %>%
  filter(main_category != "Non-motor symptoms (all)") %>%
  filter(grepl("main",Labels_plot)) %>%
  mutate(Labels_plot = gsub("\\s*\\(.*\\)", "",Labels_plot))
p = forest_plot_all(data_use,subcategory_bol = FALSE)

pdf(file = "plots/forest_nonmotor_main.pdf",height = 7,width = 8)
p
dev.off()

# -> non-motor (subcategories)
data_use <- univariate_nonmotor_general_touse %>%
  filter(main_category != "Non-motor symptoms (all)") %>%
  filter(!grepl("main",Labels_plot)) 
p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_nonmotor_subcategories.pdf",height = 12,width = 8)
p
dev.off()

## Pre-existing comorbidies 
# -> All pre-existing commorbidities 
univariate_preconditions_general_touse <- read_excel("data code output/univariate_preconditions_general_touse.xlsx")
univariate_preconditions_general_touse$`97.5 %` <- as.numeric(univariate_preconditions_general_touse$`97.5 %`)
univariate_preconditions_general_touse$lower <- as.numeric(univariate_preconditions_general_touse$lower)
univariate_preconditions_general_touse$upper <- as.numeric(univariate_preconditions_general_touse$upper)
data_use <- univariate_preconditions_general_touse %>%
  filter(main_category != "Pre-existing conditions (all)") %>%
  mutate(Labels_plot = gsub("\\s*\\(.*\\)", "",Labels_plot)) %>%
  mutate(Labels_plot = ifelse(Labels_plot == "Musculoskeletal system","Musculoskeletal system (main)",Labels_plot))

p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_preexisting_all.pdf",height = 12,width = 8)
p
dev.off()

# -> preexisting (main categories)
data_use <- univariate_preconditions_general_touse %>%
  filter(main_category != "Pre-existing conditions (all)") %>%
  filter(grepl("main",Labels_plot)) %>%
  mutate(Labels_plot = gsub("\\s*\\(.*\\)", "",Labels_plot))

p = forest_plot_all(data_use,subcategory_bol = FALSE)

pdf(file = "plots/forest_preexisting_main.pdf",height = 7,width = 8)
p
dev.off()

# -> preexisting (only suncategories)
data_use <- univariate_preconditions_general_touse %>%
  filter(main_category == "Musculoskeletal system")

p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_preexisting_subcategories.pdf",height = 4,width = 8)
p
dev.off()

## Lifestyle
univariate_lifestyle_general_touse <- read_excel("data code output/univariate_lifestyle_general_touse.xlsx")
univariate_lifestyle_general_touse <- univariate_lifestyle_general_touse %>%
  rename(Labels_plot = Features)

# -> Education 
data_use <- univariate_lifestyle_general_touse %>%
  filter(main_category %in% c("Highest professional degree"))

p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_lifestyle_1.pdf",height = 4,width = 8)
p
dev.off()

data_use <- univariate_lifestyle_general_touse %>%
  filter(main_category %in% c("Highest educational degree"))

p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_lifestyle_2.pdf",height = 4,width = 8)
p
dev.off()

# -> Sports, activity
data_use <- univariate_lifestyle_general_touse %>%
  filter(main_category %in% c("Physical activity","Sports"))
p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_lifestyle_physical_activity.pdf",height = 7,width = 8)
p
dev.off()

# -> Children, partnership
data_use <- univariate_lifestyle_general_touse %>%
  filter(main_category %in% c("Children","Partnership status","Infections","Life-changing events"))
p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_lifestyle_several.pdf",height = 10,width = 8)
p
dev.off()

# -> Substance
data_use <- univariate_lifestyle_general_touse %>%
  filter(main_category %in% c("Substance consumption"))
p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_lifestyle_substance.pdf",height = 7,width = 8)
p
dev.off()

## Contacts healthcare
univariate_healthcare_general_touse <- read_excel("data code output/univariate_healthcare_general_touse.xlsx")
data_use <- univariate_healthcare_general_touse %>%
  mutate(Labels_plot = main_category)

p = forest_plot_all(data_use,subcategory_bol = FALSE) + xlim(-0.5,1.8)

pdf(file = "plots/forest_healthcare.pdf",height = 9,width = 8)
p
dev.off()

## Height & Weight 
# -> Height & Weight (all)
univariate_dietweight_general_touse <- read_excel("data code output/univariate_dietweight_general_touse.xlsx")
univariate_dietweight_general_touse$`97.5 %` <- as.numeric(univariate_dietweight_general_touse$`97.5 %`)
univariate_dietweight_general_touse$lower <- as.numeric(univariate_dietweight_general_touse$lower)
univariate_dietweight_general_touse$upper <- as.numeric(univariate_dietweight_general_touse$upper)
data_use <- univariate_dietweight_general_touse %>%
  rename(Labels_plot = Features) %>%
  filter(!grepl("BMI",Labels_plot))

p = forest_plot_all(data_use,subcategory_bol = TRUE) 

pdf(file = "plots/forest_heightweight_all.pdf",height = 9,width = 8)
p
dev.off()

# -> Height & Weight (diet)
data_use <- univariate_dietweight_general_touse %>%
  rename(Labels_plot = Features) %>%
  filter(main_category == "Diet")

p = forest_plot_all(data_use,subcategory_bol = TRUE) 

pdf(file = "plots/forest_diet.pdf",height = 5,width = 8)
p
dev.off()

# -> Height & Weight (weight &bmi)
data_use <- univariate_dietweight_general_touse %>%
  rename(Labels_plot = Features) %>%
  filter(main_category != "Diet") %>%
  filter(!grepl("BMI",Labels_plot))

p = forest_plot_all(data_use,subcategory_bol = TRUE) 

pdf(file = "plots/forest_weight.pdf",height = 5,width = 8)
p
dev.off()

## Auxiliar functions
# forest plot
forest_plot_all <- function(data,subcategory_bol = TRUE){
  data$main_category = factor(data$main_category,
                              levels = unique(data$main_category))
  data$type <- factor(data$type,levels = c("Male","Female","Full sample"))
  data$Labels_plot <- factor(data$Labels_plot,levels = unique(data$Labels_plot))
  g <- ggplot(data,aes(x =  mean, y = fct_rev(Labels_plot))) + 
    theme_forest() +  scale_colour_ng_d(reverse = TRUE) +
    scale_fill_ng_d() +
    # Add striped background
    geom_stripes() +
    # Add vertical line at null point
    geom_vline(
      xintercept = 0,
      linetype = "solid",
      size = 0.4,
      colour = "black"
    ) + geom_effect(aes(xmin = lower,
                        xmax = upper,
                        colour = type),
                    position = ggstance::position_dodgev(height = 0.5)) +
    labs(x = "log(odds-ratio)", y = "",colour = "") + guides(colour = guide_legend(reverse = TRUE))
  if(subcategory_bol){
    g <- g + 
      ggforce::facet_col(
        facets = ~main_category,
        scales = "free_y",
        space = "free"
      )
  }
  return(g)
}

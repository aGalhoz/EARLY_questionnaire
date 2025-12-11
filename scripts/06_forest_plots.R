## Forest plots

## helper function to create the forest plots

forest_plot_all <- function(data,subcategory_bol = TRUE){
  data$`Main category` = factor(data$`Main category`,
                              levels = unique(data$`Main category`))
  data$type <- factor(data$type,levels = c("Male","Female","Full sample"))
  data$`Specific category` <- factor(data$`Specific category` ,levels = unique(data$`Specific category`))
  
  # ensure numeric columns are numeric
  data$`log(odds-ratio)` <- as.numeric(data$`log(odds-ratio)`)
  data$`log(2.5% [OR])` <- as.numeric(data$`log(2.5% [OR])`)
  data$`log(97.5% [OR])` <- as.numeric(data$`log(97.5% [OR])`)
  
  g <- ggplot(data,aes(x = `log(odds-ratio)`, y = fct_rev(`Specific category`))) + 
    scale_colour_ng_d(reverse = TRUE) +
    scale_fill_ng_d() +
    # Add striped background
    geom_stripes() +
    # Add vertical line at null point
    geom_vline(
      xintercept = 0,
      linetype = "solid",
      size = 0.4,
      colour = "black"
    ) + geom_effect(aes(xmin = `log(2.5% [OR])`,
                        xmax = `log(97.5% [OR])`,
                        colour = type),
                    position = ggstance::position_dodgev(height = 0.5)) +
    labs(x = "log(odds-ratio)", y = "",colour = "") + guides(colour = guide_legend(reverse = TRUE)) +
    theme_forest(base_size = 15) +
    theme(
      axis.text.y = element_text(size = 15),      # Y-axis study labels
      axis.text.x = element_text(size = 15),      # X-axis tick labels
      axis.title.x = element_text(size = 16),     # X-axis title
      strip.text = element_text(size = 17)        # Facet label size
    ) 
  #+
  xlim(-3,3.2)
  #+xlim(min(data$`log(2.5% [OR])`) - 10,max(data$`log(97.5% [OR])`)+10)
  if(subcategory_bol){
    g <- g + 
      ggforce::facet_col(
        facets = ~`Main category`,
        scales = "free_y",
        space = "free"
      )
  }
  return(g)
}

# -> non-motor (main) 
data_use <- univariate_nonmotor_general %>%
  filter(`Main category` == `Specific category`) %>%
  arrange(`P-value`)

p = forest_plot_all(data_use,subcategory_bol = FALSE)

pdf(file = "plots/forest_nonmotor_main.pdf",height = 6.5,width = 8)
p
dev.off()

# -> non-motor (subcategories)
top3 <- univariate_nonmotor_general %>%
  filter(`Main category` != `Specific category`,type == "Full sample") %>%
  arrange(`P-value`) %>%
  group_by(`Main category`) %>%
  slice_min(`P-value`, n = 3) %>%
  ungroup() %>%
  select(`Main category`, `Specific category`)

data_use <- univariate_nonmotor_general %>%
  semi_join(top3,by = c("Main category", "Specific category")) %>%
  mutate(`Main category` = factor(`Main category`,
                                   levels = data_use[data_use$type == "Full sample",]$`Main category`)) %>%
  group_by(`Main category`,`Specific category`) %>%
  arrange(`P-value`) %>%
  arrange(`Main category`) %>%
  filter(!is.na(`Main category`))

p = forest_plot_all(data_use[1:36,],subcategory_bol = TRUE)

pdf(file = "plots/forest_nonmotor_subcategories1.pdf",height = 6.5,width = 9)
p
dev.off()

p = forest_plot_all(data_use[37:72,],subcategory_bol = TRUE)

pdf(file = "plots/forest_nonmotor_subcategories2.pdf",height = 6.5,width = 9)
p
dev.off()

## -> Pre-existing comorbidies (main)
order <- univar_preconditions_general %>%
  filter(`Main category` == `Specific category` | `Main category` == "",type == "Full sample") %>%
  arrange(`P-value`)
data_use <- univar_preconditions_general %>%
  filter(`Main category` == `Specific category` | `Main category` == "") %>%
  mutate(`Main category` = factor(`Main category`,
                                  levels = order$`Main category`)) %>%
  arrange(`Main category`) 

data_use$`log(2.5% [OR])` = ifelse(is.na(data_use$`log(2.5% [OR])`),-Inf,data_use$`log(2.5% [OR])`)
  data_use$`Specific category` = ifelse(data_use$`Specific category` == "Endocrine, nutritional and metabolic diseases",
                                  "Endocrine, nutritional and \nmetabolic diseases",
                                  data_use$`Specific category`)

p = forest_plot_all(data_use,subcategory_bol = FALSE)

pdf(file = "plots/forest_preexisting_main.pdf",height = 6.5,width = 9)
p
dev.off()

## -> Pre-existing conditions (subcategories)
top3 <- univar_preconditions_general %>%
  filter(`Main category` != `Specific category`,type == "Full sample",
         `Main category` == "Musculoskeletal disorders") %>%
  arrange(`P-value`) %>%
  group_by(`Main category`) %>%
  slice_min(`P-value`, n = 3) %>%
  ungroup() %>%
  select(`Main category`, `Specific category`)

data_use <- univar_preconditions_general %>%
  semi_join(top3,by = c("Main category", "Specific category")) %>%
  arrange(`P-value`) 

data_use$`log(2.5% [OR])` = ifelse(is.na(data_use$`log(2.5% [OR])`),-Inf,data_use$`log(2.5% [OR])`)

p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_preexisting_subcat.pdf",height = 2.95,width = 7.6)
p
dev.off()

## Lifestyle
# -> panel A: relationship, parental, major life events and frequency of infections
top3 <- univar_lifestyle_general %>%
  filter(`Main category` %in% c("Relationship status","Parental status",
                                "Major life events","Frequency of infections"),
         type == "Full sample") %>%
  arrange(`P-value`) %>%
  group_by(`Main category`) %>%
  slice_min(`P-value`, n = 3) %>%
  ungroup() %>%
  select(`Main category`, `Specific category`) %>%
  mutate(`Main category` = factor(`Main category`,
                                  levels = c("Relationship status","Parental status",
                                               "Major life events","Frequency of infections"))) %>%
  arrange(`Main category`)

data_use <- univar_lifestyle_general %>%
  semi_join(top3,by = c("Main category", "Specific category")) %>%
  arrange(`P-value`) %>%
  mutate(`Main category` = factor(`Main category`,
                                  levels = c("Relationship status","Parental status",
                                             "Major life events","Frequency of infections")),
         `Specific category` = factor(`Specific category`,
                                      levels = top3$`Specific category`)) %>%
  arrange(`Main category`,`Specific category`)

p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_lifestyle1.pdf",height = 6.5,width = 7.5)
p
dev.off()

# -> panel B: educational and professional degree
data_use = univar_lifestyle_general %>%
  filter(`Main category` %in% c("Educational degree","Professional degree")) %>%
  arrange(`P-value`) %>%
  mutate(`Main category` = factor(`Main category`,levels = c("Educational degree",
                                                             "Professional degree")),
         `Specific category` = factor(`Specific category`,
                                      levels = c("General qualification for university entrance",
                                                 "Subject-specific university entrance qualification",
                                                 "Comprehensive school","Vocational school",
                                                 "Secondary school",
                                                 "Lower secondary school","Primary school",
                                                 "University degree (Bachelor's, Master's, or equivalent degree)",
                                                 "Vocational/tecnhnical school or business academy",
                                                 "Master craftsman examination",
                                                 "Apprenticeship/skilled worker qualification",
                                                 "No professional qualification","Other"))) %>%
  arrange(`Main category`,`Specific category`)

p = forest_plot_all(data_use,subcategory_bol = TRUE)

pdf(file = "plots/forest_lifestyle2.pdf",height = 6.5,width = 10.5)
p
dev.off()

# -> panel C: physical activity and sports
data_use = univar_lifestyle_general %>%
  filter(`Main category` %in% c("Physical activity and sports")) %>%
  arrange(`P-value`) %>%
  mutate(`Specific category` = factor(`Specific category`,
                                      levels = c("Occupational physical activity (intensive)",
                                                 "Occupational physical activity (moderate)",
                                                 "Occupational physical activity (low)",
                                                 "Recreational sports activity",
                                                 "Recreational sports activity (high intensity)",
                                                 "Recreational sports activity (moderate intensity)",
                                                 "Recreational sports activity (low intensity)",
                                                 "Contact sports","Strength training",
                                                 "Endurance sports","Racket sports",
                                                 "Exceptional high physical activity"))) %>%
  arrange(`Specific category`)

p = forest_plot_all(data_use,subcategory_bol = FALSE)

pdf(file = "plots/forest_lifestyle3.pdf",height = 6.5,width = 10.5)
p
dev.off()

# -> panel D: Substance use
data_use = univar_lifestyle_general %>%
  filter(`Main category` %in% c("Substance use")) %>%
  arrange(`P-value`) %>%
  mutate(`Specific category` = factor(`Specific category`,
                                      levels = c("Caffeine consumption (currently)",
                                                 "Caffeine consumption (in the past)",
                                                 "Caffeine consumption (never)",
                                                 "Alcohol consumption (currently)",
                                                 "Alcohol consumption (in the past)",
                                                 "Alcohol consumption (never)",
                                                 "Cigarette consumption (currently)",
                                                 "Cigarette consumption (in the past)",
                                                 "Cigarette consumption (never)",
                                                 "Illegal drugs consumption (currently)",
                                                 "Illegal drugs consumption (in the past)",
                                                 "Illegal drugs consumption (never)"))) %>%
  arrange(`Specific category`)

p = forest_plot_all(data_use,subcategory_bol = FALSE)

pdf(file = "plots/forest_lifestyle4.pdf",height = 6.5,width = 10.5)
p
dev.off()

## Contacts healthcare
top_healthcare = univar_healthcare_general %>%
  filter(type == "Full sample") %>%
  arrange(`P-value`) 

data_use = univar_healthcare_general %>%
  arrange(`P-value`) %>%
  mutate(`Main category` = factor(`Main category`,levels = top_healthcare$`Main category`)) %>%
  arrange(`Main category`)

p = forest_plot_all(data_use,subcategory_bol = FALSE)

pdf(file = "plots/forest_healthcare.pdf",height = 9,width = 8)
p
dev.off()

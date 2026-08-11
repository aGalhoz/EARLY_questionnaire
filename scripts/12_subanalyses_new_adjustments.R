########################################################################
################## analysis of all questions either ##################

# (i) adjusted for age/center

# -> make final datasest from code 04
# 1.1: Non-motor symptoms
datasets <- list(
  "Full sample" = univar_nonmotor_gender,
  "Sex reduced" = univar_nonmotor_gender_reduced[!duplicated(univar_nonmotor_gender_reduced$Variables),],
  "Sex and age" = univar_nonmotor_gender_age[!duplicated(univar_nonmotor_gender_age$Variables),],
  "Sex and center" = univar_nonmotor_gender_center[!duplicated(univar_nonmotor_gender_center$Variables),],
  "Not young" = univar_nonmotor_notyoung[!duplicated(univar_nonmotor_notyoung$Variables),]
)

univariate_nonmotor_general_extra <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univariate_nonmotor_general_extra <- univariate_nonmotor_general_extra %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univariate_nonmotor_general_extra, "data code output/univariate_nonmotor_general_extra.xlsx")

# 1.2: Pre-existing conditions
datasets <- list(
  "Full sample" = univar_preconditions_all_gender,
  "Sex reduced" = univar_preconditions_all_gender_reduced[!duplicated(univar_preconditions_all_gender_reduced$Variables),],
  "Sex and age" = univar_preconditions_all_gender_sex[!duplicated(univar_preconditions_all_gender_sex$Variables),],
  "Sex and center" = univar_preconditions_all_gender_center[!duplicated(univar_preconditions_all_gender_center$Variables),],
  "Not young" = univar_preconditions_all_notyoung[!duplicated(univar_preconditions_all_notyoung$Variables),]
)

univar_preconditions_general_extra <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_preconditions_general_extra <- univar_preconditions_general_extra %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_preconditions_general_extra, "data code output/univar_preconditions_general_extra.xlsx")

# 1.3: Lifestyle
datasets <- list(
  "Full sample" = univar_lifestyle_all,
  "Sex reduced" = univar_lifestyle_all_gender_reduced[!duplicated(univar_lifestyle_all_gender_reduced$Variables),],
  "Sex and age" = univar_lifestyle_all_gender_age[!duplicated(univar_lifestyle_all_gender_age$Variables),],
  "Sex and center" = univar_lifestyle_all_gender_center[!duplicated(univar_lifestyle_all_gender_center$Variables),],
  "Not young" = univar_lifestyle_all_notyoung[!duplicated(univar_lifestyle_all_notyoung$Variables),]
)

univar_lifestyle_general_extra <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_lifestyle_general_extra <- univar_lifestyle_general_extra %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_lifestyle_general_extra, "data code output/univar_lifestyle_general_extra.xlsx")

# 1.4: healthcare
datasets <- list(
  "Full sample" = univar_healthcare$univar_gender,
  "Sex reduced" = univar_healthcare_sex_reduced$univar_gender[!duplicated(univar_healthcare_sex_reduced$univar_gender$Variables),],
  "Sex and age" = univar_healthcare_age$univar_gender[!duplicated(univar_healthcare_age$univar_gender$Variables),],
  "Sex and center" = univar_healthcare_center$univar_gender[!duplicated(univar_healthcare_center$univar_gender$Variables),],
  "Not young" = univar_healthcare_notyoung$univar_gender[!duplicated(univar_healthcare_notyoung$univar_gender$Variables),]
)

univar_healthcare_general_extra <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_healthcare_general_extra <- univar_healthcare_general_extra %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_healthcare_general_extra, "data code output/univar_healthcare_general_extra.xlsx")

# 1.5: diet and weight
datasets <- list(
  "Full sample" = univar_dietweight$univar_gender,
  "Sex reduced" = univar_dietweight_sex_reduced[!duplicated(univar_dietweight_sex_reduced$Variables),],
  "Sex and age" = univar_dietweight_age[!duplicated(univar_dietweight_age$Variables),],
  "Sex and center" = univar_dietweight_center[!duplicated(univar_dietweight_center$Variables),],
  "Not young" = univar_dietweight_notyoung[!duplicated(univar_dietweight_notyoung$Variables),]
)

univar_dietweight_general_extra <- map2_dfr(datasets, names(datasets), ~process_univar(.x, .y))
univar_dietweight_general_extra <- univar_dietweight_general_extra %>%
  arrange(`Main category`, `Specific category`, `P-value`)

writexl::write_xlsx(univar_dietweight_general_extra, "data code output/univar_dietweight_general_extra.xlsx")

############################################
# new datasets for the signed p-value plots 

univariate_nonmotor_general_extra_new = read_excel("data code output/univariate_nonmotor_general_extra_new.xlsx")
univar_preconditions_general_extra_new  = read_excel("data code output/univar_preconditions_general_extra_new.xlsx")
univar_lifestyle_general_extra_new  = read_excel("data code output/univar_lifestyle_general_extra_new.xlsx")
univar_healthcare_general_extra_new  = read_excel("data code output/univar_healthcare_general_extra_new.xlsx")
univar_dietweight_general_extra_new = read_excel("data code output/univar_dietweight_general_extra_new.xlsx")

univariate_all_together_new = do.call("rbind",list(univariate_nonmotor_general_extra_new %>% 
                                                 mutate(question_type = "non-motor"),
                                               univar_preconditions_general_extra_new  %>% 
                                                 mutate(question_type = "pre-conditions"),
                                               univar_lifestyle_general_extra_new %>%
                                                 mutate(question_type = "lifestyle"),
                                               univar_healthcare_general_extra_new %>%
                                                 mutate(question_type = "healthcare"),
                                               univar_dietweight_general_extra_new %>%
                                                 mutate(question_type = "dietweight")))

univariate_all_together_new = univariate_all_together_new %>%
  mutate(value_axis = sign(`log(odds-ratio)`) * (-log(`P-value`))) %>%
  filter(value_axis != 0) %>%
  filter(`2.5% (OR)` != 0) 


TYPE_FULL    <- "Full sample"
TYPE_AGE     <- "Sex and age"
TYPE_CENTER  <- "Sex and center"
TYPE_REDUCED <- "Sex reduced"
TYPE_NOTYOUNG <- "Not young"

# full sample is the reference table
ref <- univariate_all_together_new %>%
  filter(type == TYPE_FULL) %>%
  dplyr::select(`Specific category`, question_type, value_axis) %>%
  dplyr::rename(value_axis_ref = value_axis)

ref_reduced <- univariate_all_together_new %>%
  filter(type == TYPE_REDUCED) %>%
  dplyr::select(`Specific category`, question_type, value_axis) %>%
  dplyr::rename(value_axis_ref = value_axis)

# plot function
make_signed_pval_plot <- function(comparison_type, comparison_label, ref_data,
                                  ref_pval_type = NULL, axis_cap = 30,
                                  n_label_per_side = 15) {
  
  pval_ref_source <- if(is.null(ref_pval_type)) TYPE_FULL else ref_pval_type
  
  df_plot <- univariate_all_together_new %>%
    filter(type == comparison_type) %>%
    dplyr::select(`Specific category`, `Main category`, question_type, value_axis, `P-value`) %>%
    dplyr::rename(value_axis_comp = value_axis, pval_comp = `P-value`) %>%
    inner_join(
      ref_data %>% left_join(
        univariate_all_together_new %>%
          filter(type == pval_ref_source) %>%
          dplyr::select(`Specific category`, question_type, `P-value`) %>%
          dplyr::rename(pval_ref = `P-value`),
        by = c("Specific category", "question_type")
      ),
      by = c("Specific category", "question_type")
    ) %>%
    mutate(
      sig_status = case_when(
        pval_ref < 0.05 & pval_comp < 0.05  ~ "Significant in both",
        pval_ref < 0.05 & pval_comp >= 0.05 ~ "Significant in sex-adjusted",
        pval_ref >= 0.05 & pval_comp < 0.05 ~ paste0("Significant in ", comparison_label),
        TRUE                                 ~ "Non-significant"
      ),
      sig_status = factor(sig_status, levels = c(
        "Significant in both",
        "Significant in sex-adjusted",
        paste0("Significant in ", comparison_label),
        "Non-significant"
      )),
      value_axis_ref_plot  = pmax(pmin(value_axis_ref,  axis_cap), -axis_cap),
      value_axis_comp_plot = pmax(pmin(value_axis_comp, axis_cap), -axis_cap),
      extremity = sqrt(value_axis_ref_plot^2 + value_axis_comp_plot^2),
      label_raw = ifelse(`Specific category` == "Other",
                         paste0(`Main category`, " (other)"),
                         `Specific category`)
    )
  
  sig_colors <- setNames(
    c("#C45C3A", "#5B8DB8", "#4A9B7F", "grey88"),
    c("Significant in both",
      "Significant in sex-adjusted",
      paste0("Significant in ", comparison_label),
      "Non-significant")
  )
  sig_sizes <- setNames(
    c(1.6, 1.6, 1.6, 0.8),
    c("Significant in both", "Significant in sex-adjusted",
      paste0("Significant in ", comparison_label), "Non-significant")
  )
  sig_alpha <- setNames(
    c(0.85, 0.85, 0.85, 0.4),
    c("Significant in both", "Significant in sex-adjusted",
      paste0("Significant in ", comparison_label), "Non-significant")
  )
  
  sig_threshold <- -log(0.05)
  
  # ── label data: ALL significant points, split by side ──────────────────────
  
  # Left side: significant points with negative x — label ALL of them
  # Left side: keep only the top n_label_per_side most extreme points
  # to avoid a crowded mess when many low-signal points cluster near zero
  df_label_left <- df_plot %>%
    filter(sig_status != "Non-significant", value_axis_ref_plot < 0) %>%
    arrange(desc(extremity)) %>%
    slice_head(n = n_label_per_side) %>%
    arrange(value_axis_comp_plot)   # re-sort top-to-bottom for even spacing
  
  # Right side: label ALL significant points (usually fewer, so still clean)
  df_label_right <- df_plot %>%
    filter(sig_status != "Non-significant", value_axis_ref_plot >= 0) %>%
    arrange(value_axis_comp_plot)
  
  # Evenly spaced y positions across the full axis range for each side
  n_left  <- nrow(df_label_left)
  n_right <- nrow(df_label_right)
  
  y_spread <- axis_cap * 0.92   # how far up/down we spread the labels
  
  spread_labels <- function(df) {
    n <- nrow(df)
    df$label_y <- if (n == 1) df$value_axis_comp_plot
    else seq(-y_spread, y_spread, length.out = n)
    df
  }
  
  df_label_left  <- spread_labels(df_label_left)
  df_label_right <- spread_labels(df_label_right)
  
  # x positions for label anchors (just outside the plot range)
  x_left_anchor  <- -(axis_cap * 1.18)
  x_right_anchor <-   axis_cap * 1.18
  
  ggplot(df_plot, aes(x = value_axis_ref_plot, y = value_axis_comp_plot,
                      color = sig_status, size = sig_status, alpha = sig_status)) +
    
    geom_hline(yintercept = 0,              linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0,              linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_hline(yintercept =  sig_threshold, linetype = "dashed", color = "grey78", linewidth = 0.25) +
    geom_hline(yintercept = -sig_threshold, linetype = "dashed", color = "grey78", linewidth = 0.25) +
    geom_vline(xintercept =  sig_threshold, linetype = "dashed", color = "grey78", linewidth = 0.25) +
    geom_vline(xintercept = -sig_threshold, linetype = "dashed", color = "grey78", linewidth = 0.25) +
    geom_abline(slope = 1, intercept = 0,   linetype = "dotted", color = "grey55", linewidth = 0.4) +
    
    geom_point(shape = 16) +
    
    # ── LEFT margin labels ───────────────────────────────────────────────────
    # text anchored far left, segment drawn from label to point
    geom_text(
      data        = df_label_left,
      aes(x = x_left_anchor, y = label_y, label = label_raw),
      hjust       = 1, size = 3.0, color = "grey20",
      inherit.aes = FALSE
    ) +
    geom_segment(
      data = df_label_left,
      aes(x    = x_left_anchor + axis_cap * 0.02,
          xend = value_axis_ref_plot - 0.5,
          y    = label_y,
          yend = value_axis_comp_plot),
      color = "grey60", linewidth = 0.25, alpha = 0.7,
      inherit.aes = FALSE
    ) +
    
    # ── RIGHT margin labels ──────────────────────────────────────────────────
    geom_text(
      data        = df_label_right,
      aes(x = x_right_anchor, y = label_y, label = label_raw),
      hjust       = 0, size = 3.0, color = "grey20",
      inherit.aes = FALSE
    ) +
    geom_segment(
      data = df_label_right,
      aes(x    = value_axis_ref_plot + 0.5,
          xend = x_right_anchor - axis_cap * 0.02,
          y    = value_axis_comp_plot,
          yend = label_y),
      color = "grey60", linewidth = 0.25, alpha = 0.7,
      inherit.aes = FALSE
    ) +
    
    scale_x_continuous(
      limits = c(-axis_cap * 2.0, axis_cap * 2.0),
      breaks = pretty(c(-axis_cap, axis_cap), n = 5)
    ) +
    scale_y_continuous(
      limits = c(-axis_cap * 1.2, axis_cap * 1.2),
      breaks = pretty(c(-axis_cap, axis_cap), n = 5)
    ) +
    coord_cartesian(clip = "off") +
    
    labs(
      x     = paste0("Signed  \u2212log(p) for sex-adjusted"),
      y     = paste0("Signed  \u2212log(p) for  ", comparison_label),
      title = paste0("Sex-adjusted vs ", comparison_label)
    ) +
    
    scale_color_manual(values = sig_colors, name = NULL,
                       guide = guide_legend(override.aes = list(size = 2.5))) +
    scale_size_manual(values  = sig_sizes,  guide = "none") +
    scale_alpha_manual(values = sig_alpha,  guide = "none") +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.grid           = element_blank(),
      axis.line            = element_line(color = "grey60", linewidth = 0.4),
      axis.ticks           = element_line(color = "grey60", linewidth = 0.3),
      legend.position      = c(0.01, 0.99),
      legend.justification = c("left", "top"),
      legend.background    = element_rect(fill = "white", color = "grey80", linewidth = 0.3),
      legend.margin        = margin(4, 8, 4, 6),
      legend.text          = element_text(size = 9),
      legend.key.size      = unit(0.65, "lines"),
      plot.title           = element_text(size = 10, face = "bold", color = "grey35",
                                          hjust = 0.5, margin = margin(b = 4)),
      axis.title           = element_text(size = 9.5, color = "grey30"),
      axis.text            = element_text(size = 8.5, color = "grey45"),
      plot.margin          = margin(10, 80, 6, 80)   # wide left/right margins for labels
    )
}

p_age     <- make_signed_pval_plot(TYPE_AGE,     "sex and age-adjusted",ref,
                                   ref_pval_type = TYPE_FULL,n_label_per_side = 15)
p_age_vs_reduced     <- make_signed_pval_plot(TYPE_AGE,"sex and age-adjusted (both reduced)",
                                              ref_reduced, ref_pval_type = TYPE_REDUCED)
p_center  <- make_signed_pval_plot(TYPE_CENTER,  "sex and center-adjusted",ref,
                                   ref_pval_type = TYPE_FULL)
p_reduced <- make_signed_pval_plot(TYPE_REDUCED, "sex-adjusted (reduced)",ref,
                                   ref_pval_type = TYPE_FULL)
p_notyoung <- make_signed_pval_plot(TYPE_NOTYOUNG, "without age<40",ref,
                                    ref_pval_type = TYPE_FULL, n_label_per_side =40)

ggsave("plots/fig_sensitivity_age.pdf",     p_age,     width = 8, height = 7)
#ggsave("plots/fig_sensitivity_age_bothreduced.pdf",     p_age_vs_reduced,     width = 7, height = 7)
ggsave("plots/fig_sensitivity_center.pdf",  p_center,  width = 8, height = 7)
ggsave("plots/fig_sensitivity_reduced.pdf", p_reduced, width = 8, height = 7)
ggsave("plots/fig_sensitivity_notyoung.pdf", p_notyoung, width = 8, height = 7)

p_age_distribution = ggplot(age_CTR_ALS, aes(x = age, y = after_stat(density), fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 5) +
  geom_density(aes(color = type), linewidth = 0.7, fill = NA) +
  scale_fill_manual(values = c("ALS" = "#D85A30", "CTR" = "#378ADD"),
                    labels = c(paste0("ALS (n=", length(na.omit(age)), ")"),
                               paste0("CTR (n=", length(na.omit(age_CTR)), ")"))) +
  scale_color_manual(values = c("ALS" = "#D85A30", "CTR" = "#378ADD"), guide = "none") +
  labs(x = "Age (years)", y = "Density", fill = NULL) +
  theme_minimal() +
  geom_vline(xintercept = 40, color = "red")


library(patchwork)
fig_sensitivity <- (p_age_distribution | p_notyoung) /
  (p_age            | p_reduced) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 15, face = "bold"))

ggsave("plots/fig_sensitivity_age_all.pdf",
       fig_sensitivity,
       width  = 15,
       height = 12)

## to save results
univariate_all_together_new_tmp = univariate_all_together_new
univariate_all_together_new_tmp = univariate_all_together_new_tmp %>% 
  filter(type %in% c("Sex and age","Sex reduced","Not young"))

writexl::write_xlsx(univariate_all_together_new_tmp,
                    "data code output/univariate_new_age_adjustment.xlsx")


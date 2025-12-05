## Barplots of specific questions

##########################################################
# HELPER FUNCTIONS

barplot_categories <- function(data,raw_numbers = FALSE){
  
  plot_con <- list()  
  
  categories <- unique(data$Variable_en)
  
  for (i in seq_along(categories)){
    
    tmp <- data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en, Levels_en, Freq_ALS, Freq_CTR)
    
    tmp <- reshape2::melt(tmp, id.vars = c("Variable_en","Levels_en"))
    
    tmp$Status <- ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    
    if(raw_numbers){
      tmp$counts = tmp$value
    }else{
      tmp$counts <- ifelse(tmp$Status == "ALS",
                           round((tmp$value * 100) / 475, 1),
                           round((tmp$value * 100) / 285, 1))
    }
    
    tmp$Levels <- tmp$Levels_en
    
    plot_con[[i]] <- ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) +
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(
        name = "Group",
        breaks = c("ALS", "CTR"),
        labels = c("ALS", "Controls"),
        values = c("ALS" = "#5f91bd", "CTR" = "#BD8B5F")
      ) +
      scale_fill_manual(
        name = "Group",
        breaks = c("ALS", "CTR"),
        labels = c("ALS", "Controls"),
        values = c("ALS" = "#5f91bd", "CTR" = "#BD8B5F")
      ) +
      geom_text(aes(label = counts, group = Status),
                position = position_dodge(0.8),
                vjust = -0.3, size = 6) +
      labs(x = " ", y = ifelse(raw_numbers,
                               "Number of Participants \n",
                               "Frequency of Participants (%) \n"),
           title = categories[i]) +
      theme_minimal(base_size = 17) +
      guides(fill = guide_legend(title="Group"),
             color = guide_legend(title="Group")) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)
      )
    
    pdf(file = paste0("plots/boxplots/boxplot_",
                      gsub("/", "", categories[i]), ".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
  
  return(plot_con)
}

barplot_categories_gender <- function(data){
  categories <- data$Variable_en %>% unique()
  
  plot_con <- list()  
  
  for (i in 1:(length(categories))){
    print(i)
    tmp = data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en,Levels_en,Freq_ALS,Freq_CTR,gender)
    print(tmp)
    tmp = reshape2::melt(tmp, id.vars = c("Variable_en","Levels_en","gender"))
    print(tmp)
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","Controls")
    tmp$counts = ifelse(tmp$Status == "ALS" & tmp$gender == "female",
                        round((tmp$value*100)/180, digits = 1),
                        ifelse(tmp$Status == "ALS" & tmp$gender == "male",
                               round((tmp$value*100)/289, digits = 1),
                               ifelse(tmp$Status == "Controls" & tmp$gender == "female",
                                      round((tmp$value*100)/168,digits = 1),
                                      round((tmp$value*100)/114,digits = 1))))
    tmp$Sex <- tmp$gender
    print(tmp)
    #tmp$Levels <- factor(tmp$Levels_en, levels = rev(tmp$Levels_en %>% unique()))
    tmp$Levels <- as.factor(tmp$Levels_en)
    print(tmp)
    title_i <- ifelse(categories[i] == "Regular consumption of caffeine","Caffeine consumption",
                      ifelse(categories[i] == "Regular consumption of alcohol","Alcohol consumption",
                             ifelse(categories[i] == "Regular smoke","Cigarette consumption",
                                    ifelse(categories[i] == "Physical activity - professional","Physical activity in professional activities",
                                           ifelse(categories[i] == "Physical activity - sport","Physical activity in sport activities",
                                                  ifelse(categories[i] == "Highest educational degree","Educational degree",
                                                         ifelse(categories[i] == "Highest professional degree","Professional degree",
                                                                categories[i])))))))
    tmp$Sex = ifelse(tmp$Sex == "female","Female","Male")
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#5f91bd","Controls"="#BD8B5F"))+
      scale_fill_manual(values=c("ALS"="#5f91bd","Controls"="#BD8B5F")) +
      # geom_text(aes(label = paste0(counts," %"), group = Status), 
      #           position = position_dodge(0.8),
      #           vjust = -0.5, size = 3) + 
      geom_text(aes(label = counts, group = Status), 
                position = position_dodge(0.8),
                vjust = -0.5, size = 4.5) + 
      facet_wrap(. ~ Sex,ncol = 1) +
      #scales = "free_x",space = "free_x",
      labs(x = " ", y = "Frequency (%) \n", title = title_i) +
      guides(fill=guide_legend(title="Group"),color = guide_legend(title="Group")) + 
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
        axis.title = element_text(size = 12.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        strip.background  = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.border = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x  = element_blank(),
        axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
        strip.text = element_text(size = 14)) + 
      ylim(0,(max(tmp$counts) + 15)) 
    #coord_flip()
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
  }
  return(plot_con)
}

barplot_categories_new <- function(data){
  categories <- data$Variable_en %>% unique()
  
  plot_con <- list()  
  
  for (i in 1:(length(categories))){
    print(i)
    tmp = data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en,Levels_en,Freq_ALS,Freq_CTR)
    tmp = reshape2::melt(tmp, id.vars = c("Variable_en","Levels_en"))
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    tmp$counts =  ifelse(tmp$Status == "ALS",round((tmp$value*100)/475, digits = 1),
                         ifelse(tmp$Status == "CTR",round((tmp$value*100)/285,digits = 1),
                                tmp$value))
    print(tmp)
    tmp$Levels <- as.factor(tmp$Levels_en)
    title_i <- ifelse(categories[i] == "Regular consumption of caffeine","Caffeine consumption",
                      ifelse(categories[i] == "Regular consumption of alcohol","Alcohol consumption",
                             ifelse(categories[i] == "Regular smoke","Cigarette consumption",
                                    ifelse(categories[i] == "Physical activity - professional activity","Level of physical activity in professional work",
                                           ifelse(categories[i] == "Physical activity - sport activity","Level of physical activity in sport activities",
                                                  categories[i])))))
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+
      scale_fill_manual(values=c("ALS"="#5f91bd","CTR"="#BD8B5F")) +
      geom_text(aes(label = counts, group = Status), 
                position = position_dodge(0.8),
                vjust = -0.5, size = 4.5)+
                #hjust = -0.15, size = 3) + 
      labs(x = "", y =  "Frequency (%)\n", title = title_i) +
      guides(fill = guide_legend(title="Group"),
             color = guide_legend(title="Group")) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
        axis.title = element_text(size = 12.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        strip.background  = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.border = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x  = element_blank(),
        axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8),
        strip.text = element_text(size = 14)) + 
      ylim(0,(max(tmp$counts) + 15)) 
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
    # Use position = position_dodge()
  }
  return(plot_con)
}

frequency_status <- function(data,status_bol,name_freq){
  dt <- data %>%
    filter(status == status_bol)
  dt_res = data.frame()
  for (i in 1:ncol(dt)){
    dt_temp = data.frame(t(table(dt[,i])))
    dt_temp$Var1 = names(dt)[i]
    dt_res = rbind(dt_res, dt_temp)
  }
  names(dt_res) = c("Variable","Levels",name_freq)
  return(dt_res)
}

plot_substance_gender <- function(data, title) {
  
  gender_col <- names(data)[grepl("Geschlecht|gender|sex", names(data), ignore.case = TRUE)][1]
  
  quantity_col <- names(data)[grepl("quant", names(data), ignore.case = TRUE)][1]
  
  count_col <- names(data)[grepl("^count$|freq|anzahl", names(data), ignore.case = TRUE)][1]
  
  # --- 4. Standardize internally ---
  df <- data %>%
    dplyr::rename(
      #sex = !!rlang::sym(gender_col),
      quantity = !!rlang::sym(quantity_col),
      count = !!rlang::sym(count_col)
    ) %>%
    dplyr::mutate(
      quantity = ifelse(is.na(quantity) | quantity == "", "n.a.", quantity),
      quantity = case_when(
        grepl("(?i)low", quantity) ~ "Low",
        grepl("(?i)moderate", quantity) ~ "Moderate",
        grepl("(?i)high", quantity) ~ "High",
        TRUE ~ "n.a."
      ),
      quantity = factor(quantity, levels = c("Low","Moderate","High","n.a.")),
      group_order = factor(
        paste(status, type),
        levels = c("ALS Currently","Controls Currently",
                   "ALS In the past","Controls In the past")
      )
    ) %>%
    # dplyr::group_by(sex, status, type, quantity, group_order) %>%
    # dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(sex, status, type) %>%
    dplyr::mutate(percentage = count / sum(count) * 100)
  
    
    ggplot(df,aes(
      x = quantity,
      y = percentage,
      fill = status,
      pattern = type,
      group = group_order
    )) +
    geom_bar_pattern(
      position = position_dodge(width = 0.9),
      stat = "identity",
      colour = NA,
      pattern_fill = "white",        
      pattern_colour = "grey39",
      pattern_angle = 45,
      pattern_density = 0.05,
      pattern_spacing = 0.03,
      pattern_key_scale_factor = 0.5,
      #key_glyph = "rect"
    ) +
    geom_text(
      aes(label = sprintf("%.1f", percentage), group = group_order),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 4.8,
      color = "black"
    ) +
    # Color legend (Groups)
    scale_fill_manual(
      values = c("ALS" = "#5f91bd", "Controls" = "#BD8B5F"),
      name = "Group",
      guide = guide_legend(override.aes = list(pattern = "none"))
    ) +
    # Pattern legend (Time)
    scale_pattern_manual(
      values = c("Currently" = "none", "In the past" = "stripe"),
      name = "" ,
      guide = guide_legend(
        override.aes = list(
          pattern_fill = NA,   # white background
          pattern_colour = "grey39",  # visible stripes
          colour = "black"
        ))
    ) +
    facet_wrap(~sex, ncol = 1) +
    labs(
      title = title,
      x = "",
      y = "Frequency (%) \n"
    ) +
    theme_minimal(base_size = 14.5) +
    theme(
      plot.title = element_text(face = "bold", size = 15.5, hjust = 0.5),
      axis.title = element_text(size = 14.7),
      axis.text = element_text(size = 14.2),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      panel.grid.minor = element_blank(),
      strip.background  = element_blank(),
      panel.grid.major = element_line(colour = "lightgrey"),
      panel.border = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x  = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
      strip.text = element_text(size = 15.5),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5)
    )
}

plot_substance <- function(data, title) {
  
  quantity_col <- names(data)[grepl("quant", names(data), ignore.case = TRUE)][1]
  
  count_col <- names(data)[grepl("^count$|freq|anzahl", names(data), ignore.case = TRUE)][1]
  
  df <- data %>%
    dplyr::rename(
      quantity = !!rlang::sym(quantity_col),
      count = !!rlang::sym(count_col)
    ) %>%
    dplyr::mutate(
      quantity = ifelse(is.na(quantity) | quantity == "", "n.a.", quantity),
      quantity = case_when(
        grepl("(?i)low", quantity) ~ "Low",
        grepl("(?i)moderate", quantity) ~ "Moderate",
        grepl("(?i)high", quantity) ~ "High",
        TRUE ~ "n.a."
      ),
      quantity = factor(quantity, levels = c("Low","Moderate","High","n.a.")),
      group_order = factor(
        paste(status, type),
        levels = c("ALS Currently","Controls Currently",
                   "ALS In the past","Controls In the past")
      )
    ) %>%
    # dplyr::group_by(sex, status, type, quantity, group_order) %>%
    # dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(status, type) %>%
    dplyr::mutate(percentage = count / sum(count) * 100)
  
  
  ggplot(df,aes(
    x = quantity,
    y = percentage,
    fill = status,
    pattern = type,
    group = group_order
  )) +
    geom_bar_pattern(
      position = position_dodge(width = 0.9),
      stat = "identity",
      colour = NA,
      pattern_fill = "white",        
      pattern_colour = "grey39",
      pattern_angle = 45,
      pattern_density = 0.05,
      pattern_spacing = 0.03,
      pattern_key_scale_factor = 0.5,
      #key_glyph = "rect"
    ) +
    geom_text(
      aes(label = sprintf("%.1f", percentage), group = group_order),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 4.8,
      color = "black"
    ) +
    # Color legend (Groups)
    scale_fill_manual(
      values = c("ALS" = "#5f91bd", "Controls" = "#BD8B5F"),
      name = "Group",
      guide = guide_legend(override.aes = list(pattern = "none"))
    ) +
    # Pattern legend (Time)
    scale_pattern_manual(
      values = c("Currently" = "none", "In the past" = "stripe"),
      name = "" ,
      guide = guide_legend(
        override.aes = list(
          pattern_fill = NA,   # white background
          pattern_colour = "grey39",  # visible stripes
          colour = "black"
        ))
    ) +
    labs(
      title = title,
      x = "",
      y = "Frequency (%) \n"
    ) +
    theme_minimal(base_size = 14.5) +
    theme(
      plot.title = element_text(face = "bold", size = 15.5, hjust = 0.5),
      axis.title = element_text(size = 14.7),
      axis.text = element_text(size = 14.2),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      panel.grid.minor = element_blank(),
      strip.background  = element_blank(),
      panel.grid.major = element_line(colour = "lightgrey"),
      panel.border = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x  = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
      strip.text = element_text(size = 15.5),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5)
    )
}

analyze_current_vs_past <- function(df,
                                    status_levels = c("ALS", "Controls"),
                                    type_levels   = c("Currently", "In the past")) {
  
  df <- df %>%
    mutate(
      quantity = ifelse(is.na(quantity) | quantity == "" | quantity == "NA", "n.a.", quantity),
      quantity = as.character(quantity),
      status   = as.character(status),
      type     = as.character(type),
      count    = as.numeric(count)
    )
  
  # Summarise counts first (NO list columns here!)
  summary_tbl <- df %>%
    filter(status %in% status_levels,
           type   %in% type_levels) %>%
    dplyr::group_by(quantity) %>%
    dplyr::summarise(
      ALS_current = sum(count[status == status_levels[1] & type == type_levels[1]], na.rm = TRUE),
      ALS_past    = sum(count[status == status_levels[1] & type == type_levels[2]], na.rm = TRUE),
      CTR_current = sum(count[status == status_levels[2] & type == type_levels[1]], na.rm = TRUE),
      CTR_past    = sum(count[status == status_levels[2] & type == type_levels[2]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Now run statistics row-by-row
  results <- summary_tbl %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tab = list(matrix(
        c(ALS_current, ALS_past,
          CTR_current, CTR_past),
        nrow = 2, byrow = TRUE,
        dimnames = list(
          status = status_levels,
          type   = type_levels
        )
      )),
      small_cells = any(unlist(tab) < 5),
      test = list(
        if (small_cells) {
          fisher.test(tab)
        } else {
          chisq.test(tab)
        }
      ),
      test_type = ifelse(small_cells, "Fisher", "Chi-squared"),
      p_value   = test$p.value,
      ALS_total = ALS_current + ALS_past,
      CTR_total = CTR_current + CTR_past,
      ALS_prop_current = ALS_current / ALS_total,
      CTR_prop_current = CTR_current / CTR_total
    ) %>%
    ungroup() %>%
    dplyr::select(
      quantity,
      ALS_current, ALS_past, ALS_total, ALS_prop_current,
      CTR_current, CTR_past, CTR_total, CTR_prop_current,
      test_type, p_value
    )
  
  return(results)
}

##########################################################
# ANALYSIS ON EACH SUBCATEGORY

# -> non motor ALS specific
subcats <- c(
  "Kalte, blasse oder bläulich verfärbte Extremitäten",
  "Übermaß an Speichel im Mund",
  "Zittern der Arme oder Beine"
)

dt_nonmotor_all <- map_dfr(subcats, function(subcat) {
  
  # Filter for this sub-subcategory
  final_i <- final_nonmotor_diff %>%
    filter(`sub-subcategory` == subcat)
  
  # Extract relevant columns based on ALS questions
  cols_i <- intersect(make.names(final_i$`original question (ALS)`), colnames(dat_final))
  
  dat_i <- dat_final[, c(cols_i, "status2")]
  
  # ALS frequencies
  dt_ALS <- as.data.frame(table(c(dat_i[dat_i$status2 == 1, cols_i]))) %>%
    arrange(desc(Freq))
  names(dt_ALS) <- c("Levels_en", "Freq_ALS")
  
  # Control frequencies
  dt_CTR <- as.data.frame(table(c(dat_i[dat_i$status2 == 0, cols_i]))) %>%
    arrange(desc(Freq))
  names(dt_CTR) <- c("Levels_en", "Freq_CTR")
  
  # Merge ALS and controls
  dt_tmp <- merge(dt_ALS, dt_CTR, by = "Levels_en", all = TRUE)
  
  # Add the variable name for later plotting
  dt_tmp$Variable_en <- subcat
  
  return(dt_tmp)
})

dt_nonmotor_all <- dt_nonmotor_all %>%
  mutate(
    Levels_en = case_when(
      Levels_en %in% c("X..1Monat", "Vor...1Monat") ~ "<1 month",
      Levels_en == "X1.12Monate" ~ "1-12 months",
      Levels_en == "X1.5Jahre" ~ "1-5 years",
      Levels_en == "X5.10Jahre" ~ "5-10 years",
      TRUE ~ Levels_en
    )
  )

dt_nonmotor_all$Levels_en <- factor(
  dt_nonmotor_all$Levels_en,
  levels = c("5-10 years", "1-5 years", "1-12 months", "<1 month")
)

dt_nonmotor_all <- dt_nonmotor_all %>%
  mutate(
    Variable_en = case_when(
      Variable_en == "Kalte, blasse oder bläulich verfärbte Extremitäten" ~ "Cold, pale extremities",
      Variable_en == "Übermaß an Speichel im Mund" ~ "Excessive saliva",
      Variable_en == "Zittern der Arme oder Beine" ~ "Trembling of arms or legs",
      TRUE ~ Variable_en
    )
  )

plots <- barplot_categories(dt_nonmotor_all)

pdf("plots/barplots_specific_prodromal.pdf",width = 19,height = 7)
plot_grid(plots[[3]],plots[[2]],plots[[1]],ncol = 3, nrow=1, width = 10, height = 6, labels = c("A","B","C")) 
dev.off()

# -> slipped disc
data_slipped_dic = data_combined[,colnames(data_combined) %in% c("status",
                                                               "Welche Krankheits des Muskel-Skelett-Systems liegt bzw. lag vor und seit wann? [Bandscheibenvorfall][Kommentar]")]
# ALS
slipped_disc_ALS = data_slipped_dic %>%
  filter(status == 1)
slipped_disc_ALS = data.frame(slipped_disc = slipped_disc_ALS[,1],
                              onset = ALS_open_answers$date)
slipped_disc_ALS = slipped_disc_ALS[!is.na(slipped_disc_ALS[,1]),]
slipped_disc_ALS$year_prioronset = c(14,6,2,2,3,17,2,0,1,38,27,33,0,27,7,12,26,13,0,13,
                                     7,20,8,1,17,36,21,-3,-1,3,18,-3,26,15,7,15,-2,3,3,16,
                                     16,4,25,13)
slipped_disc_ALS$time_prioronset = ifelse(slipped_disc_ALS$year_prioronset < 0,
                                          "after onset",
                                          ifelse(slipped_disc_ALS$year_prioronset <= 1,
                                                 "1-12 months",
                                                 ifelse(slipped_disc_ALS$year_prioronset < 5,
                                                        "1-5 years",
                                                        ifelse(slipped_disc_ALS$year_prioronset > 5,
                                                               "5-10 years",NA))))
median(na.omit(slipped_disc_ALS$year_prioronset))
IQR(na.omit(slipped_disc_ALS$year_prioronset))
summaryStats(na.omit(slipped_disc_ALS$year_prioronset),quartiles = TRUE)

# CTR
slipped_disc_CTR = data_slipped_dic %>%
  filter(status == 0)
slipped_disc_CTR = data.frame(slipped_disc = slipped_disc_CTR[,1],
                              onset = as.numeric(str_extract(CTR_common$`Datum Abgeschickt`, "\\d{4}")))
slipped_disc_CTR = slipped_disc_CTR[!is.na(slipped_disc_CTR[,1]),]
slipped_disc_CTR$year_prioronset = c(41,3,3,4,20,14,2,4,10,4,2,15,4,15,1)
slipped_disc_CTR$time_prioronset = ifelse(slipped_disc_CTR$year_prioronset < 1,
                                          "1-12 months",
                                          ifelse(slipped_disc_CTR$year_prioronset < 5,
                                                 "1-5 years",
                                                 ifelse(slipped_disc_CTR$year_prioronset > 5,
                                                        "5-10 years",NA)))
table(slipped_disc_ALS$time_prioronset)
table(slipped_disc_CTR$time_prioronset)
slipped_disc_ALS_CTR = data.frame(Levels_en = c("1-12 months","1-5 years","5-10 years"),
                                  Freq_ALS = c(1,6,25),
                                  Freq_CTR = c(0,9,6),
                                  Variable_en = rep("Slipped disc",3))

# all together
ALS_vec <- slipped_disc_ALS$time_prioronset
CTR_vec <- slipped_disc_CTR$time_prioronset
levels_keep <- c("after onset","1-12 months", "1-5 years", "5-10 years")
ALS_tab <- table(factor(ALS_vec, levels = levels_keep))
CTR_tab <- table(factor(CTR_vec, levels = levels_keep))

slipped_disc_ALS_CTR <- data.frame(
  Levels_en   = levels_keep,
  Freq_ALS    = as.numeric(ALS_tab),
  Freq_CTR    = as.numeric(CTR_tab),
  Variable_en = "Slipped disc",
  row.names = NULL
)

slipped_disc_ALS_CTR$Levels_en <- factor(slipped_disc_ALS_CTR$Levels_en,
                                         levels = c("5-10 years","1-5 years","1-12 months","after onset"))

# version all with percentage
plot <- barplot_categories(slipped_disc_ALS_CTR)

pdf("plots/barplot_slipped_disc_percentage.pdf",width = 7.5,height = 4.5)
print(plot)
dev.off()

# version reduced with percentage
plot <- barplot_categories(slipped_disc_ALS_CTR %>% filter(Levels_en != "after onset"))

pdf("plots/barplot_slipped_disc_percentage_reduced.pdf",width = 7.5,height = 4.5)
print(plot)
dev.off()

# version all with numbers
plot <- barplot_categories(slipped_disc_ALS_CTR,raw_numbers = TRUE)

pdf("plots/barplot_slipped_disc.pdf",width = 8.3,height = 5)
print(plot)
dev.off()

# version reduced
plot <- barplot_categories(slipped_disc_ALS_CTR %>% filter(Levels_en != "after onset"),raw_numbers = TRUE)

pdf("plots/barplot_slipped_disc_reduced.pdf",width = 8.3,height = 5)
print(plot)
dev.off()

#--> freqs for variables with several categories
lifestyle_cols <- c(
  make.names(final_lifestyle_categorical$`original question (ALS)`),
  "Wie.ist.aktuell.Ihr.Partnerschaftsstatus..",
  "Bitte.beschreiben.Sie.das.Maß.an.körperlicher.Aktivität.im.Rahmen.der.in.Ihrem.Leben.durchgeführten.sportlichen.Aktivitäten..",
  "Bitte.beschreiben.Sie.das.Maß.an.körperlicher.Aktivität.im.Rahmen.Ihrer.beruflichen.Tätigkeit..",
  "Rauchen.Sie.aktuell.oder.haben.Sie.jemals.in.Ihrem.Leben.regelmäßig.Zigaretten..oder.Zigarren.etc...geraucht.."
)

dat_lifestyle <- dat_final[, colnames(dat_final) %in% lifestyle_cols] %>%
  mutate(across(everything(), ~ ifelse(is.na(.x), "NA", .x)))

# -> select only specific columns of interest
dat_lifestyle <- dat_lifestyle[, c(2:6, 8, 9)]

dat_lifestyle$gender <- dat_final$Bitte.geben.Sie.Ihr.Geschlecht.an.
dat_lifestyle$status <- dat_final$status

# freq of answers by ALS/CTR + F/M
freq_by_status <- function(data, status_value, freq_name) {
  frequency_status(data, status_value, freq_name)
}

dt_ALS  <- freq_by_status(dat_lifestyle, 1, "Freq_ALS")
dt_CTR  <- freq_by_status(dat_lifestyle, 0, "Freq_CTR")

dt_ALS_f  <- freq_by_status(filter(dat_lifestyle, gender == "weiblich"), 1, "Freq_ALS")
dt_ALS_m  <- freq_by_status(filter(dat_lifestyle, gender == "männlich"), 1, "Freq_ALS")
dt_CTR_f  <- freq_by_status(filter(dat_lifestyle, gender == "weiblich"), 0, "Freq_CTR")
dt_CTR_m  <- freq_by_status(filter(dat_lifestyle, gender == "männlich"), 0, "Freq_CTR")

dt_res <- dt_ALS %>%
  mutate(Freq_CTR = dt_CTR$Freq_CTR)

dt_gender_ALS <- bind_rows(
  cbind(dt_ALS_f, gender = "female"),
  cbind(dt_ALS_m, gender = "male")
)

dt_gender_CTR <- bind_rows(
  cbind(dt_CTR_f, gender = "female"),
  cbind(dt_CTR_m, gender = "male")
)

dt_res_gender <- dt_gender_ALS %>%
  mutate(Freq_CTR = dt_gender_CTR$Freq_CTR)

## translation of names in DE to EN
variable_translation <- tibble::tribble(
  ~Variable, ~Variable_en,
  unique(dt_res$Variable)[1], "Highest educational degree",
  unique(dt_res$Variable)[2], "Highest professional degree",
  unique(dt_res$Variable)[3], "Regular consumption of caffeine",
  unique(dt_res$Variable)[4], "Regular consumption of alcohol",
  unique(dt_res$Variable)[5], "Regular smoke",
  unique(dt_res$Variable)[6], "Physical activity - professional",
  unique(dt_res$Variable)[7], "Physical activity - sport"
)

dt_res <- left_join(dt_res, variable_translation, by = "Variable") %>%
  filter(!is.na(Variable_en))
dt_res_gender <- left_join(dt_res_gender, variable_translation, by = "Variable") %>%
  filter(!is.na(Variable_en))

levels_translation <- tibble::tribble(
  ~Levels, ~Levels_en,
  "Gering", "Low",
  "Mäßig", "Moderate",
  "Intensiv", "Intensive",
  "Gering..z.B..Bürojob.", "Low",
  "Mäßig..z.B..Pflegewesen.", "Moderate",
  "Intensiv..z.B..Bauarbeiten.", "Intensive",
  "Ja..aktuell", "Currently",
  "Ja..in.der.Vergangenheit", "In the past",
  "Ja..in.Vergangenheit","In the past",
  "Nein..noch.nie", "Never",
  "Abitur...Fachholschulreife", "General qualification \nfor university entrance",
  "Fachabitur", "Subject-specific university \nentrance qualification",
  "Gesamtschule", "Comprehensive school",
  "Berufsschule", "Vocational school",
  "Realschule", "Secondary school",
  "Hauptschule", "Lower secondary school",
  "Grundschule", "Primary school",
  "Universität...Fachhochschule...Hochschule", "University degree (Bachelor's, \nMaster's, or equivalent)",
  "Fachschule...Technikerschule...Handelsakademie", "Vocational/technical school \nor business academy",
  "Meisterprüfung", "Master craftsman examination",
  "Lehre...Facharbeiterabschluss", "Apprenticeship/skilled \nworker qualification",
  "Kein.beruflicher.Abschluss", "No professional qualification",
  "Sonstiges", "Other",
  "N.A.", "n.a"
)
add_na_rows <- function(df, total_ALS = 475, total_CTR = 285) {
  
  df2 <- df %>%
    mutate(
      Freq_ALS = as.numeric(as.character(Freq_ALS)),
      Freq_CTR = as.numeric(as.character(Freq_CTR))
    )
  
  print(df2)
  
  # compute sums per Variable_en
  sums <- df2 %>%
    dplyr::group_by(Variable_en) %>%
    dplyr::mutate(
      sum_ALS = sum(Freq_ALS, na.rm = TRUE),
      sum_CTR = sum(Freq_CTR, na.rm = TRUE),
      missing_ALS = total_ALS - sum_ALS,
      missing_CTR = total_CTR - sum_CTR
    ) %>%
    dplyr::group_modify(function(data, key) {
      if (data$missing_ALS[1] > 0 | data$missing_CTR[1] > 0) {
        na_row <- data[1, ]  # copy structure
        na_row$Levels     <- "N.A."
        na_row$Freq_ALS   <- data$missing_ALS[1]
        na_row$Freq_CTR   <- data$missing_CTR[1]
        bind_rows(data, na_row)
      } else {
        data
      }
    }) %>%
    ungroup() %>%
    select(Variable, Levels, Freq_ALS, Freq_CTR, Variable_en,sum_ALS,missing_ALS)
  sums
}

add_na_rows_gender <- function(df) {
  
  totals <- list(
    male   = c(ALS = 289, CTR = 114),
    female = c(ALS = 180, CTR = 168)
  )
  
  df %>%
    dplyr::group_by(Variable_en, gender) %>%
    dplyr::mutate(
      target_ALS = totals[[gender[1]]]["ALS"],
      target_CTR = totals[[gender[1]]]["CTR"],
      
      sum_ALS = sum(Freq_ALS, na.rm = TRUE),
      sum_CTR = sum(Freq_CTR, na.rm = TRUE),
      
      missing_ALS = target_ALS - sum_ALS,
      missing_CTR = target_CTR - sum_CTR
    ) %>%
    dplyr::group_modify(function(data, key) {
      if (data$missing_ALS[1] > 0 | data$missing_CTR[1] > 0) {
        na_row <- data[1, ]
        na_row$Levels     <- "N.A."
        na_row$Freq_ALS   <- data$missing_ALS[1]
        na_row$Freq_CTR   <- data$missing_CTR[1]
        bind_rows(data, na_row)
      } else {
        data
      }
    }) %>%
    ungroup() %>%
    dplyr::select(Variable, Levels, Freq_ALS, gender, Freq_CTR, Variable_en)
}

dt_res = add_na_rows(dt_res)
dt_res_gender = add_na_rows_gender(dt_res_gender)

dt_res <- left_join(dt_res, levels_translation, by = "Levels")
dt_res_gender <- left_join(dt_res_gender, levels_translation, by = "Levels")

ordered_levels <- levels_translation$Levels_en[match(unique(levels_translation$Levels), levels_translation$Levels)]

dt_res$Levels_en <- factor(dt_res$Levels_en, levels = unique(ordered_levels))
dt_res_gender$Levels_en <- factor(dt_res_gender$Levels_en, levels = unique(ordered_levels))

plots_categories = barplot_categories_new(dt_res)
plots_categories_gender = barplot_categories_gender(dt_res_gender)

###########################################
# final plots for supplementary figures 
# -> educational and professional degrees
pdf("plots/barplots_educational_professional_degree_gender.pdf",height = 14,width = 10)
plot_grid(plots_categories_gender[[1]],plots_categories_gender[[2]],
          ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()
# -> physical activity in professional and sport activities
pdf("plots/barplots_professional_sport_activities_gender.pdf",height = 12,width = 8)
plot_grid(plots_categories_gender[[3]],plots_categories_gender[[4]],
          ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()
# -> substance consumption
pdf("plots/barplots_substance_use_gender.pdf",height = 12,width = 12)
plot_grid(plots_categories_gender[[6]],plots_categories_gender[[5]],
          plots_categories_gender[[7]],
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()
pdf("plots/barplots_substance_use.pdf",height = 8,width = 12)
plot_grid(plots_categories[[6]],plots_categories[[5]],
          plots_categories[[7]],
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()

## check on frequency of past use of caffeine 
freq_coffee <- data_combined[,c(2,451:454,492)]
freq_coffee_past <- freq_coffee %>%
  filter(`Konsumieren Sie aktuell regelmäßig koffeinhaltige Getränke oder haben Sie jemals in Ihrem Leben regelmäßig koffeinhaltige Getränke konsumiert?` == "Ja, in der Vergangenheit")
freq_coffee_past_dist <- freq_coffee_past %>%
  #filter(!is.na(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`)) %>%
  dplyr::mutate(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` = ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`== "Sonstiges",
                                                                                       "Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)",
                                                                                       `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`)) %>%
  dplyr::group_by(status, `Bitte geben Sie Ihr Geschlecht an.`,
           `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`) %>%
  dplyr::summarise(count = n()) 

## check on frequency of past use of alcohol
freq_alcohol <- data_combined[,c(2,455:458,492)] 
freq_alcohol_past <- freq_alcohol %>%
  filter(`Konsumieren Sie aktuell regelmäßig alkoholische Getränke oder haben Sie jemals in Ihrem Leben regelmäßig alkoholische Getränke konsumiert?` == "Ja, in der Vergangenheit")
freq_alcohol_past_dist = freq_alcohol_past %>%
  #filter(!is.na(`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? `)) %>%
  dplyr::group_by(status, `Bitte geben Sie Ihr Geschlecht an.`,
                  `Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert?  `) %>%
  dplyr::summarise(count = n())

## check on frequency of past use of cigarettes
freq_smoke <- data_combined[,c(2,459:462,492)]
freq_smoke_past <- freq_smoke %>%
  filter(`Rauchen Sie aktuell oder haben Sie jemals in Ihrem Leben regelmäßig Zigaretten (oder Zigarren etc.) geraucht? ` == "Ja, in Vergangenheit") %>%
  mutate(
    cig_num = as.numeric(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`),
    quantity = case_when(
      cig_num < 7 ~ "Few (1–6 p/ day)",
      cig_num < 15 ~ "Moderate (7–14 p/ day)",
      cig_num >= 15 ~ "High (>14 p/ day)",
      TRUE ~ NA_character_
    )
  )
freq_smoke_past_dist = freq_smoke_past %>%
  group_by(status,`Bitte geben Sie Ihr Geschlecht an.`,quantity) %>%
  dplyr::summarise(count = n())
summaryStats(na.omit(as.numeric(freq_smoke_past %>% filter(status == 1) %>% pull(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`))), quartiles = TRUE) 
summaryStats(na.omit(as.numeric(freq_smoke_past %>% filter(status == 0) %>% pull(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`))), quartiles = TRUE) 

## check on frequency of current use of caffeine 
freq_coffee_current <- freq_coffee %>%
  filter(`Konsumieren Sie aktuell regelmäßig koffeinhaltige Getränke oder haben Sie jemals in Ihrem Leben regelmäßig koffeinhaltige Getränke konsumiert?` == "Ja, aktuell")
freq_coffee_current_dist = freq_coffee_current %>%
  mutate(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` = ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` %in% c("früher 3 – 5 Tassen Kaffee pro Tag und momentan nur 1 Tasse pro Tag",
                                                                                                                                                                               "2-5 Tassen in der Woche. Bin Teetrinker",
                                                                                                                                                                               "2 Tassen pro Woche"),
                                                                                       "Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)",
                                                                                       ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` %in% c("Cola 2 Liter ca pro tag",
                                                                                                                                                                                      "tee  6tassen"),
                                                                                              "Viel (> 5 Tassen Kaffee pro Tag)",
                                                                                              ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` %in% c("Kein Kaffee, sondern Cola (ca. 0,5L)",
                                                                                                                                                                                             "4 Tassen Schw.Tee","2 - 3 Tassen schwarzer Tee"),
                                                                                                     "Moderat (z.B. 3 – 5 Tassen Kaffee pro Tag)",
                                                                                                     `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`)))) %>%
  mutate(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` = ifelse(is.na(`Bitte geben Sie Ihr Geschlecht an.`),NA,
                                                                                       `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`)) %>%
  dplyr::group_by(status, `Bitte geben Sie Ihr Geschlecht an.`,
                  `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`) %>%
  dplyr::summarise(count = n())

## check on frequency of current use of alcohol
freq_alcohol_current <- freq_alcohol %>%
  filter(`Konsumieren Sie aktuell regelmäßig alkoholische Getränke oder haben Sie jemals in Ihrem Leben regelmäßig alkoholische Getränke konsumiert?` == "Ja, aktuell")
freq_alcohol_current_dist = freq_alcohol_current %>%
  mutate(`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert?  ` = ifelse(is.na(`Bitte geben Sie Ihr Geschlecht an.`),NA,
                                                                                                                                  `Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert?  `
  )) %>%
  # filter(!is.na(`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? `)) %>%
  dplyr::group_by(status, `Bitte geben Sie Ihr Geschlecht an.`,
                  `Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert?  `) %>%
  dplyr::summarise(count = n())

## check on frequency of past use of cigarettes
freq_smoke_current <- freq_smoke %>%
  filter(`Rauchen Sie aktuell oder haben Sie jemals in Ihrem Leben regelmäßig Zigaretten (oder Zigarren etc.) geraucht? ` == "Ja, aktuell") %>%
  mutate(
    cig_num = as.numeric(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`),
    quantity = case_when(
      cig_num < 7 ~ "Few (1–6 p/ day)",
      cig_num < 15 ~ "Moderate (7–14 p/ day)",
      cig_num >= 15 ~ "High (>14 p/ day)",
      TRUE ~ NA_character_
    ))
freq_smoke_current_dist = freq_smoke_current %>%
  #filter(!is.na(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`)) %>%
  group_by(status,`Bitte geben Sie Ihr Geschlecht an.`,quantity) %>%
  dplyr::summarise(count = n())
summaryStats(na.omit(as.numeric(freq_smoke_current %>% filter(status == 1) %>% pull(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`))), quartiles = TRUE) 
summaryStats(na.omit(as.numeric(freq_smoke_current %>% filter(status == 0) %>% pull(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`))), quartiles = TRUE) 

# make data together and plot quantities 
# -> alcohol
freq_alcohol_dist = rbind(freq_alcohol_current_dist %>% mutate(type = rep("Currently",13)),
                          freq_alcohol_past_dist %>% mutate(type = rep("In the past",13))) %>%
  dplyr::rename(quantity = `Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert?  `) %>%
  mutate(status = ifelse(status == 0,"Controls",ifelse(status == 1,"ALS",NA)),
         sex = ifelse(`Bitte geben Sie Ihr Geschlecht an.` == "männlich","Male",
                      ifelse(`Bitte geben Sie Ihr Geschlecht an.` == "weiblich","Female",NA)),
         quantity = ifelse(quantity %in% c("Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche",
                                           "Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche"),
                           "Low (1-2 drinks per day)",
                           ifelse(quantity %in% c("Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche",
                                                  "Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche"),
                                  "High (> 1-2 drinks per day)",NA))) %>%
  filter(!is.na(sex))
# -> coffee
freq_coffee_dist = rbind((freq_coffee_current_dist) %>% mutate(type = rep("Currently")),
                         (freq_coffee_past_dist) %>% mutate(type = rep("In the past"))) %>%
  dplyr::rename(quantity = `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`) %>%
  mutate(status = ifelse(status == 0,"Controls",ifelse(status == 1,"ALS",NA)),
         sex = ifelse(`Bitte geben Sie Ihr Geschlecht an.` == "männlich","Male",
                      ifelse(`Bitte geben Sie Ihr Geschlecht an.` == "weiblich","Female",NA)),
         quantity = ifelse(quantity %in% c("Moderat (z.B. 3 – 5 Tassen Kaffee pro Tag)"),
                           "Moderate (3-5 cups per day)",
                           ifelse(quantity %in% c("Viel (> 5 Tassen Kaffee pro Tag)"),
                                  "High (> 5 cups per day)",
                                  ifelse(quantity %in% c("Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)"),
                                         "Low (1-2 cups per day)",NA)))) %>%
  filter(!is.na(sex))
# -> smoking
freq_smoke_dist = rbind((freq_smoke_current_dist) %>% mutate(type = rep("Currently")),
                        (freq_smoke_past_dist) %>% mutate(type = rep("In the past"))) %>%
  mutate(status = ifelse(status == 0,"Controls",ifelse(status == 1,"ALS",NA)),
         sex = ifelse(`Bitte geben Sie Ihr Geschlecht an.` == "männlich","Male",
                      ifelse(`Bitte geben Sie Ihr Geschlecht an.` == "weiblich","Female",NA)),
         quantity = ifelse(quantity %in% c("Moderate (7–14 p/ day)"),
                           "Moderate (1-14 cigarettes per day)",
                           ifelse(quantity %in% c("High (>14 p/ day)"),
                                  "High (> 14 cigarettes per day)",
                                  ifelse(quantity %in% c("Few (1–6 p/ day)"),
                                         "Low (1-6 cigarettes per day)",NA)))) %>%
  filter(!is.na(sex))

# plots stratified by gender
plot_alcohol = plot_substance_gender((freq_alcohol_dist),"Quantity of alcohol consumption")
plot_smoke = plot_substance_gender(freq_smoke_dist,"Quantity of cigarette consumption")
plot_caffeine = plot_substance_gender(freq_coffee_dist,"Quantity of caffeine consumption")

pdf("plots/barplots_substance_quantity_gender.pdf",height = 15.5,width = 18)
plot_grid(plot_caffeine,plot_alcohol,
          plot_smoke,
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()

# quantity consumption all together (nor stratified by gender)
freq_alcohol_dist_all = freq_alcohol_dist %>% 
  dplyr::group_by(status,quantity,type) %>% 
  dplyr::summarise(count = sum(count, na.rm = TRUE),.groups = "drop")
stats_alcohol_all = analyze_current_vs_past(freq_alcohol_dist_all)

freq_smoke_dist_all = freq_smoke_dist %>% 
  dplyr::group_by(status,quantity,type) %>% 
  dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
stats_smoke_all = analyze_current_vs_past(freq_smoke_dist_all)

freq_coffee_dist_all = freq_coffee_dist %>% 
  dplyr::group_by(status,quantity,type) %>% 
  dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
stats_coffee_all = analyze_current_vs_past(freq_coffee_dist_all)

plot_alcohol_all = plot_substance(freq_alcohol_dist_all,"Quantity of alcohol consumption")
plot_smoke_all = plot_substance(freq_smoke_dist_all,"Quantity of cigarette consumption")
plot_caffeine_all = plot_substance(freq_coffee_dist_all,"Quantity of caffeine consumption")

pdf("plots/barplots_substance_quantity.pdf",height = 10.9,width = 18)
plot_grid(plot_caffeine_all,plot_alcohol_all,
          plot_smoke_all,
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()


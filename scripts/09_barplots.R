## Barplots of specific questions

##########################################################
# HELPER FUNCTIONS

barplot_categories <- function(data){
  
  plot_con <- list()  
  
  categories <- unique(data$Variable_en)
  
  for (i in seq_along(categories)){
    
    tmp <- data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en, Levels_en, Freq_ALS, Freq_CTR)
    
    tmp <- reshape2::melt(tmp, id.vars = c("Variable_en","Levels_en"))
    
    tmp$Status <- ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    
    tmp$counts <- ifelse(tmp$Status == "ALS",
                         round((tmp$value * 100) / 475, 1),
                         round((tmp$value * 100) / 285, 1))
    
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
      labs(x = " ", y = "Frequency of Participants (%) \n",
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
  for (i in 1:(length(categories))){
    print(i)
    tmp = data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en,Levels_en,Freq_ALS,Freq_CTR,gender)
    print(tmp)
    tmp = melt(tmp, id.vars = c("Variable_en","Levels_en","gender"))
    print(tmp)
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","Controls")
    tmp$counts = ifelse(tmp$Status == "ALS" & tmp$gender == "female",
                        round((tmp$value*100)/200, digits = 1),
                        ifelse(tmp$Status == "ALS" & tmp$gender == "male",
                               round((tmp$value*100)/285, digits = 1),
                               ifelse(tmp$Status == "Controls" & tmp$gender == "female",
                                      round((tmp$value*100)/178,digits = 1),
                                      round((tmp$value*100)/121,digits = 1))))
    tmp$Sex <- tmp$gender
    print(tmp)
    #tmp$Levels <- factor(tmp$Levels_en, levels = rev(tmp$Levels_en %>% unique()))
    tmp$Levels <- as.factor(tmp$Levels_en)
    print(tmp)
    title_i <- ifelse(categories[i] == "Regular consumption of caffeine","Coffee consumption ever",
                      ifelse(categories[i] == "Regular consumption of alcohol","Alcohol consumption ever",
                             ifelse(categories[i] == "Regular smoke","Cigarette consumption ever",
                                    ifelse(categories[i] == "Physical activity - professional activity","Physical activity in professional activities",
                                           ifelse(categories[i] == "Physical activity - sport activity","Physical activity in sport activities",
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
    # Use position = position_dodge() 
    pdf(file = paste0("plots/boxplots/boxplot_gender_",gsub("/", "", categories[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
  return(plot_con)
}

barplot_categories_new <- function(data){
  categories <- data$Variable_en %>% unique()
  for (i in 1:(length(categories))){
    print(i)
    tmp = data %>%
      filter(Variable_en == categories[i]) %>%
      select(Variable_en,Levels_en,Freq_ALS,Freq_CTR)
    tmp = melt(tmp, id.vars = c("Variable_en","Levels_en"))
    tmp$Status = ifelse(tmp$variable == "Freq_ALS","ALS","CTR")
    tmp$counts =  ifelse(tmp$Status == "ALS",round((tmp$value*100)/475, digits = 1),
                         ifelse(tmp$Status == "CTR",round((tmp$value*100)/285,digits = 1),
                                tmp$value))
    print(tmp)
    tmp$Levels <- as.factor(tmp$Levels_en)
    title_i <- ifelse(categories[i] == "Regular consumption of caffeine","Coffee consumption",
                      ifelse(categories[i] == "Regular consumption of alcohol","Alcohol consumption",
                             ifelse(categories[i] == "Regular smoke","Cigarette smoking",
                                    ifelse(categories[i] == "Physical activity - professional activity","Level of physical activity in professional work",
                                           ifelse(categories[i] == "Physical activity - sport activity","Level of physical activity in sport activities",
                                                  categories[i])))))
    plot_con[[i]] = ggplot(tmp, aes(x = Levels, y = counts, fill = Status)) + 
      geom_col(aes(color = Status, fill = Status),
               position = position_dodge(0.8), width = 0.7) +
      scale_color_manual(values = c("ALS"="#5f91bd","CTR"="#BD8B5F"))+
      scale_fill_manual(values=c("ALS"="#5f91bd","CTR"="#BD8B5F")) +
      geom_text(aes(label = paste0(counts," %"), group = Status), 
                position = position_dodge(0.8),
                #vjust = -0.5, size = 3)+
                hjust = -0.15, size = 3) + 
      labs(x = "\n Categories", y = "Frequency (%) \n", title = title_i) +
      theme_minimal() + 
      theme( strip.background  = element_blank(),
             panel.grid.major = element_line(colour = "lightgrey"),
             panel.border = element_blank(),
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x  = element_blank(),
             plot.title = element_text(hjust = 0.5)) +
      #  axis.text.x=element_text(angle=45,hjust=0.8,vjust=0.8)) + 
      ylim(0,(max(tmp$counts) + 15)) +
      coord_flip()
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
    # Use position = position_dodge() 
    pdf(file = paste0("plots/boxplots/boxplot_",gsub("/", "", categories[i]),".pdf"))
    print(plot_con[[i]])
    dev.off()
  }
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
  
  data %>%
    mutate(
      quantity = ifelse(is.na(quantity) | quantity == "", "n.a.", quantity),
      
      # Normalize quantity names
      quantity = case_when(
        grepl("(?i)low", quantity) ~ "Low",
        grepl("(?i)moderate", quantity) ~ "Moderate",
        grepl("(?i)high", quantity) ~ "High",
        TRUE ~ "n.a."
      ),
      quantity = factor(quantity, levels = c("Low", "Moderate", "High", "n.a.")),
      
      # Define bar order
      group_order = paste(status, type),
      group_order = factor(
        group_order,
        levels = c("ALS Currently", "Controls Currently",
                   "ALS In the past", "Controls In the past")
      )
    ) %>%
    group_by(sex, status, type, quantity, group_order) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    group_by(sex,status) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    
    ggplot(aes(
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
  
  data %>%
    mutate(
      quantity = ifelse(is.na(quantity) | quantity == "", "n.a.", quantity),
      
      # Normalize quantity names
      quantity = case_when(
        grepl("(?i)low", quantity) ~ "Low",
        grepl("(?i)moderate", quantity) ~ "Moderate",
        grepl("(?i)high", quantity) ~ "High",
        TRUE ~ "n.a."
      ),
      quantity = factor(quantity, levels = c("Low", "Moderate", "High", "n.a.")),
      
      # Define bar order
      group_order = paste(status, type),
      group_order = factor(
        group_order,
        levels = c("ALS Currently", "Controls Currently",
                   "ALS In the past", "Controls In the past")
      )
    ) %>%
    group_by(status, type, quantity, group_order) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    group_by(status) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    
    ggplot(aes(
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
  # # basic checks
  # required_cols <- c("status", "quantity", "type", "count")
  # if (!all(required_cols %in% names(df))) {
  #   stop("Data frame must contain columns: status, quantity, type, count")
  # }
  
  df %>%
    # keep only the levels we care about (in case there are others)
    filter(status %in% status_levels,
           type   %in% type_levels) %>%
    group_by(quantity) %>%
    summarise(
      ALS_current = sum(count[status == status_levels[1] & type == type_levels[1]], na.rm = TRUE),
      ALS_past    = sum(count[status == status_levels[1] & type == type_levels[2]], na.rm = TRUE),
      CTR_current = sum(count[status == status_levels[2] & type == type_levels[1]], na.rm = TRUE),
      CTR_past    = sum(count[status == status_levels[2] & type == type_levels[2]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      # build 2x2 table for each quantity
      tab = list(matrix(
        c(ALS_current, ALS_past,
          CTR_current, CTR_past),
        nrow = 2, byrow = TRUE,
        dimnames = list(
          status = status_levels,
          type   = type_levels
        )
      )),
      test = list(
        if (any(tab < 5)) fisher.test(tab) else chisq.test(tab)
      ),
      test_type = ifelse(any(tab < 5), "Fisher", "Chi-squared"),
      p_value   = test$p.value,
      ALS_total = ALS_current + ALS_past,
      CTR_total = CTR_current + CTR_past,
      ALS_prop_current = ifelse(ALS_total > 0, ALS_current / ALS_total, NA_real_),
      CTR_prop_current = ifelse(CTR_total > 0, CTR_current / CTR_total, NA_real_)
    ) %>%
    ungroup() %>%
    select(
      quantity,
      ALS_current, ALS_past, ALS_total, ALS_prop_current,
      CTR_current, CTR_past, CTR_total, CTR_prop_current,
      test_type, p_value
    )
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
dat_final[,c(which(original_names %in% "Welche Krankheits des Muskel-Skelett-Systems liegt bzw. lag vor und seit wann? [Bandscheibenvorfall]"),ncol(dat_final))]
View(data_patients_control_combined[,c(150,483)])
slipped_disc_ALS = data_patients_control_combined[,c(150,483)] %>%
  filter(status == 1)
slipped_disc_ALS = data.frame(slipped_disc = slipped_disc_ALS[,1],
                              onset = ALS_open_answers_nonmotor$date)
slipped_disc_ALS = slipped_disc_ALS[!is.na(slipped_disc_ALS$slipped_disc),]
slipped_disc_ALS$year_prioronset = c(14,7,2,6,4,NA,23,0.5,1,57,28,37,1,27,11,12,NA,17,NA,15,11,
                                     20,8,NA,19,NA,22,-3,3,4,18,-1,NA,15,8,20,NA,NA,26,16,16,NA,28,NA)
slipped_disc_ALS$time_prioronset = ifelse(slipped_disc_ALS$year_prioronset < 0,
                                          NA,
                                          ifelse(slipped_disc_ALS$year_prioronset < 1,
                                                 "1-12 months",
                                                 ifelse(slipped_disc_ALS$year_prioronset < 5,
                                                        "1-5 years",
                                                        ifelse(slipped_disc_ALS$year_prioronset > 5,
                                                               "5-10 years",NA))))
median(na.omit(slipped_disc_ALS$year_prioronset))
library(EnvStats)
IQR(na.omit(slipped_disc_ALS$year_prioronset))
summaryStats(na.omit(slipped_disc_ALS$year_prioronset),quartiles = TRUE)
slipped_disc_CTR = data_patients_control_combined[,c(150,483)] %>%
  filter(status == 0)
slipped_disc_CTR = data.frame(slipped_disc = slipped_disc_CTR[,1],
                              onset = as.numeric(str_extract(data_control_common_final$`Datum Abgeschickt`, "\\d{4}")))
slipped_disc_CTR = slipped_disc_CTR[!is.na(slipped_disc_CTR$slipped_disc),]
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

slipped_disc_ALS_CTR$Levels_en <- factor(slipped_disc_ALS_CTR$Levels_en,
                                         levels = c("5-10 years","1-5 years","1-12 months"))


plot <- barplot_categories(slipped_disc_ALS_CTR)


pdf("plots/barplot_slipped_disc.pdf",width = 9)
print(plot[1])
dev.off()

#--> freqs for variables with several categories
dat_final_nosubcategories_lifestyle_temp <- dat_final_nosubcategories_lifestyle[,c(1:3,7:11,17,18)]
dat_final_nosubcategories_lifestyle_temp <- apply(dat_final_nosubcategories_lifestyle_temp, 2, 
                                                  function(x) ifelse(is.na(x),"NA",x)) %>%
  as.data.frame()
dat_final_nosubcategories_lifestyle_temp$`Was ist Ihr höchster schulischer Abschluss?` <- ifelse(dat_final_nosubcategories_lifestyle_temp$`Was ist Ihr höchster schulischer Abschluss?` == "Gesamtschule",
                                                                                                 "NA",
                                                                                                 dat_final_nosubcategories_lifestyle_temp$`Was ist Ihr höchster schulischer Abschluss?`)

dat_final_nosubcategories_lifestyle_temp$gender <- data_patients_control_combined_all$`Bitte geben Sie Ihr Geschlecht an.`
dt_categories_ALS <- frequency_status(dat_final_nosubcategories_lifestyle_temp,
                                      1,
                                      "Freq_ALS")
dt_categories_CTR <- frequency_status(dat_final_nosubcategories_lifestyle_temp,
                                      0,
                                      "Freq_CTR")
dt_categories_ALS_female <- frequency_status(dat_final_nosubcategories_lifestyle_temp  %>% filter(gender == "weiblich"),
                                             1,
                                             "Freq_ALS")
dt_categories_ALS_male <- frequency_status(dat_final_nosubcategories_lifestyle_temp %>% filter(gender == "männlich"),
                                           1,
                                           "Freq_ALS")
dt_categories_CTR_female <- frequency_status(dat_final_nosubcategories_lifestyle_temp  %>% filter(gender == "weiblich"),
                                             0,
                                             "Freq_CTR")
dt_categories_CTR_male <- frequency_status(dat_final_nosubcategories_lifestyle_temp %>% filter(gender == "männlich"),
                                           0,
                                           "Freq_CTR")
dt_categories_ALS_gender <- rbind(cbind(dt_categories_ALS_female,
                                        data.frame(gender = rep("female",45)))[1:43,],
                                  cbind(dt_categories_ALS_male,
                                        data.frame(gender = rep("male",45)))[1:43,])
dt_categories_CTR_gender <- rbind(cbind(dt_categories_CTR_female,
                                        data.frame(gender = rep("female",45)))[1:43,],
                                  cbind(dt_categories_CTR_male,
                                        data.frame(gender = rep("male",45)))[1:43,])

# -> also the category of which sport 
dat_final_nosubcategories_lifestyle_sport <- dat_final_nosubcategories_lifestyle[,c(13:16,18)]
dat_final_nosubcategories_lifestyle_sport$gender <- data_patients_control_combined_all$`Bitte geben Sie Ihr Geschlecht an.`
dat_final_nosubcategories_lifestyle_sport[,c(1:4)] <- apply(dat_final_nosubcategories_lifestyle_sport[,c(1:4)],2,
                                                            function(x) as.numeric(ifelse(x == "Ja",1,ifelse(x=="Nein",0,x))))
dt_sport <- dat_final_nosubcategories_lifestyle_sport %>%
  group_by(status) %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm = TRUE)))
dt_sport_gender <- dat_final_nosubcategories_lifestyle_sport %>%
  group_by(status,gender) %>%
  summarise(across(where(is.numeric),~sum(.x,na.rm = TRUE)))
dt_sport_ALS <- data.frame(Freq_ALS = t(dt_sport %>%
                                          filter(status == 1) %>%
                                          select(-status)))
dt_sport_ALS_gender <- rbind(data.frame(Freq_ALS = t(dt_sport_gender %>%
                                                       filter(status == 1 & gender == "weiblich") %>%
                                                       ungroup() %>%
                                                       select(-c(status,gender)))) %>%
                               mutate(gender = rep("female",4)),
                             data.frame(Freq_ALS = t(dt_sport_gender %>%
                                                       filter(status == 1 & gender == "männlich") %>%
                                                       ungroup() %>%
                                                       select(-c(status,gender)))) %>%
                               mutate(gender = rep("male",4)))
dt_sport_CTR <- data.frame(Freq_CTR = t(dt_sport %>%
                                          filter(status == 0) %>%
                                          select(-status)))
dt_sport_CTR_gender <- rbind(data.frame(Freq_CTR = t(dt_sport_gender %>%
                                                       filter(status == 0 & gender == "weiblich") %>%
                                                       ungroup() %>%
                                                       select(-c(status,gender)))) %>%
                               mutate(gender = rep("female",4)),
                             data.frame(Freq_CTR = t(dt_sport_gender %>%
                                                       filter(status == 0 & gender == "männlich") %>%
                                                       ungroup() %>%
                                                       select(-c(status,gender)))) %>%
                               mutate(gender = rep("male",4)))

names_rows <- strsplit(gsub('\\]', '', rownames(dt_sport_ALS)), '\\[',) %>% unlist()
names_rows_gender <- strsplit(gsub('\\]', '', rownames(dt_sport_ALS_gender)), '\\[',) %>% unlist()
dt_sport_ALS$Levels <- names_rows[c(2,4,6,8)]
dt_sport_ALS$Variable <- names_rows[c(1,3,5,7)]
dt_sport_ALS <- dt_sport_ALS[,c(3,2,1)]
dt_sport_all <- cbind(dt_sport_ALS,dt_sport_CTR)
dt_sport_ALS_gender$Levels <- names_rows_gender[c(2,4,6,8)]
dt_sport_ALS_gender$Variable <- names_rows_gender[c(1,3,5,7)]
dt_sport_ALS_gender <- dt_sport_ALS_gender[,c(4,3,1,2)]
dt_sport_all_gender <- cbind(dt_sport_ALS_gender,Freq_CTR = dt_sport_CTR_gender$Freq_CTR)

dt_res <- cbind(dt_categories_ALS,dt_categories_CTR[,"Freq_CTR"])
dt_res_gender <- cbind(dt_categories_ALS_gender,Freq_CTR = dt_categories_CTR_gender[,"Freq_CTR"])
names(dt_res) <- c("Variable","Levels","Freq_ALS","Freq_CTR")
dt_res <- rbind(dt_res[-44,],dt_sport_all)
dt_res_gender <- rbind(dt_res_gender,dt_sport_all_gender)

writexl::write_xlsx(dt_res,"data code output/freq_lifestyle_categories.xlsx")

dt_res_tmp <- data.frame(Variable = dt_res$Variable %>% unique(),
                         Variable_en = c("Partnership status",
                                         "Highest professional degree",
                                         "Highest educational degree",
                                         "Regular consumption of caffeine",
                                         "Regular smoke",
                                         "Regular consumption of alcohol",
                                         "Regular use of drugs",
                                         "Physical activity - professional activity",
                                         "Physical activity - sport activity",
                                         "Which sport"))
dt_res <- merge(dt_res,dt_res_tmp)
dt_res_gender <- merge(dt_res_gender,dt_res_tmp)

dt_res_tmp <- data.frame(Levels = dt_res$Levels %>% unique(),
                         Levels_en = c("Low","Intensive","Moderate","NA",
                                       "Low (e.g., office job)",
                                       "Intensive (e.g., construction work)","Moderate (e.g. care work)",
                                       "Yes, currently",
                                       "Yes, in the past","No, never", "Yes, in the past",
                                       "Fachschule/Technikerschule","Kein beruflicher Abschluss",
                                       "Lehre/Facharbeiterabschluss","Meisterprüfung","Other",
                                       "Universität/Hochschule","Abitur","Berufsschule",
                                       "Fachabitur","Grundschule","Hauptschule",
                                       "Realschule","Endurance sport","Contact sport",
                                       "Strength sport","Racket sport","Single","Living in partnership"))

dt_res_tmp_gender <- data.frame(Levels = dt_res_gender$Levels %>% unique(),
                                Levels_en = c("Low","Intensive","Moderate","NA",
                                              "Intensive (e.g., construction work)","Moderate (e.g. care work)",
                                              "Low (e.g., office job)",
                                              "Yes, in the past","No, never",
                                              "Yes, currently", "Yes, in the past","Other",
                                              "Universität/Hochschule","Meisterprüfung",
                                              "Lehre/Facharbeiterabschluss",
                                              "Fachschule/Technikerschule","Kein beruflicher Abschluss",
                                              "Hauptschule","Abitur","Berufsschule",
                                              "Fachabitur","Grundschule",
                                              "Realschule","Endurance sport","Contact sport",
                                              "Strength sport","Racket sport","Single","Living in partnership"))
dt_res <- merge(dt_res,dt_res_tmp)
dt_res_gender <- merge(dt_res_gender,dt_res_tmp_gender)
dt_res$Levels_en <- factor(dt_res$Levels_en,
                           levels = c("Low","Moderate","Intensive",
                                      "Low (e.g., office job)",
                                      "Moderate (e.g. care work)",
                                      "Intensive (e.g., construction work)",
                                      "Yes, currently", "Yes, in the past",
                                      "No, never",
                                      "Universität/Hochschule","Meisterprüfung",
                                      "Fachschule/Technikerschule","Abitur",
                                      "Fachabitur",
                                      "Lehre/Facharbeiterabschluss","Gesamtschule",
                                      "Realschule","Berufsschule",
                                      "Hauptschule","Grundschule",
                                      "Other",
                                      "Kein beruflicher Abschluss","NA",
                                      "Endurance sport","Contact sport",
                                      "Strength sport","Racket sport","Living in partnership",
                                      "Single"))
dt_res_gender$Levels_en <- factor(dt_res_gender$Levels_en,
                                  levels = c("Low","Moderate","Intensive",
                                             "Low (e.g., office job)",
                                             "Moderate (e.g. care work)",
                                             "Intensive (e.g., construction work)",
                                             "Yes, currently", "Yes, in the past",
                                             "No, never",
                                             "Universität/Hochschule","Meisterprüfung",
                                             "Fachschule/Technikerschule","Abitur",
                                             "Fachabitur",
                                             "Lehre/Facharbeiterabschluss","Gesamtschule",
                                             "Realschule","Berufsschule",
                                             "Hauptschule","Grundschule",
                                             "Other",
                                             "Kein beruflicher Abschluss","NA",
                                             "Endurance sport","Contact sport",
                                             "Strength sport","Racket sport","Living in partnership",
                                             "Single"))
dt_res$Levels_en_old <- dt_res$Levels_en
dt_res$Levels_en <- ifelse(dt_res$Levels_en == "Abitur","General qualification for university entrance",
                           ifelse(dt_res$Levels_en == "Fachabitur","Subject-specific university entrance qualification",
                                  ifelse(dt_res$Levels_en == "Gesamtschule","Comprehensive school",
                                         ifelse(dt_res$Levels_en == "Berufsschule","Vocational school",
                                                ifelse(dt_res$Levels_en == "Realschule","Secondary school",
                                                       ifelse(dt_res$Levels_en == "Hauptschule","Lower secondary school",
                                                              ifelse(dt_res$Levels_en == "Grundschule","Primary school",
                                                                     ifelse(dt_res$Levels_en == "Universität/Hochschule","University degree (Bachelor's, Master's, or equivalent degree)",
                                                                            ifelse(dt_res$Levels_en == "Fachschule/Technikerschule","Vocational/tecnhnical school or business academy",
                                                                                   ifelse(dt_res$Levels_en == "Meisterprüfung","Master craftsman examination",
                                                                                          ifelse(dt_res$Levels_en == "Lehre/Facharbeiterabschluss","Apprenticeship/skilled worker qualification",
                                                                                                 ifelse(dt_res$Levels_en == "Kein beruflicher Abschluss","No professional qualification",
                                                                                                        ifelse(dt_res$Levels_en == "Low (e.g., office job)","Low",
                                                                                                               ifelse(dt_res$Levels_en == "Yes, in the past","In the past",
                                                                                                                      ifelse(dt_res$Levels_en == "Yes, currently","Currently",
                                                                                                                             ifelse(dt_res$Levels_en == "No, never","Never",
                                                                                                                                    ifelse(dt_res$Levels_en == "Intensive (e.g., construction work)","Intensive",
                                                                                                                                           ifelse(dt_res$Levels_en == "Moderate (e.g. care work)","Moderate",
                                                                                                                                                  ifelse(dt_res$Levels_en == "Low","Low",
                                                                                                                                                         ifelse(dt_res$Levels_en == "Intensive","Intensive",
                                                                                                                                                                ifelse(dt_res$Levels_en == "Moderate","Moderate",
                                                                                                                                                                       ifelse(dt_res$Levels_en == "Other","Other",
                                                                                                                                                                              ifelse(dt_res$Levels_en == "Endurance sport","Endurance sport",
                                                                                                                                                                                     ifelse(dt_res$Levels_en =="Strength sport","Strength sport",
                                                                                                                                                                                            ifelse(dt_res$Levels_en == "Racket sport","Racket sport",
                                                                                                                                                                                                   ifelse(dt_res$Levels_en == "Contact sport","Contact sport",
                                                                                                                                                                                                          ifelse(dt_res$Levels_en == "Living in partnership","Living in partnership",
                                                                                                                                                                                                                 ifelse(dt_res$Levels_en == "Single","Single",
                                                                                                                                                                                                                        "NA"))))))))))))))))))))))))))))





dt_res_gender$Levels_en_old <- dt_res_gender$Levels_en 
dt_res_gender$Levels_en <- ifelse(dt_res_gender$Levels_en == "Abitur","General qualification \nfor university entrance",
                                  ifelse(dt_res_gender$Levels_en == "Fachabitur","Subject-specific university \nentrance qualification",
                                         ifelse(dt_res_gender$Levels_en == "Gesamtschule","Comprehensive school",
                                                ifelse(dt_res_gender$Levels_en == "Berufsschule","Vocational school",
                                                       ifelse(dt_res_gender$Levels_en == "Realschule","Secondary school",
                                                              ifelse(dt_res_gender$Levels_en == "Hauptschule","Lower secondary school",
                                                                     ifelse(dt_res_gender$Levels_en == "Grundschule","Primary school",
                                                                            ifelse(dt_res_gender$Levels_en == "Universität/Hochschule","University degree (Bachelor's, \nMaster's, or equivalent degree)",
                                                                                   ifelse(dt_res_gender$Levels_en == "Fachschule/Technikerschule","Vocational/tecnhnical school \nor business academy",
                                                                                          ifelse(dt_res_gender$Levels_en == "Meisterprüfung","Master craftsman examination",
                                                                                                 ifelse(dt_res_gender$Levels_en == "Lehre/Facharbeiterabschluss","Apprenticeship/skilled \nworker qualification",
                                                                                                        ifelse(dt_res_gender$Levels_en == "Kein beruflicher Abschluss","No professional qualification",
                                                                                                               ifelse(dt_res_gender$Levels_en == "Low (e.g., office job)","Low",
                                                                                                                      ifelse(dt_res_gender$Levels_en == "Yes, in the past","In the past",
                                                                                                                             ifelse(dt_res_gender$Levels_en == "Yes, currently","Currently",
                                                                                                                                    ifelse(dt_res_gender$Levels_en == "No, never","Never",
                                                                                                                                           ifelse(dt_res_gender$Levels_en == "Intensive (e.g., construction work)","Intensive",
                                                                                                                                                  ifelse(dt_res_gender$Levels_en == "Moderate (e.g. care work)","Moderate",
                                                                                                                                                         ifelse(dt_res_gender$Levels_en == "Low","Low",
                                                                                                                                                                ifelse(dt_res_gender$Levels_en == "Intensive","Intensive",
                                                                                                                                                                       ifelse(dt_res_gender$Levels_en == "Moderate","Moderate",
                                                                                                                                                                              ifelse(dt_res_gender$Levels_en == "Other","Other",
                                                                                                                                                                                     ifelse(dt_res_gender$Levels_en == "Endurance sport","Endurance sport",
                                                                                                                                                                                            ifelse(dt_res_gender$Levels_en =="Strength sport","Strength sport",
                                                                                                                                                                                                   ifelse(dt_res_gender$Levels_en == "Racket sport","Racket sport",
                                                                                                                                                                                                          ifelse(dt_res_gender$Levels_en == "Contact sport","Contact sport",
                                                                                                                                                                                                                 ifelse(dt_res_gender$Levels_en == "Living in partnership","Living in partnership",
                                                                                                                                                                                                                        ifelse(dt_res_gender$Levels_en == "Single","Single",
                                                                                                                                                                                                                               "n.a"))))))))))))))))))))))))))))




dt_res$Levels_en <- factor(dt_res$Levels_en,levels = c("Low","Moderate","Intensive",
                                                       "Currently", "In the past", "Never",
                                                       "General qualification for university entrance",
                                                       "Subject-specific university entrance qualification",
                                                       "Comprehensive school","Vocational school",
                                                       "Secondary school","Lower secondary school","Primary school",
                                                       "University degree (Bachelor's, Master's, or equivalent degree)",
                                                       "Vocational/tecnhnical school or business academy",
                                                       "Master craftsman examination",
                                                       "Apprenticeship/skilled worker qualification",
                                                       "No professional qualification", "Other",
                                                       "NA", "Endurance sport","Contact sport",
                                                       "Strength sport","Racket sport","Living in partnership",
                                                       "Single"))
dt_res_gender$Levels_en <- factor(dt_res_gender$Levels_en,levels = c("Low","Moderate","Intensive",
                                                                     "Currently", "In the past", "Never",
                                                                     "General qualification \nfor university entrance",
                                                                     "Subject-specific university \nentrance qualification",
                                                                     "Comprehensive school","Vocational school",
                                                                     "Secondary school","Lower secondary school","Primary school",
                                                                     "University degree (Bachelor's, \nMaster's, or equivalent degree)",
                                                                     "Vocational/tecnhnical school \nor business academy",
                                                                     "Master craftsman examination",
                                                                     "Apprenticeship/skilled \nworker qualification",
                                                                     "No professional qualification", "Other",
                                                                     "n.a", "Endurance sport","Contact sport",
                                                                     "Strength sport","Racket sport","Living in partnership",
                                                                     "Single"))
barplot_categories_new(dt_res %>% filter(Levels_en != "NA"))
plots_categories_gender = barplot_categories_gender(dt_res_gender)

###########################################
# final plots for supplementary figures 
# -> educational and professional degrees
pdf("plots/barplots_educational_professional_degree.pdf",height = 14,width = 9)
plot_grid(plots_categories_gender[[1]],plots_categories_gender[[4]],
          ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()
# -> physical activity in professional and sport activities
pdf("plots/barplots_professional_sport_activities.pdf",height = 12,width = 8)
plot_grid(plots_categories_gender[[6]],plots_categories_gender[[5]],
          ncol = 1, nrow=2, 
          labels = c("A","B")) 
dev.off()
# -> substance consumption
pdf("plots/barplots_substance_use.pdf",height = 12,width = 12)
plot_grid(plots_categories_gender[[9]],plots_categories_gender[[7]],
          plots_categories_gender[[8]],
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()

## check on frequency of past use of caffeine
freq_coffee <- data_patients_control_combined_all[,c(9,36,44,88,357,401,550)]
freq_coffee_past <- freq_coffee %>%
  filter(`Konsumieren Sie aktuell regelmäßig koffeinhaltige Getränke oder haben Sie jemals in Ihrem Leben regelmäßig koffeinhaltige Getränke konsumiert?` == "Ja, in der Vergangenheit")
freq_coffee_past_dist <- freq_coffee_past %>%
  #filter(!is.na(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`)) %>%
  mutate(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` = ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`== "Sonstiges",
                                                                                       "Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)",
                                                                                       `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`)) %>%
  group_by(status, `Bitte geben Sie Ihr Geschlecht an.`,
           `Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.`) %>%
  summarise(count = n()) 

## check on frequency of past use of alcohol
freq_alcohol <- data_patients_control_combined_all[,c(9,38,53,209,319,550)] 
freq_alcohol_past <- freq_alcohol %>%
  filter(`Konsumieren Sie aktuell regelmäßig alkoholische Getränke oder haben Sie jemals in Ihrem Leben regelmäßig alkoholische Getränke konsumiert?` == "Ja, in der Vergangenheit")
freq_alcohol_past_dist = freq_alcohol_past %>%
  #filter(!is.na(`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? `)) %>%
  group_by(status, `Bitte geben Sie Ihr Geschlecht an.`,
           `Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? `) %>%
  summarise(count = n())

## check on frequency of past use of cigarettes
freq_smoke <- data_patients_control_combined_all[,c(9,37,52,139,213,550)]
freq_smoke_past <- freq_smoke %>%
  filter(`Rauchen Sie aktuell oder haben Sie jemals in Ihrem Leben regelmäßig Zigaretten (oder Zigarren etc.) geraucht?` == "Ja, in Vergangenheit") %>%
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
  summarise(count = n())
summaryStats(na.omit(as.numeric(freq_smoke_past %>% filter(status == 1) %>% pull(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`))), quartiles = TRUE) 
summaryStats(na.omit(as.numeric(freq_smoke_past %>% filter(status == 0) %>% pull(`Wieviele Zigaretten (bzw. Zigarren etc.) rauchen Sie durchschnittlich pro Tag bzw. haben Sie durchschnittlich pro Tag geraucht?`))), quartiles = TRUE) 

## check on frequency of current use of caffeine
freq_coffee_current <- freq_coffee %>%
  filter(`Konsumieren Sie aktuell regelmäßig koffeinhaltige Getränke oder haben Sie jemals in Ihrem Leben regelmäßig koffeinhaltige Getränke konsumiert?` == "Ja, aktuell")
freq_coffee_current_dist = freq_coffee_current %>%
  mutate(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an.` = ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an. [Sonstiges]` %in% c("früher 3 – 5 Tassen Kaffee pro Tag und momentan nur 1 Tasse pro Tag",
                                                                                                                                                                               "2-5 Tassen in der Woche. Bin Teetrinker",
                                                                                                                                                                               "2 Tassen pro Woche"),
                                                                                       "Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)",
                                                                                       ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an. [Sonstiges]` %in% c("Cola 2 Liter ca pro tag",
                                                                                                                                                                                      "tee  6tassen"),
                                                                                              "Viel (> 5 Tassen Kaffee pro Tag)",
                                                                                              ifelse(`Bitte geben Sie die durchschnittliche Menge des Koffeinkonsums an. [Sonstiges]` %in% c("Kein Kaffee, sondern Cola (ca. 0,5L)",
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
  mutate(`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? ` = ifelse(is.na(`Bitte geben Sie Ihr Geschlecht an.`),NA,
                                                                                                                                  `Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? `
  )) %>%
  # filter(!is.na(`Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? `)) %>%
  dplyr::group_by(status, `Bitte geben Sie Ihr Geschlecht an.`,
                  `Wie viele alkoholische Getränke konsumieren Sie durchschnittlich bzw. haben Sie durchschnittlich konsumiert? `) %>%
  dplyr::summarise(count = n())

## check on frequency of past use of cigarettes
freq_smoke_current <- freq_smoke %>%
  filter(`Rauchen Sie aktuell oder haben Sie jemals in Ihrem Leben regelmäßig Zigaretten (oder Zigarren etc.) geraucht?` == "Ja, aktuell") %>%
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
freq_alcohol_dist = rbind(data.frame(freq_alcohol_current_dist) %>% mutate(type = rep("Currently",13)),
                          data.frame(freq_alcohol_past_dist) %>% mutate(type = rep("In the past",13))) %>%
  rename(quantity = Wie.viele.alkoholische.Getränke.konsumieren.Sie.durchschnittlich.bzw..haben.Sie.durchschnittlich.konsumiert..) %>%
  mutate(status = ifelse(status == 0,"Controls",ifelse(status == 1,"ALS",NA)),
         sex = ifelse(Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich","Male",
                      ifelse(Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich","Female",NA)),
         quantity = ifelse(quantity %in% c("Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche",
                                           "Bis zu 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche"),
                           "Low (1-2 drinks per day)",
                           ifelse(quantity %in% c("Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk (Frauen) an max. 5 Tagen in der Woche",
                                                  "Mehr als 2 Standardgetränke pro Tag (Männer) bzw. 1 Standardgetränk pro Tag (Frauen) an max. 5 Tagen in der Woche"),
                                  "High (> 1-2 drinks per day)",NA))) %>%
  filter(!is.na(sex))
# -> coffee
freq_coffee_dist = rbind(data.frame(freq_coffee_current_dist) %>% mutate(type = rep("Currently",19)),
                         data.frame(freq_coffee_past_dist) %>% mutate(type = rep("In the past",14))) %>%
  rename(quantity = Bitte.geben.Sie.die.durchschnittliche.Menge.des.Koffeinkonsums.an.) %>%
  mutate(status = ifelse(status == 0,"Controls",ifelse(status == 1,"ALS",NA)),
         sex = ifelse(Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich","Male",
                      ifelse(Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich","Female",NA)),
         quantity = ifelse(quantity %in% c("Moderat (z.B. 3 – 5 Tassen Kaffee pro Tag)"),
                           "Moderate (3-5 cups per day)",
                           ifelse(quantity %in% c("Viel (> 5 Tassen Kaffee pro Tag)"),
                                  "High (> 5 cups per day)",
                                  ifelse(quantity %in% c("Wenig (z.B. 1 – 2 Tassen Kaffee pro Tag)"),
                                         "Low (1-2 cups per day)",NA)))) %>%
  filter(!is.na(sex))
# -> smoking
freq_smoke_dist = rbind(data.frame(freq_smoke_current_dist) %>% mutate(type = rep("Currently",14)),
                        data.frame(freq_smoke_past_dist) %>% mutate(type = rep("In the past",18))) %>%
  mutate(status = ifelse(status == 0,"Controls",ifelse(status == 1,"ALS",NA)),
         sex = ifelse(Bitte.geben.Sie.Ihr.Geschlecht.an. == "männlich","Male",
                      ifelse(Bitte.geben.Sie.Ihr.Geschlecht.an. == "weiblich","Female",NA)),
         quantity = ifelse(quantity %in% c("Moderate (7–14 p/ day)"),
                           "Moderate (1-14 cigarettes per day)",
                           ifelse(quantity %in% c("High (>14 p/ day)"),
                                  "High (> 14 cigarettes per day)",
                                  ifelse(quantity %in% c("Few (1–6 p/ day)"),
                                         "Low (1-6 cigarettes per day)",NA)))) %>%
  filter(!is.na(sex))


# plots stratified by gender
plot_alcohol = plot_substance_gender(freq_alcohol_dist,"Quantity of alcohol consumption")
plot_smoke = plot_substance_gender(freq_smoke_dist,"Quantity of cigarette consumption")
plot_caffeine = plot_substance_gender(freq_coffee_dist,"Quantity of caffeine consumption")

pdf("plots/barplots_substance_quantity.pdf",height = 13,width = 15.8)
plot_grid(plot_caffeine,plot_alcohol,
          plot_smoke,
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()

# quantity consumption all together (nor stratified by gender)
freq_alcohol_dist_all = freq_alcohol_dist %>% 
  group_by(status,quantity,type) %>% 
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
stats_alcohol_all = analyze_current_vs_past(freq_alcohol_dist_all)

freq_smoke_dist_all = freq_smoke_dist %>% 
  group_by(status,quantity,type) %>% 
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
stats_smoke_all = analyze_current_vs_past(freq_smoke_dist_all)

freq_coffee_dist_all = freq_coffee_dist %>% 
  group_by(status,quantity,type) %>% 
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
stats_coffee_all = analyze_current_vs_past(freq_coffee_dist_all)

plot_alcohol_all = plot_substance(freq_alcohol_dist_all,"Quantity of alcohol consumption")
plot_smoke_all = plot_substance(freq_smoke_dist_all,"Quantity of cigarette consumption")
plot_caffeine_all = plot_substance(freq_coffee_dist_all,"Quantity of caffeine consumption")

pdf("plots/barplots_substance_quantity_all.pdf",height = 10.8,width = 15.8)
plot_grid(plot_caffeine_all,plot_alcohol_all,
          plot_smoke_all,
          ncol = 2, nrow=2, 
          labels = c("A","B","C")) 
dev.off()


# 10_descriptives_and_period_models.R
# Descriptivos generales, gráficos y regresiones históricas/interacciones
# Ejecutado desde main.R

    # Individuals without any title (using both siblings)
    
    all_titles <- c(rankable_pairs$titles_1, rankable_pairs$titles_2)
    
    share_no_titles <- mean(all_titles == 0, na.rm = TRUE)
    
    share_pairs_no_titles <- mean(
      rankable_pairs$Titles_Older == 0 & rankable_pairs$Titles_Younger == 0,
      na.rm = TRUE
    )
    
  descriptives_tab <- tibble(
    Statistic = c(
      "Number of brother pairs",
      "Mean entry age in academia",
      "Mean age gap (years)",
      "Father in academia (share)",
      "Titles (older brother)",
      "Titles (younger brother)",
      "Older brother more productive (share)",
      "Younger brother more productive (share)",
      "Share of individuals with no titles",
      "Share of pairs with no titles"
    ),
    Value = c(
      nrow(rankable_pairs),
      mean_entry_age,
      mean(rankable_pairs$Age_Gap, na.rm = TRUE),
      mean(rankable_pairs$Father_in_academia, na.rm = TRUE),
      mean(rankable_pairs$Titles_Older, na.rm = TRUE),
      mean(rankable_pairs$Titles_Younger, na.rm = TRUE),
      mean(rankable_pairs$Titles_Older > rankable_pairs$Titles_Younger, na.rm = TRUE),
      mean(rankable_pairs$Titles_Older < rankable_pairs$Titles_Younger, na.rm = TRUE),
      share_no_titles,
      share_pairs_no_titles
    )
  )
  
  productivity_share <- rankable_pairs %>%
    mutate(
      Category = case_when(
        Titles_Older > Titles_Younger ~ "Older more productive",
        Titles_Older < Titles_Younger ~ "Younger more productive",
        TRUE ~ "Equal productivity"
      )
    ) %>%
    count(Category) %>%
    mutate(
      Share = n / sum(n)
    )
  
  p_productivity_pie <- ggplot(productivity_share,
                               aes(x = "", y = Share, fill = Category)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = scales::percent(Share, accuracy = 0.1)),
      position = position_stack(vjust = 0.5),
      size = 1.5
    ) +
    
    scale_fill_manual(
      values = c(
        "Older more productive" = "#1F4A6F",
        "Younger more productive" = "#E8A2B0",
        "Equal productivity" = "#BDBDBD"
      )
    ) +
    labs(fill = "") +
    theme_void() +
    theme(
      legend.text = element_text(size = 5),
      legend.key.size = unit(0.4, "cm")
    )
  
  # ------------------------------------------------------------
  # Productivity comparison by historical period
  # ------------------------------------------------------------
  
  productivity_period <- rankable_pairs %>%
    mutate(
      Category = case_when(
        Titles_Older > Titles_Younger ~ "Older more productive",
        Titles_Older < Titles_Younger ~ "Younger more productive",
        TRUE ~ "Equal productivity"
      )
    ) %>%
    group_by(Post15th, Category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Post15th) %>%
    mutate(
      Share = n / sum(n)
    )
  
  p_productivity_period <- ggplot(
    productivity_period,
    aes(x = "", y = Share, fill = Category)
  ) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    facet_wrap(~Post15th) +
    geom_text(
      aes(label = scales::percent(Share, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    scale_fill_manual(
      values = c(
        "Older more productive" = "#1F4A6F",
        "Younger more productive" = "#E8A2B0",
        "Equal productivity" = "#BDBDBD"
      )
    ) +
    theme_void()
  # Descriptive statistics: inheritance systems
  # ------------------------------------------------------------
  
  inheritance_summary <- rankable_pairs_primogeniture %>%
    count(Inheritance_Todd) %>%
    mutate(
      Share = n / sum(n)
    )
  inheritance_summary <- inheritance_summary %>%
    mutate(
      Share = round(Share,3)
    )
  # ------------------------------------------------------------
  # Regressions by historical period
  # ------------------------------------------------------------
  
  reg_pre15 <- feols(
    asinh_gap ~ Inheritance_Todd +
      Age_Gap +
      Father_in_academia +
      diff_inst + + i(Century, ref = 15),
    
    data = rankable_pairs_primogeniture %>%
       filter(as.numeric(as.character(Century)) < 16),
    
    vcov = ~ family_cluster
  )
  
  reg_post15 <- feols(
    asinh_gap ~ Inheritance_Todd +
      Age_Gap +
      Father_in_academia +
      diff_inst + + i(Century, ref = 18),
    
    data = rankable_pairs_primogeniture %>%
      filter(as.numeric(as.character(Century)) >= 16),
    
    vcov = ~ family_cluster
  )
  reg_agegap_interaction <- feols(
    asinh_gap ~
      Inheritance_Todd * Age_Gap +
      Father_in_academia +
      diff_inst |
      Century,
    
    data = rankable_pairs_primogeniture,
    vcov = ~ family_cluster
  )
  rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
    mutate(
      Post15th = ifelse(Century >= 16,1,0)
    )
  
  reg_period_interaction <- feols(
    asinh_gap ~
      Inheritance_Todd * Post15th +
      Age_Gap +
      Father_in_academia +
      diff_inst,
    
    data = rankable_pairs_primogeniture,
    vcov = ~ family_cluster
  )

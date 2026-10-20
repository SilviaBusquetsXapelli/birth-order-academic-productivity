# 13_save_figures.R
# Figura herencia por siglo y exportación de gráficos
# Ejecutado desde main.R

  # ------------------------------------------------------------
  # Share of Non-Egalitarian inheritance systems by century
  # ------------------------------------------------------------
  
  inheritance_century <- rankable_pairs_primogeniture %>%
    filter(Inheritance_Todd %in% c("Egalitarian","Non Egalitarian"),
           !is.na(Century)) %>%
    group_by(Century) %>%
    summarise(
      Total = n(),
      NonEgalitarian = sum(Inheritance_Todd == "Non Egalitarian"),
      Share_NonEgalitarian = NonEgalitarian / Total
    )
  
  p_inheritance_century <- ggplot(inheritance_century,
                                  aes(x = Century,
                                      y = Share_NonEgalitarian)) +
    geom_line(color = "#E8A2B0", linewidth = 1) +
    geom_point(size = 3, color = "#E8A2B0") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_text(aes(label = Total), vjust = -0.7, size = 3)+
    labs(
      #title = "Share of Non-Egalitarian Inheritance Systems by Century",
      x = "Century",
      y = "Share of Non-Egalitarian systems"
    ) +
    theme_minimal()
  ggsave("Write/figures/Inheritance_by_Century.png", p_inheritance_century, width = 5, height = 3.5, dpi = 300, bg = "white")
  ggsave("Write/figures/Map_Birthplaces_ToddColors.png",  p_colored_map, width = 8, height = 5, dpi = 300, bg = "white")
  ggsave("Write/figures/Plot_Age_Gap_Histogram.png",      p_agegap,      width = 5,  height = 3, dpi = 300, bg = "white")
  ggsave("Write/figures/Plot_Rankable_vs_Unrankable.png", p_rankable, width = 5, height = 3, dpi = 300, bg = "white")
  ggsave("Write/figures/Plot_Productivity_Distribution.png", p_titles_distribution, width = 5, height = 3, dpi = 300, bg = "white")
  ggsave("Write/figures/Plot_Age_Entry.png", p_age_entry, width = 5, height = 3, dpi = 300, bg = "white")
  ggsave("Write/figures/Pie_Productivity_BirthOrder.png", p_productivity_pie, width = 2, height = 0.75, dpi = 300, bg = "white")
  ggsave(
    "Write/figures/Pie_Productivity_Period.png",
    p_productivity_period,
    width = 5,
    height = 2.5,
    dpi = 300,
    bg = "white"
  )
  ggsave(
    "Write/figures/Plot_Productivity_Distribution_Period.png",
    p_titles_period,
    width = 6,
    height = 3,
    dpi = 300,
    bg = "white"
  )

  rankable_pairs_primogeniture %>%
         count(Country_FE, sort = TRUE) %>%
         print(n = Inf)

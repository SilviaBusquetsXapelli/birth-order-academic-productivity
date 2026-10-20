# 06_geographical_maps.R
# Mapa de lugares de nacimiento; herencia por país
# Ejecutado desde main.R

  # ------------------------------------------------------------
  #   
  #   19. Map with Todd inheritance (NUTS3 + country-level) and birth bubbles
  # 
  # ------------------------------------------------------------
    
  brother_ids <- unique(c(rankable_pairs$Sibling_1,
                          rankable_pairs$Sibling_2))
  
  brothers_with_bp <- persons %>%
    filter(PKey %in% brother_ids & !is.na(BirthPlace)) %>%
    left_join(locations, by = c("BirthPlace" = "LKey"))
  
  birth_counts <- brothers_with_bp %>%
    group_by(BirthPlace, LName, Country2016, Latitude, Longitude) %>%
    summarise(N_Scholars = n(), .groups = "drop")
  
  birth_counts_clean <- birth_counts %>%
    filter(!is.na(Longitude) & !is.na(Latitude))
  
  # --- Birthplaces as sf ---
    
    birth_sf <- st_as_sf(
      birth_counts_clean,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
  
  # --- Countries (for borders + Eastern Europe) ---
    
    countries_sf <- ne_countries(
      scale = "medium",
      returnclass = "sf"
    )
  
  # --- Final map ---
    
    p_colored_map <- ggplot() +
    geom_sf(
      data = countries_sf,
      fill="grey95",
      color = NA
    ) +
    
    # NUTS3 regions (no internal borders)
  
  geom_sf(
    data = nuts3_sf,
    aes(fill = Inheritance_Todd),
    color = NA
  ) +
    
    # Country borders
  
  geom_sf(
    data = countries_sf,
    fill  = NA,
    color = "black",
    linewidth = 0.4
  ) +
    
    # Birthplace bubbles
  
  geom_sf(
    data = birth_sf,
    aes(size = N_Scholars),
    shape  = 21,
    fill   = "#D8C2A6",
    color  = "black",
    alpha  = 0.8,
    stroke = 0.4
  ) +
    
    scale_fill_manual(
      name = "Inheritance system",
      values = c(
        "Non Egalitarian"  = "#E8A2B0",
        "Egalitarian"   = "#1F4A6F"
      ),
      na.value = "grey95"
    ) +
    
    scale_size_continuous(
      name  = "Number of scholars born here",
      range = c(3, 15)
    ) +
    
    coord_sf(xlim = c(-15, 40), ylim = c(35, 65)) +
    theme_minimal() +
    # labs(
      # title    = "Birthplaces of Scholars and Inheritance Systems in Europe",
      # subtitle = "Inheritance systems assigned at NUTS3 level (Todd); bubbles show number of scholars born",
    #   x = NULL, y = NULL
    # ) +
    theme(
      legend.position = "right",
      panel.grid = element_blank()
    )

# 05_rankability_inheritance.R
# Rankability, clasificación Todd, NUTS3 y descriptivos históricos
# Ejecutado desde main.R

# ------------------------------------------------------------
#   
#   16. Ranking completeness (VIAF / Wikipedia)
# 
# ------------------------------------------------------------
  
  rankable_pairs_df   <- merged_brothers[!is.na(merged_brothers$Older_Brother), ]
unrankable_pairs_df <- merged_brothers[ is.na(merged_brothers$Older_Brother), ]

persons_links <- persons[, c("PKey", "VIAF", "Wikipedia")]
colnames(persons_links) <- c("Sibling_1", "VIAF_1", "Wikipedia_1")

rankable_pairs_df   <- left_join(rankable_pairs_df,   persons_links, by = "Sibling_1")
unrankable_pairs_df <- left_join(unrankable_pairs_df, persons_links, by = "Sibling_1")

colnames(persons_links) <- c("Sibling_2", "VIAF_2", "Wikipedia_2")

rankable_pairs_df   <- left_join(rankable_pairs_df,   persons_links, by = "Sibling_2")
unrankable_pairs_df <- left_join(unrankable_pairs_df, persons_links, by = "Sibling_2")

rankable_pairs_df$Has_Link <- ifelse(
  (!is.na(rankable_pairs_df$VIAF_1)      & rankable_pairs_df$VIAF_1      != "NA") |
    (!is.na(rankable_pairs_df$Wikipedia_1) & rankable_pairs_df$Wikipedia_1 != "NA") |
    (!is.na(rankable_pairs_df$VIAF_2)      & rankable_pairs_df$VIAF_2      != "NA") |
    (!is.na(rankable_pairs_df$Wikipedia_2) & rankable_pairs_df$Wikipedia_2 != "NA"),
  1, 0
)

unrankable_pairs_df$Has_Link <- ifelse(
  (!is.na(unrankable_pairs_df$VIAF_1)      & unrankable_pairs_df$VIAF_1      != "NA") |
    (!is.na(unrankable_pairs_df$Wikipedia_1) & unrankable_pairs_df$Wikipedia_1 != "NA") |
    (!is.na(unrankable_pairs_df$VIAF_2)      & unrankable_pairs_df$VIAF_2      != "NA") |
    (!is.na(unrankable_pairs_df$Wikipedia_2) & unrankable_pairs_df$Wikipedia_2 != "NA"),
  1, 0
)


# ------------------------------------------------------------
# Compare rankable vs unrankable pairs
# ------------------------------------------------------------

rankable_pairs_df$Rankability <- "Rankable"
unrankable_pairs_df$Rankability <- "Unrankable"

comparison_df <- bind_rows(
  rankable_pairs_df,
  unrankable_pairs_df
)



  # ------------------------------------------------------------
  #   
  #   17B. Attach Century to rankable / unrankable datasets
  # 
  # ------------------------------------------------------------
    
    rankable_pairs_df$Century <- merged_brothers$Century[
      match(
        paste(rankable_pairs_df$Sibling_1, rankable_pairs_df$Sibling_2),
        paste(merged_brothers$Sibling_1, merged_brothers$Sibling_2)
      )
    ]
  
  unrankable_pairs_df$Century <- merged_brothers$Century[
    match(
      paste(unrankable_pairs_df$Sibling_1, unrankable_pairs_df$Sibling_2),
      paste(merged_brothers$Sibling_1, merged_brothers$Sibling_2)
    )
  ]
  
  # ------------------------------------------------------------
  #   
  #   17C. Plots: Century distributions
  # 
  # ------------------------------------------------------------
    
    rankable_by_century <- rankable_pairs_df %>%
    filter(!is.na(Century)) %>%
    count(Century) %>%
    mutate(Type = "Rankable (birth order known)")
  
  unrankable_by_century <- unrankable_pairs_df %>%
    filter(!is.na(Century)) %>%
    count(Century) %>%
    mutate(Type = "Unrankable")
  
  combined_century <- bind_rows(rankable_by_century, unrankable_by_century)
  combined_century$Type <- factor(
    combined_century$Type,
    levels = c(
      "Rankable (birth order known)",
      "Unrankable"
    )
  )
  
  p_rankable <- ggplot(
    combined_century,
    aes(x = as.factor(Century), y = n, fill = Type)
  ) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = c(
      "Rankable (birth order known)" = "#1F78B4",
      "Unrankable"                  = "#BDBDBD"
    )) +
    labs(
      # title = "Brother Pairs by Century",
      x = "Century",
      y = "Count",
      fill = ""
    ) +
    theme_minimal()
  p_age_entry <- ggplot(entry_age, aes(x = AgeEntry)) +
    geom_histogram(binwidth = 2, fill = "#4C72B0", alpha = 0.8) +
    labs(
      # title = "Age of Entry into Academia",
      x = "Age",
      y = "Frequency"
    ) +
    theme_minimal()
  # ------------------------------------------------------------
  #   
  #   18. Inheritance system analysis (Todd – NUTS3 based)
  # 
  # ------------------------------------------------------------
    
    # --- Load NUTS3 with inheritance (same as map) ---
    
    nuts3_sf <- gisco_get_nuts(
      year = 2021,
      resolution = "03",
      nuts_level = 3
    )
  
  nuts3_inheritance <- read_excel("Data/NUTS3_base.xlsx")
  
  nuts3_sf <- nuts3_sf %>%
    left_join(nuts3_inheritance, by = "NUTS_ID")
  
  
  # ------------------------------------------------------------
  #   
  #   Impute inheritance systems at NUTS3 level for mapping
  # 
  # ------------------------------------------------------------
  #   
  nuts3_sf <- nuts3_sf %>%
    mutate(
      Inheritance_Todd = case_when(
        
        # Existing Todd classification
        !is.na(Inheritance_Todd) ~ Inheritance_Todd,
        
        # Andorra forced
        CNTR_CODE.x == "AND" ~ "Non Egalitarian",
        
        # Otherwise unknown
        TRUE ~ "Unknown"
      )
    )
  
  # --- Birthplaces as sf ---
    
    birth_sf_all <- st_as_sf(
      locations,
      coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE
    )
  
  # --- Assign NUTS3 inheritance to birthplaces ---
    
    birth_nuts <- st_join(
      birth_sf_all,
      nuts3_sf[, c("NUTS_ID", "Inheritance_Todd")],
      left = TRUE
    )
  
    birth_nuts <- birth_nuts %>%
      mutate(
        Inheritance_Todd = case_when(
          
          Country2016 == "AND" ~ "Non Egalitarian",
          
          !is.na(Inheritance_Todd) ~ Inheritance_Todd,
          
          TRUE ~ "Unknown"
        )
      )
    birth_nuts <- birth_nuts %>%
      mutate(
        Region_FE = substr(NUTS_ID, 1, 4)
      )
  
    birth_nuts <- birth_nuts %>%
      mutate(
        Country_FE = substr(NUTS_ID, 1, 2)
      )
  # --- Attach inheritance to brothers ---
    
    birth_info_1 <- birth_nuts %>%
      st_drop_geometry() %>%
      select(LKey, Inheritance_Todd, Region_FE, Country_FE) %>%
      rename(
        BirthPlace_1 = LKey,
        Inheritance_Todd_1 = Inheritance_Todd,
        Region_FE_1 = Region_FE,
        Country_FE_1 = Country_FE
      )
    
    birth_info_2 <- birth_nuts %>%
      st_drop_geometry() %>%
      select(LKey, Inheritance_Todd, Region_FE, Country_FE) %>%
      rename(
        BirthPlace_2 = LKey,
        Inheritance_Todd_2 = Inheritance_Todd,
        Region_FE_2 = Region_FE,
        Country_FE_2 = Country_FE
      )
    
    merged_brothers <- merged_brothers %>%
      left_join(birth_info_1, by = "BirthPlace_1") %>%
      left_join(birth_info_2, by = "BirthPlace_2")
    
  # --- Use inheritance of the older brother ---
    
    merged_brothers <- merged_brothers %>%
      mutate(
        
        # Inheritance system
        Inheritance_Todd = case_when(
          
          Older_Brother == Sibling_1 &
            !is.na(Inheritance_Todd_1)
          ~ Inheritance_Todd_1,
          
          Older_Brother == Sibling_2 &
            !is.na(Inheritance_Todd_2)
          ~ Inheritance_Todd_2,
          
          !is.na(Inheritance_Todd_1)
          ~ Inheritance_Todd_1,
          
          !is.na(Inheritance_Todd_2)
          ~ Inheritance_Todd_2,
          
          TRUE ~ "Unknown"
        ),
        
        # Regional FE
        Region_FE = case_when(
          
          Older_Brother == Sibling_1 &
            !is.na(Region_FE_1)
          ~ Region_FE_1,
          
          Older_Brother == Sibling_2 &
            !is.na(Region_FE_2)
          ~ Region_FE_2,
          
          !is.na(Region_FE_1)
          ~ Region_FE_1,
          
          !is.na(Region_FE_2)
          ~ Region_FE_2,
          
          TRUE ~ "Unknown"
        ),
        # Country FE
        Country_FE = case_when(
          
          Older_Brother == Sibling_1 &
            !is.na(Country_FE_1)
          ~ Country_FE_1,
          
          Older_Brother == Sibling_2 &
            !is.na(Country_FE_2)
          ~ Country_FE_2,
          
          !is.na(Country_FE_1)
          ~ Country_FE_1,
          
          !is.na(Country_FE_2)
          ~ Country_FE_2,
          
          TRUE ~ "Unknown"
        )
      )
    
  rankable_pairs <- rankable_pairs %>%
    left_join(
      merged_brothers %>% select(Sibling_1, Sibling_2, Inheritance_Todd),
      by = c("Sibling_1", "Sibling_2")
    )
  rankable_pairs <- rankable_pairs %>%
    left_join(
      merged_brothers %>%
        select(Sibling_1, Sibling_2, Region_FE),
      by = c("Sibling_1", "Sibling_2")
    )
  rankable_pairs <- rankable_pairs %>%
    left_join(
      merged_brothers %>%
        select(Sibling_1, Sibling_2, Country_FE),
      by = c("Sibling_1", "Sibling_2")
    )
  rankable_pairs$Inheritance_Todd[is.na(rankable_pairs$Inheritance_Todd)] <- "Unknown"
  
  rankable_pairs$Inheritance_Todd <- factor(
    rankable_pairs$Inheritance_Todd,
    levels = c(
      "Egalitarian",
      "Non Egalitarian",
      "Unknown"
    )
  )
  
  
  # ------------------------------------------------------------
  #   
  #   18B. Primogeniture-relevant rankable pairs (NEW)
  # 
  # ------------------------------------------------------------
    
    # Identify absolute firstborn within each family
  
  rankable_pairs_with_inheritance <- rankable_pairs %>%
    group_by(Father) %>%
    mutate(
      Absolute_Firstborn = case_when(
        !is.na(YearBorn_1) & YearBorn_1 <= YearBorn_2 ~ Sibling_1,
        !is.na(YearBorn_2) & YearBorn_2 <  YearBorn_1 ~ Sibling_2,
        TRUE ~ "Unknown"
      )
    ) %>%
    ungroup()
  rankable_pairs_primogeniture <- rankable_pairs_with_inheritance %>%
    filter(
      is.na(Father) |                       # sin estructura familiar clara
        Inheritance_Todd != "Non Egalitarian" |
        Sibling_1 == Absolute_Firstborn |
        Sibling_2 == Absolute_Firstborn
    )
  
  rankable_pairs_primogeniture$Inheritance_Todd[
    is.na(rankable_pairs_primogeniture$Inheritance_Todd)
  ] <- "Unknown"
  
  rankable_pairs_primogeniture$Inheritance_Todd <- factor(
    rankable_pairs_primogeniture$Inheritance_Todd,
    levels = c(
      "Egalitarian",
      "Non Egalitarian",
      "Unknown"
    )
  )
  
  
  comparison_df <- bind_rows(
    rankable_pairs_df,
    unrankable_pairs_df
  ) %>%
    left_join(
      merged_brothers %>%
        select(Sibling_1, Sibling_2, Inheritance_Todd),
      by = c("Sibling_1", "Sibling_2")
    )
  
  comparison_descriptives <- comparison_df %>%
    mutate(
      Pair_NoTitles = ifelse(
        titles_1 == 0 & titles_2 == 0,
        1, 0
      ),
      
      Any_NoTitles = (
        ifelse(titles_1 == 0,1,0) +
          ifelse(titles_2 == 0,1,0)
      ) / 2,
      
      Mean_EntryAge = (
        (FirstAcadYear_1 - YearBorn_1) +
          (FirstAcadYear_2 - YearBorn_2)
      ) / 2
    ) %>%
    group_by(Rankability) %>%
    summarise(
      
      N_pairs = n(),
      
      Mean_AgeGap =
        mean(Age_Gap, na.rm=TRUE),
      
      Mean_EntryAge =
        mean(Mean_EntryAge, na.rm=TRUE),
      
      Share_Father_Academia =
        mean(Father_in_academia, na.rm=TRUE),
      
      Mean_Titles =
        mean((titles_1 + titles_2)/2, na.rm=TRUE),
      
      Share_Individuals_NoTitles =
        mean(Any_NoTitles, na.rm=TRUE),
      
      Share_Pairs_NoTitles =
        mean(Pair_NoTitles, na.rm=TRUE),
      
      Share_NonEgalitarian =
        mean(Inheritance_Todd == "Non Egalitarian",
             na.rm=TRUE)
    )
  descriptives_period <- rankable_pairs %>%
    mutate(
      Period = ifelse(
        Century < 16,
        "Before 16th century",
        "16th century onward"
      ),
      
      Pair_NoTitles = ifelse(
        Titles_Older == 0 &
          Titles_Younger == 0,
        1,0
      )
    ) %>%
    group_by(Period) %>%
    summarise(
      
      N_pairs = n(),
      
      Mean_AgeGap =
        mean(Age_Gap, na.rm=TRUE),
      
      Mean_Titles_Older =
        mean(Titles_Older, na.rm=TRUE),
      
      Mean_Titles_Younger =
        mean(Titles_Younger, na.rm=TRUE),
      
      Share_Older_More_Productive =
        mean(Titles_Older > Titles_Younger,
             na.rm=TRUE),
      
      Share_Pairs_NoTitles =
        mean(Pair_NoTitles,
             na.rm=TRUE),
      
      Share_Father_Academia =
        mean(Father_in_academia,
             na.rm=TRUE)
    )

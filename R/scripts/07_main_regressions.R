# 07_main_regressions.R
# Variables de resultado, modelos principales 1-6
# Ejecutado desde main.R

    # ------------------------------------------------------------
    # Logarithmic productivity gap
    # ------------------------------------------------------------
    
    epsilon <- 0.1
    
    rankable_pairs <- rankable_pairs %>%
      mutate(
        log_titles_older =
          log(Titles_Older + epsilon),
        
        log_titles_younger =
          log(Titles_Younger + epsilon),
        
        log_gap =
          log_titles_older - log_titles_younger
      )
    
    # ------------------------------------------------------------
    # LEVEL PRODUCTIVITY GAP
    # ------------------------------------------------------------
    
    rankable_pairs <- rankable_pairs %>%
      mutate(
        level_gap =
          Titles_Older - Titles_Younger
      )
    
    # ------------------------------------------------------------
    # 20. Birth order regressions (FIXEST + CLUSTERED SE)
    # ------------------------------------------------------------
    
    library(fixest)
    
    # Birth-order regressions
    
    # asinh_gap = productivity difference Older - Younger
    
    rankable_pairs$asinh_gap <- rankable_pairs$Asinh_Titles_Older - rankable_pairs$Asinh_Titles_Younger
    
    # same_inst = 1 if both brothers have the same main institution
    
    rankable_pairs$same_inst <- ifelse(rankable_pairs$Inst_1 == rankable_pairs$Inst_2, 1, 0)
    rankable_pairs$diff_inst <- ifelse(rankable_pairs$Inst_1 != rankable_pairs$Inst_2, 1, 0)
    
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      mutate(
        asinh_gap = Asinh_Titles_Older - Asinh_Titles_Younger
      )
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      mutate(
        log_titles_older =
          log(Titles_Older + epsilon),
        
        log_titles_younger =
          log(Titles_Younger + epsilon),
        
        log_gap =
          log_titles_older - log_titles_younger
      )
    
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      mutate(
        level_gap =
          Titles_Older - Titles_Younger
      )
    
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      left_join(
        merged_brothers %>%
          select(Sibling_1, Sibling_2, Century),
        by = c("Sibling_1", "Sibling_2")
      )
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      left_join(
        rankable_pairs %>%
          select(Sibling_1, Sibling_2, diff_inst),
        by = c("Sibling_1", "Sibling_2")
      )
    
    
    # 1) Mean birth-order gap (equivalent to paired t-test)
    reg1 <- feols(
      asinh_gap ~ 1,
      data = rankable_pairs,
      vcov = ~ family_cluster
      )
    
    # 2) Age gap
    reg2 <- feols(
      asinh_gap ~ Age_Gap,
      data = rankable_pairs,vcov = ~ family_cluster
    )
    
    # 3) Full controls
    reg3 <- feols(
      asinh_gap ~ Father_in_academia + Age_Gap + diff_inst,
      data = rankable_pairs,vcov = ~ family_cluster
    )
    
    # 4) Inheritance effect
    reg4 <- feols(
      asinh_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst,
      data = rankable_pairs_primogeniture,
      vcov = ~ family_cluster
    )
    
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      left_join(
        merged_brothers %>%
          select(Sibling_1, Sibling_2, Century),
        by = c("Sibling_1", "Sibling_2")
      )
    
    
    # 5) Century fixed effects (PROPER FE)
    reg5 <- feols(
      asinh_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst +
        + i(Century, ref = 18),
      data = rankable_pairs_primogeniture %>%
        filter(!is.na(Century)),
      vcov = ~ family_cluster)
    
    region_counts <- rankable_pairs_primogeniture %>%
      count(Region_FE)
    
    big_regions <- region_counts %>%
      filter(n >= 10) %>%
      pull(Region_FE)
    
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      mutate(
        Region_FE_grouped = ifelse(
          Region_FE %in% big_regions,
          Region_FE,
          "Other"
        )
      )
    
    rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
      mutate(
        
        Country_1 = stringr::str_extract(
          trimws(BirthPlace_1),
          "[A-Z]{3}$"
        ),
        
        Country_2 = stringr::str_extract(
          trimws(BirthPlace_2),
          "[A-Z]{3}$"
        ),
        
        Country_FE = case_when(
          
          !is.na(Country_1) ~ Country_1,
          
          !is.na(Country_2) ~ Country_2,
          
          TRUE ~ "Unknown"
        )
      )
    
    reg6 <- feols(
      asinh_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst + 
        + i(Century, ref = 18) + i(Country_FE, ref = "DE"),
      
      data = rankable_pairs_primogeniture %>%
        filter(!is.na(Century)),
      
      vcov = ~ family_cluster
    )

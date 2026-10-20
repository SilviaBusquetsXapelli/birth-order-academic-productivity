# 08_robustness_and_alternatives.R
# Alternativas LOG/LEVELS, muestras restringidas y otros indicadores
# Ejecutado desde main.R

    # ------------------------------------------------------------
    # MAIN REGRESSIONS — LOG SPECIFICATION
    # ------------------------------------------------------------
    
    reg1_log <- feols(
      log_gap ~ Age_Gap,
      data = rankable_pairs,
      vcov = ~ family_cluster
    )
    
    reg2_log <- feols(
      log_gap ~ Father_in_academia +
        Age_Gap +
        diff_inst,
      data = rankable_pairs,
      vcov = ~ family_cluster
    )
    reg3_log <- feols(
      log_gap ~
        Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst,
      data = rankable_pairs,
      vcov = ~ family_cluster
    )
    
    reg4_log <- feols(
      log_gap ~
        Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst
        + i(Century, ref = 18),
      
      data = rankable_pairs %>%
        filter(!is.na(Century)),
      
      vcov = ~ family_cluster
    )
    
    reg_pre15_log <- feols(
      log_gap ~ Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst + + i(Century, ref = 15),
      
      data = rankable_pairs_primogeniture %>%
         filter(as.numeric(as.character(Century)) < 16),
      
      vcov = ~ family_cluster
    )
    
    reg_post15_log <- feols(
      log_gap ~ Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst + + i(Century, ref = 18),
      
      data = rankable_pairs_primogeniture %>%
        filter(as.numeric(as.character(Century)) >= 16),
      
      vcov = ~ family_cluster
    )
    
    # ------------------------------------------------------------
    # MAIN REGRESSIONS — LEVELS
    # ------------------------------------------------------------
    
    reg1_levels <- feols(
      level_gap ~ Age_Gap,
      data = rankable_pairs,
      vcov = ~ family_cluster
    )
    
    reg2_levels <- feols(
      level_gap ~ Father_in_academia +
        Age_Gap +
        diff_inst,
      data = rankable_pairs,
      vcov = ~ family_cluster
    )
    
    reg3_levels <- feols(
      level_gap ~
        Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst,
      data = rankable_pairs,
      vcov = ~ family_cluster
    )
    
    reg4_levels <- feols(
      level_gap ~
        Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst + i(Century, ref = 18),
      
      data = rankable_pairs %>%
        filter(!is.na(Century)),
      
      vcov = ~ family_cluster
    )
    
    reg_pre15_levels <- feols(
      level_gap ~ Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst + + i(Century, ref = 15),
      
      data = rankable_pairs_primogeniture %>%
         filter(as.numeric(as.character(Century)) < 16),
      
      vcov = ~ family_cluster
    )
    
    reg_post15_levels <- feols(
      level_gap ~ Inheritance_Todd +
        Age_Gap +
        Father_in_academia +
        diff_inst + + i(Century, ref = 18),
      
      data = rankable_pairs_primogeniture %>%
        filter(as.numeric(as.character(Century)) >= 16),
      
      vcov = ~ family_cluster
    )
    
    # 6) Interaction with century
    # reg6 <- feols(
    #   asinh_gap ~ Inheritance_Todd * i(Century) + Age_Gap + Father_in_academia + diff_inst,
    #   data = rankable_pairs_primogeniture %>%
    #     filter(!is.na(Century)),
    #   vcov = ~ family_cluster
    # )
    
    reg_trim20 <- feols(
      asinh_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst + i(Century, ref = 18),
      data = rankable_pairs_primogeniture %>%
        filter(Age_Gap <= 20,
               !is.na(Century)),
      vcov = ~ family_cluster
    )

    individual_counts <- rankable_pairs %>%
      pivot_longer(
        cols = c(Sibling_1, Sibling_2),
        values_to = "person"
      ) %>%
      count(person)
    individuals_large_families <- individual_counts %>%
      filter(n > 1) %>%
      pull(person)
    rankable_pairs_two_sons <- rankable_pairs %>%
      filter(
        !(Sibling_1 %in% individuals_large_families |
            Sibling_2 %in% individuals_large_families)
      )
    reg_no_large_families <- feols(
      asinh_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst + i(Century, ref = 18),
      data = rankable_pairs_two_sons,
      vcov = ~ family_cluster
    )
  reg_agegap_sq <- feols(
    asinh_gap ~ Inheritance_Todd +
      Age_Gap +
      I(Age_Gap^2) +
      Father_in_academia +
      diff_inst |
      Century,
    
    data = rankable_pairs_primogeniture,
    vcov = ~ family_cluster
  )
  # ------------------------------------------------------------
  # ROBUSTNESS REGRESSIONS
  # ------------------------------------------------------------
  # Alternative productivity gaps (robustness outcomes)
  
  rankable_pairs_primogeniture <- rankable_pairs_primogeniture %>%
    mutate(
      asinh_wiki_gap = Asinh_Wiki_Older - Asinh_Wiki_Younger,
      langs_gap = Langs_Older - Langs_Younger,
      publishers_gap = Publishers_Older - Publishers_Younger
    )
  # ------------------------------------------------------------
  # Alternative productivity measures (ROBUSTNESS)
  # ------------------------------------------------------------
  
  reg_wiki <- feols(
    asinh_wiki_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst + i(Century, ref = 18),
    data = rankable_pairs_primogeniture,
    vcov = ~ family_cluster
  )
  reg_languages <- feols(
    langs_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst + i(Century, ref = 18),
    data = rankable_pairs_primogeniture,
    vcov = ~ family_cluster
  )
  
  reg_publishers <- feols(
    publishers_gap ~ Inheritance_Todd + Age_Gap + Father_in_academia + diff_inst + i(Century, ref = 18),
    data = rankable_pairs_primogeniture,
    vcov = ~ family_cluster
  )

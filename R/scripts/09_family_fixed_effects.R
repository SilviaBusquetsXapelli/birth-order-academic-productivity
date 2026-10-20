# 09_family_fixed_effects.R
# Efectos fijos familiares, individuos únicos, muestra 3+
# Ejecutado desde main.R

    # ============================================================
    # FAMILY FIXED EFFECTS - UNIQUE INDIVIDUALS
    # ============================================================
    
    # Build individual-level data using all sibling pairs
    fe_raw <- bind_rows(
      
      merged_brothers %>%
        transmute(
          family_cluster,
          PKey = as.character(Sibling_1),
          YearBorn = as.numeric(YearBorn_1),
          Titles = as.numeric(titles_1)
        ),
      
      merged_brothers %>%
        transmute(
          family_cluster,
          PKey = as.character(Sibling_2),
          YearBorn = as.numeric(YearBorn_2),
          Titles = as.numeric(titles_2)
        )
      
    )
    
    # Check consistency across repeated appearances
    fe_checks <- fe_raw %>%
      group_by(PKey) %>%
      summarise(
        n_families = n_distinct(family_cluster),
        n_years = n_distinct(YearBorn, na.rm = TRUE),
        n_titles = n_distinct(Titles, na.rm = TRUE),
        .groups = "drop"
      )
    
    problem_cases <- fe_checks %>%
      filter(
        n_families > 1 |
          n_years > 1 |
          n_titles > 1
      )
    
    print(problem_cases)
    
    if (nrow(problem_cases) > 0) {
      stop("Check inconsistent individual records before estimating.")
    }
    
    # Keep one observation per person
    first_available <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA_real_)
      x[1]
    }
    
    fe_persons <- fe_raw %>%
      group_by(family_cluster, PKey) %>%
      summarise(
        YearBorn = first_available(YearBorn),
        Titles = first_available(Titles),
        .groups = "drop"
      )
    
    # Identify the oldest sibling in each family
    # Conservative approach: require birth years for all
    # identified siblings and no ties
    
    fe_family_sample <- fe_persons %>%
      group_by(family_cluster) %>%
      filter(
        n() >= 2,
        all(!is.na(YearBorn)),
        n_distinct(YearBorn) == n()
      ) %>%
      mutate(
        IsFirstborn = as.integer(
          YearBorn == min(YearBorn)
        )
      ) %>%
      ungroup() %>%
      filter(!is.na(Titles)) %>%
      group_by(family_cluster) %>%
      filter(
        n() >= 2,
        sum(IsFirstborn) == 1,
        any(IsFirstborn == 0)
      ) %>%
      ungroup() %>%
      mutate(
        asinh_titles = asinh(Titles)
      )
    
    # Main family fixed-effects regression
    reg_fe_family <- feols(
      asinh_titles ~ IsFirstborn | family_cluster,
      data = fe_family_sample,
      vcov = ~ family_cluster
    )
    
    summary(reg_fe_family)
    
    # ============================================================
    # FAMILY FE - FAMILIES WITH 3+ SIBLINGS
    # ============================================================
    
    fe_3plus_sample <- fe_family_sample %>%
      group_by(family_cluster) %>%
      filter(n() >= 3) %>%
      ungroup()
    
    cat(
      "Families with 2+ usable siblings:",
      n_distinct(fe_family_sample$family_cluster),
      "\n"
    )
    
    cat(
      "Families with 3+ usable siblings:",
      n_distinct(fe_3plus_sample$family_cluster),
      "\n"
    )
    
    cat(
      "Unique individuals in 3+ sibling families:",
      n_distinct(fe_3plus_sample$PKey),
      "\n"
    )
    
    reg_fe_3plus <- NULL
    
    if (n_distinct(fe_3plus_sample$family_cluster) >= 2) {
      
      reg_fe_3plus <- feols(
        asinh_titles ~ IsFirstborn | family_cluster,
        data = fe_3plus_sample,
        vcov = ~ family_cluster
      )
      
      summary(reg_fe_3plus)
      
    } else {
      
      message("Not enough eligible families for 3+ sibling FE.")
      
    }

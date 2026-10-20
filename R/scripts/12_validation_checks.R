# 12_validation_checks.R
# Integridad de IDs, coherencia individual, tamaños de muestras y missing
# Ejecutado desde main.R

  # ============================================================
  # FINAL CHECKS - FAMILY FIXED EFFECTS AND SAMPLE
  # ============================================================
  
  library(dplyr)
  
  # ------------------------------------------------------------
  # CHECK 1: FAMILY IDENTIFIERS
  # ------------------------------------------------------------
  
  cat("\n===== CHECK 1: FAMILY IDENTIFIERS =====\n")
  
  father_conflicts <- merged_brothers %>%
    filter(
      !is.na(Father),
      !trimws(as.character(Father)) %in%
        c("", "0", "NA", "Unknown")
    ) %>%
    distinct(family_cluster, Father) %>%
    count(family_cluster, name = "n_fathers") %>%
    filter(n_fathers > 1)
  
  cat("Families with conflicting father IDs:",
      nrow(father_conflicts), "\n")
  
  print(father_conflicts)
  
  
  # ------------------------------------------------------------
  # CHECK 2: DUPLICATED OR INCONSISTENT INDIVIDUALS
  # ------------------------------------------------------------
  
  cat("\n===== CHECK 2: INDIVIDUAL CONSISTENCY =====\n")
  
  problem_cases <- fe_raw %>%
    group_by(PKey) %>%
    summarise(
      n_families = n_distinct(family_cluster),
      n_years = n_distinct(YearBorn, na.rm = TRUE),
      n_titles = n_distinct(Titles, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(
      n_families > 1 |
        n_years > 1 |
        n_titles > 1
    )
  
  cat("Individuals with inconsistencies:",
      nrow(problem_cases), "\n")
  
  print(problem_cases)
  
  
  # ------------------------------------------------------------
  # CHECK 3: HOW MANY FAMILIES ENTER THE FE REGRESSIONS?
  # ------------------------------------------------------------
  
  cat("\n===== CHECK 3: FAMILY FE SAMPLE =====\n")
  
  cat("Families in main Family FE:",
      n_distinct(fe_family_sample$family_cluster), "\n")
  
  cat("Individuals in main Family FE:",
      n_distinct(fe_family_sample$PKey), "\n")
  
  cat("Observations in main regression:",
      nobs(reg_fe_family), "\n")
  
  cat("\nFamilies in 3+ siblings FE:",
      n_distinct(fe_3plus_sample$family_cluster), "\n")
  
  cat("Individuals in 3+ siblings FE:",
      n_distinct(fe_3plus_sample$PKey), "\n")
  
  cat("Observations in 3+ regression:",
      if (is.null(reg_fe_3plus)) NA_integer_ else nobs(reg_fe_3plus), "\n")
  
  # Distribution of family sizes in the 3+ FE sample
  
  family_sizes <- fe_3plus_sample %>%
    distinct(family_cluster, PKey) %>%
    count(family_cluster, name = "n_siblings")
  
  cat("\nFamily size distribution:\n")
  
  print(table(family_sizes$n_siblings))
  
  
  # ------------------------------------------------------------
  # CHECK 4: WHY DO MAIN REGRESSIONS LOSE 5 PAIRS?
  # ------------------------------------------------------------
  
  cat("\n===== CHECK 4: MISSING OBSERVATIONS =====\n")
  
  sample_check <- rankable_pairs %>%
    mutate(
      Missing_outcome =
        is.na(asinh_gap) | !is.finite(asinh_gap),
      
      Missing_age =
        is.na(Age_Gap),
      
      Missing_father =
        is.na(Father_in_academia),
      
      Missing_institution =
        is.na(diff_inst),
      
      Missing_family =
        is.na(family_cluster)
    )
  
  missing_summary <- sample_check %>%
    summarise(
      Missing_outcome = sum(Missing_outcome),
      Missing_age = sum(Missing_age),
      Missing_father = sum(Missing_father),
      Missing_institution = sum(Missing_institution),
      Missing_family = sum(Missing_family)
    )
  
  print(missing_summary)
  
  # Identify the actual pairs with incomplete data
  
  missing_pairs <- sample_check %>%
    filter(
      Missing_outcome |
        Missing_age |
        Missing_father |
        Missing_institution |
        Missing_family
    ) %>%
    select(
      Sibling_1,
      Sibling_2,
      Age_Gap,
      Father_in_academia,
      diff_inst,
      Missing_outcome,
      Missing_age,
      Missing_father,
      Missing_institution,
      Missing_family
    )
  
  cat("\nPairs with missing regression data:",
      nrow(missing_pairs), "\n")
  
  print(missing_pairs)
  
  cat("\nModel 1 observations:", nobs(reg1), "\n")
  cat("Model 3 observations:", nobs(reg3), "\n")
  
  cat("\n===== CHECKS COMPLETED =====\n")

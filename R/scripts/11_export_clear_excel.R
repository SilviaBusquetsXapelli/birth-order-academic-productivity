# 11_export_clear_excel.R
# Único Excel: Results_Brothers_Clear.xlsx
# Ejecutado desde main.R

  # ============================================================
  # NEW EXCEL EXPORT - CLEAR RESULTS
  # ============================================================
  
  
  wb <- createWorkbook()
  
  # ------------------------------------------------------------
  # STYLES
  # ------------------------------------------------------------
  
  title_style <- createStyle(
    fgFill = "#183153",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    fontSize = 13
  )
  
  header_style <- createStyle(
    fgFill = "#267E83",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    wrapText = TRUE
  )
  
  note_style <- createStyle(
    fgFill = "#EAF1F7",
    wrapText = TRUE
  )
  
  # ------------------------------------------------------------
  # CREATE WORKSHEETS
  # ------------------------------------------------------------
  
  sheets <- c(
    "01_Descriptives",
    "02_Main_Results",
    "03_Family_FE",
    "04_Robustness",
    "05_Historical_Periods",
    "06_Additional_Analyses",
    "07_Sample_Data"
  )
  
  for (s in sheets) {
    addWorksheet(wb, s, gridLines = FALSE)
    
    setColWidths(wb, s, cols = 1, widths = 38)
    setColWidths(wb, s, cols = 2:12, widths = 26)
    
    freezePane(wb, s, firstActiveRow = 2)
  }
  
  # ------------------------------------------------------------
  # FUNCTION: WRITE A CLEAR SECTION
  # ------------------------------------------------------------
  
  write_section <- function(sheet, title, data,
                            row = 2, note = NULL) {
    
    data <- as.data.frame(data)
    
    if (ncol(data) == 0) {
      data <- data.frame(Information = "No data")
    }
    
    if (nrow(data) == 0) {
      data <- data.frame(
        Information = "No observations available"
      )
    }
    
    ncols <- max(6, ncol(data))
    
    # Section title
    mergeCells(
      wb, sheet,
      cols = 1:ncols,
      rows = row
    )
    
    writeData(
      wb, sheet, title,
      startRow = row,
      colNames = FALSE
    )
    
    addStyle(
      wb, sheet, title_style,
      rows = row,
      cols = 1:ncols,
      gridExpand = TRUE
    )
    
    setRowHeights(wb, sheet, rows = row, heights = 27)
    
    row <- row + 1
    
    # Optional explanation
    if (!is.null(note)) {
      
      mergeCells(
        wb, sheet,
        cols = 1:ncols,
        rows = row
      )
      
      writeData(
        wb, sheet, note,
        startRow = row,
        colNames = FALSE
      )
      
      addStyle(
        wb, sheet, note_style,
        rows = row,
        cols = 1:ncols,
        gridExpand = TRUE
      )
      
      setRowHeights(wb, sheet, rows = row, heights = 36)
      
      row <- row + 1
    }
    
    # Results table
    writeData(
      wb, sheet, data,
      startRow = row,
      headerStyle = header_style,
      keepNA = FALSE
    )
    
    setRowHeights(wb, sheet, rows = row, heights = 27)
    
    return(row + nrow(data) + 4)
  }
  
  # ------------------------------------------------------------
  # FUNCTION: SIGNIFICANCE STARS
  # ------------------------------------------------------------
  
  stars <- function(p) {
    ifelse(
      is.na(p), "",
      ifelse(
        p < 0.01, "***",
        ifelse(p < 0.05, "**",
               ifelse(p < 0.10, "*", ""))
      )
    )
  }
  
  # ------------------------------------------------------------
  # FUNCTION: REGRESSION RESULTS
  # ------------------------------------------------------------
  
  reg_table <- function(model) {
    
    x <- broom::tidy(model)
    
    results <- data.frame(
      Variable = x$term,
      Coefficient = round(x$estimate, 4),
      Std_Error = round(x$std.error, 4),
      P_Value = round(x$p.value, 4),
      Significance = stars(x$p.value)
    )
    
    r2_type <- if (inherits(model, "feglm")) {
      "pr2"
    } else {
      "r2"
    }
    
    r2 <- tryCatch(
      as.numeric(fixest::fitstat(model, r2_type)[[1]]),
      error = function(e) NA_real_
    )
    
    extra <- data.frame(
      Variable = c(
        "Observations",
        if (r2_type == "pr2") "Pseudo R-squared" else "R-squared"
      ),
      Coefficient = c(
        nobs(model),
        round(r2, 4)
      ),
      Std_Error = NA_real_,
      P_Value = NA_real_,
      Significance = ""
    )
    
    rbind(results, extra)
  }
  
  # ------------------------------------------------------------
  # FUNCTION: COMPARE MODELS IN COLUMNS
  # ------------------------------------------------------------
  
  compare_models <- function(models, labels) {
    
    stopifnot(length(models) == length(labels))
    
    tables <- lapply(models, broom::tidy)
    
    variables <- unique(
      unlist(lapply(tables, function(x) x$term))
    )
    
    result <- data.frame(
      Variable = c(
        variables,
        "Observations",
        "R-squared"
      )
    )
    
    for (i in seq_along(models)) {
      
      x <- tables[[i]]
      
      values <- sapply(variables, function(v) {
        
        j <- match(v, x$term)
        
        if (is.na(j)) return("")
        
        paste0(
          sprintf("%.3f", x$estimate[j]),
          stars(x$p.value[j]),
          " (",
          sprintf("%.3f", x$std.error[j]),
          ")"
        )
      })
      
      r2 <- tryCatch(
        as.numeric(fixest::fitstat(models[[i]], "r2")[[1]]),
        error = function(e) NA_real_
      )
      
      result[[labels[i]]] <- c(
        values,
        as.character(nobs(models[[i]])),
        if (is.na(r2)) "" else sprintf("%.3f", r2)
      )
    }
    
    return(result)
  }
  
  # ------------------------------------------------------------
  # FUNCTION: EXPORT SEVERAL REGRESSIONS
  # ------------------------------------------------------------
  
  write_models <- function(sheet, models, labels,
                           start_row = 2) {
    
    row <- start_row
    
    for (i in seq_along(models)) {
      
      row <- write_section(
        sheet,
        paste0(i, ". ", labels[i]),
        reg_table(models[[i]]),
        row
      )
    }
    
    return(row)
  }
  

  # ------------------------------------------------------------
  # SIDE-BY-SIDE FAMILY FE COMPARISON
  # ------------------------------------------------------------

  compare_family_fe <- function(model_all, model_3plus,
                                sample_all, sample_3plus) {
    models <- list(model_all)
    labels <- c("All eligible families")
    if (!is.null(model_3plus)) {
      models <- list(model_all, model_3plus)
      labels <- c("All eligible families", "Families with 3+ siblings")
    }
    tab <- compare_models(models, labels)

    # A cluster is a family here. Count it using the estimation samples,
    # not the number of sibling pairs (an individual can occur in several pairs).
    family_count <- c(
      dplyr::n_distinct(sample_all$family_cluster),
      if (!is.null(model_3plus)) dplyr::n_distinct(sample_3plus$family_cluster)
    )
    within_r2 <- vapply(models, function(m) {
      x <- tryCatch(as.numeric(fixest::fitstat(m, "wr2")[[1]]),
                    error = function(e) NA_real_)
      if (is.na(x)) "" else sprintf("%.3f", x)
    }, character(1))

    extra <- data.frame(
      Variable = c("Families (clusters)", "Within R-squared"),
      check.names = FALSE
    )
    for (i in seq_along(labels)) {
      extra[[labels[i]]] <- c(as.character(family_count[i]), within_r2[i])
    }
    rbind(tab, extra)
  }

  # ============================================================
  # 01 - DESCRIPTIVE STATISTICS
  # ============================================================
  
  r <- write_section(
    "01_Descriptives",
    "MAIN DESCRIPTIVE STATISTICS",
    descriptives_tab
  )
  
  r <- write_section(
    "01_Descriptives",
    "DESCRIPTIVES BY HISTORICAL PERIOD",
    descriptives_period,
    r
  )
  
  r <- write_section(
    "01_Descriptives",
    "PAIRED T-TESTS",
    ttest_summary,
    r
  )
  
  r <- write_section(
    "01_Descriptives",
    "INHERITANCE SYSTEMS",
    inheritance_summary,
    r
  )
  
  r <- write_section(
    "01_Descriptives",
    "PRODUCTIVITY BY PERIOD",
    productivity_period,
    r
  )
  
  # ============================================================
  # 02 - MAIN REGRESSIONS
  # ============================================================
  
  main_models <- list(
    reg1, reg2, reg3,
    reg4, reg5, reg6
  )
  
  main_results <- compare_models(
    main_models,
    paste0("Model ", 1:6)
  )
  
  write_section(
    "02_Main_Results",
    "MAIN REGRESSIONS - ACADEMIC PRODUCTIVITY",
    main_results,
    note = paste(
      "Dependent variable: asinh productivity gap.",
      "Coefficients with clustered standard errors",
      "in parentheses.",
      "* p<0.10; ** p<0.05; *** p<0.01."
    )
  )
  
  # ============================================================
  # 03 - FAMILY FIXED EFFECTS: TWO MODELS SIDE BY SIDE
  # ============================================================

  family_comparison <- compare_family_fe(
    model_all = reg_fe_family,
    model_3plus = reg_fe_3plus,
    sample_all = fe_family_sample,
    sample_3plus = fe_3plus_sample
  )

  write_section(
    "03_Family_FE",
    "FAMILY FIXED EFFECTS - SIDE-BY-SIDE COMPARISON",
    family_comparison,
    note = paste(
      "Outcome: asinh(number of titles); unit: unique scholar.",
      "Regressor: IsFirstborn (oldest documented sibling).",
      "Family fixed effects; standard errors clustered by family.",
      "Coefficient (clustered SE). * p<0.10; ** p<0.05; *** p<0.01."
    )
  )

  write_section(
    "03_Family_FE",
    "SAMPLE DEFINITION",
    data.frame(
      Model = c("All eligible families", "Families with 3+ siblings"),
      Families = c(
        dplyr::n_distinct(fe_family_sample$family_cluster),
        dplyr::n_distinct(fe_3plus_sample$family_cluster)
      ),
      Unique_scholars = c(
        dplyr::n_distinct(fe_family_sample$PKey),
        dplyr::n_distinct(fe_3plus_sample$PKey)
      ),
      stringsAsFactors = FALSE
    ),
    row = nrow(family_comparison) + 7,
    note = paste(
      "The restricted model uses only families with 3+ usable siblings.",
      "Family birth years must be complete and have no ties."
    )
  )

  # ============================================================
  # 04 - OTHER ROBUSTNESS CHECKS
  # ============================================================

  robust_models <- list(
    reg_trim20,
    reg_no_large_families,
    reg_wiki,
    reg_languages,
    reg_publishers,
    reg_agegap_sq
  )

  robust_labels <- c(
    "Exclude age gaps above 20 years",
    "Exclude large families",
    "Alternative outcome - Wikipedia length",
    "Alternative outcome - Wikipedia languages",
    "Alternative outcome - Publishers",
    "Nonlinear age gap"
  )

  write_models(
    "04_Robustness",
    robust_models,
    robust_labels
  )

  # ============================================================
  # 04 - HISTORICAL PERIODS
  # ============================================================
  
  historical_models <- list(
    reg_pre15,
    reg_post15,
    reg_pre15_log,
    reg_post15_log,
    reg_pre15_levels,
    reg_post15_levels,
    reg_agegap_interaction,
    reg_period_interaction
  )
  
  historical_labels <- c(
    "Before 16th century - ASINH",
    "16th century onward - ASINH",
    "Before 16th century - LOG",
    "16th century onward - LOG",
    "Before 16th century - LEVELS",
    "16th century onward - LEVELS",
    "Inheritance and age gap interaction",
    "Inheritance and historical period interaction"
  )
  
  write_models(
    "05_Historical_Periods",
    historical_models,
    historical_labels
  )
  
  # ============================================================
  # 05 - ADDITIONAL ANALYSES
  # ============================================================
  
  r <- write_section(
    "06_Additional_Analyses",
    "EXTENSIVE MARGIN - LOGIT",
    reg_table(extensive_logit),
    note = "Probability of having any publications."
  )
  
  r <- write_section(
    "06_Additional_Analyses",
    "INTENSIVE MARGIN - OLS",
    reg_table(intensive_ols),
    r,
    note = "Productivity conditional on having publications."
  )
  
  log_models <- list(
    reg1_log, reg2_log,
    reg3_log, reg4_log
  )
  
  r <- write_section(
    "06_Additional_Analyses",
    "ALTERNATIVE SPECIFICATION - LOG",
    compare_models(
      log_models,
      paste0("Log ", 1:4)
    ),
    r
  )
  
  levels_models <- list(
    reg1_levels, reg2_levels,
    reg3_levels, reg4_levels
  )
  
  r <- write_section(
    "06_Additional_Analyses",
    "ALTERNATIVE SPECIFICATION - LEVELS",
    compare_models(
      levels_models,
      paste0("Levels ", 1:4)
    ),
    r
  )
  
  # ============================================================
  # 06 - SAMPLE CONSTRUCTION
  # ============================================================
  
  both_missing <- sum(
    is.na(merged_brothers$YearBorn_1) &
      is.na(merged_brothers$YearBorn_2)
  )
  
  one_missing <- sum(
    xor(
      is.na(merged_brothers$YearBorn_1),
      is.na(merged_brothers$YearBorn_2)
    )
  )
  
  same_year <- sum(
    !is.na(merged_brothers$YearBorn_1) &
      !is.na(merged_brothers$YearBorn_2) &
      merged_brothers$YearBorn_1 ==
      merged_brothers$YearBorn_2
  )
  
  sample_flow <- data.frame(
    Category = c(
      "Total brother pairs",
      "Rankable brother pairs",
      "Both birth years missing",
      "One birth year missing",
      "Same birth year"
    ),
    N = c(
      nrow(merged_brothers),
      nrow(rankable_pairs),
      both_missing,
      one_missing,
      same_year
    )
  )
  
  r <- write_section(
    "07_Sample_Data",
    "SAMPLE CONSTRUCTION",
    sample_flow
  )
  
  r <- write_section(
    "07_Sample_Data",
    "RANKABLE VS UNRANKABLE PAIRS",
    comparison_descriptives,
    r
  )
  
  # ------------------------------------------------------------
  # FAMILIES WITH 3 OR MORE BROTHERS
  # ------------------------------------------------------------
  
  if (requireNamespace("igraph", quietly = TRUE)) {
    
    count_groups <- function(data) {
      
      g <- igraph::graph_from_data_frame(
        data[, c("Sibling_1", "Sibling_2")],
        directed = FALSE
      )
      
      sum(igraph::components(g)$csize >= 3)
    }
    
    groups_table <- data.frame(
      Category = c(
        "Groups with 3+ brothers - Full sample",
        "Groups with 3+ brothers - Rankable sample"
      ),
      N = c(
        count_groups(merged_brothers),
        count_groups(rankable_pairs)
      )
    )
    
    r <- write_section(
      "07_Sample_Data",
      "GROUPS WITH THREE OR MORE BROTHERS",
      groups_table,
      r,
      note = paste(
        "Groups based on connected sibling pairs.",
        "Not necessarily verified family fixed-effect identifiers."
      )
    )
  }
  
  # ============================================================
  # SAVE EXCEL
  # ============================================================
  
  dir.create("Output", recursive = TRUE, showWarnings = FALSE)
  saveWorkbook(
    wb,
    "Output/Results_Brothers_Clear.xlsx",
    overwrite = TRUE
  )
  
  cat(
    "\nNew Excel successfully created:",
    "Output/Results_Brothers_Clear.xlsx\n"
  )

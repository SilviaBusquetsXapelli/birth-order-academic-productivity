# 04_birth_order_and_margins.R
# Orden relativo, características, t-tests y márgenes extensivo/intensivo
# Ejecutado desde main.R

# ------------------------------------------------------------
#   
#   14. Build rankable_pairs and birth-order-based variables
# 
# ------------------------------------------------------------
  
  rankable_pairs <- merged_brothers %>%
  filter(!is.na(Older_Brother)) %>%
  select(
    Sibling_1, Sibling_2, Father,
    everything()
  )

# Older_is_1 indicator

rankable_pairs$Older_is_1 <- ifelse(
  rankable_pairs$YearBorn_1 < rankable_pairs$YearBorn_2, 1, 0
)

rankable_pairs <- rankable_pairs %>%
  mutate(
    Titles_Older        = ifelse(Older_is_1 == 1, titles_1, titles_2),
    Titles_Younger      = ifelse(Older_is_1 == 1, titles_2, titles_1),
    Wiki_Older          = ifelse(Older_is_1 == 1, LengthWikiN_1, LengthWikiN_2),
    Wiki_Younger        = ifelse(Older_is_1 == 1, LengthWikiN_2, LengthWikiN_1),
    Asinh_Titles_Older  = ifelse(Older_is_1 == 1, asinh_titles_1, asinh_titles_2),
    Asinh_Titles_Younger= ifelse(Older_is_1 == 1, asinh_titles_2, asinh_titles_1),
    Asinh_Wiki_Older    = ifelse(Older_is_1 == 1, asinh_wiki_1, asinh_wiki_2),
    Asinh_Wiki_Younger  = ifelse(Older_is_1 == 1, asinh_wiki_2, asinh_wiki_1),
    HasTitle_1          = ifelse(titles_1 > 0, 1, 0),
    HasTitle_2          = ifelse(titles_2 > 0, 1, 0),
    HasTitle_Older      = ifelse(Older_is_1 == 1, HasTitle_1, HasTitle_2),
    HasTitle_Younger    = ifelse(Older_is_1 == 1, HasTitle_2, HasTitle_1),
    Langs_Older         = ifelse(Older_is_1 == 1, NWikiLanguage_1, NWikiLanguage_2),
    Langs_Younger       = ifelse(Older_is_1 == 1, NWikiLanguage_2, NWikiLanguage_1),
    Countries_Older     = ifelse(Older_is_1 == 1, countries_1, countries_2),
    Countries_Younger   = ifelse(Older_is_1 == 1, countries_2, countries_1),
    Publishers_Older    = ifelse(Older_is_1 == 1, publishers_1, publishers_2),
    Publishers_Younger  = ifelse(Older_is_1 == 1, publishers_2, publishers_1),
    Coauthors_Older     = ifelse(Older_is_1 == 1, coauthors_1, coauthors_2),
    Coauthors_Younger   = ifelse(Older_is_1 == 1, coauthors_2, coauthors_1),
    ISBN_Older          = ifelse(Older_is_1 == 1, isbn_1, isbn_2),
    ISBN_Younger        = ifelse(Older_is_1 == 1, isbn_2, isbn_1),
    PubsPre1900_Older   = ifelse(Older_is_1 == 1, pubs_pre1900_1, pubs_pre1900_2),
    PubsPre1900_Younger = ifelse(Older_is_1 == 1, pubs_pre1900_2, pubs_pre1900_1),
    PubsPost1900_Older  = ifelse(Older_is_1 == 1, pubs_post1900_1, pubs_post1900_2),
    PubsPost1900_Younger= ifelse(Older_is_1 == 1, pubs_post1900_2, pubs_post1900_1),
    Affiliations_Older = ifelse(Older_is_1 == 1, Affiliations_1, Affiliations_2),
    Affiliations_Younger = ifelse(Older_is_1 == 1, Affiliations_2, Affiliations_1),
    Occupations_Older   = ifelse(Older_is_1 == 1, noccupations_1, noccupations_2),
    Occupations_Younger = ifelse(Older_is_1 == 1, noccupations_2, noccupations_1),
    Positions_Older     = ifelse(Older_is_1 == 1, npositions_1, npositions_2),
    Positions_Younger   = ifelse(Older_is_1 == 1, npositions_2, npositions_1),
    Longevity_Older = ifelse(Older_is_1 == 1, Longevity_1, Longevity_2),
    Longevity_Younger = ifelse(Older_is_1 == 1, Longevity_2, Longevity_1)
  )

# ------------------------------------------------------------
# 14B. Distribution of productivity (Older vs Younger)
# ------------------------------------------------------------

titles_dist <- rankable_pairs %>%
  select(Asinh_Titles_Older, Asinh_Titles_Younger) %>%
  tidyr::pivot_longer(
    cols = c(Asinh_Titles_Older, Asinh_Titles_Younger),
    names_to = "BirthOrder",
    values_to = "asinh_titles"
  ) %>%
  mutate(
    BirthOrder = ifelse(
      BirthOrder == "Asinh_Titles_Older",
      "Older brother",
      "Younger brother"
    )
  )

p_titles_distribution <- ggplot(
  titles_dist,
  aes(x = asinh_titles, fill = BirthOrder)
) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c(
      "Older brother" = "#1F4A6F",
      "Younger brother" = "#E8A2B0"
    )
  ) +
  labs(
    # title = "Distribution of Scholarly Productivity by Birth Order",
    x = "Inverse hyperbolic sine of titles",
    y = "Density",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ------------------------------------------------------------
# Productivity distribution by historical period
# ------------------------------------------------------------

rankable_pairs <- rankable_pairs %>%
  mutate(
    Post15th = ifelse(
      Century >= 16,
      "16th century onward",
      "Before 16th century"
    )
  )
titles_period <- rankable_pairs %>%
  select(
    Post15th,
    Asinh_Titles_Older,
    Asinh_Titles_Younger
  ) %>%
  pivot_longer(
    cols = c(Asinh_Titles_Older,
             Asinh_Titles_Younger),
    names_to = "BirthOrder",
    values_to = "asinh_titles"
  ) %>%
  mutate(
    BirthOrder = ifelse(
      BirthOrder == "Asinh_Titles_Older",
      "Older brother",
      "Younger brother"
    )
  )

p_titles_period <- ggplot(
  titles_period,
  aes(x = asinh_titles,
      fill = BirthOrder)
) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Post15th) +
  scale_fill_manual(
    values = c(
      "Older brother" = "#1F4A6F",
      "Younger brother" = "#E8A2B0"
    )
  ) +
  theme_minimal()

# ------------------------------------------------------------
#   
#   15. T-tests (paired) — ALL PRODUCTIVITY MEASURES
# 
# ------------------------------------------------------------
  
  paired_test <- function(data, var_old, var_young, label) {
    df <- data %>%
      dplyr::select(all_of(c(var_old, var_young))) %>%
      dplyr::filter(!is.na(.data[[var_old]]) & !is.na(.data[[var_young]]))
    
    test <- t.test(df[[var_old]], df[[var_young]], paired = TRUE)
    
    tibble::tibble(
      Measure         = label,
      Mean_Difference = mean(df[[var_old]] - df[[var_young]], na.rm = TRUE),
      p_value         = test$p.value,
      N_pairs         = nrow(df)
    )
  }

ttest_summary <- dplyr::bind_rows(
  
  paired_test(rankable_pairs, "Titles_Older", "Titles_Younger", "Titles (VIAF)"),
  paired_test(rankable_pairs, "Asinh_Titles_Older", "Asinh_Titles_Younger", "Titles (VIAF, asinh)"),
  
  paired_test(rankable_pairs, "Wiki_Older", "Wiki_Younger", "Longest Wikipedia page (chars)"),
  paired_test(rankable_pairs, "Asinh_Wiki_Older", "Asinh_Wiki_Younger", "Longest Wikipedia page (asinh)"),
  
  paired_test(rankable_pairs, "HasTitle_Older", "HasTitle_Younger", "Has at least one title (extensive margin)"),
  
  paired_test(rankable_pairs, "Langs_Older", "Langs_Younger", "Number of Wikipedia languages"),
  paired_test(rankable_pairs, "Countries_Older", "Countries_Younger", "Publication countries (VIAF)"),
  paired_test(rankable_pairs, "Publishers_Older", "Publishers_Younger", "Publishers (VIAF)"),
  paired_test(rankable_pairs, "Coauthors_Older", "Coauthors_Younger", "Coauthors (VIAF)"),
  paired_test(rankable_pairs, "ISBN_Older", "ISBN_Younger", "Publications with ISBN"),
  paired_test(rankable_pairs, "PubsPre1900_Older", "PubsPre1900_Younger", "Publications before 1900"),
  paired_test(rankable_pairs, "PubsPost1900_Older", "PubsPost1900_Younger", "Publications after 1900"),
  paired_test(rankable_pairs, "Affiliations_Older", "Affiliations_Younger", "Academic affiliations"),
  paired_test(rankable_pairs, "Longevity_Older", "Longevity_Younger", "Longevity (years)"),
  paired_test(rankable_pairs, "Occupations_Older", "Occupations_Younger", "Number of occupations"),
  paired_test(rankable_pairs, "Positions_Older", "Positions_Younger", "Number of positions"),
)

# Percent interpretation for asinh

ttest_summary <- ttest_summary %>%
  mutate(
    Percent_Diff = ifelse(
      grepl("asinh", Measure),
      exp(Mean_Difference) - 1,
      NA
    ),
    Percent_DiffLabel = ifelse(
      !is.na(Percent_Diff),
      paste0(round(100 * Percent_Diff, 1), "%"),
      NA
    )
  )

# ------------------------------------------------------------
#   
#   15B. Extensive and Intensive margin regressions (CORRECTED)
# 
# ------------------------------------------------------------
# ============================================================
# CORRECT FAMILY IDENTIFIERS
# ============================================================

library(igraph)

# Use all identified sibling relationships
family_edges <- merged_brothers %>%
  transmute(
    from = as.character(Sibling_1),
    to   = as.character(Sibling_2)
  ) %>%
  filter(
    !is.na(from),
    !is.na(to),
    from != to
  ) %>%
  dplyr::distinct()

# Identify connected groups of siblings
family_graph <- graph_from_data_frame(
  family_edges,
  directed = FALSE
)

family_components <- components(family_graph)$membership

# One identifier per connected sibling group
family_lookup <- setNames(
  paste0("family_", as.integer(family_components)),
  V(family_graph)$name
)

# Assign family identifiers to all pairs
merged_brothers$family_cluster <- unname(
  family_lookup[as.character(merged_brothers$Sibling_1)]
)

rankable_pairs$family_cluster <- unname(
  family_lookup[as.character(rankable_pairs$Sibling_1)]
)

# Verify that both siblings belong to the same family
stopifnot(
  !anyNA(merged_brothers$family_cluster),
  !anyNA(rankable_pairs$family_cluster),
  
  all(
    merged_brothers$family_cluster ==
      unname(family_lookup[
        as.character(merged_brothers$Sibling_2)
      ])
  ),
  
  all(
    rankable_pairs$family_cluster ==
      unname(family_lookup[
        as.character(rankable_pairs$Sibling_2)
      ])
  )
)

cat(
  "Number of identified family groups:",
  n_distinct(merged_brothers$family_cluster),
  "\n"
)

# Check whether groups contain conflicting father IDs
father_conflicts <- merged_brothers %>%
  filter(
    !is.na(Father),
    as.character(Father) != "0"
  ) %>%
  distinct(family_cluster, Father) %>%
  count(family_cluster, name = "n_fathers") %>%
  filter(n_fathers > 1)

print(father_conflicts)

  # ------------------------------------------------------------
  # 
  # Extensive margin: does the scholar have at least one title?
  # 
  # ------------------------------------------------------------
  # 
  # Create individual-level dataset from rankable pairs

extensive_long <- rankable_pairs %>%
  mutate(pair_id = row_number()) %>%
  select(
    pair_id,
    family_cluster,   # ← AÑADIR ESTO
    Older_is_1,
    HasTitle_1,
    HasTitle_2,
    Age_Gap,
    Father_in_academia
  ) %>%
  tidyr::pivot_longer(
    cols = c(HasTitle_1, HasTitle_2),
    names_to = "Sibling",
    values_to = "HasTitle"
  ) %>%
  mutate(
    IsOlder = ifelse(
      (Sibling == "HasTitle_1" & Older_is_1 == 1) |
        (Sibling == "HasTitle_2" & Older_is_1 == 0),
      1, 0
    )
  )
# Extensive margin regression (logit, clustered by pair)

extensive_logit <- feglm(
  HasTitle ~ IsOlder + Age_Gap + Father_in_academia,
  data = extensive_long,
  family = binomial(),
  vcov = ~ family_cluster
)


# ------------------------------------------------------------
#   
#   Intensive margin: conditional on having at least one title
# 
# ------------------------------------------------------------
  
  intensive_long <- rankable_pairs %>%
  mutate(pair_id = row_number()) %>%
  select(
    pair_id,
    family_cluster,
    Titles_Older,
    Titles_Younger,
    Age_Gap,
    Father_in_academia
  ) %>%
  tidyr::pivot_longer(
    cols = c(Titles_Older, Titles_Younger),
    names_to = "Sibling",
    values_to = "Titles"
  ) %>%
  mutate(
    IsOlder = ifelse(Sibling == "Titles_Older", 1, 0),
    asinh_titles = asinh(Titles)
  ) %>%
  filter(Titles > 0)

# Intensive margin regression (OLS)

intensive_ols <- feols(
  asinh_titles ~ IsOlder + Age_Gap + Father_in_academia,
  data = intensive_long,
  vcov = ~ family_cluster
)

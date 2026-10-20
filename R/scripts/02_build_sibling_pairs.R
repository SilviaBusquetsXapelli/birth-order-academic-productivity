# 02_build_sibling_pairs.R
# Construcción de pares desde familias y Brothers without fathers; desduplicación
# Ejecutado desde main.R

# ------------------------------------------------------------
#   
#   3. Clean and prepare brothers/families
# 
# ------------------------------------------------------------
  
  brothers <- na.omit(brothers)
colnames(brothers) <- c("id", "Sibling_1", "Sibling_2")
families_same <- families_same[, c("Father", "Son", "IKey")]
families_diff <- families_diff[, c("Father", "Son", "IKey son")]
colnames(families_diff)[3] <- "IKey"

families_same <- unique(families_same[, c("Father", "Son")])
families_diff <- unique(families_diff[, c("Father", "Son")])

families_same$Father_in_academia <- 1
families_same$Father_same_inst   <- 1
families_same$Father_diff_inst   <- 0

families_diff$Father_in_academia <- 1
families_diff$Father_same_inst   <- 0
families_diff$Father_diff_inst   <- 1

families_all <- unique(rbind(families_same, families_diff))

# ------------------------------------------------------------
#   
#   4. Build brother pairs from families
# 
# ------------------------------------------------------------
  
  build_brother_pairs <- function(families_df) {
    brother_pairs <- data.frame()
    for (f in unique(families_df$Father)) {
      sons <- families_df$Son[families_df$Father == f]
      if (length(sons) > 1) {
        pairs <- t(combn(sons, 2))
        temp <- data.frame(
          Father     = f,
          Sibling_1  = pairs[, 1],
          Sibling_2  = pairs[, 2],
          stringsAsFactors = FALSE
        )
        brother_pairs <- rbind(brother_pairs, temp)
      }
    }
    return(brother_pairs)
  }

brothers_from_fathers <- build_brother_pairs(families_all)

# Add father info

brothers_from_fathers <- brothers_from_fathers %>%
  left_join(
    families_all[, c("Father", "Son", "Father_in_academia", "Father_same_inst", "Father_diff_inst")],
    by = c("Father" = "Father", "Sibling_1" = "Son")
  ) %>%
  rename(
    Father_in_academia_sib1 = Father_in_academia,
    Father_same_inst_sib1   = Father_same_inst,
    Father_diff_inst_sib1   = Father_diff_inst
  ) %>%
  left_join(
    families_all[, c("Father", "Son", "Father_in_academia", "Father_same_inst", "Father_diff_inst")],
    by = c("Father" = "Father", "Sibling_2" = "Son")
  ) %>%
  rename(
    Father_in_academia_sib2 = Father_in_academia,
    Father_same_inst_sib2   = Father_same_inst,
    Father_diff_inst_sib2   = Father_diff_inst
  )

brothers_from_fathers[is.na(brothers_from_fathers)] <- 0

brothers_from_fathers <- brothers_from_fathers %>%
  mutate(
    Father_in_academia = ifelse(
      Father_in_academia_sib1 == 1 | Father_in_academia_sib2 == 1, 1, 0
    )
  )

# ------------------------------------------------------------
#   
#   5. Merge both sets of brothers
# 
# ------------------------------------------------------------
  
  brothers$Father_in_academia    <- 0
brothers$Father_same_inst_sib1 <- 0
brothers$Father_same_inst_sib2 <- 0
brothers$Father_diff_inst_sib1 <- 0
brothers$Father_diff_inst_sib2 <- 0

merged_brothers <- rbind(
  brothers %>%
    mutate(Father = NA_character_) %>%
    select(Sibling_1, Sibling_2, Father, Father_in_academia,
           Father_same_inst_sib1, Father_same_inst_sib2,
           Father_diff_inst_sib1, Father_diff_inst_sib2),
  
  brothers_from_fathers %>%
    select(Sibling_1, Sibling_2, Father, Father_in_academia,
           Father_same_inst_sib1, Father_same_inst_sib2,
           Father_diff_inst_sib1, Father_diff_inst_sib2)
)

merged_brothers$pair_id <- apply(
  merged_brothers[, c("Sibling_1", "Sibling_2")],
  1,
  function(x) paste(sort(x), collapse = "_")
)

merged_brothers <- merged_brothers[!duplicated(merged_brothers$pair_id), ]
merged_brothers <- merged_brothers[merged_brothers$Sibling_1 != merged_brothers$Sibling_2, ]
merged_brothers$pair_id <- NULL

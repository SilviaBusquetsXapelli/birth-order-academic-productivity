# 03_enrich_scholars.R
# Biografías, productividad, afiliaciones y fechas
# Ejecutado desde main.R

# ------------------------------------------------------------
#   
#   6. Add YearBorn, YearDied, BirthPlace and rank brothers
# 
# ------------------------------------------------------------
  
  persons_sub <- persons[, c("PKey", "YearBorn", "YearDied")]
colnames(persons_sub) <- c("Sibling_1", "YearBorn_1", "YearDeath_1")
merged_brothers <- left_join(merged_brothers, persons_sub, by = "Sibling_1")

colnames(persons_sub) <- c("Sibling_2", "YearBorn_2", "YearDeath_2")
merged_brothers <- left_join(merged_brothers, persons_sub, by = "Sibling_2")

# Birthplaces

bp_sub <- persons[, c("PKey", "BirthPlace")]
colnames(bp_sub) <- c("Sibling_1", "BirthPlace_1")
merged_brothers <- merged_brothers %>%
  left_join(bp_sub, by = "Sibling_1")

colnames(bp_sub) <- c("Sibling_2", "BirthPlace_2")
merged_brothers <- merged_brothers %>%
  left_join(bp_sub, by = "Sibling_2")

# Older / younger

merged_brothers <- merged_brothers %>%
  mutate(
    Older_Brother = ifelse(
      YearBorn_1 < YearBorn_2, Sibling_1,
      ifelse(YearBorn_1 > YearBorn_2, Sibling_2, NA)
    ),
    Younger_Brother = ifelse(
      YearBorn_1 > YearBorn_2, Sibling_1,
      ifelse(YearBorn_1 < YearBorn_2, Sibling_2, NA)
    )
  )

# ------------------------------------------------------------
#   
#   7. Age gap
# 
# ------------------------------------------------------------
  
  merged_brothers$Age_Gap <- abs(merged_brothers$YearBorn_1 - merged_brothers$YearBorn_2)

p_agegap <- ggplot(merged_brothers, aes(x = Age_Gap)) +
  geom_histogram(binwidth = 2, fill = "#4C72B0", alpha = 0.8) +
  labs(
    # title = "Distribution of Age Gaps",
       x = "Age Gap (years)", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA)
  )

# ------------------------------------------------------------
#   
#   8. Merge productivity and quality data
# 
# ------------------------------------------------------------
  
  dictionary_sub <- dictionary[, c(
    "PKey", "titles", "LengthWikiN", "NWikiLanguage","countries", "publishers", "coauthors", "isbn", "noccupations", "npositions",
    grep("^pubs", names(dictionary), value = TRUE)
  )]

# Sibling 1

merged_brothers <- merged_brothers %>%
  left_join(dictionary_sub, by = c("Sibling_1" = "PKey"), relationship = "many-to-many")

cols_to_rename <- setdiff(names(dictionary_sub), "PKey")
names(merged_brothers)[names(merged_brothers) %in% cols_to_rename] <-
  paste0(cols_to_rename, "_1")

# Sibling 2

merged_brothers <- merged_brothers %>%
  left_join(dictionary_sub, by = c("Sibling_2" = "PKey"), relationship = "many-to-many")

cols_to_rename_2 <- setdiff(names(dictionary_sub), "PKey")
dup_cols <- cols_to_rename_2[cols_to_rename_2 %in% names(merged_brothers)]
names(merged_brothers)[names(merged_brothers) %in% dup_cols] <-
  paste0(cols_to_rename_2, "_2")

# ------------------------------------------------------------
#   
#   9. Publications before/after 1900
# 
# ------------------------------------------------------------
  
  pub_cols      <- grep("^pubs", names(dictionary), value = TRUE)
pre1900_cols  <- pub_cols[as.numeric(sub("pubs", "", pub_cols)) < 1900]
post1900_cols <- pub_cols[as.numeric(sub("pubs", "", pub_cols)) >= 1900]

merged_brothers <- merged_brothers %>%
  mutate(
    pubs_pre1900_1  = rowSums(select(., paste0(pre1900_cols, "_1")),  na.rm = TRUE),
    pubs_pre1900_2  = rowSums(select(., paste0(pre1900_cols, "_2")),  na.rm = TRUE),
    pubs_post1900_1 = rowSums(select(., paste0(post1900_cols, "_1")), na.rm = TRUE),
    pubs_post1900_2 = rowSums(select(., paste0(post1900_cols, "_2")), na.rm = TRUE)
  )

# ------------------------------------------------------------
#   
#   10. Count academic affiliations
# 
# ------------------------------------------------------------
  
  affiliations <- activities %>%
  group_by(PKey) %>%
  summarise(Affiliations = n())

merged_brothers <- merged_brothers %>%
  left_join(affiliations, by = c("Sibling_1" = "PKey")) %>%
  rename(Affiliations_1 = Affiliations) %>%
  left_join(affiliations, by = c("Sibling_2" = "PKey")) %>%
  rename(Affiliations_2 = Affiliations)

# ------------------------------------------------------------
#   
#   11. Longevity
# 
# ------------------------------------------------------------
  
  merged_brothers <- merged_brothers %>%
  mutate(
    Longevity_1 = ifelse(!is.na(YearDeath_1) & !is.na(YearBorn_1),
                         YearDeath_1 - YearBorn_1, NA),
    Longevity_2 = ifelse(!is.na(YearDeath_2) & !is.na(YearBorn_2),
                         YearDeath_2 - YearBorn_2, NA)
  )

# ------------------------------------------------------------
#   
#   12. asinh transformation
# 
# ------------------------------------------------------------
  
  merged_brothers <- merged_brothers %>%
  mutate(
    asinh_titles_1 = asinh(titles_1),
    asinh_titles_2 = asinh(titles_2),
    asinh_wiki_1   = asinh(LengthWikiN_1),
    asinh_wiki_2   = asinh(LengthWikiN_2)
  )


# ------------------------------------------------------------
#   
#   13. Institutions (main institution per scholar)
# 
# ------------------------------------------------------------
  
  inst_df <- activities %>%
  group_by(PKey) %>%
  summarise(
    MainInstitution = names(which.max(table(IKey))),
    .groups = "drop"
  )

merged_brothers <- merged_brothers %>%
  left_join(inst_df, by = c("Sibling_1" = "PKey")) %>%
  rename(Inst_1 = MainInstitution) %>%
  left_join(inst_df, by = c("Sibling_2" = "PKey")) %>%
  rename(Inst_2 = MainInstitution)


# ------------------------------------------------------------
#   
#   17*. Academic entry year proxy (Beginning, fallback DateApprox)
# 
# ------------------------------------------------------------

# First academic year per individual:
# 
# Priority: Beginning → DateApprox

first_academic_year <- activities %>%
  mutate(
    AcademicYear = ifelse(
      !is.na(Beginning), Beginning,
      DateApprox
    )
  ) %>%
  filter(!is.na(AcademicYear)) %>%
  group_by(PKey) %>%
  summarise(
    FirstAcademicYear = min(AcademicYear),
    .groups = "drop"
  )

# Attach to merged_brothers

merged_brothers <- merged_brothers %>%
  left_join(
    first_academic_year,
    by = c("Sibling_1" = "PKey"),
    suffix = c("_1", "")
  ) %>%
  rename(FirstAcadYear_1 = FirstAcademicYear) %>%
  left_join(
    first_academic_year,
    by = c("Sibling_2" = "PKey"),
    suffix = c("_2", "")
  ) %>%
  rename(FirstAcadYear_2 = FirstAcademicYear)

# ------------------------------------------------------------
#   
#   17A*. Final temporal assignment
# 
# Priority:
#   
#   1) Both birth years → mean
# 
# 2) One birth year
# 
# 3) Both academic years → mean
# 
# 4) One academic year
# 
# ------------------------------------------------------------
# ------------------------------------------------------------
# Calculate average age of entry into academia
# ------------------------------------------------------------

# Merge academic entry year with birth year

entry_age <- first_academic_year %>%
  left_join(
    persons %>% select(PKey, YearBorn),
    by = "PKey"
  ) %>%
  mutate(
    AgeEntry = FirstAcademicYear - YearBorn
  ) %>%
  filter(!is.na(AgeEntry) & AgeEntry > 10 & AgeEntry < 80)

# Mean age of entry

mean_entry_age <- mean(entry_age$AgeEntry, na.rm = TRUE)

merged_brothers <- merged_brothers %>%
  mutate(
    
    # Case 1: both birth years known  
    Year_final = ifelse(  
      !is.na(YearBorn_1) & !is.na(YearBorn_2),  
      (YearBorn_1 + YearBorn_2) / 2,  
      NA  
    ),  
    
    # Case 2: one birth year known  
    Year_final = ifelse(  
      is.na(Year_final) &  
        xor(is.na(YearBorn_1), is.na(YearBorn_2)),  
      ifelse(!is.na(YearBorn_1), YearBorn_1, YearBorn_2),  
      Year_final  
    ),  
    
    # Case 3: no birth years → mean academic year -33,8
    Year_final = ifelse(  
      is.na(Year_final) &  
        !is.na(FirstAcadYear_1) & !is.na(FirstAcadYear_2),  
      (FirstAcadYear_1 + FirstAcadYear_2) / 2 - mean_entry_age,  
      Year_final  
    ),  
    
    # Case 4: one academic year only  
    Year_final = ifelse(  
      is.na(Year_final) &  
        xor(is.na(FirstAcadYear_1), is.na(FirstAcadYear_2)),  
      ifelse(!is.na(FirstAcadYear_1), FirstAcadYear_1 - mean_entry_age, FirstAcadYear_2-mean_entry_age),  
      Year_final  
    )
    
  )

# Century assignment

merged_brothers$Century <- floor(merged_brothers$Year_final / 100) + 1

# =========================
# A. BACKGROUND CHARACTERISTICS TABLE
# =========================

# 1) Ensure types/labels are ready (safe if already done earlier)
bdhs_bg <- bdhs_final %>%
  mutate(
    # continuous vars as numeric
    head_age = as.numeric(head_age),
    average_parent_edu = as.numeric(average_parent_edu),
  ) %>%
  select(
    residence,               # Urban/Rural
    wealth_quintile,         # Poorest→Richest
    household_size_cat,      # Small/Medium/Large
    head_sex,                # Male/Female
    head_age,                # continuous
    relationship,            # Traditional/Non_traditional (Household type)
    average_parent_edu,      # continuous (years)
    children_cat             # parity (categorical)
  )

# 2) Build the overall table (no 'by' argument)
table_background <- bdhs_bg %>%
  tbl_summary(
    label = list(
      residence ~ "Residence",
      wealth_quintile ~ "Wealth Quintile",
      household_size_cat ~ "Household Size",
      head_sex ~ "Sex of Household Head",
      head_age ~ "Age of Household Head, years",
      relationship ~ "Household Type",
      average_parent_edu ~ "Average Parental Education, years",
      children_cat ~ "Total Children Ever Born (Parity)"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous()  ~ "{mean} ({sd})"
    ),
    type = list(
      all_of(c("head_age", "average_parent_edu")) ~ "continuous"
    ),
    digits = all_continuous() ~ 1,
    missing_text = "Missing"
  ) %>%
  modify_header(
    label ~ "**Characteristic**",
    all_stat_cols() ~ "**Overall**"
  ) %>%
  add_n() %>%  # adds total N to the header
  modify_caption("**Background characteristics of the sample (overall, unstratified)**") %>%
  bold_labels()

table_background


# =========================
# B. DESCRIPTIVE CHARACTERISTICS TABLES (STRATIFIED)
# =========================

# --- Preparation: factor outcomes/predictors and build a categorical parity var ---
bdhs_final <- bdhs_final %>%
  mutate(
    # outcomes as factors for stratification
    stunted = factor(stunted, levels = c(0, 1), labels = c("Not Stunted", "Stunted")),
    wasted = factor(wasted, levels = c(0, 1), labels = c("Not Wasted", "Wasted")),
    underweight = factor(underweight, levels = c(0, 1), labels = c("Not Underweight", "Underweight")),
    any_malnutrition = factor(any_malnutrition, levels = c(0, 1),
                              labels = c("Not Malnourished (Z ≥ -2)", "Malnourished (Any Z < -2)")),
    # predictors as factors with explicit levels
    residence = factor(residence, levels = c("Urban", "Rural")),
    wealth_quintile = factor(wealth_quintile, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    household_size_cat = factor(household_size_cat, levels = c("Small (≤4)", "Medium (5-6)", "Large (>6)")),
    head_sex = factor(head_sex, levels = c("Male", "Female")),
    child_sex = factor(child_sex, levels = c("Male", "Female")),
    # ensure relationship levels match earlier coding exactly
    relationship = factor(relationship, levels = c("Traditional", "Non_traditional")),
    # parity (categorical) for tables
    total_children_born_cat = case_when(
      total_children_born == 1 ~ "1 Child",
      total_children_born == 2 ~ "2 Children",
      total_children_born >= 3 ~ "3+ Children",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("1 Child", "2 Children", "3+ Children")),
    # ensure continuous vars are numeric
    head_age = as.numeric(head_age),
    average_parent_edu = as.numeric(average_parent_edu)
  ) %>%
  select(
    any_malnutrition, stunted, wasted, underweight,
    child_age_months, child_sex, residence, wealth_quintile,
    household_size_cat, head_sex, head_age, relationship,
    average_parent_edu, total_children_born_cat
  )

# predictors included in stratified tables
predictor_vars <- c(
  "residence", "wealth_quintile",
  "household_size_cat", "head_sex", "head_age", "relationship",
  "average_parent_edu", "total_children_born_cat"
)

# Clean labels
variable_labels <- list(
  residence ~ "Residence",
  wealth_quintile ~ "Wealth Quintile",
  household_size_cat ~ "Household Size",
  head_sex ~ "Sex of Household Head",
  head_age ~ "Age of Household Head",
  relationship ~ "Household Type",
  average_parent_edu ~ "Average Parental Education (Years)",
  total_children_born_cat ~ "Total Children Ever Born (Parity)"
)

cat("Data prepared and labels defined.\n")

# --- Base function to create a stratified table ---
create_stratified_table <- function(data, by_var, title, label_list, predictors) {
  theme_gtsummary_journal("jama")  # set a clean theme
  
  data %>%
    tbl_summary(
      by = !!sym(by_var),
      include = all_of(predictors),
      label = label_list,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1,
      missing_text = "Missing"
    ) %>%
    add_p(
      test = list(
        all_continuous() ~ "t.test",
        all_categorical() ~ "chisq.test"
      ),
      pvalue_fun = ~ style_pvalue(.x, digits = 3)
    ) %>%
    add_overall(last = TRUE) %>%
    modify_header(
      label ~ "**Characteristic**",
      p.value ~ "**P-value**",
      all_stat_cols() ~ "**{level}**"
    ) %>%
    modify_caption(paste0("**", title, "**")) %>%
    bold_labels() %>%
    bold_levels()
}

table_any <- create_stratified_table(
  bdhs_final, "any_malnutrition",
  "Descriptive Characteristics by Any Malnutrition Status",
  variable_labels, predictor_vars
)
table_any

table_stunt <- create_stratified_table(
  bdhs_final, "stunted",
  "Descriptive Characteristics by Stunting Status",
  variable_labels, predictor_vars
)
table_stunt

table_waste <- create_stratified_table(
  bdhs_final, "wasted",
  "Descriptive Characteristics by Wasting Status",
  variable_labels, predictor_vars
)
table_waste

table_uw <- create_stratified_table(
  bdhs_final, "underweight",
  "Descriptive Characteristics by Underweight Status",
  variable_labels, predictor_vars
)
table_uw

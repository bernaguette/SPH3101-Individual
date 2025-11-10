# --- 0. Load Required Libraries ---
library(gtsummary)
library(dplyr)

# --- Preparation for gtsummary: Factor Outcomes and Categorical Predictors ---
# gtsummary works best with factors, especially for the 'by' argument.
bdhs_final <- bdhs_final %>%
  mutate(
    # Ensure all outcome variables are factors for stratification
    stunted = factor(stunted, levels = c(0, 1), labels = c("Not Stunted", "Stunted")),
    wasted = factor(wasted, levels = c(0, 1), labels = c("Not Wasted", "Wasted")),
    underweight = factor(underweight, levels = c(0, 1), labels = c("Not Underweight", "Underweight")),
    any_malnutrition = factor(any_malnutrition, 
                              levels = c(0, 1), 
                              labels = c("Not Malnourished (Z ≥ -2)", "Malnourished (Any Z < -2)")),
    
    # Ensure categorical predictors are factors with proper levels
    residence = factor(residence, levels = c("Urban", "Rural")),
    wealth_quintile = factor(wealth_quintile, levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    household_size_cat = factor(household_size_cat, levels = c("Small (≤4)", "Medium (5-6)", "Large (>6)")),
    head_sex = factor(head_sex, levels = c("Male", "Female")),
    child_sex = factor(child_sex, levels = c("Male", "Female")),
    relationship = factor(relationship, levels = c("Traditional", "Non_traditional")),
    
    # Re-categorize 'Total Children Ever Born' for table use
    total_children_born_cat = case_when(
      total_children_born == 1 ~ "1 Child",
      total_children_born == 2 ~ "2 Children",
      total_children_born >= 3 ~ "3+ Children",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("1 Child", "2 Children", "3+ Children"))
  ) %>%
  # Select variables for the table body
  select(
    any_malnutrition, stunted, wasted, underweight,
    child_age_months, child_sex, residence, wealth_quintile, 
    household_size_cat, head_sex, head_age, relationship, 
    average_parent_edu, total_children_born_cat
  )

# Define predictor variables vector to be included in table body
predictor_vars <- c(
  "residence", "wealth_quintile", 
  "household_size_cat", "head_sex", "head_age", "relationship", 
  "average_parent_edu", "total_children_born_cat"
)

# Define clean labels for the table
variable_labels <- list(
  residence ~ "Residence",
  wealth_quintile ~ "Wealth Quintile",
  household_size_cat ~ "Household Size",
  head_sex ~ "Sex of Household Head",
  head_age ~ "Age of Household Head",
  relationship ~ "Mother's Relationship to Head",
  average_parent_edu ~ "Average Parental Education (Years)",
  total_children_born_cat ~ "Total Children Ever Born (Parity)"
)

cat("Data prepared and labels defined.\n")

# --- 2. Create the Base Table Function ---

create_stratified_table <- function(data, by_var, title, label_list, predictors) {
  
  # Set a clean journal theme
  theme_gtsummary_journal("jama") # Try "lancet" or "nejm" for alternatives
  
  table <- data %>%
    tbl_summary(
      by = !!sym(by_var),             # Stratifies by the chosen outcome
      include = all_of(predictors),   # Use the explicit predictor list
      label = label_list,             # Applies your clean labels
      
      # Specify statistics
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",    # Mean (SD) for continuous
        all_categorical() ~ "{n} ({p}%)"       # N (%) for categorical
      ),
      digits = all_continuous() ~ 1,
      missing_text = "Missing"
    ) %>%
    
    # Add p-values comparing the two groups
    add_p(
      test = list(
        all_continuous() ~ "t.test",      # T-test for continuous
        all_categorical() ~ "chisq.test"  # Chi-squared for categorical
      ),
      pvalue_fun = ~style_pvalue(., digits = 3)
    ) %>%
    
    # Add an "Overall" column
    add_overall(last = TRUE) %>%
    
    # Clean up the headers
    modify_header(
      label ~ "**Characteristic**",
      p.value ~ "**P-value**",
      all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)"
    ) %>%
    
    # Add a title
    modify_caption(paste("**", title, "**")) %>%
    
    # Apply bold formatting
    bold_labels() %>%
    bold_levels()
  
  return(table)
}


# --- 3. Generate and Print All Four Tables ---

cat("\n--- Generating Table 1: Stratified by ANY MALNUTRITION ---\n")
table1 <- create_stratified_table(bdhs_final, "any_malnutrition", 
                                  "Table 1: Descriptive Characteristics by Any Malnutrition Status", 
                                  variable_labels, predictor_vars)
print(table1)

cat("\n--- Generating Table 2: Stratified by STUNTING ---\n")
table2 <- create_stratified_table(bdhs_final, "stunted", 
                                  "Table 2: Descriptive Characteristics by Stunting Status", 
                                  variable_labels, predictor_vars)
print(table2)

cat("\n--- Generating Table 3: Stratified by WASTING ---\n")
table3 <- create_stratified_table(bdhs_final, "wasted", 
                                  "Table 3: Descriptive Characteristics by Wasting Status", 
                                  variable_labels, predictor_vars)
print(table3)

cat("\n--- Generating Table 4: Stratified by UNDERWEIGHT ---\n")
table4 <- create_stratified_table(bdhs_final, "underweight", 
                                  "Table 4: Descriptive Characteristics by Underweight Status", 
                                  variable_labels, predictor_vars)
print(table4)

cat("\nAll four gtsummary tables have been successfully generated and printed to the console/viewer.\n")
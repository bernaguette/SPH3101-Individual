# 2. Comprehensive Exploratory Analysis with Statistical Tests

# PART 1: BASIC DESCRIPTIVE STATISTICS

# Sample size
cat("\nTotal sample size:", nrow(bdhs_final), "\n")

# Malnutrition prevalence
cat("\n--- Malnutrition Prevalence ---\n")
cat("Any Malnutrition:", mean(bdhs_final$any_malnutrition, na.rm=T)*100, "%\n")
cat("Stunting:", mean(bdhs_final$stunted, na.rm=T)*100, "%\n")
cat("Wasting:", mean(bdhs_final$wasted, na.rm=T)*100, "%\n")
cat("Underweight:", mean(bdhs_final$underweight, na.rm=T)*100, "%\n")


# PART 2: OBJECTIVE 1 - WEALTH AND MALNUTRITION

cat("\n==================== OBJECTIVE 1: WEALTH AND MALNUTRITION ====================\n")

# Test different wealth variables
cat("\n--- Testing Different Wealth Variables ---\n")

# 1. Wealth combined (V190)
cat("\n1. Wealth Combined Index:\n")
table(bdhs_final$wealth_combined)

# 2. Wealth urban/rural specific (V190A)
cat("\n2. Wealth Urban/Rural Index:\n")
table(bdhs_final$wealth_urban_rural)

# Chi-square tests for categorical wealth
cat("\n--- Chi-square Tests: Wealth Categories vs Malnutrition ---\n")

# Stunting
cat("\nStunting by wealth quintiles:\n")
stunting_wealth_tab <- table(bdhs_final$stunted, bdhs_final$wealth_quintile)
print(prop.table(stunting_wealth_tab, 2) * 100)
chi_stunting_wealth <- chisq.test(stunting_wealth_tab)
cat("Chi-square test: χ² =", chi_stunting_wealth$statistic, ", p =", chi_stunting_wealth$p.value, "\n")


# Wasting
cat("\nWasting by wealth quintiles:\n")
wasting_wealth_tab <- table(bdhs_final$wasted, bdhs_final$wealth_quintile)
print(prop.table(wasting_wealth_tab, 2) * 100)
chi_wasting_wealth <- chisq.test(wasting_wealth_tab)
cat("Chi-square test: χ² =", chi_wasting_wealth$statistic, ", p =", chi_wasting_wealth$p.value, "\n")

# Underweight
cat("\nUnderweight by wealth quintiles:\n")
uw_wealth_tab <- table(bdhs_final$underweight, bdhs_final$wealth_quintile)
print(prop.table(uw_wealth_tab, 2) * 100)
chi_uw_wealth <- chisq.test(uw_wealth_tab)
cat("Chi-square test: χ² =", chi_uw_wealth$statistic, ", p =", chi_uw_wealth$p.value, "\n")


# PART 3: OBJECTIVE 2 - HOUSEHOLD CONDITIONS AND MALNUTRITION
cat("\n==================== OBJECTIVE 2A: HOUSEHOLD STRUCTURES (HEAD) ====================\n")

# T-test: Compare head age between malnourished vs normal
cat("\n--- T-tests: Head Age by Malnutrition Status ---\n")

# Malnutrition
malnutrition_head_age <- bdhs_final$head_age[bdhs_final$any_malnutrition == 1]
no_malnutrition_head_age <- bdhs_final$head_age[bdhs_final$any_malnutrition == 0]
t.test(malnutrition_head_age, no_malnutrition_head_age)

df_malnutrition_head_age <- data.frame(
  group = c(rep("Malnutrition", length(malnutrition_head_age)),
            rep("No Malnutrition",  length(no_malnutrition_head_age))),
  head_age = c(malnutrition_head_age, no_malnutrition_head_age)
)

ggplot(df_malnutrition_head_age, aes(group, head_age, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  scale_x_discrete(limits = c("No Malnutrition", "Malnutrition")) + 
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Malnutrition" = "chartreuse4", "Malnutrition" = "brown")) +
  labs(x = NULL, y = "Head age (months)", fill = NULL) +
  theme_minimal() + theme(legend.position = "none")

# Stunting
stunted_head_age <- bdhs_final$head_age[bdhs_final$stunted == 1]
normal_head_age <- bdhs_final$head_age[bdhs_final$any_malnutrition == 0]
t_stunting_age <- t.test(stunted_head_age, normal_head_age)
cat("t test: t =", t_stunting_age$statistic, ", p =", t_stunting_age$p.value, "\n")

# Wasting  
wasted_head_age <- bdhs_final$head_age[bdhs_final$wasted == 1]
t_wasting_age <- t.test(wasted_head_age, normal_head_age)
cat("t test: t =", t_wasting_age$statistic, ", p =", t_wasting_age$p.value, "\n")

# Underweight  
uw_head_age <- bdhs_final$head_age[bdhs_final$underweight == 1]
t_uw_age <- t.test(uw_head_age, normal_head_age)
cat("t test: t =", t_uw_age$statistic, ", p =", t_uw_age$p.value, "\n")

# Head Sex Analysis
cat("\n--- Chi-square Test: Head Sex ---\n")

# Stunting & Head Sex
data_stunting_sex <- bdhs_final[!is.na(bdhs_final$stunted) & !is.na(bdhs_final$head_sex), ]
chi_stunting_sex <- chisq.test(table(data_stunting_sex$stunted, data_stunting_sex$head_sex))
cat("Stunting & Head Sex: χ² =", chi_stunting_sex$statistic, ", p =", chi_stunting_sex$p.value, "\n")

# Wasting & Head Sex
data_wasting_sex <- bdhs_final[!is.na(bdhs_final$wasted) & !is.na(bdhs_final$head_sex), ]
chi_wasting_sex <- chisq.test(table(data_wasting_sex$wasted, data_wasting_sex$head_sex))
cat("Wasting & Head Sex: χ² =", chi_wasting_sex$statistic, ", p =", chi_wasting_sex$p.value, "\n")

# Underweight & Head Sex
data_uw_sex <- bdhs_final[!is.na(bdhs_final$underweight) & !is.na(bdhs_final$head_sex), ]
chi_uw_sex <- chisq.test(table(data_uw_sex$underweight, data_uw_sex$head_sex))
cat("Underweight & Head Sex: χ² =", chi_uw_sex$statistic, ", p =", chi_uw_sex$p.value, "\n")

# Relationship to Head Analysis
cat("\n--- Chi-square Test: Relationship ---\n")

# Stunting & Relationship
data_stunting_rel <- bdhs_final[!is.na(bdhs_final$stunted) & !is.na(bdhs_final$relationship), ]
chi_stunting_rel <- chisq.test(table(data_stunting_rel$stunted, data_stunting_rel$relationship))
cat("Stunting & Relationship: χ² =", chi_stunting_rel$statistic, ", p =", chi_stunting_rel$p.value, "\n")

# Wasting & Relationship
data_wasting_rel <- bdhs_final[!is.na(bdhs_final$wasted) & !is.na(bdhs_final$relationship), ]
chi_wasting_rel <- chisq.test(table(data_wasting_rel$wasted, data_wasting_rel$relationship))
cat("Wasting & Relationship: χ² =", chi_wasting_rel$statistic, ", p =", chi_wasting_rel$p.value, "\n")

# Underweight & Relationship
data_uw_rel <- bdhs_final[!is.na(bdhs_final$underweight) & !is.na(bdhs_final$relationship), ]
chi_uw_rel <- chisq.test(table(data_uw_rel$underweight, data_uw_rel$relationship))
cat("Underweight & Relationship: χ² =", chi_uw_rel$statistic, ", p =", chi_uw_rel$p.value, "\n")

cat("\n==================== OBJECTIVE 2B: HOUSEHOLD SIZE ====================\n")

# Descriptive statistics
cat("\n--- Household Size Distribution ---\n")
cat("Mean:", mean(bdhs_final$household_members), "\n")
cat("Median:", median(bdhs_final$household_members), "\n")
cat("SD:", sd(bdhs_final$household_members), "\n")

# T-test: Compare household size between malnourished vs normal
cat("\n--- T-tests: Household Size by Malnutrition Status ---\n")

# Stunting
stunted_hh_size <- bdhs_final$household_members[bdhs_final$stunted == 1]
normal_hh_size <- bdhs_final$household_members[bdhs_final$any_malnutrition == 0]
t_stunting_hh <- t.test(stunted_hh_size, normal_hh_size)
cat("t test: t =", t_stunting_hh$statistic, ", p =", t_stunting_hh$p.value, "\n")

# Wasting
wasted_hh_size <- bdhs_final$household_members[bdhs_final$wasted == 1]
t_wasting_hh <- t.test(wasted_hh_size, normal_hh_size)
cat("t test: t =", t_wasting_hh$statistic, ", p =", t_wasting_hh$p.value, "\n")

# Underweight
uw_hh_size <- bdhs_final$household_members[bdhs_final$underweight == 1]
t_uw_hh <- t.test(uw_hh_size, normal_hh_size)
cat("t test: t =", t_uw_hh$statistic, ", p =", t_uw_hh$p.value, "\n")

cat("\n==================== OBJECTIVE 2C: NUMBER OF CHILDREN ====================\n")

# Test different children variables
cat("\n--- Comparing Different Children Variables ---\n")

# 1. Total children ever born
cat("\n1. Total children ever born:\n")
cat("Mean:", mean(bdhs_final$total_children_born), "\n")
t_tcn_mal <- t.test(bdhs_final$total_children_born, bdhs_final$any_malnutrition)
cat("t test: t =", t_tcn_mal$statistic, ", p =", t_tcn_mal$p.value, "\n")

# 2. Births in last 5 years
cat("\n2. Births in last 5 years:\n")
cat("Mean:", mean(bdhs_final$births_last5y), "\n")
t_b5_mal <- t.test(bdhs_final$births_last5y, bdhs_final$any_malnutrition)
cat("t test: t =", t_b5_mal$statistic, ", p =", t_b5_mal$p.value, "\n")

# 3. Living children
cat("\n3. Living children:\n")
cat("Mean:", mean(bdhs_final$living_children), "\n")
t_lc_mal <- t.test(bdhs_final$living_children, bdhs_final$any_malnutrition)
cat("t test: t =", t_lc_mal$statistic, ", p =", t_lc_mal$p.value, "\n")

# Similar results, but one's household condition may not be related to only births last 5 years, so use total children born

# T-tests for total children ever born
cat("\n--- T-tests: Children by Malnutrition Status ---\n")

# Stunting
stunted_children <- bdhs_final$total_children_born[bdhs_final$stunted == 1]
normal_children <- bdhs_final$total_children_born[bdhs_final$any_malnutrition == 0]
t_stunting_child <- t.test(stunted_children, normal_children)
cat("t test: t =", t_stunting_child$statistic, ", p =", t_stunting_child$p.value, "\n")

# Wasting
wasted_children <- bdhs_final$total_children_born[bdhs_final$wasted == 1]
t_wasting_child <- t.test(wasted_children, normal_children)
cat("t test: t =", t_wasting_child$statistic, ", p =", t_wasting_child$p.value, "\n")

# Underweight
uw_children <- bdhs_final$total_children_born[bdhs_final$underweight == 1]
t_uw_child <- t.test(uw_children, normal_children)
cat("t test: t =", t_uw_child$statistic, ", p =", t_uw_child$p.value, "\n")

# PART 4: OBJECTIVE 3 - WEALTH AND HOUSEHOLD COMPOSITION ASSOCIATION

cat("\n==================== OBJECTIVE 3: WEALTH-HOUSEHOLD ASSOCIATION ====================\n")

# Correlation analysis
cat("\n--- Correlations with Wealth ---\n")
cor_wealth_hh <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$household_members)
cat("Wealth and HH size: r =", cor_wealth_hh$estimate, ", p =", cor_wealth_hh$p.value, "\n")
cor_wealth_total_children <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$total_children_born)
cat("Wealth and total children born: r =", cor_wealth_total_children$estimate, ", p =", cor_wealth_total_children$p.value, "\n")
# Possible Colinearity - indicates to do VIF later

# PART 5: OBJECTIVE 4 - PARENT EDUCATION AND MALNUTRITION
cat("\n==================== OBJECTIVE 4: PARENT EDUCATION ====================\n")

# Descriptive statistics
cat("\n--- Education Distribution ---\n")
cat("Mother's education - Mean:", mean(bdhs_final$mother_edu_years), "years\n")
cat("Father's education - Mean:", mean(bdhs_final$father_edu_years), "years\n")
cat("Average parent education - Mean:", mean(bdhs_final$average_parent_edu), "years\n")

# Chi-square for education categories
cat("\n--- Chi-square Tests: Education Categories ---\n")
mother_edu_cat_test <- chisq.test(table(bdhs_final$any_malnutrition, bdhs_final$mother_edu_cat))
cat("Mother's education category: X² =", mother_edu_cat_test$statistic, 
    ", p =", mother_edu_cat_test$p.value, "\n")
father_edu_cat_test <- chisq.test(table(bdhs_final$any_malnutrition, bdhs_final$father_edu_cat))
cat("Father's education category: X² =", father_edu_cat_test$statistic, 
    ", p =", father_edu_cat_test$p.value, "\n")

# T-tests for education by malnutrition status
cat("\n--- T-tests: Average Education Years by Malnutrition Status ---\n")

# Average parent education
mal_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$any_malnutrition == 1]
normal_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$any_malnutrition == 0]
t_test_avg <- t.test(mal_avg_edu, normal_avg_edu)
cat("\nAverage parent education:\n")
cat("  Malnutrition:", mean(mal_avg_edu, na.rm=T), "years\n")
cat("  Normal:", mean(normal_avg_edu, na.rm=T), "years\n")
cat("  T-test: t =", t_test_avg$statistic, ", p =", t_test_avg$p.value, "\n")
# We will use average parent education years as the final one since parent's impact on malnutrition is basically same

# PART 6: URBAN vs RURAL DIFFERENCES

cat("\n==================== URBAN vs RURAL ====================\n")

# Basic distribution
cat("\n--- Residence Distribution ---\n")
residence_table <- table(bdhs_final$residence)
print(residence_table)
print(prop.table(residence_table) * 100)

# Malnutrition by residence
cat("\n--- Malnutrition by Residence ---\n")
malnutrition_residence <- table(bdhs_final$any_malnutrition, bdhs_final$residence)
cat("\nMalnutrition prevalence:\n")
print(prop.table(malnutrition_residence, 2) * 100)
malnutrition_res_test <- chisq.test(malnutrition_residence)
cat("Chi-square test: X² =", malnutrition_res_test$statistic, ", p =", malnutrition_res_test$p.value, "\n")

stunting_residence <- table(bdhs_final$stunted, bdhs_final$residence)
cat("\nStunting prevalence:\n")
print(prop.table(stunting_residence, 2) * 100)
stunting_res_test <- chisq.test(stunting_residence)
cat("Chi-square test: X² =", stunting_res_test$statistic, ", p =", stunting_res_test$p.value, "\n")

wasting_residence <- table(bdhs_final$wasted, bdhs_final$residence)
cat("\nWasting prevalence:\n")
print(prop.table(wasting_residence, 2) * 100)
wasting_res_test <- chisq.test(wasting_residence)
cat("Chi-square test: X² =", wasting_res_test$statistic, ", p =", wasting_res_test$p.value, "\n")

uw_residence <- table(bdhs_final$underweight, bdhs_final$residence)
cat("\nUnderweight prevalence:\n")
print(prop.table(uw_residence, 2) * 100)
uw_res_test <- chisq.test(uw_residence)
cat("Chi-square test: X² =", uw_res_test$statistic, ", p =", uw_res_test$p.value, "\n")

# It is only a little significant

# Draw all the plots in this analysis
source('scripts/2.1_plottings.R')
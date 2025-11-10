# 3. Statistical Modeling with Survey Weights

# PREPARE MODELING DATA

# Center continuous variables for better interpretation
bdhs_final$wealth_c <- scale(bdhs_final$wealth_urban_rural, center = TRUE, scale = FALSE)
bdhs_final$household_members_c <- scale(bdhs_final$household_members, center = TRUE, scale = FALSE)
bdhs_final$children_c <- scale(bdhs_final$total_children_born, center = TRUE, scale = FALSE)
bdhs_final$average_parent_edu_c <- scale(bdhs_final$average_parent_edu, center = TRUE, scale = FALSE)
bdhs_final$head_age_c <- scale(bdhs_final$head_age, center = TRUE, scale = FALSE)
write.csv(bdhs_final, "data/bdhs_final.csv", row.names = FALSE)

# PART A: BUILD MODELS BASED ON OBJECTIVES
cat("\n==================== MODEL BUILDING ====================\n")

# =========== STUNTING MODELS ===========

cat("\n--- STUNTING MODELS ---\n")
# Model 1: Wealth only
stunting_m1 <- glm(stunted ~ wealth_c, data = bdhs_final, family = binomial)
cat("\nModel 1 - Wealth only:\n")
print(summary(stunting_m1)$coefficients)

# Model 2: Household size only
stunting_m2 <- glm(stunted ~ household_members_c, data = bdhs_final, family = binomial)
cat("\nModel 2 - Household size only:\n")
print(summary(stunting_m2)$coefficients)

# Model 3: Number of children only
stunting_m3 <- glm(stunted ~ children_c, data = bdhs_final, family = binomial)
cat("\nModel 3 - Children number only:\n")
print(summary(stunting_m3)$coefficients)

# Model 4: Education only
stunting_m4 <- glm(stunted ~ average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 4 - Parent education only:\n")
print(summary(stunting_m4)$coefficients)

# Model 5: Head Age only
stunting_m5 <- glm(stunted ~ head_age_c, data = bdhs_final, family = binomial)
cat("\nModel 5 - Head Age only:\n")
print(summary(stunting_m5)$coefficients)

# Model 6: Wealth + Children
stunting_m6 <- glm(stunted ~ wealth_c + children_c, data = bdhs_final, family = binomial)
cat("\nModel 6 - Wealth + Children:\n")
print(summary(stunting_m6)$coefficients)

# Model 7: Wealth + Head Age
stunting_m7 <- glm(stunted ~ wealth_c + head_age_c, data = bdhs_final, family = binomial)
cat("\nModel 7 - Wealth + Head Age:\n")
print(summary(stunting_m7)$coefficients)

# Model 8: Wealth + Education
stunting_m8 <- glm(stunted ~ wealth_c + average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 8 - Wealth + Education:\n")
print(summary(stunting_m8)$coefficients)

# Model 9: Children + Education
stunting_m9 <- glm(stunted ~ children_c + average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 9 - Children + Education:\n")
print(summary(stunting_m9)$coefficients)

# Full model with all key variables
stunting_full <- glm(stunted ~ head_age_c + household_members_c + wealth_c + children_c + average_parent_edu_c, 
                        data = bdhs_final, family = binomial)
cat("\nFULL MODEL - All key variables:\n")
print(summary(stunting_full)$coefficients)

# Final model with controls
stunting_final <- glm(stunted ~ wealth_c + children_c + average_parent_edu_c, 
                         data = bdhs_final, family = binomial)
cat("\nFINAL MODEL - With controls:\n")
print(summary(stunting_final)$coefficients)
# Do VIF to check colinearality

# =========== WASTING MODELS ===========
cat("\n--- WASTING MODELS ---\n")

# Model 1: Wealth only
wasting_m1 <- glm(wasted ~ wealth_c, data = bdhs_final, family = binomial)
cat("\nModel 1 - Wealth only:\n")
print(summary(wasting_m1)$coefficients)

# Model 2: Household size only
wasting_m2 <- glm(wasted ~ household_members_c, data = bdhs_final, family = binomial)
cat("\nModel 2 - Household size only:\n")
print(summary(wasting_m2)$coefficients)

# Model 3: Number of children only
wasting_m3 <- glm(wasted ~ children_c, data = bdhs_final, family = binomial)
cat("\nModel 3 - Children Numbers only:\n")
print(summary(wasting_m3)$coefficients)

# Model 4: Education only
wasting_m4 <- glm(wasted ~ average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 4 - Parent education only:\n")
print(summary(wasting_m4)$coefficients)

# Model 5: Head Age only
wasting_m5 <- glm(wasted ~ head_age_c, data = bdhs_final, family = binomial)
cat("\nModel 5 - Head Age only:\n")
print(summary(wasting_m5)$coefficients)

# Model 6: Wealth + Children
wasting_m6 <- glm(wasted ~ wealth_c + children_c, data = bdhs_final, family = binomial)
cat("\nModel 6 - Wealth + Children:\n")
print(summary(wasting_m6)$coefficients)

# Model 7: Wealth + Head Age
wasting_m7 <- glm(wasted ~ wealth_c + head_age_c, data = bdhs_final, family = binomial)
cat("\nModel 7 - Wealth + Head Age:\n")
print(summary(wasting_m7)$coefficients)

# Model 8: Wealth + Education
wasting_m8 <- glm(wasted ~ wealth_c + average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 8 - Wealth + Education:\n")
print(summary(wasting_m8)$coefficients)

# Model 9: Children + Education
wasting_m9 <- glm(wasted ~ children_c + average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 9 - Children + Education:\n")
print(summary(wasting_m9)$coefficients)

# Full model with all key variables
wasting_full <- glm(wasted ~ head_age_c + household_members_c + wealth_c + children_c + average_parent_edu_c, 
                     data = bdhs_final, family = binomial)
cat("\nFULL MODEL - All key variables:\n")
print(summary(wasting_full)$coefficients)

# Not a single coefficient is significant, wasting is hard to build model

# =========== UNDERWEIGHT MODELS ===========
cat("\n--- UNDERWEIGHT MODELS ---\n")

# Model 1: Wealth only
underweight_m1 <- glm(underweight ~ wealth_c, data = bdhs_final, family = binomial)
cat("\nModel 1 - Wealth only:\n")
print(summary(underweight_m1)$coefficients)

# Model 2: Household size only
underweight_m2 <- glm(underweight ~ household_members_c, data = bdhs_final, family = binomial)
cat("\nModel 2 - Household size only:\n")
print(summary(underweight_m2)$coefficients)

# Model 3: Number of children only
underweight_m3 <- glm(underweight ~ children_c, data = bdhs_final, family = binomial)
cat("\nModel 3 - Children under 5 only:\n")
print(summary(underweight_m3)$coefficients)

# Model 4: Education only
underweight_m4 <- glm(underweight ~ average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 4 - Parent education only:\n")
print(summary(underweight_m4)$coefficients)

# Model 5: Head Age only
underweight_m5 <- glm(underweight ~ head_age_c, data = bdhs_final, family = binomial)
cat("\nModel 5 - Head Age only:\n")
print(summary(underweight_m5)$coefficients)

# Model 6: Wealth + Children
underweight_m6 <- glm(underweight ~ wealth_c + children_c, data = bdhs_final, family = binomial)
cat("\nModel 6 - Wealth + Children:\n")
print(summary(underweight_m6)$coefficients)

# Model 7: Wealth + Head Age
underweight_m7 <- glm(underweight ~ wealth_c + head_age_c, data = bdhs_final, family = binomial)
cat("\nModel 7 - Wealth + Head Age:\n")
print(summary(underweight_m7)$coefficients)

# Model 8: Wealth + Education
underweight_m8 <- glm(underweight ~ wealth_c + average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 8 - Wealth + Education:\n")
print(summary(underweight_m8)$coefficients)

# Model 9: Children + Education
underweight_m9 <- glm(underweight ~ children_c + average_parent_edu_c, data = bdhs_final, family = binomial)
cat("\nModel 9 - Children + Education:\n")
print(summary(underweight_m9)$coefficients)

# Full model with all key variables
underweight_full <- glm(underweight ~ head_age_c + household_members_c + wealth_c + children_c + average_parent_edu_c, 
                           data = bdhs_final, family = binomial)
cat("\nFULL MODEL - All key variables:\n")
print(summary(underweight_full)$coefficients)

# Final model with controls
underweight_final <- glm(underweight ~ wealth_c + children_c + average_parent_edu_c, 
                            data = bdhs_final, family = binomial)
cat("\nFINAL MODEL - With controls:\n")
print(summary(underweight_final)$coefficients)

# PART B: TESTING VARIABLE IMPORTANCE
cat("\n==================== VARIABLE IMPORTANCE ====================\n")

# Likelihood ratio tests for nested models
cat("\nLikelihood Ratio Tests - Adding variables sequentially:\n")

cat("\nFor Stunting Model:")

# Test 1: Does children number improve wealth model?
lr_test1 <- anova(stunting_m1, stunting_m6, test = "Chisq")
cat("\nAdding children to wealth model:\n")
print(lr_test1)

# Test 2: Does education improve wealth model?
lr_test2 <- anova(stunting_m1, stunting_m8, test = "Chisq")
cat("\nAdding education to wealth model:\n")
print(lr_test2)

# Test 3: Does education improve wealth+children model?
lr_test3 <- anova(stunting_m6, stunting_final, test = "Chisq")
cat("\nAdding education to wealth+children model:\n")
print(lr_test3)

# Test 4: Does children number improve wealth+education model?
lr_test4 <- anova(stunting_m7, stunting_final, test = "Chisq")
cat("\nAdding children number to wealth+education model:\n")
print(lr_test4)

cat("\nFor Underweight Model:")

# Test 1: Does children number improve wealth model?
lr_test1_uw <- anova(underweight_m1, underweight_m6, test = "Chisq")
cat("\nAdding children to wealth model:\n")
print(lr_test1_uw)

# Test 2: Does head age improve wealth model?
lr_test2_uw <- anova(underweight_m1, underweight_m7, test = "Chisq")
cat("\nAdding head age to wealth model:\n")
print(lr_test2_uw)

# Test 3: Does education improve wealth model?
lr_test3_uw <- anova(underweight_m1, underweight_m8, test = "Chisq")
cat("\nAdding education to wealth model:\n")
print(lr_test3_uw)

# Test 4: Does education improve wealth+children model?
lr_test4_uw <- anova(underweight_m6, underweight_final, test = "Chisq")
cat("\nAdding education to wealth+children model:\n")
print(lr_test4_uw)

# Test 5: Does children number improve wealth+education model?
lr_test5_uw <- anova(underweight_m8, underweight_final, test = "Chisq")
cat("\nAdding children number to wealth+education model:\n")
print(lr_test5_uw)

# ==================== VIF VALUES FOR ALL VARIABLES ====================

# VIF analysis for stunting final model
cat("\n=== VIF Analysis for Stunting Final Model ===\n")
stunting_vif <- vif(stunting_final)
print(stunting_vif)

# VIF analysis for underweight final model
cat("\n=== VIF Analysis for Underweight Final Model ===\n")
underweight_vif <- vif(underweight_final)
print(underweight_vif)

# ==================== AIC VALUES FOR ALL MODELS ====================
cat("\n==================== AIC VALUES ====================\n")

# Stunting models AIC
stunting_aic <- data.frame(
  Model = c("stunting_m1", "stunting_m2", "stunting_m3", "stunting_m4", "stunting_m5",
            "stunting_m6", "stunting_m7", "stunting_m8", "stunting_m9", 
            "stunting_full", "stunting_final"),
  AIC = c(AIC(stunting_m1), AIC(stunting_m2), AIC(stunting_m3), AIC(stunting_m4), AIC(stunting_m5),
          AIC(stunting_m6), AIC(stunting_m7), AIC(stunting_m8), AIC(stunting_m9),
          AIC(stunting_full), AIC(stunting_final))
)

cat("\n=== Stunting Models AIC ===\n")
print(stunting_aic)

# Underweight models AIC
underweight_aic <- data.frame(
  Model = c("underweight_m1", "underweight_m2", "underweight_m3", "underweight_m4", "underweight_m5",
            "underweight_m6", "underweight_m7", "underweight_m8", "underweight_m9",
            "underweight_full", "underweight_final"),
  AIC = c(AIC(underweight_m1), AIC(underweight_m2), AIC(underweight_m3), AIC(underweight_m4), AIC(underweight_m5),
          AIC(underweight_m6), AIC(underweight_m7), AIC(underweight_m8), AIC(underweight_m9),
          AIC(underweight_full), AIC(underweight_final))
)

cat("\n=== Underweight Models AIC ===\n")
print(underweight_aic)

# Final models for stunting and underweight is the same: wealth + education + children_numbers

# ==================== Plots ====================
if (!dir.exists("plots/model_building")) dir.create("plots/model_building", recursive = TRUE)
# Forest Plot
stunting_coef <- tidy(stunting_final, conf.int = TRUE, exponentiate = TRUE)
underweight_coef <- tidy(underweight_final, conf.int = TRUE, exponentiate = TRUE)

stunting_coef$model <- "Stunting"
underweight_coef$model <- "Underweight"
forest_data <- rbind(stunting_coef, underweight_coef)
forest_data <- forest_data[forest_data$term != "(Intercept)", ]  # Delete the intercept

p_forest <- ggplot(forest_data, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_log10() +
  labs(title = "Forest Plot: Odds Ratios for Final Models",
       x = "Odds Ratio (95% CI)", y = "Variables") +
  theme(legend.position = "top")
ggsave("plots/model_building/01_forest_plot_final_models.png", p_forest, width = 10, height = 6, dpi = 300)

# ROC Curves
# Stunting Model
stunting_models <- list(stunting_m1, stunting_m3, stunting_m4,
                        stunting_full, stunting_final)
stunting_names <- c("W", "CN", "Edu", "Full", "Final")

png("plots/model_building/02_roc_curves_comparison.png", width = 12, height = 6, units = "in", res = 300)
par(mfrow = c(1, 2))

# Stunting ROC
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "1 - Specificity", ylab = "Sensitivity",
     main = "ROC Curves - Stunting Models")
abline(0, 1, lty = 2, col = "gray")

colors <- rainbow(length(stunting_models))
stunting_auc <- numeric(length(stunting_models))

for(i in 1:length(stunting_models)) {
  roc_obj <- roc(stunting_models[[i]]$y, fitted(stunting_models[[i]]), quiet = TRUE)
  lines(1 - roc_obj$specificities, roc_obj$sensitivities, col = colors[i], lwd = 2)
  stunting_auc[i] <- auc(roc_obj)
}

stunting_legend <- paste0(stunting_names, " (AUC: ", round(stunting_auc, 3), ")")
legend("bottomright", legend = stunting_legend, col = colors, lwd = 2, cex = 0.6)

# Underweight ROC
underweight_models <- list(underweight_m1, underweight_m3, underweight_m4,
                           underweight_full, underweight_final)
underweight_names <- c("W", "CN", "Edu", "Full", "Final")
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1 - Specificity", ylab = "Sensitivity",
     main = "ROC Curves - Underweight Models")
abline(0, 1, lty = 2, col = "gray")

underweight_auc <- numeric(length(underweight_models))

for(i in 1:length(underweight_models)) {
  roc_obj <- roc(underweight_models[[i]]$y, fitted(underweight_models[[i]]), quiet = TRUE)
  lines(1 - roc_obj$specificities, roc_obj$sensitivities, col = colors[i], lwd = 2)
  underweight_auc[i] <- auc(roc_obj)
}
underweight_legend <- paste0(stunting_names, " (AUC: ", round(underweight_auc, 3), ")")
legend("bottomright", legend = underweight_legend, col = colors, lwd = 2, cex = 0.6)

dev.off()

# AIC Line Chart
stunting_aic$order <- 1:nrow(stunting_aic)
stunting_aic$outcome <- "Stunting"

underweight_aic$order <- 1:nrow(underweight_aic)
underweight_aic$outcome <- "Underweight"

aic_combined <- rbind(stunting_aic, underweight_aic)

p_aic <- ggplot(aic_combined, aes(x = order, y = AIC, color = outcome, group = outcome)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:11, 
                     labels = c("W", "HH", "CN", "Edu", "HAge", 
                                "W+CN", "W+HAge", "W+Edu", "CN+Edu", 
                                "Full", "Final")) +
  labs(title = "AIC Values Line Chart",
       x = "Model", y = "AIC Value", color = "Outcome") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/model_building/03_aic_line_chart.png", p_aic, width = 10, height = 6, dpi = 300)

# Load libraries
library(ggplot2)
library(lme4)
library(lmerTest)
library(performance)
library(jtools)
# ==============================
# Load data
# ==============================
merged_milk_df <- read.csv("/home/rajesh/work/ketosis_project/files/processed_files/merged_milk_df.csv")

# ==============================

merged_milk_df$ketotic <- ifelse(merged_milk_df$Serum.BHB..mmol.L. > 1, 1, 0)

# 1. Fit linear mixed effect model for milk BHB vs breath acetone 
# ==============================
model <- lmer(`Milk.BHB..mM.L.` ~ `Breath.Acetone..ppm.` + (1 | cowid), 
              data = merged_milk_df)

summary(model)

# ==============================
# Extract R² and p-value
# ==============================
# R-squared (marginal and conditional)
r2_vals <- r2(model)

# p-value for fixed effect (Breath.Acetone)
p_val <- summary(model)$coefficients["Breath.Acetone..ppm.", "Pr(>|t|)"]

# Round values
r2_marg <- round(r2_vals$R2_marginal, 3)
r2_cond <- round(r2_vals$R2_conditional, 3)
p_val_rounded <- signif(p_val, 3)

coefs<-fixef(model)
intercept<-round(coefs[1],5)
slope <-round(coefs[2],5)
eq_text <-paste0('y=', slope, 'x+', intercept)
# Create annotation text
annot_text <- paste0("Marginal R² = ", r2_marg, 
                     "\nConditional R² = ", r2_cond, 
                     "\nP-value = ", p_val_rounded)

# ==============================
# Plot data with regression line & annotation
# ==============================
ggplot(merged_milk_df, aes(x = Breath.Acetone..ppm., y = Milk.BHB..mM.L.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Milk BHB vs Breath Acetone",
    x = "Breath Acetone (ppm)",
    y = "Milk BHB (mM/L)"
  ) +
  theme_classic() +
  annotate("text", 
           x = max(merged_milk_df$Breath.Acetone..ppm., na.rm = TRUE) * 0.8, 
           y = max(merged_milk_df$Milk.BHB..mM.L., na.rm = TRUE) * 0.9, 
           label = annot_text, hjust = 0)+
  annotate('text',
           x=max(merged_milk_df$Breath.Acetone..ppm., na.rm=TRUE)*0.7,
           y=max(merged_milk_df$Milk.BHB..mM.L., na.rm = TRUE)*0.8,
           label= eq_text, hjust=0, fontface='bold')


#================================================================================================
  ## 2.  Fitting breath acetone vs serum bhba concentration 
#===============================================================================================
  
  model <- lmer(`Serum.BHB..mmol.L.` ~ `Breath.Acetone..ppm.` + (1 | cowid), 
                data = merged_milk_df)

summary(model)

# ==============================
# Extract R² and p-value
# ==============================
# R-squared (marginal and conditional)
r2_vals <- r2(model)

# p-value for fixed effect (Breath.Acetone)
p_val <- summary(model)$coefficients["Breath.Acetone..ppm.", "Pr(>|t|)"]

# Round values
r2_marg <- round(r2_vals$R2_marginal, 3)
r2_cond <- round(r2_vals$R2_conditional, 3)
p_val_rounded <- signif(p_val, 3)

# Create annotation text
annot_text <- paste0("Marginal R² = ", r2_marg, 
                     "\nConditional R² = ", r2_cond, 
                     "\nP-value = ", p_val_rounded)
coefs<-fixef(model)
intercept<-round(coefs[1],5)
slope <-round(coefs[2],5)
eq_text <-paste0('y=', slope, 'x+', intercept)
# ==============================
# Plot data with regression line & annotation
# ==============================
ggplot(merged_milk_df, aes(x = Breath.Acetone..ppm., y = Serum.BHB..mmol.L.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Serum BHB vs Breath Acetone",
    x = "Breath Acetone (ppm)",
    y = "serum BHB (mM/L)"
  ) +
  theme_classic() +
  annotate("text", 
           x = max(merged_milk_df$Breath.Acetone..ppm., na.rm = TRUE) * 0.8, 
           y = max(merged_milk_df$Serum.BHB..mmol.L., na.rm = TRUE) * 0.9, 
           label = annot_text, hjust = 0)+
annotate('text',
         x=max(merged_milk_df$Breath.Acetone..ppm., na.rm=TRUE)*0.7,
         y=max(merged_milk_df$Serum.BHB..mmol.L., na.rm = TRUE)*0.8,
         label= eq_text, hjust=0, fontface='bold')



#================================================================================================
 # 

#=======================================================================================================
# 3. Milk acetone vs Breath acetone
#=======================================================================================================
library(lme4)
library(performance)   # for r2()
library(ggplot2)

# Fit model
model_acetone <- lmer(`Milk.Acetone..mM.L.` ~ `Breath.Acetone..ppm.` + (1 | cowid),
                      data = merged_milk_df)

# Extract fixed effects (intercept & slope)
coefs <- fixef(model_acetone)
intercept <- round(coefs[1], 5)
slope <- round(coefs[2], 5)

# R2 values
r2_vals <- r2(model_acetone)
r2_marg <- round(r2_vals$R2_marginal, 3)
r2_cond <- round(r2_vals$R2_conditional, 3)

# p-value for slope
p_val <- summary(model_acetone)$coefficients["Breath.Acetone..ppm.", "Pr(>|t|)"]
p_val_rounded <- signif(p_val, 3)

# Annotation texts
annot_text <- paste0("Marginal R² = ", r2_marg, 
                     "\nConditional R² = ", r2_cond, 
                     "\nP-value = ", p_val_rounded)

eq_text <- paste0("y = ", slope, "x + ", intercept)

# ==============================
# Plot
# ==============================
ggplot(merged_milk_df, aes(x = Breath.Acetone..ppm., y = Milk.Acetone..mM.L.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Milk acetone vs Breath Acetone",
    x = "Breath Acetone (ppm)",
    y = "Milk Acetone (mM/L)"
  ) +
  theme_classic() +
  annotate("text", 
           x = max(merged_milk_df$Breath.Acetone..ppm., na.rm = TRUE) * 0.7, 
           y = max(merged_milk_df$Milk.Acetone..mM.L., na.rm = TRUE) * 0.9, 
           label = annot_text, hjust = 0) +
  annotate("text",
           x = max(merged_milk_df$Breath.Acetone..ppm., na.rm = TRUE) * 0.7,
           y = max(merged_milk_df$Milk.Acetone..mM.L., na.rm = TRUE) * 0.6,
           label = eq_text, hjust = -0, fontface = "bold")

#====================================================================================================
 # 4. NEFA blood 
# ========================================================================================
library(lme4)
library(performance)   # for r2()
library(ggplot2)

# Fit model
model_nefa <- lmer(`Serum.NEFA..meEq.L.` ~ `Breath.Acetone..ppm.` + (1 | cowid),
                      data = merged_milk_df)

# Extract fixed effects (intercept & slope)
coefs <- fixef(model_nefa)
intercept <- round(coefs[1], 5)
slope <- round(coefs[2], 5)

# R2 values
r2_vals <- r2(model_nefa)
r2_marg <- round(r2_vals$R2_marginal, 3)
r2_cond <- round(r2_vals$R2_conditional, 3)

# p-value for slope
p_val <- summary(model_nefa)$coefficients["Breath.Acetone..ppm.", "Pr(>|t|)"]

# Round to 3 decimals
p_val_rounded <- round(p_val, 3)

# If rounded value is 0, make it "<0.001"
if (p_val_rounded == 0) {
  p_val_rounded <- "<0.001"
} else {
  p_val_rounded <- format(round(p_val, 3), nsmall = 3)  # keeps 3 decimal places
}
# Annotation texts
annot_text <- paste0("Marginal R² = ", r2_marg, 
                     "\nConditional R² = ", r2_cond, 
                     "\nP-value  ", p_val_rounded)

eq_text <- paste0("y = ", slope, "x + ", intercept)

ggplot(merged_milk_df, aes(x = Breath.Acetone..ppm., y = Serum.NEFA..meEq.L.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Serum NEFA vs Breath Acetone",
    x = "Breath Acetone (ppm)",
    y = "Serum NEFA meEq/L."
  ) +
  theme_classic() +
  annotate("text", 
           x = max(merged_milk_df$Breath.Acetone..ppm., na.rm = TRUE) * 0.7, 
           y = max(merged_milk_df$Serum.NEFA..meEq.L., na.rm = TRUE) * 0.9, 
           label = annot_text, hjust = 0) +
  annotate("text",
           x = max(merged_milk_df$Breath.Acetone..ppm., na.rm = TRUE) * 0.7,
           y = max(merged_milk_df$Serum.NEFA..meEq.L., na.rm = TRUE) * 0.8,
           label = eq_text, hjust = 0, fontface = "bold") 


#==================================================================================
  
library(pROC)
library(ggplot2)


# Ensure 'ketotic' is binary factor (0 = not ketotic, 1 = ketotic)
merged_milk_df$ketotic <- as.factor(merged_milk_df$ketotic)

roc_obj <- roc(response = merged_milk_df$ketotic,
               predictor = merged_milk_df$Breath.Acetone..ppm.,
               levels = c("0", "1"),    # 0 = non-ketotic, 1 = ketotic
               direction = "<")         # change to ">" if inverted

# ================================
# 4. AUC
# ================================
auc_val <- auc(roc_obj)
print(paste("AUC:", auc_val))
# Best threshold (Youden's J) -> force as data.frame
best_coords <- coords(roc_obj, "best",
                      ret = c("threshold", "sensitivity", "specificity", "accuracy"),
                      transpose = FALSE)

# Convert to data.frame in case multiple rows
best_coords <- as.data.frame(best_coords)

# Take the first row if multiple thresholds tie
best_thresh <- best_coords$threshold[1]
best_sens   <- best_coords$sensitivity[1]
best_spec   <- best_coords$specificity[1]
best_acc    <- best_coords$accuracy[1]

print(best_coords)   # all best thresholds (if ties)
print(paste("Chosen threshold:", best_thresh))

# Now plot with ggplot
roc_df <- data.frame(
  tpr = rev(roc_obj$sensitivities),
  fpr = rev(1 - roc_obj$specificities)
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  # add point with annotate() since it's a single value
  annotate("point", x = 1 - best_spec, y = best_sens,
           color = "darkgreen", size = 3) +
  labs(title = paste("ROC Curve for Breath Acetone (AUC =", round(auc_val, 3), ")"),
       subtitle = paste("Best threshold =", round(best_thresh, 2),
                        "| Sens =", round(best_sens, 2),
                        "| Spec =", round(best_spec, 2)),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal(base_size = 14)





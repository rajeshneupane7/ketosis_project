# Load libraries
library(ggplot2)
library(lme4)
library(lmerTest)
library(performance)

# ==============================
# Load data
# ==============================
merged_milk_df <- read.csv("/home/rajesh/work/ketosis_project/files/processed_files/merged_milk_df.csv")

# ==============================
# Fit linear mixed effect model
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
           label = annot_text, hjust = 0)

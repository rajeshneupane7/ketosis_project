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

merged_milk_df$ketotic <- ifelse(merged_milk_df$Serum.BHB..mmol.L. > 0.8, 1, 0)



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



## plotting descriptive data
# Prepare data
plot_df <- merged_milk_df %>%
  select(
    ketotic,
    Breath.Acetone..ppm.,
    Milk.Acetone..mM.L.,
    Milk.BHB..mM.L.,
    Serum.NEFA..meEq.L.
  ) %>%
  pivot_longer(
    cols = -ketotic,
    names_to = "Biomarker",
    values_to = "Value"
  )

# Plot
ggplot(plot_df, aes(x = factor(ketotic), y = Value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  facet_wrap(
    ~ Biomarker,
    scales = "free_y",
    labeller = labeller(
      Biomarker = c(
        `Breath.Acetone..ppm.` = "Breath acetone (ppm)",
        `Milk.Acetone..mM.L.`  = "Milk acetone (mM)",
        `Milk.BHB..mM.L.`      = "Milk BHB (mM)",
        `Serum.NEFA..meEq.L.`  = "Serum NEFA (mEq/L)"
      )
    )
  ) +
  scale_x_discrete(
    labels = c("0" = "Subclinical negative", "1" = "Subclinical positive")
  ) +
  labs(
    x = "Subclinical ketosis status",
    y = "Concentration",
    title = "Biomarker distributions by subclinical ketosis status"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )


###Counting unique cow id with the ketotic thing
library(dplyr)

merged_milk_df %>%
  group_by(ketotic) %>%
  summarise(n_unique_cows = n_distinct(cowid))



################################
# ============================================================
# 0. Libraries
# ============================================================
library(lme4)
library(performance)   # r2()
library(ggplot2)
library(patchwork)

# ============================================================
# 1. Global theme (bigger fonts)
# ============================================================
big_theme <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text  = element_text(size = 10)
  )

# ============================================================
# 2. Function to fit LMM + return ggplot
# ============================================================
make_lmm_plot <- function(df, x, y, ylab, title) {
  
  # Mixed model
  form <- as.formula(paste0("`", y, "` ~ `", x, "` + (1 | cowid)"))
  model <- lmer(form, data = df)
  
  # Fixed effects
  coefs <- fixef(model)
  intercept <- round(coefs[1], 4)
  slope <- round(coefs[2], 4)
  
  # R²
  r2_vals <- r2(model)
  r2_marg <- round(r2_vals$R2_marginal, 3)
  r2_cond <- round(r2_vals$R2_conditional, 3)
  
  # P-value
  p_val <- summary(model)$coefficients[x, "Pr(>|t|)"]
  p_txt <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
  
  # Annotations
  annot_text <- paste0(
    "Marginal R² = ", r2_marg,
    "\nConditional R² = ", r2_cond,
    "\nP-value = ", p_txt
  )
  
  eq_text <- paste0("y = ", slope, "x + ", intercept)
  
  # Plot
  ggplot(df, aes_string(x = x, y = y)) +
    geom_point(alpha = 0.4, size = 1) +
    geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 1) +
    labs(
      title = title,
      x = "Breath Acetone (ppm)",
      y = ylab
    ) +
    annotate(
      "text",
      x = max(df[[x]], na.rm = TRUE) * 0.65,
      y = max(df[[y]], na.rm = TRUE) * 0.80,
      label = annot_text,
      size = 5,
      hjust = 0
    ) +
    annotate(
      "text",
      x = max(df[[x]], na.rm = TRUE) * 0.65,
      y = max(df[[y]], na.rm = TRUE) * 0.45,
      label = eq_text,
      size = 5,
      hjust = 0,
      fontface = "bold"
    ) +
    big_theme
}

# ============================================================
# 3. Create individual plots
# ============================================================
p1 <- make_lmm_plot(
  merged_milk_df,
  x = "Breath.Acetone..ppm.",
  y = "Milk.BHB..mM.L.",
  ylab = "Milk BHB (mM/L)",
  title = "Milk BHB vs Breath Acetone"
)

p2 <- make_lmm_plot(
  merged_milk_df,
  x = "Breath.Acetone..ppm.",
  y = "Serum.BHB..mmol.L.",
  ylab = "Serum BHB (mM/L)",
  title = "Serum BHB vs Breath Acetone"
)

p3 <- make_lmm_plot(
  merged_milk_df,
  x = "Breath.Acetone..ppm.",
  y = "Milk.Acetone..mM.L.",
  ylab = "Milk Acetone (mM/L)",
  title = "Milk Acetone vs Breath Acetone"
)

p4 <- make_lmm_plot(
  merged_milk_df,
  x = "Breath.Acetone..ppm.",
  y = "Serum.NEFA..meEq.L.",
  ylab = "Serum NEFA (mEq/L)",
  title = "Serum NEFA vs Breath Acetone"
)

# ============================================================
# 4. Combine into 2×2 panel with tags (A–D)
# ============================================================
final_plot <- (p1 | p2) /
  (p3 | p4) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 20, face = "bold")
    )
  )

# ============================================================
# 5. Print
# ============================================================
final_plot


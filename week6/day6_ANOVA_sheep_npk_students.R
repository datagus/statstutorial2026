#==================================================================
#                     🐑 ANOVA Exercise — Part 1
#             What drives weight gain in sheep?
#==================================================================

# BACKGROUND -------------------------------------------------------
#
#   A farmer measured weight GAIN in sheep raised on different diets
#   and nutritional supplements.
#   Two factors were manipulated:
#
#     🌾  DIET        – barley, oats, or wheat
#     💊  SUPPLEMENT  – agrimore, control, supergain, or supersupp
#
#   Your job: run a two-way ANOVA to identify which factors matter
#   and find out what combination you would recommend!
#
# ------------------------------------------------------------------

# YOUR TASK --------------------------------------------------------
#
#   Run a two-way ANOVA with gain as the response variable
#   and diet × supplement as predictors.
#
#   Remember:
#   1. Check the preconditions to run the ANOVA
#   2. Fit a full model (with interaction) and a reduced model
#      (main effects only) depending on what's significant
#   3. Check the residuals of your final model
#   4. Run a post-hoc test to identify which groups differ
#   5. Answer the questions at the end based on the output
#
# Note: You are guided via pseudocode; only code you haven't seen
#       before is written out explicitly.
#
# ------------------------------------------------------------------

# YOUR CODE GOES HERE =============================================

# -> 1. Load the data and attach
shaun <- read.table("growth.txt", header = TRUE)

# -> 2. Inspect the data


# -> 3. Convert categorical variables to factors


# -> 4. Precondition 1 (normality): Is the response variable normally distributed?


# -> 5. Precondition 2 (balanced design): Are group sizes equal?


# -> 6. Precondition 3 (homoscedasticity): Are within-group variances similar?

# Add las=2 and cex.axis=0.6 as arguments to make x-axis labels readable
boxplot(gain ~ diet:supplement, las = 2, cex.axis = 0.6, xlab = "")

# What happens if you switch diet and supplement in the boxplot?

# -> 7. Fit the full two-way ANOVA (with interaction term)


# -> 8. Is the interaction significant? If not, fit the additive (main effects) model


# -> 9. Check residuals of the final model


# -> 10. Run a post-hoc test to find out which groups differ significantly


# -> 11. Visualise the effect of each factor separately


# ❓ QUESTIONS — Part 1 -------------------------------------------
#
#   1. Is the interaction between diet and supplement significant?
#
#      A)  Yes  – p < 0.05
#      B)  No   – p = 0.917
#      C)  Yes  – p < 0.001
#      D)  No   – p = 0.325
#
# -----------------------------------------------------------------
#
#   2. Which diet leads to the highest mean weight gain?
#
#      A)  wheat
#      B)  oats
#      C)  barley
#      D)  All diets produce the same gain
#
# -----------------------------------------------------------------
#
#   3. Based on TukeyHSD, which supplement pair is NOT
#      significantly different from each other?
#
#      A)  agrimore vs. control
#      B)  agrimore vs. supergain
#      C)  agrimore vs. supersupp
#      D)  supersupp vs. supergain
#
# -----------------------------------------------------------------
#
#   4. In the reduced model, how much of the total variation in gain is 
#      explained by diet and supplement together?
#
#      A)  23.4 %
#      B)  56.1 %
#      C)  85.3 %
#      D)  14.7 %
#
# -----------------------------------------------------------------




#==================================================================
#                     🌿 ANOVA Exercise — Part 2
#             What nutrients drive pea yield?
#==================================================================

# BACKGROUND -------------------------------------------------------
#
#   The built-in R dataset `npk` records the yield of peas grown
#   in 6 experimental blocks. Three nutrients were applied (or not):
#
#     🧪  N  – Nitrogen
#     🧪  P  – Phosphate
#     🧪  K  – Potassium
#
#   Plots were arranged in BLOCKS to control for field variability.
#   Your job: find the MINIMUM ADEQUATE MODEL — the simplest model
#   that retains only significant terms.
#
# ------------------------------------------------------------------

# YOUR TASK --------------------------------------------------------
#
#   Fit a series of three-way ANOVA models, progressively removing
#   non-significant terms, until all remaining terms are significant.
#   Include block as an error (blocking) term throughout.
#
#   Remember:
#   1. Check the preconditions to run the ANOVA
#   2. Start with the full model; inspect each term's p-value
#   3. Remove the least significant term one step at a time
#   4. Stop when all remaining terms are significant
#   5. Answer the questions at the end based on the output
#
# ------------------------------------------------------------------

# YOUR CODE GOES HERE =============================================

# -> 1. Load the built-in dataset and attach
data("npk")
attach(npk)
?npk

# -> 2. Inspect the data 


# -> 3. Precondition 1 (normality): Is yield normally distributed?

# -> 4. Precondition 2 (balanced design): Are group sizes equal?

# -> 5. Precondition 3 (homoscedasticity): Are within-group variances similar?

# Visualise yield by each factor and by block, separately in 4 plots
par(mfrow = c(2, 2))
par(mar = c(2, 2, 1, 1))
# Nitrogen
boxplot(yield ~ N, main = "Nitrogen")
# Phosphate

# Potassium

# Block

graphics.off()

# Visualise yield by combinations of N, P, K


# -> 6. Fit the full model without block


# -> 7. Fit the full model including block as an error term
model2 <- aov(yield ~ N * P * K + Error(block))
summary(model2)

# -> 8. Remove the three-way interaction (N:P:K) — not significant
model3 <- aov(yield ~ N + P + K + N:P + N:K + P:K + Error(block))
summary(model3)

# -> 9. Remove the least significant two-way interaction


# -> 10. Continue removing non-significant terms until you have a 
# minimum adequate model (only significant terms)



# ❓ QUESTIONS — Part 2 -------------------------------------------
#
#   1. Which factors are significant in the minimum adequate model?
#
#      A)  N and P only
#      B)  N, P, and K
#      C)  N and K only
#      D)  K only
#
# -----------------------------------------------------------------
#
#   2. What is the purpose of including Error(block) in the formula?
#
#      A)  It removes random variation within treatment groups
#      B)  It accounts for systematic differences between blocks
#      C)  It models the interaction between treatments and yield
#      D)  It flags missing values in the dataset
#
# -----------------------------------------------------------------
#
#   3. Compared to the full model with Error(block),
#      what changes when block is not included?
#
#      A)  The degrees of freedom for residuals are lower when block is 
#          not included
#      B)  The block effect is absorbed into residuals when block is not 
#          included, inflating unexplained variance  # correct
#      C)  The F-values for N, P, K are higher when block is not included
#      D)  Nothing changes — block has no effect here
#
# -----------------------------------------------------------------


# --- Content by HvW team. AI was used to adapt the design of the script. ---

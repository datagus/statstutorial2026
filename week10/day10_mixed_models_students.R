#==================================================================
#
#        🌾  Mixed-Effect Models — Week 10
#     Can field-level decisions predict crop yield?
#
#==================================================================

# BACKGROUND -------------------------------------------------------
#
#   You are working at a sustainable agriculture consultancy. In the
#   past, you analysed a split-plot field experiment using ANOVA with
#   Error(block). Today you revisit the same data with a mixed-effect
#   model — a more flexible approach that explicitly estimates how much
#   yield variation comes from field-to-field differences versus the
#   treatments applied.
#
#   The experiment ran across 4 field blocks. Each block was divided
#   into irrigation strips, each strip sub-divided for planting
#   density, and each sub-plot received one of three fertilizer types.
#   In a mixed-effect model, block and its nested sub-divisions become
#   random effects — they account for the layered field structure
#   without absorbing degrees of freedom from the fixed treatment terms.
#
#   Variables in the dataset:
#
#     🏗️  block       – one of 4 field sections (random effect)
#     💧  irrigation  – irrigated vs non-irrigated (fixed effect)
#     🌱  density     – low vs high planting density (fixed effect)
#     🧪  fertilizer  – fertilizer type: N, P, or NP (fixed effect)
#     📦  yield       – grain yield per plot (mg)  ← response
#
#   Your job: fit a mixed-effect model that separates random field
#   variation from the fixed treatment effects, then simplify it to
#   the minimum adequate model and check that it fits the data well.
#
# ------------------------------------------------------------------

# YOUR TASK --------------------------------------------------------
#
#   1. Load packages and data, convert to factors, and inspect the design.
#   2. Check preconditions: normality and homoscedasticity.
#   3. Fit the full mixed-effect model with nested random effects.
#   4. Remove the three-way interaction.
#   5. Find the minimum adequate model.
#   6. Check model residuals.
#   7. Inspect fitted values at each random-effect level.
#
# ------------------------------------------------------------------


# -> 1. Load packages and data, convert to factors, attach and inspect
# Note: lme() from the nlme package fits linear mixed-effect models.
#       multcomp provides post-hoc tests if needed.
# install.packages("nlme")
# install.packages("multcomp")
library(nlme)
library(multcomp)

yields <- read.table("splityield.txt", header = TRUE)

# Note: Convert to factor before modeling — otherwise lme() treats
#       these as continuous and gives wrong results.
yields$block      <- as.factor(yields$block)
yields$irrigation <- as.factor(yields$irrigation)
yields$density    <- as.factor(yields$density)
yields$fertilizer <- as.factor(yields$fertilizer)

attach(yields)


# -> 2. Verify the design and visualize its structure
ftable(block ~ irrigation + density + fertilizer)
ftable(fertilizer ~ irrigation + density)

mosaicplot(~ block + irrigation + density + fertilizer,
           data     = yields,
           color    = c("brown", "orange", "yellow"),
           cex.axis = 0.6,
           las      = 0,
           main     = "Split-plot experiment design")


# -> 3. Health check up: How is our data behaving? Explore the distribution of yield and check for outliers


# -> 4. Fit the full mixed-effect model
# Note: ~1 | block/irrigation/density tells lme() to estimate a
#       separate intercept at each level of the nested hierarchy.
model1 <- lme(yield ~ irrigation * density * fertilizer,
              random = ~1 | block / irrigation / density)
summary(model1)
anova(model1)


# -> 5. Remove the three-way interaction
model2 <-
anova(model2)


# -> 6. Find the minimum adequate model
model3 <-
anova(model3)


# -> 7. Check model residuals
# Hint: Use hist() on model3$residuals, then plot(model3) for the
#       residuals-vs-fitted plot.


# -> 8. Inspect fitted values at each random-effect level
# Note: model3$fitted returns a matrix — one column per level of the
#       random-effect hierarchy.
#       Column 1 = population (fixed effects only)
#       Column 2 = block level
#       Column 3 = irrigation level
#       Column 4 = density level (most conditional — all random effects included)
#       The red dashed line (slope = 1, intercept = 0) marks perfect prediction.
#       Points above the line = model underpredicts; below = overpredicts.
model3$fitted

plot(model3$fitted[, 2], yield,
     xlab = "Fitted (block level)",
     ylab = "Observed yield",
     main = "Block-level fit vs observed")
abline(a = 0, b = 1, col = "red", lty = 2)

plot(model3$fitted[, 3], yield,
     xlab = "Fitted (irrigation level)",
     ylab = "Observed yield",
     main = "Irrigation-level fit vs observed")
abline(a = 0, b = 1, col = "red", lty = 2)

plot(model3$fitted[, 4], yield,
     xlab = "Fitted (density level)",
     ylab = "Observed yield",
     main = "Density-level fit vs observed")
abline(a = 0, b = 1, col = "red", lty = 2)


# ❓ QUESTIONS — Mixed-effect models: splityield ------------------
#
#   1. Which of the following is the minimum adequate model for
#      crop yield in this mixed-effect analysis?
#
#      A)  lme(yield ~ irrigation * density * fertilizer,
#              random = ~1 | block / irrigation / density)
#
#      B)  lme(yield ~ (irrigation + density + fertilizer)^2,
#              random = ~1 | block / irrigation / density)
#
#      C)  lme(yield ~ irrigation * density + irrigation * fertilizer,
#              random = ~1 | block / irrigation / density)
#
#      D)  lme(yield ~ irrigation + density + fertilizer,
#              random = ~1 | block / irrigation / density)
#
#
#   2. Based on anova(model2), which two-way interaction is not
#      significant and should be removed to reach the minimum
#      adequate model?
#
#      A)  irrigation:density
#      B)  irrigation:fertilizer
#      C)  density:fertilizer
#      D)  All two-way interactions are significant
#
#
#   3. Based on plot(model3), what does a well-fitting model's
#      residuals-vs-fitted plot look like?
#
#      A)  Residuals fan out at higher fitted values, indicating
#          unequal spread
#      B)  Residuals follow a curve, suggesting a non-linear
#          relationship was missed
#      C)  Residuals scatter randomly around zero with roughly
#          constant spread — model assumptions look reasonable
#      D)  Residuals cluster into two groups, one above and one
#          below zero
#
#
#   4. model3$fitted returns a matrix with one column per level
#      of the random-effect hierarchy. Which column gives the most
#      precise predictions — i.e., accounts for all random effects?
#
#      A)  Column 1 — population-level predictions (fixed effects only)
#      B)  Column 2 — block-level predictions
#      C)  Column 3 — irrigation-level predictions
#      D)  Column 4 — density-level predictions (all random effects
#          included)
#
#
#   5. Is model3 the minimum adequate model, given that the main
#      effect of density is not significant at the 0.05 level
#      (p = 0.0532 in anova(model3))?
#
#      A)  No — density should be removed since p = 0.0532
#          exceeds the 0.05 threshold
#      B)  Yes — model3 is the minimum adequate model, but only
#          because p = 0.0532 is close enough to treat as significant
#      C)  No — both density and irrigation:density should be removed,
#          since a non-significant main effect makes its interaction
#          unreliable
#      D)  Yes — density cannot be removed while its interaction
#          with irrigation remains significant. A main effect that
#          is part of a significant interaction must stay in the model
#
# -----------------------------------------------------------------

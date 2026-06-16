#==================================================================
#
#     Model reduction — Week 11
#     Can we find the minimum adequate model for species diversity (varespec)
#     and crop yield (npk)?
#
#
#     AI Disclaimer: The overall format of this script was done by Claude AI.
#     The content is originally human. The script was revised by the Henrik von Wehrden
#==================================================================
# BACKGROUND -------------------------------------------------------
#
#   Example one is a classical dataset from ecology, find more about it in the library vegan
#
#   Part 1 — Linear model reduction (varespec / varechem)
#   -------------------------------------------------------
#   The  varespec (species percentage cover across 24 plots) and 
#		varechem (14 soil chemistry measurements for the same plots). 
#		Convert the data in presence abesence data and calculate the 
#		Number of species per plot. The we will do
#   (a) statistical fishing — loop over all 14 predictors one at a
#       time to identify candidates, then
#   (b) manual backward elimination guided by p-values, cross-checked
#       with AIC and the step() function.
#
#   Part 2 — Mixed-effect model reduction (npk)
#   -------------------------------------------------------
#   The npk dataset records a factorial fertilizer experiment (N, P, K)
#   run across 6 blocks. Because plots within a block share unmeasured
#   environmental conditions, block is treated as a random effect.
#   Models are compared using AIC and reudced by p-values— yet to make 
#		AIC comparable across models with different fixed effects, they must
#		all be fitted with method = "ML" rather than the default REML.
#
#   Variables in varespec / varechem:
#
#     🌿  varespec   – species percentage cover (24 plots × 44 species)
#     🧪  varechem   – 14 soil chemistry variables for the same 24 plots
#     📦  varediv    – derived species richness per plot  ← response (Part 1)
#
#   Variables in npk:
#
#     🏗️  block      – one of 6 field blocks (random effect)
#     🧪  N          – nitrogen applied (0/1, fixed effect)
#     🧪  P          – phosphate applied (0/1, fixed effect)
#     🧪  K          – potassium applied (0/1, fixed effect)
#     📦  yield      – crop yield per plot  ← response (Part 2)
#
#   Your job: find the minimum adequate model in both settings using
#   p-value-based backward elimination, the step() shortcut, and AIC.
# 	Compare the outcomes
#
# ------------------------------------------------------------------
# YOUR TASK --------------------------------------------------------
#
#   Part 1 — Species richness (linear model)
#   1. Load packages and data; derive species richness from cover data.
#   2. Check for multicollinearity among predictors.
#   3. Loop over all predictors to identify significant candidates.
#   4. Fit the full model with the significant predictors.
#   5. Check for redundancy using VIF.
#   6. Reduce the model step-by-step using p-values.
#   7. Use step() as an automated alternative.
#   8. Compare all candidate models with AIC.
#
#   Part 2 — Crop yield (mixed-effect model)
#   9.  Load data, convert to factors, fit the full mixed-effect model.
#   10. Reduce the model step-by-step by removing non-significant terms.
#   11. Compare against a classical ANOVA with Error(block).
#   12. Compare all candidate models with AIC.
#
#   # Note: comments explain non-obvious choices throughout.
# ------------------------------------------------------------------


# ── PART 1: Species richness ─────────────────────────────────────


# -> 1. Load packages and data; derive species richness
# Note: vegan ships two paired objects — varespec (species matrix)
#       and varechem (soil chemistry). They are loaded automatically
#       with data(varespec); varechem comes along for free.
# install.packages("vegan")
library(vegan)

data("varespec")       # load both varespec and varechem
data("varechem")
dim(varespec)        # 24 plots × 44 species

# Note: Convert percentage cover to presence/absence (1/0) first,
#       then sum across species to get richness per plot.
varespec01 <- ifelse(varespec > 0, 1, 0)
varediv    <- apply(varespec01, 1, sum)   # species richness per plot

#Is the number of species normally distributed? Compare in a histogram
hist(varediv,
     main = "Distribution of species richness",
     xlab = "Number of species per plot")


# -> 2. Check for multicollinearity among predictors
# Note: Highly correlated predictors may lead to unreliable
#       coefficient estimates. Inspect the correlation matrix before
#       building any model.
round(cor(varechem), d = 2)


# -> 3. Loop over all predictors to identify significant candidates
# Note: Fitting each predictor in isolation ("statistical fishing")
#       gives a first filter. This does not replace proper model
#       selection — it only flags variables worth including together.
modelresults <- matrix(ncol = 4, nrow = ncol(varechem))

for (i in 1:14) {
  model <- lm(varediv ~ varechem[, i])
  modelresults[i, 1] <- colnames(varechem)[i]
  modelresults[i, 2] <- round(summary(model)$coefficients[2, 1], d = 4)
  modelresults[i, 3] <- round(summary(model)$coefficients[2, 4], d = 10)
  modelresults[i, 4] <- round(summary(model)$r.squared,          d = 2)
  print(i)
}

colnames(modelresults) <- c("name", "Estimate", "p", "r2")
modelresults
# Note: Mn, Baresoil, and Humdepth emerge as significant candidates.


# -> 4. Fit the full model with significant predictors
fullmodel <- lm(varediv ~ varechem$Mn + varechem$Baresoil + varechem$Humdepth)
summary(fullmodel)


# -> 5. Check for redundancy using VIF
# Note: VIF (variance inflation factor) quantifies multicollinearity.
#       Values below 5-10 indicate no problematic redundancy between
#       the retained predictors.
library(car)
vif(fullmodel)


# -> 6. Reduce the model step-by-step using p-values
# Note: Remove the least significant term (highest p-value) one at
#       a time and refit.
model2 <- 

# Note: Remove the least significant term (highest p-value) one at
#       a time and refit.
model3 <- 


# -> 7. Use step() as an automated alternative
# Note: step() uses AIC at each step to decide whether dropping a
#       term improves the model. It may reach a different conclusion
#       than manual p-value elimination — compare the two.
model4 <- step(fullmodel)
summary(model4)   # inspect — is this the same minimum adequate model?


# -> 8. Compare all candidate models with AIC
# Note: Lower AIC = better balance of fit and complexity.
#       A difference of less than 2 AIC units is considered negligible,
#       meaning both models are essentially equivalent.
AIC(fullmodel); AIC(model2); AIC(model3)
# Note: model2 has the lowest AIC, yet the difference from model3
#       is small (< 2), so one could make a case for either of the 
# 			the models to be the least parsimonious ones.


# ── PART 2: Crop yield (mixed-effect model) ──────────────────────


# -> 9. Load data, convert to factors, fit the full mixed-effect model
# Note: method = "ML" (maximum likelihood) is required whenever you
#       compare models with different fixed effects using AIC or
#       likelihood-ratio tests. The default REML is fine for final
#       parameter estimates but makes fixed-effect comparisons invalid.
library(nlme)
data(npk)

npk$block <- as.factor(npk$block)
npk$N     <- as.factor(npk$N)
npk$P     <- as.factor(npk$P)
npk$K     <- as.factor(npk$K)

fullmodel <- lme(yield ~ N * P * K, random = ~1 | block, data   = npk, method = "ML")
summary(fullmodel)


# -> 10. Reduce the model step-by-step by using p-value
# Note: Remove terms from the most complex (three-way interaction)
#       downward, refitting after each removal. Keep method = "ML"
#       throughout so all AIC values remain on the same scale.
model2 <- 


# -> 11. Compare minimum adequate model against a classical ANOVA 
# with Error(block).
# Note: The ANOVA with Error(block) is the classical split-plot
#       approach. Results should be broadly similar to the mixed-
#       effect model, but are not identical — the mixed model
#       explicitly estimates the block variance rather than
#       absorbing it into the error stratum.
anovamodel <- 


# -> 12. Compare all candidate models with AIC
# Note: All models were fitted with ML, so AIC comparisons are valid.
#       The model with the lowest AIC is preferred; differences < 2
#       are treated as negligible.
AIC(fullmodel); AIC(model2); 

# Are the most parsimonious model according to p-value reduction and AIC the same?


# ❓ QUESTIONS 
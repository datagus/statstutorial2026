#==================================================================
#
#           🌾  SECTION 1 — Split-Plot ANOVA
#       Can sustainable farming practices improve crop yield?
#
#==================================================================

# BACKGROUND -------------------------------------------------------
#
#   You are working at a sustainable agriculture consultancy.
#   A client — a cooperative of smallholder farmers — wants to know
#   how three low-cost, field-level decisions affect how much grain
#   their crop produces (yield, in mg per plot). Getting this right
#   matters: over-irrigating wastes water in drought-prone regions,
#   over-fertilizing pollutes nearby waterways, and wrong planting
#   densities cut profits and increase soil compaction.
#
#   The experiment was run across 4 large field blocks. Each block
#   was divided into irrigation strips, each strip was sub-divided
#   for planting density, and each sub-plot received one of three
#   fertilizer types. This layered structure is called a split-plot
#   design. Blocks absorb unavoidable field-to-field variation
#   (slope, soil texture, shade) so it does not inflate the error.
#
#   Variables in the dataset:
#
#     🏗️  block       – one of 4 field sections (nuisance factor)
#     💧  irrigation  – irrigated vs non-irrigated
#     🌱  density     – low vs high planting density
#     🧪  fertilizer  – fertilizer type: N, P, or NP
#     📦  yield       – grain yield per plot (mg)  ← response
#
#   Your job: build the most parsimonious ANOVA model that explains
#   yield while correctly accounting for the block structure. The
#   result will tell the cooperative which practices to adopt —
#   and which expensive inputs they can safely skip.
#
# ------------------------------------------------------------------

# YOUR TASK --------------------------------------------------------
#
#   1. Load the data, convert categorical columns to factors,
#      and inspect the experimental design.
#   2. Check ANOVA preconditions: normality and homoscedasticity.
#   3. Build models from full to minimum adequate, using Error(block)
#      to correctly partition block variation.
#   4. Run a Tukey post-hoc test and inspect residuals.
#   5. Explore how a biased Error() structure can distort results
#      (the Monsanto scenario).
#
#   # Note: comments explain non-obvious choices throughout.
# ------------------------------------------------------------------


# -> 1. Import data, convert to factors, attach and inspect
crops <- read.table("splityield.txt", header = TRUE)

# Note: Convert to factor before modeling — otherwise ANOVA treats
#       these as continuous and gives wrong results.
crops$block      <- as.factor(crops$block)
crops$irrigation <- as.factor(crops$irrigation)
crops$density    <- as.factor(crops$density)
crops$fertilizer <- as.factor(crops$fertilizer)

attach(crops)
str(crops)
summary(crops)


# -> 2. Verify the design is balanced and visualize its structure
ftable(block ~ irrigation + density + fertilizer)

mosaicplot(~ block + irrigation + density + fertilizer,
           data     = crops,
           color    = c("brown", "orange", "yellow"),
           cex.axis = 0.6,
           las      = 0,
           main     = "Split-plot experiment design")


# -> 3. Check preconditions: normality and homoscedasticity
hist(yield,
     main = "Distribution of crop yield",
     xlab = "Yield (mg)")
shapiro.test(yield)

boxplot(yield ~ irrigation * density * fertilizer,
        las      = 2,
        cex.axis = 0.5,
        xlab     = "",
        main     = "Yield by treatment combination")


# -> 4. Run the null model
null_model <- aov(yield ~ 1)
summary(null_model)


# -> 5. Run the full model (ignoring block)
fullmodel <- aov(yield ~ irrigation * density * fertilizer)
summary(fullmodel)


# -> 6. Run the ANOVA including block as a fixed effect
boxplot(yield ~ block,
        main = "Yield by block",
        xlab = "Block",
        ylab = "Yield (mg)")

fullmodel_b <- aov(yield ~ block + irrigation * density * fertilizer)
summary(fullmodel_b)


# -> 7. Run the ANOVA with block as Error
fullmodel_e <- aov(yield ~ irrigation * density * fertilizer + Error(block))
summary(fullmodel_e)
# Note: Error(block) puts block into its own stratum. The summary
#       shows two strata — "Error: block" and "Error: Within".


# -> 8. Remove the three-way interaction
twoway_model <- aov(yield ~ irrigation + density + fertilizer
                    + irrigation:density
                    + irrigation:fertilizer
                    + density:fertilizer
                    + Error(block))
summary(twoway_model)


# -> 9. Find the minimum adequate model
# Note: density:fertilizer is not significant — remove it.
min_model <- aov(yield ~ irrigation + density + fertilizer
                 + irrigation:density
                 + irrigation:fertilizer
                 + Error(block))
summary(min_model)


# -> 10. Post-hoc comparisons with Tukey HSD
# Note: TukeyHSD() requires aov() without Error() — re-fit without it.
min_model_tukey <- aov(yield ~ irrigation + density + fertilizer
                       + irrigation:density
                       + irrigation:fertilizer)
summary(min_model_tukey)
TukeyHSD(min_model_tukey)


# -> 11. Check residuals
hist(resid(min_model_tukey),
     main = "Residuals — minimum adequate model",
     xlab = "Residuals")

plot(resid(min_model_tukey),
     ylab = "Residuals",
     main = "Residuals vs observation order")
abline(h = 0, col = "red")

plot(min_model_tukey, which = 1)


# -> 12. Compare null model vs full model
# Note: anova() cannot handle Error() objects — use the non-Error versions.
anova(null_model, fullmodel)


# -> 13. Role play — you work for Monsanto
### Which of your fertilizers works best?
### If you don't want to account for variation introduced by some factors,
### you can put them in the Error.

### Full model with nested Error
evil_m1 <- aov(yield ~ irrigation * density * fertilizer
               + Error(block / irrigation / density))
summary(evil_m1)

### Remove the three-way interaction
evil_m2 <- aov(yield ~ (irrigation + density + fertilizer)^2
               + Error(block / irrigation / density))
summary(evil_m2)

### Remove the two-way interaction of density and fertilizer.
### Is this the minimum adequate model?
evil_m3 <- aov(yield ~ irrigation + density + fertilizer
               + irrigation:density
               + irrigation:fertilizer
               + Error(block / irrigation / density))
summary(evil_m3)


# ❓ QUESTIONS — Section 1: Splityield ----------------------------
#
#   1. Which of the following is the minimum adequate model for
#      crop yield in the splityield experiment?
#
#      A)  aov(yield ~ irrigation * density * fertilizer
#                     + Error(block))
#
#      B)  aov(yield ~ irrigation + density + fertilizer
#                     + irrigation:density
#                     + irrigation:fertilizer
#                     + density:fertilizer
#                     + Error(block))
#
#      C)  aov(yield ~ irrigation + density + fertilizer  # correct
#                     + irrigation:density
#                     + irrigation:fertilizer
#                     + Error(block))
#
#      D)  aov(yield ~ irrigation + fertilizer
#                     + irrigation:fertilizer
#                     + Error(block))
#
#
#   2. Using the SS values from the minimum adequate model,
#      calculate the percentage of total yield variation that
#      remains unexplained.
#
#      A)  23 %
#      B)  33 % # correct
#      C)  43 %
#      D)  53 %
#
#
#   3. The cooperative must decide whether to simply buy the best
#      fertilizer and apply it everywhere, or whether their
#      fertilizer strategy should also depend on whether a plot is
#      irrigated. Based on the minimum adequate model, which
#      recommendation is correct?
#
#      A)  Buy NP fertilizer for all plots — fertilizer works the
#          same regardless of irrigation
#      B)  Irrigate all plots — irrigation alone drives yield,
#          fertilizer type does not matter
#      C)  Fertilizer choice and irrigation should be decided  # correct
#          together — their interaction is significant
#      D)  Focus on planting density — it has the largest effect
#          on yield
#
#
#   4. A cooperative board member argues that using 4 separate
#      field blocks just complicates the experiment unnecessarily.
#      What does boxplot(yield ~ block) reveal that counters this?
#
#      A)  Yield is nearly identical across all 4 blocks —
#          the board member has a point
#      B)  Only one block stands out; the others are comparable
#      C)  Yield differs across blocks, suggesting that field-to-field  # correct
#          variation is real and worth accounting for
#      D)  Block explains more variation than any treatment factor
#
#
#   5.  Why would a fertilizer company prefer to report evil_m3n instead of min_model?
#
#      A)  evil_m3 gives fertilizer a more conservative test,
#          making it harder to claim significance
#      B)  evil_m3 gives fertilizer a more favorable test,  # correct
#          making it easier to show significance
#      C)  Both models test fertilizer against the same error term
#      D)  The nested Error() only affects the irrigation term,
#          not fertilizer
#
# -----------------------------------------------------------------




#==================================================================
#
#        🚗  SECTION 2 — Unbalanced ANOVA (Type II / III)
#     Which vehicle design features drive fuel consumption?
#
#==================================================================

# BACKGROUND -------------------------------------------------------
#
#   You are a sustainability analyst at a government transport
#   agency. Your team is developing national fleet decarbonisation
#   guidelines: recommendations on which types of vehicle the
#   public sector should phase out first. A key question is whether
#   a vehicle's mechanical design — its engine layout, transmission
#   type, or number of gears — predicts how fuel-efficient it is.
#   More fuel = more CO2 = higher climate impact.
#
#   A 1974 Motor Trend survey measured fuel efficiency and design
#   specs for 32 vehicles. The 1970s oil crisis had just hit, and
#   engineers were under pressure to understand what made some cars
#   burn far more fuel than others. Your analysis re-examines that
#   data with a modern sustainability lens.
#
#   Variables (after recoding):
#
#     ⛽  fuel_eff     – fuel efficiency in miles per gallon  ← response
#     ⚙️  eng_layout   – engine layout: V-shaped vs straight
#     🔄  transmission – transmission type: automatic vs manual
#     ⚙️  n_gears      – number of forward gears: 3, 4, or 5
#
#   Critical design problem: not all combinations of these three
#   factors are equally represented in the data — some cells have
#   zero cars. This unbalanced design means that the standard
#   aov() function (which uses Type I sequential sums of squares)
#   gives answers that depend on the order you list predictors.
#   Instead, we use Anova() from the car package, which applies
#   Type II or Type III SS and handles imbalance correctly.
#
#   Your job: find the minimum adequate model for fuel efficiency
#   — the simplest model whose predictors are all significant.
#   This will tell the agency which design features matter most
#   when setting fleet replacement priorities.
#
# ------------------------------------------------------------------

# YOUR TASK --------------------------------------------------------
#
#   1. Load the dataset, rename columns, and recode factor levels.
#   2. Check whether the design is balanced — and why it matters.
#   3. Visualize fuel efficiency across the two key predictors.
#   4. Show why aov() cannot be trusted here, then use Anova().
#   5. Find the minimum adequate model through stepwise simplification.
#   6. Verify that the excluded factor genuinely adds nothing.
#
#   # Note: Use Type III when the model includes interactions,
#            Type II when there are no interactions.
# ------------------------------------------------------------------


# -> 14. Load the dataset, copy, rename columns, and attach
data(mtcars)

# Note: Work on a copy so the original stays intact.
fleet <- mtcars
names(fleet)[names(fleet) == "mpg"] <- "fuel_eff"

fleet$transmission <- factor(fleet$am,
                             levels = c(0, 1),
                             labels = c("automatic", "manual"))

fleet$eng_layout   <- factor(fleet$vs,
                             levels = c(0, 1),
                             labels = c("V-shaped", "straight"))

fleet$n_gears      <- factor(fleet$gear,
                             levels = c(3, 4, 5),
                             labels = c("3 gears", "4 gears", "5 gears"))

fleet <- fleet[, c("fuel_eff", "transmission", "eng_layout", "n_gears")]

attach(fleet)
str(fleet)


# -> 15. Check design balance
table(eng_layout, transmission)
table(eng_layout, n_gears)      # straight + 5 gears: only 1 car
table(transmission, n_gears)    # two cells with zero cars

ftable(eng_layout, transmission, n_gears)
# Note: Several cells are empty or contain a single observation.
#       This is an unbalanced design. Standard aov() uses Type I
#       (sequential) SS — results change depending on the order
#       predictors are listed. We need Anova() from the car package,
#       which applies Type II/III SS and is order-independent.


# -> 16. Visualize fuel efficiency across the two main predictors
boxplot(fuel_eff ~ eng_layout,
        main = "Fuel efficiency by engine layout",
        ylab = "Fuel efficiency (mpg)",
        xlab = "Engine layout")

boxplot(fuel_eff ~ transmission,
        main = "Fuel efficiency by transmission type",
        ylab = "Fuel efficiency (mpg)",
        xlab = "Transmission")

mosaicplot(~ transmission + eng_layout + n_gears,
           data     = fleet,
           color    = TRUE,
           cex.axis = 0.6,
           las      = 0,
           main     = "Fleet design structure")


# -> 17. Show the aov() limitation, then run Anova() with Type III SS
model_aov <- aov(fuel_eff ~ eng_layout * transmission * n_gears)
summary(model_aov)
# Note: Looks reasonable, but these SS values shift if you reorder
#       the predictors. Do not use this output for interpretation.

#install.packages("car")
library(car)

# Full model — Type III
model_1 <- Anova(lm(fuel_eff ~ eng_layout * transmission * n_gears,
                    data = fleet),
                 type = "III")
model_1
# Note: The three-way model fails — not enough statistical power.
#       Use type III when you have interactions, type II otherwise.


# -> 18. Remove transmission — test eng_layout × n_gears (Type III)
model_2 <- Anova(lm(fuel_eff ~ eng_layout * n_gears,
                    data = fleet),
                 type = "III")
model_2


# -> 19. Remove eng_layout:n_gears interaction (Type II)
model_3 <- Anova(lm(fuel_eff ~ eng_layout + n_gears,
                    data = fleet),
                 type = "II")
model_3


# -> 20. Test eng_layout × transmission (Type III)
model_4 <- Anova(lm(fuel_eff ~ eng_layout * transmission,
                    data = fleet),
                 type = "III")
model_4


# -> 21. Remove eng_layout:transmission interaction (Type II)
model_5 <- Anova(lm(fuel_eff ~ eng_layout + transmission,
                    data = fleet),
                 type = "II")
model_5
# Note: Both predictors are significant — this is the Minimum
#       Adequate Model. Straight engines and manual transmissions
#       are each independently associated with higher fuel efficiency.


# -> 22. All three factors, no interactions (Type II)
model_6 <- Anova(lm(fuel_eff ~ eng_layout + transmission + n_gears,
                    data = fleet),
                 type = "II")
model_6
# Note: n_gears is not significant when the other two predictors
#       are already in the model — confirmed excluded from the MAM.


# ❓ QUESTIONS — Section 2: Vehicle fleet -------------------------
#
#   1. The agency initially runs the full three-way model with
#      standard aov(). A colleague warns the results cannot be
#      trusted. What is the core problem with this approach here?
#
#      A)  aov() cannot handle more than two factors at once
#      B)  The design is unbalanced — some factor combinations have  # correct
#          zero or one observation, making Type I SS unreliable
#      C)  The response variable is not normally distributed
#      D)  aov() requires a blocking variable, which is absent here
#
#
#   2. The agency wants to know whether engine layout and
#      transmission type interact — i.e., whether a V-shaped engine
#      is especially inefficient only in combination with a specific
#      transmission type. Based on model_4, what is the answer?
#
#      A)  Yes — the interaction is significant; the two features
#          must always be considered together
#      B)  No — the interaction is not significant; each design  # correct
#          feature has an independent effect on fuel efficiency
#      C)  The model cannot be evaluated — the design is too sparse
#      D)  Only engine layout matters; transmission is not significant
#
#
#   3. A data analyst proposes keeping n_gears in the final model
#      to give the agency a more complete picture of what drives
#      fuel efficiency. Based on model_6, is this justified?
#
#      A)  Yes — n_gears is significant once the other two
#          predictors are controlled for
#      B)  No — n_gears is not significant in model_6; it does not  # correct
#          improve the model once eng_layout and transmission
#          are already included
#      C)  Yes — a model with more predictors always explains
#          more variance and should be preferred
#      D)  No — n_gears has too many levels to be modelled with
#          this sample size
#
#
#   4. The agency's report must recommend which type of vehicle to
#      phase out first on fuel efficiency grounds. Based on the
#      Minimum Adequate Model (model_5), which category should
#      top the replacement list?
#
#      A)  Manual transmission with a straight engine — the least
#          common combination in the fleet
#      B)  Automatic transmission with a V-shaped engine — both  # correct
#          features are independently linked to lower fuel
#          efficiency
#      C)  Any vehicle with 3 gears — gear count is the strongest
#          predictor of fuel efficiency
#      D)  Automatic transmission with a straight engine — the
#          interaction between these two features makes them
#          the worst combination
#
# -----------------------------------------------------------------

#==================================================================
#                        🧪 ANOVA Exercise
#             What's driving differences in crop yield?
#==================================================================

# BACKGROUND ------------------------------------------------------ 
#
#   Same dataset, new lens.
#
#   Previously you tested whether SOIL TYPE explains yield variation.
#   Now shift your focus:
#
#     🧪  FERTILIZER  – does the type of fertilizer applied matter?
#
#   Three fertilizer types were used across the plots:
#     🌿  lushy   – organic fertilizer
#     🏔️  giant   – synthetic high-yield fertilizer
# -----------------------------------------------------------------

# YOUR TASK -------------------------------------------------------
#
#   Run a one-way ANOVA with yield as the response (dependent) variable
#      and fertilizer as predictor
#
#   Remember:
#   1. Check the preconditions to run the ANOVA
#
#   2. Inspect the ANOVA table carefully
#
#   3. Check the residuals of the ANOVA model
#
#   4. Answer the questions at the end based on the generated output
#
# Note: You are going to be guided via pseudocode and only the code
#       that you haven't seen so far will be explicitly written.
# -----------------------------------------------------------------

# YOUR CODE GOES HERE =============================================

# -> 1. Import the data


# -> 2. Attach the data


# -> 3. Inspect the data


# -> 4. Precondition 1 (normality): Is the response variable normally distributed?



# -> 5. Precondition 2 (balanced design): Are there the same number of samples per fertilizer type?




# -> 6. Precondition 3 (homoscedasticity): Is the variance within fertilizer groups similar?



# -> 7. Precondition 4 (independence): Are the observations independent?


# -> 8. Convert fertilizer variable as factor.
# When running ANOVAs and other modelling, converting categorical variables as factor is best practice
production$fertilizer <- factor(fertilizer, levels = c("lushy", "giant"))

# -> 9. Run the ANOVA model


# -> 10. Print the summary of the ANOVA model
# Hint: Use summary function


# -> 11. Evaluate the normality of the residuals
# Hint: To retrieve residuals, use resid() with your ANOVA model as argument.



# -> 12. Create a scatter plot of the residuals. Do you see stars in the sky?
# Hint: Use resid() nested inside plot()



# -> 13. Create a scatter plot of the fitted values vs the residuals.
#        Are the mean of the residuals in each group close to zero?


# -> 14. Run a Post Hoc test to compare all fertilizer types against each other



# ❓ QUESTIONS ----------------------------------------------------
#
#   1. When running the test for normality, what is the p-value obtained?
#     Is the data normally distributed?
#
#      A)  0.5993 and yes the data is normally distributed
#      B)  0.5993 and no the data is not normally distributed 
#      C)  0.9721 and yes the data is normally distributed
#      D)  0.05 and yes the data is not normally distributed
#----------------------------------------------------------------
#   2. Is the difference between "lushy" and "giant" fertilizer
#      statistically significant at α = 0.05?
#
#      A)  Yes, and the yield is higher with lushy
#      B)  Yes, and the yield is lower with lushy
#      C)  No, the p-value is above 0.05
#      D)  Cannot be determined from the output

# -----------------------------------------------------------------
#
#   3. What percentage of the total variation in yield is
#      explained by fertilizer type?
#
#      A)  2.89%
#      B)  28.9%
#      C)  97.1%
#      D)  36.8%
#
# -----------------------------------------------------------------
#    4. Considering the variation explained by soil type, 
#       what percentage approximately of the total variation in yield remains unexplained?
#
#      A)  26 %
#      B)  24 %
#      C)  73 % 
#      D)  76 %

# -----------------------------------------------------------------
#
#    5. A colleague claims: "Since the variation in yield due to fertilizer is insignificant,
#       soil should be accounted as unique source of variation".
#      Based on your earlier soil ANOVA and this one, is this claim valid?
#
#      A)  Yes – soil is significant and sufficient to explain yield
#      B)  No  – fertilizer also explained a significant portion
#      C)  No  – neither variable was significant
#      D)  No  – we haven't accounted for the interaction of soil and fertilizer
#
# -----------------------------------------------------------------
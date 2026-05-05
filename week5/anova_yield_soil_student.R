#==================================================================
#                        🌱 ANOVA Exercise
#             What's driving differences in crop yield?
#==================================================================

# BACKGROUND ------------------------------------------------------ 
#
#   A researcher measured crop YIELD across different plots.
#   Two factors were manipulated:
#
#     🌍  SOIL TYPE   – does the type of soil matter?
#     🧪  FERTILIZER  – does the fertilizer applied matter?
#
#   Your job: run an ANOVA and interpret the results!
# ----------------------------------------------------------------- 

# YOUR TASK -------------------------------------------------------
#
#   Run a one-way ANOVA with yield as the response(dependent) variable
#      and SOIL type as predictor
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
#       that you haven't seen so far will be explicity written.

# YOUR CODE GOES HERE =============================================

# -> 1. Import the data
production <-read.table("yields.txt",header=T)

# -> 2. Attach the data


# -> 3. Inspect the data


# -> 4. Precondition 1 (normality): Is response/dependent variable normally distributed?



# -> 5. Precondition 2 (balanced design): Are there the same number of samples in each soil type?
# Hint: use the table function.



# -> 6. Precondition 3 (homoscedasticity): Is the variance within soil tyoes similar?
# Hint: create and visualize a boxplot


# -> 7. Precondition 4 (independence): Are the observations independent?
# Hint: You don't need code for this.

# -> 8. Convert soil variable as factor.
# When running anovas and other modelling, converting categorical variables as factor is best practice
production$soil<-factor(soil,levels=c("sand","clay","loam"))

# -> 9. Run the ANOVA model
# There are two possibilities to calculate ANOVA
# Option 1: Calculating ANOVA as a linear model
model_1 <-lm(yield~soil)
anova(model_1)

# Option 2: Applying directly the ANOVA function
model_2<-aov(yield~soil)

# -> 10. Print the summary of the ANOVA model:
#Hint: Use summary function


# -> 11. Evaluate the normality of the residuals
# Hint: To retrive residuals, you have to use the function resid() and put your anova model as an argument.
hist(resid(model_2))


# -> 12. Create a scatter plot of the residuals. Do you see stars in the sky?
# Hint: Use function resid() nested inside plot()

abline(h=0, col="red")

# -> 13. Create a scatter plot of the fitted values vs the residuals. Are the mean of the residuals in each group close to zero?
plot(model_2, which=1)

# -> 14. Run a PostHoc test
# Note: You have to apply directly the ANOVA function (not using lm() function) 
# for a posthoc-Test = to test every factor level against each other
TukeyHSD(model_2)

# ❓ QUESTIONS ----------------------------------------------------
#
#   1. How much of the total variation in yield is explained
#      by soil type alone?
#
#      A)  0.025 %
#      B)  23.92 % 
#      C)  99.2 %
#      D)  75.96 %
# -----------------------------------------------------------------
#
#   2. On average, how much does yield differ between
#      sand and clay soil?
#
#      A)  1.6
#      B)  0.9
#      C)  4.4
#      D)  8.2
# -----------------------------------------------------------------
#
#   3. What is the mean of the residuals from your ANOVA model?
#
#      A)  0.3
#      B)  0 
#      C)  -1.8
#      D)  0.01
# -----------------------------------------------------------------
#
#   4. Based on the residuals from your ANOVA model,
#      which observation has the highest absolute residual value?
#
#      A)  loam,  9,  lushy
#      B)  sand,  6,  lushy
#      C)  loam, 18,  giant
#      D)  clay,  3,  lushy  
# -----------------------------------------------------------------



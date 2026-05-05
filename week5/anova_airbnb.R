####------------3. Airbnb example-------------#####
# Load airbnb data and attach it
air<-read.csv("airbnb.csv",header=T, sep=",")
attach(air)
str(air)

# Step 1: Visually inspect your data with boxplots
# Which variables could be your factors and which your dependent variable?

boxplot(price~city) #It looks nice, doesn't it?

# Better try log() transformation
boxplot(log(price)~city) # better right?
table(city)

# Let's narrow down the data and consider only the following cities: Amsterdam, Barcelona, London and Paris
# don't get stressed with this code section
cities_subselected <- c("Amsterdam", "Barcelona", "London", "Paris") # defining cities
number_rows <- 859 #859 is the total number of rows or datapoints corresponding to Amsterdam

# Define the function
get_first_rows <- function(city_name, number_rows) {
  city_data <- subset(air, city == city_name)
  city_data[1:min(number_rows, nrow(city_data)), ]
}

# Applying function
air2 <- do.call(rbind, lapply(cities_subselected, function(city) {
  get_first_rows(city, number_rows)
}))

detach(air)
attach(air2)

# Inspecting again

table(city)

boxplot(price~city, las = 2, cex.axis = 0.8)
boxplot(log(price)~city, las = 2, cex.axis = 0.8)

# Check the normality of price
hist(price)

hist(log(price)) #let's take this one

# Converting into factor
air2$city <- factor(air2$city)

# Running anova
modelair <-aov(log(price)~city)
summary(modelair)

# Checking the residuals
hist(resid(modelair))

#Now PostHoc Test
TukeyHSD(modelair)

# Which pair of cities can we conclude that have similar prices on average?

# Challege: now test if price significantly differs between Amsterdam, Barcelona, Berlin, Budapest, Vienna with an ANOVA
cities_subselected <- c("Athens", "Budapest", "Lisbon", "Rome", "Vienna")
number_rows <- 1000 

# Applying function
air3 <- do.call(rbind, lapply(cities_subselected, function(city) {
  get_first_rows(city, number_rows)
}))

detach(air2)
attach(air3)

air3$city <- factor(air3$city)
table(city)

####-------HOMEWORK----------######
# With same subselected dataframe "air3", perform an anova regarding:
# price and bedrooms, 
# price and day, 
# price and room_Type
# price and person_capacity



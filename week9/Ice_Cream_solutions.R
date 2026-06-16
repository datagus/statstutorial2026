########## Tutorial Day 9 - Graphics ##########

# About the data set: The data was collected on 200 random citizens.
# It includes scores on a video game and a puzzle as well as the person’s
# favorite ice cream flavor – vanilla (1), chocolate (2) or strawberry (3).

# -> 1. Set the working directory and load the data
getwd()
setwd()

ice<-read.table("Ice_cream.txt",header=T)

# -> 2. Inspect the structure of the dataset
str(ice)
names(ice)
summary(ice)
head(ice)
attach(ice)

# -> 3. Look at the help function of different graphics
?plot
?boxplot
?hist
?barplot
?par

# ask students: what is the difference between a histogram and a barplot

# -> 4. First exercise: re-create the graphs you see up front

#first graph has purple triangles, main title, x axis and y axis label

#second graph has three different colors, a x axis an y axis label

#third graph has three different colors, main tittle, y axis label and a legend
# for this graph, you may use this cool colour palette
install.packages("wesanderson")
library(wesanderson)
?wes_palette
pal<-wes_palette("Moonrise3",3,type="discrete")

##### SOLUTIONS #####
#1st graph
plot(puzzle,video,ylab="video score",xlab ="puzzle score",main="Video vs. puzzle score",pch=17,col="purple")

#2nd graph
boxplot(video~ice_cream, xlab="ice cream flavor",ylab="video score",col=c("white","chocolate","pink"),boxwex=0.5)

#3rd graph
pal<-wes_palette("Moonrise3",3,type="discrete")
barplot(table(puzzle_type,ice_cream),beside=T,main="Puzzle type meets ice cream flavour",ylab ="number of people",col=pal,legend.text=TRUE)
barplot(table(id,ice_cream))

#changing the labels to the names of the ice cream flavour
barplot(table(puzzle_type,ice_cream),names=c("vanilla","chocolate","strawberry"),col=pal)

#sorting puzzle types
boxplot(puzzle~as.factor(factor(puzzle_type,levels=c("rookie","average","pro"))))

# -> 5. Second exercise
#Create a graph with this data set that is colorful and use a diversity of symbols!



# -> 6. Just to show: Plot airbnb data with ggplot
# ggplot2 is a package for generating all kinds graphics (also advanced ones)
# it uses a specific kind of coding framework that we do not require you to learn,
# but we rather want to show you some possibilities

# First, install ggplot package. Installation only necessary once
install.package("ggplot2")
# Load library. Loading a library is always necessary when you want to use the code
library(ggplot2)

air<-read.csv("airbnb.csv",header=T, sep=",")
attach(air)
str(air)

# for each city distances to nearest metro station and city center, each listing colorized by its price
pal2<-wes_palette("GrandBudapest1",3,type="discrete")
pal2
ggplot(air, aes(x = distance, y = metro_dist, color = price)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(low = pal2[1], mid = pal2[2],high = pal2[3]) +
  labs(title = "AirBnB prices in 10 European cities",
       subtitle = "price listings with distance to city center and nearest metro",
       x = "Distance to city center (km)",
       y = "Distance to nearest metro (km)",
       color = "Price (EUR)") +
  facet_wrap(~ city) +
  theme_minimal() +
  theme(legend.position = "bottom")

# boxplots for prices by room type and superhost status
ggplot(air, aes(x = room_type, y = price, fill = factor(superhost))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "orange"),
                    labels = c("0" = "Host", "1" = "Superhost")) +
  labs(title = "AirBnB prices by room type and superhost status",
       x = "room type",
       y = "price (EUR)",
       fill = "superhost status") +
  theme_minimal() +
  theme(legend.position = "bottom")


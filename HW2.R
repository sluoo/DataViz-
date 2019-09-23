library(tidyverse)
library(ggplot2)

## This should not be part of a script that you share, because not everyone has the same directory structure.
## setwd("~/Stat744")

#Import Data
df <- read.csv("vaccine.csv")

###QUESTION 
#Write a short statement (a few sentences) that explains what question you think the 
#graphic is trying to answer, or what pattern it's trying to display

#I believe the visualization is trying to illustrate that the introduction of vaccines has
#drastically decreased the number of cases of several infectious diseases. It is visualizing 
#the number of cases in a form a circle. Everyone can see the circles shrinking as the years 
#advance along with when the licensing of the vaccine was established denoted by yellow circles. 

length(unique(df$disease)) #9 diseases 

#Extract vaccination licensing information
#Create new data frame
index <- which(df$vaccine != FALSE)
vac1 <- df$vaccine[index]
year1 <- df$year[index]
disease1 <-df$disease[index]
count1 <- df$cases[index]
df2 <- data.frame(disease1,year1,count1,vac1)
df2<- df2[-c(4,9,12),] #remove repeats
df2 <- df2[,c(1:3)]

## How did you choose the order of the diseases? Could you have done a better job? What about the colors?
## Did you consider log vs.~linear scaling?

#First plot 
plot1 <- ggplot(df, aes(x=year,y=cases/1e5,color=disease)) + 
  geom_line(size=0.5) + 
  geom_point(df2,mapping = aes(x=year1,y=count1/1e5,color=disease1), size=2, shape=16) +
  ylab("Cases in 100,000") + 
  xlab("Years") + 
  ggtitle("Number of reported cases with year of vaccine formulation") +
  scale_x_continuous(breaks=seq(1945,2015,by=10)) +
  theme_bw(base_size = 12, base_family = "Times")

plot1

## Nice job with the rescaling (100,000) and with the scale_size_area.
#Another figure 
plot2<- ggplot(df, aes(x = year, y = disease, size = cases/1e5)) + 
  geom_point(shape = 21, colour = "black", fill = "#40b8d0") + 
  scale_x_continuous(breaks = seq(1945, 2015, by  = 10)) +
  ylab("Diseases") + 
  xlab("Year") + 
  ggtitle("Number of reported cases with year of vaccine formulation")+
  labs(size = "Cases (in 100,000)") + 
  scale_size_area(max_size = 15) + 
  geom_point(data=df2,aes(x=year1, y=disease1, size=count1/1e5), shape = 21, colour = "black", fill = "yellow")+
  theme_bw(base_size = 12, base_family = "Times")

plot2

### QUESTION
#Explain (in a few sentences) why your graph or graphs answer the question better than the 
#original graph

#My first graph is much better than the second graph or the original graph. 
#Though the second and original graph seem to be more visually appealing,
#the information is not very clear or insightful compared to my first graph.
#The size of the circles is incorrectly proportioned and the overlapping of the circles
#makes it difficult to differentiate each year. 
#In comparison, the first graph allows us to observe and understand which diseases were 
#more prevalent in the US during specific time periods whereas the circles are a little 
#less obvious. Additionally, the overall dynamics of each disease is easier to interpret 
#compared to the circles. For example, the rise and fall of measles is clearer in 
#the first graph. 

## JD 2/3 (Good)

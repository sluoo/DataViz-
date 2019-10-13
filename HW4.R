library(tidyverse)
#devtools::install_github("johannesbjork/LaCroixColoR")
library(LaCroixColoR)
library(readr)

#Source: 
#https://doi.org/10.1080/10705510701575396


df <-read_csv("simulationsLCA.csv")

#tidy dataset
df1 <- (df 
  %>% pivot_longer(cols=3:22,names_to="Class-IC",values_to="pct")
  %>% separate(col="Class-IC",into = c("Class","IC"),sep="-")
  %>% mutate(
        SampleSize=as.factor(SampleSize),
        IC=forcats::fct_inorder(IC),
        Class=forcats::fct_inorder(Class)))
 
#Re-labelling for clarity
#State the true population parameter for each model
df1 <- (df1 
        %>% mutate(
          PopulationLC = factor(PopulationLC,levels=sort(unique(PopulationLC)),
                                labels = c("True Model K=3", 
                                           "True Model K = 4"))))
df1 <-(df1
       %>% mutate(
         Simulation = factor(Simulation,levels = paste0("Sim",c(4,1:3)),
                              labels = c("10-Item (CUQ)",
                                         "8-Item (SQ)",
                                         "8-Item (SUQ)",
                                        "15-Item (SE)"))))

#Plot, label bars where percentage is greater than 10
#Also change width of bar so it's not so squished
g1 <- ggplot(df1, aes(x=SampleSize,y=pct, fill=Class)) + 
  geom_bar(stat="identity",width=0.9,color="black",lwd=0.1) + 
  facet_grid(PopulationLC + Simulation ~ IC) +
  geom_text(aes(label=ifelse(pct >=10, paste0(pct,"%"),"")),size=3,
            position=position_stack(vjust=0.5), colour="black")

#Nice white theme to make the colors pop
g1 <- g1 + 
  scale_fill_manual(values=lacroix_palette("PassionFruit",type="discrete")) + 
  labs(x="Sample Size", y= "Percentage") +
  ggtitle("Percentage of Times the Lowest Value Occured for 500 replications in each model") + 
  theme_classic() + theme(legend.position="bottom")

g1 


#Explaintion of design choice: 

#Since we are comparing proportions, I believe a stacked bar plot would represent the results of 
#the simulation nicely. The main goal of the study was to establish if a superior fit index for class 
#enumeration existed. I felt placing the ICs at the top x-axis would best highlight this goal.
#The true number of latent classes was also labelled with each model for more clarity. For a cleaner appearance, 
#I chose the classic theme and outlined the bars in black. I added labels since some of the values
#were difficult to differentiate such as graph row 1 column 1. I avoided plotting labels for values
#less than 10 since they weren't too important. 











library(tidyverse)
#devtools::install_github("johannesbjork/LaCroixColoR")
library(LaCroixColoR)
library(readr)

#Source: 
#https://doi.org/10.1080/10705510701575396
# https://www.tandfonline.com.libaccess.lib.mcmaster.ca/doi/full/10.1080/10705510701575396
#Table 7

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
                                           "True Model K=4"))))
df1 <-(df1
       %>% mutate(
         Simulation = factor(Simulation,levels = paste0("Sim",c(4,1:3)),
                              labels = c("10-items (C-Ueq)",
                                         "8-items (S-Eq)",
                                         "8-items (S-Ueq)",
                                        "15-items (S-Ueq)"))))

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

## JD: I can't figure out what's going on here at all. What kind of proportions are you comparing? What do you think you found? Which part of this is statistical inference? Please summarize the study (don't rely on me to read a long, deep paper (which you didn't even ask me to read, I'm glad I even noticed the "source" link), and some more explanation, and resubmit.

## 
 ######UPDATE with JD comments
###

#Information criterions (ICs) were examined over a range of modelling conditions to 
#determine which IC best recovered the correct number of latent classes.

#4 populations models with the known number of classes were considered that varied
#in number of items, model complexity (simple-S/complex-C) and class size (equal-E or unequal (Ueq)). 
#Samples of size 200, 500 and 1000 were generated from these structures with 500 replications each. 

#For each replication, LCA models were fit testing 2- through 6- class solution and AIC, CAIC, BIC and 
#adjusted BIC were recorded for each analysis. Proportions reflect the number of times where the lowest 
#values occurred across each model for each IC. 

#For example, looking at the "8-item(S-Eq)" model (which is a true 4-class solution), for sample size 500,
#the lowest values of AIC occurred at the 4-class model 41% of the time and 100% of the CAIC, BIC and 
#adjusted BIC. 

#We infer that the AIC is the worst of all ICs, consistently overestimating the number of
#latent classes across modelling conditions. It seems the ABIC is the best followed by BIC. 
#Also, as sample size decreased so did accuracy for all ICs. Further conclusions can be made related 
#the structures of the model, but I will end it here since it requires more explanation of 
#latent class models. 

## JD: Explanation is clearer. I'm not completely convinced clear that this is good dataviz. It's not always immediately obvious which answers are right, and the patterns of who does better are not so easy to pick out. It's true that there's fun stuff that you can pick out with the stacked bar, but given that your explanation focused only on correct answers, you could convey that information much more clearly by focusing on that aspect, I think.







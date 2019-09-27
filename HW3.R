library(ggplot2)
theme_set(theme_bw())
library(readr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggpubr)

#SOURCE from Table 1 : 
#Dunn (2006) https://doi.org/10.1093/aje/kwj100

#Responses to back pain survey
#Paper used Latent Class Analysis 
df <- read_table2("LCAdataHW3.txt") 
df <- df[,-6] #remove last column 


#Create 3 different graphs based on each response
#First sort the different levels of pain for each month
pain.f <- function(data,pain){
  d <- data %>% filter(str_detect(Items,pain))
}

no_pain <- pain.f(df,"No.pain") %>% melt(id.vars="Items") 
mild_pain <-pain.f(df,"Mild") %>% melt(id.vars="Items")
high_pain <-pain.f(df,"High") %>% melt(id.vars="Items")

#Order the pain level from recovery to severe/chronic pain
no_pain$variable <-factor(no_pain$variable, levels=c("C2","C1","C4","C3"))
mild_pain$variable <-factor(mild_pain$variable, levels=c("C2","C1","C4","C3"))
high_pain$variable <-factor(high_pain$variable, levels=c("C2","C1","C4","C3"))

#No Pain Plot
fp1 <- ggplot(no_pain, 
              aes(x=Items,y=value, color=variable, group=variable)) +
  geom_point() + geom_line() +
  scale_y_continuous(limits=c(0,1)) +
  labs(x="Months", y="Probability", title="Response: No Pain") +
  scale_x_discrete(labels=c("1","2","3","4","5","6")) +

p11 <-fp1 + guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  scale_color_discrete(
    name="Latent Classes",
    labels=c("Recovering","Persistent Mild","Fluctuating","Severe Chronic"))

#Mild Pain Plot 
fp2 <- ggplot(mild_pain, aes(x=Items,y=value, color=variable, group=variable)) +
  geom_point() + geom_line() +
  scale_y_continuous(limits=c(0,1)) + 
  labs(x="Months", y="Probability", title="Response: Mild Pain") +
  scale_x_discrete(labels=c("1","2","3","4","5","6")) 

#High Pain Plot
fp3 <- ggplot(high_pain, aes(x=Items,y=value, color=variable, group=variable)) + 
  geom_point() + geom_line() +
  scale_y_continuous(limits=c(0,1)) +
  labs(x="Months", y="Probability", title="Response: High Pain") +
  scale_x_discrete(labels=c("1","2","3","4","5","6"))

#Arrange plots nicely with one legend
plot1 <-ggarrange(p11,fp2,fp3, nrow=1,ncol=3, common.legend = TRUE, legend = "bottom")

#Annotate
annotate_figure(plot1, 
                top = text_grob("Visualizing Conditional Item Response Probabilties", face="bold",size=14))

##
  #Explain what features of the data you are trying to draw attention to,   
  #and what story you think your figures tell (or fail to tell).
##


#The table collects data from individuals with back pain. A 6-month survey was conducted 
#asking individual's level of pain each month. The responses were reported on a Likert-type 
#scale - no pain, mild-moderate pain and high pain. Interpreting and characterizing the 
#latent classes are heavily dependent on the conditional item response probabilities. I feel
#depicting these probabilities graphically eases interpretation. For a specific response, 
#you can view the pattern/trend of the probabilities across the 6 months for each latent class.
#For example, those who are in the recovering latent class (graph 1) will have a higher 
#probability of having no pain across the 6 months and those with severe/chronic pain have a 
#lower probability of responding non pain in the 6 months. Similar interpretations can be made 
#for the remaining two graphs.  Additionally, an important quality of latent class model (LCM) 
#is high class separation. In these graphs, we can distinguish between the classes thus it has 
#relatively high-class separation. This confirms with the labels/interpretation that the 
#authors have chosen. 



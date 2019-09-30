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
df <- read_table2("LCAdataHW3.txt") #datafile in repository 
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
  scale_x_discrete(labels=c("1","2","3","4","5","6"))

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

## BMB: would definitely have been better to do this by faceting ...
## did you consider using a directional colour ramp to indicate the
## ordering of the latent classes? (see below)
##
  #Explain what features of the data you are trying to draw attention to,   
  #and what story you think your figures tell (or fail to tell).
##


#The table collects data from individuals with back pain. A 6-month survey was conducted 
#asking individual's level of pain each month. The responses were reported on a Likert-type 
#scale - no pain, mild-moderate pain and high pain. Interpreting and characterizing the 
#latent classes are heavily dependent on the conditional item response probabilities. I feel
#depicting these probabilities graphically eases the interpretation. For a specific response, 
#you can view the pattern/trend of the probabilities across the 6 months for each latent class.
#For example in graph 1, those who are in the recovering latent class will have a higher 
#probability of having no pain across the 6 months compared to those in the severe/chronic pain latent class who
#have a lower probability of responding to no pain in the 6 months. Similar interpretations can be made 
#for the remaining two graphs. Additionally, an important quality of latent class models
#is high class separation. In these graphs, the classes are distinguishable from each other (no lines crossing each other)
#thus it has high-class separation. Therefore I feel more confident of the labels/interpretation that the authors have 
#given to the analysis. 


## BMB: could have indicated size of clusters via line width
library(tidyverse)
dfm <- (df
    %>% slice(-1)
    %>% separate(Items,into=c("month","cat"),sep=1)
    %>% pivot_longer(cols=C1:C4,names_to="class",values_to="pct")
    %>% mutate(
            cat=forcats::fct_inorder(cat),
            month=as.numeric(month)
        )
)

cc <- (df
    %>% slice(1)
    %>% pivot_longer(cols=C1:C4,names_to="class",values_to="n")
    %>% select(-Items)
)

dfm <- (dfm
    %>% full_join(cc,by="class")
    %>% mutate(class=factor(class,levels=paste0("C",1:4),
                            labels=c("Recovering",
                                     "Persistent mild",
                                     "Fluctuating",
                                     "Severe chronic")))
)

library(colorspace)
library(cowplot)
theme_set(theme_cowplot()+
          theme(panel.background = element_rect(fill = "#BFD5E3",
                                                colour="#EEEEEE"),
                panel.margin=grid::unit(0,"lines")))

ggplot(dfm,
       aes(x=month,y=pct, color=class)) +
    geom_point() + geom_line(aes(size=n)) +
    scale_y_continuous(limits=c(0,1)) +
    labs(x="Months", y="Probability") +
    facet_wrap(~cat) +
    scale_colour_discrete_sequential(palette = "Reds")+
    scale_x_continuous(breaks=c(1,3,6))+
    scale_size_continuous(range=c(1,4),
                          breaks=c(15,35))

## BMB: less repetition, less data manipulation
## changed background to make light colour visible
## I wonder if there's a way to reinforce the compositional nature
## of the data? (Consider Wilkie's sections on proportions)

dfm <- dfm %>% group_by(cat,month) %>%
    mutate(pct_norm=pct/sum(pct))

## BMB: I don't quite understand why the numbers don't add up to 1
##  in each column??

ggplot(dfm,
       aes(x=month,y=pct_norm, fill=class)) +
    geom_area(position="stack") +
    facet_wrap(~cat)+
    scale_y_continuous(expand=c(0,0)) +
    labs(x="Months", y="Probability") +
    scale_fill_discrete_sequential(palette = "Reds")+
    scale_x_continuous(breaks=c(1,3,6))
## interesting but I would probably flip the order

## generally a useful reformulation: score=2

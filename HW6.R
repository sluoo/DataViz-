library(tidyverse)
library(cansim)
library(stringr)

#installing gganimate
#devtools::install_github('thomasp85/gganimate')
library(gganimate)

df <- get_cansim(1710000501) 

#Population Estimate for Canada by age brackets and sex between 1971 to 2018
df1 <- (df 
        %>% select(REF_DATE, GEO, Sex, `Age group`,VALUE)
        %>% filter(GEO == "Canada", Sex != "Both sexes", 
                   str_detect(`Age group`, "to|over"),
                   REF_DATE %in% c(1971:2018))
        %>% rename(Year = REF_DATE, Geo = GEO, AgeGroup = `Age group`, Population= VALUE))

#Ages counted twice 
#Group ages by 0-4, 5-9, ... 95-99, 100 years and over, discard the rest
ages <- c(unique(df1$AgeGroup))[-c(19:33)] #strings I want to filter/order by

df2 <- (df1
        %>% filter(str_detect(AgeGroup, paste(ages, collapse = "|")))
        %>% mutate(Year = as.integer(Year), Sex = factor(Sex),
                   AgeGroup = factor(AgeGroup, levels = ages)))

p <- (ggplot(df2, aes(x=AgeGroup,y=Population,fill=Sex)) 
           + geom_col(data= subset(df2,Sex== "Males")) 
           + geom_col(data=subset(df2, Sex== "Females"), aes(y=(Population)*-1))
           + scale_y_continuous(breaks = c(-2e+06,-1e+06,0,1e+06,2e+06),
                           label=c("2M","1M","0","1M","2M"))
           + scale_fill_brewer(palette = "Set1")
           + annotate("text", x=20, y=500000, label="Males", size=4.5, color="#377eb8") 
           + annotate("text", x=20, y=-500000, label="Females", size=4.5, color="#e41a1c") 
           + coord_flip()
           + theme_classic())

print(p)

p1 <- (p 
       + labs(y= "Population in Millions",subtitle = "Population of Canada (1971 to 2018)")
       + theme(legend.position="none",axis.title.y=element_blank()))

#Animation Part
p2 <- (p1
       + labs(title = 'Year:{frame_time}')
       + transition_time(Year) 
       + ease_aes('linear'))

animate(p2, fps = 2) #slower
anim_save("myanimation1.gif")

##Explaination

#SOURCE: Table: 17-10-0005-01 (formerly CANSIM 051-0001)

# This dataset was found on the StatsCanada website which summarizes
# the demographic trends of the Canadian population over time. 
# It is clear that Canada has an aging population - majoirty 
# of people are older than 50 years old. The change between female and male are similar.

# A pyramid style bar plot is better than a stacked bar plot. Any differences/similarities 
# between the gender and age groups can be detected more clearly. Additionally, with the animated 
# component we can see which age group changes more with time. 








  